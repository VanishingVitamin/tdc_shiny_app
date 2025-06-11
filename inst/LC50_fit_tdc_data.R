# Script to fit thiamine-dependent fry mortality (TDFM) model
# Author: Joe Zemmels (jzemmels@usgs.gov)
# Adapted from script by Miles Daniels (miles.daniels@ucsc.edu)
# Do not distribute without author(s) consent
# Date last updated: 2025-04-03

# load libraries
library(rjags)
library(coda)
library(boa)

library(tidyr)
library(ggplot2)
library(dplyr)

# NOTE: change this to TRUE to make model/data creation code re-run
force_rerun <- TRUE

set.seed(20250311)

# load data using R project format
tdc_data <-
  readr::read_csv('inst/misc_data/tdc_data.csv') |>
  # remove missing values/obvious outliers that had zero survival/mortality
  filter(!is.na(Percent_survive)) |>
  filter(0 < Percent_survive & Percent_survive < 100) |>
  # remove rows missing information needed for model fitting
  filter(!is.na(N) & !is.na(N_survive)) |>
  filter(!is.na(Thiamin_conc)) |>
  mutate(
    N = round(N),
    N_survive = round(N_survive)
  )

# build data for JAGS
jags_data <-
  list(N = nrow(tdc_data),
       Nsucc = tdc_data$N_survive,
       Ntrys = tdc_data$N,
       total_thi = tdc_data$Thiamin_conc)

# See
# https://github.com/MilesEDaniels/Thiamine-Dependent-Fry-Mortality?tab=readme-ov-file#methods
# for information on 4-parameter sigmoid dose-response model
TDFM_mod <-
  "model{
    # likelihood
    for (i in 1:N){
            Nsucc[i] ~ dbin(p[i], Ntrys[i])
             p[i]<- upper_p+((0-upper_p)/(1+((total_thi[i]/ec50_p[i])^slope_p)))
            ec50_p[i] ~ dnorm(ec50_mu, 1/ec50_sigma^2)
    }
    # priors
    upper_p ~ dunif(0,1)
    slope_p ~ dunif(0,20)
    # hyperpriors
    ec50_mu ~ dunif(0,10)
    ec50_sigma ~ dunif(0,5)
}"

initial_condition = function() {
  list(
    "upper_p" = runif(1, 0.1, 1),
    "slope_p" = runif(1, 1, 20),
    # NOTE: needed to change the upper limit from 10 to 2 due to initialization
    # errors
    "ec50_mu" = runif(1, .5, 2),
    "ec50_sigma" = runif(1, 0.01, 3))
}

# fit jags model, if it doesn't exist already
if(!file.exists("inst/misc_data/jags_lc50_model_tdc_data.RData")| force_rerun){
  jags_model <-
    jags.model(textConnection(TDFM_mod),
               data = jags_data,
               n.chains = 4,
               n.adapt = 1000
               ,inits = initial_condition
    )

  save(jags_model, file = "inst/misc_data/jags_lc50_model_tdc_data.RData")
} else{
  load("inst/misc_data/jags_lc50_model_tdc_data.RData")
}

jags_model$recompile()

# sample posteriors
params = c('slope_p', 'upper_p', 'ec50_mu','ec50_sigma' )

if(!file.exists("inst/misc_data/jags_lc50_samples_tdc_data.RData") | force_rerun){
  samples <- coda.samples(jags_model ,
                          params,
                          n.iter = 100000,
                          thin = 2,
                          n.burnin = 10000)
  save(samples, file = "inst/misc_data/jags_lc50_samples_tdc_data.RData")
} else{
  load(file = "inst/misc_data/jags_lc50_samples_tdc_data.RData")
}

summary(samples)

# assess convergence
gelman.diag(samples)

# simple diagnostic plots
while (dev.cur() > 1) {
  dev.off()
}
param_names <- c("ec50_mu", "ec50_sigma", "slope_p", "upper_p")
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
# Loop through parameters and create individual traceplots
for (param in param_names) {
  traceplot(samples[, param],
            main = paste("Traceplot for", param),
            ylab = param,
            col = c("blue", "red", "green", "purple"))
}


while (dev.cur() > 1) {
  dev.off()
}
gelman.plot(samples)

dose_response <-
  function(Thiamin_conc, ec50_mu, slope_p, upper_p, lower_p = 0){
    upper_p + (lower_p - upper_p)/(1 + (Thiamin_conc/ec50_mu)**slope_p)
  }

# create quantile dataset
tdc_data <- readr::read_csv("inst/misc_data/tdc_data.csv")

thiamin_conc_range <-
  c(seq(from = 0,
        to = max(tdc_data$Thiamin_conc,na.rm = TRUE),
        by = .01),
    tdc_data$Thiamin_conc) |>
  sort()

lc50_curve <-
  samples[[1]] |>
  as_tibble() |>
  summarize(
    ec50_2.5 = quantile(ec50_mu, 0.025),
    ec50_50 = quantile(ec50_mu, 0.5),
    ec50_97.5 = quantile(ec50_mu, 0.975),
    slope_2.5 = quantile(slope_p, 0.025),
    slope_50 = quantile(slope_p, 0.5),
    slope_97.5 = quantile(slope_p, 0.975),
    upper_2.5 = quantile(upper_p, 0.025),
    upper_50 = quantile(upper_p, 0.5),
    upper_97.5 = quantile(upper_p, 0.975)
  )  |>
  bind_cols(data.frame(Thiamin_conc = thiamin_conc_range)) |>
  mutate(
    survival_ci_2.5 = 100*dose_response(Thiamin_conc, ec50_2.5, slope_2.5, 1, 0),
    survival_median = 100*dose_response(Thiamin_conc, ec50_50, slope_50, 1, 0),
    survival_ci_97.5 = 100*dose_response(Thiamin_conc, ec50_97.5, slope_97.5, 1, 0),
    plot_label = paste0("Thiamin Conc: ", Thiamin_conc," nmol/g\n",
                        "Estimated % Survived: ", round(survival_median,2),"%",
                        " (",round(survival_ci_2.5,2),"%, ",round(survival_ci_97.5,2),"%)")
  )

readr::write_csv(x = lc50_curve, "inst/misc_data/lc50_curve_tdc_data.csv")

{ggplot() +
    geom_point(data = tdc_data,
               aes(x = Thiamin_conc,
                   y = Percent_survive)) +
    geom_ribbon(
      data = ci_data,
      aes(x = Thiamin_conc,
          ymin = survival_ci_2.5*100,
          ymax = survival_ci_97.5*100)
    ) +
    geom_line(data = ci_data,
              aes(x = Thiamin_conc,
                  y = survival_median*100),
              linewidth = 1, colour = "red") +
    scale_x_continuous(limits = c(0,10))} |>
  plotly::ggplotly()

