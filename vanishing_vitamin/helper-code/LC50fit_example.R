# Script to fit thiamine-dependent fry mortality (TDFM) model
# Author: Miles Daniels (miles.daniels@ucsc.edu)
# Do not distribute without author consent
# Date last updated: 12/27/2024

# clear workspace
# rm(list=ls())

# load libraries
library(rjags)
library(coda)
library(boa)


# load data using R project format
Data <- read.csv('vanishing_vitamin/data/Total_surv_data_Nov_28_2022.csv')

# Data are:
# sample_id: unique id for given family
# run: id for run type
# year: id for brood year
# treatment: id for if family was or was not treated for thiamine
# total_thi: estimate of thiamine for a given family
# prop.mort: calculated proportion mortality in family
# prop.surv: calculated proportion survival in family
# num.mort: number of observed moralities at end point of study
# num.surv: number of observed survived at end point of study
# obs.tdc: id if there were observed symptoms of TDC in family
# Grp_Num: generated group id

# remove obvious outliers that had zero survival
Data$prop.surv = Data$num.surv/(Data$num.surv+Data$num.mort);
Data = subset(Data, prop.surv  > 0)
NoSurvivalIdx = which(Data$prop.surv == 0)

# exclude spring run 2021 as issues with experimental design
IDX = which(Data$run == 'spring' & Data$year == 2021)
Data = Data[-c(IDX), ]

# calculate values to build data
Num_trial = Data$num.mort+Data$num.surv
Obs_Surv = Data$num.surv
N_sim = length(Obs_Surv)

# build data for JAGS
jags_data = list(N=length(Data[,1]), Nsucc=Obs_Surv, Ntrys=Num_trial,total_thi=Data$total_thi)

# See
# https://github.com/MilesEDaniels/Thiamine-Dependent-Fry-Mortality?tab=readme-ov-file#methods
# for information on 4-parameter sigmoid dose-response model
TDFM_mod =
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
    "ec50_mu" = runif(1, .5, 10),
    "ec50_sigma" = runif(1, 0.01, 4))
}

jags.m = jags.model(textConnection(TDFM_mod), data = jags_data,
                    n.chains=4,n.adapt = 1000,inits=initial_condition)


# sample posteriors
params = c('slope_p', 'upper_p', 'ec50_mu','ec50_sigma' )
samples = coda.samples(jags.m , params, n.iter = 100000, thin = 2, n.burnin=10000)
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
  traceplot(samples[, param], main = paste("Traceplot for", param), ylab = param, col = c("blue", "red", "green", "purple"))
}


while (dev.cur() > 1) {
  dev.off()
}
gelman.plot(samples)

dose_response <-
  function(conc, ec50_mu, slope_p, upper_p, lower_p = 0){
    upper_p + (lower_p - upper_p)/(1 + (conc/ec50_mu)**slope_p)
  }

samples[[1]] |>
  as_tibble() |>
  slice(1:100) |>
  mutate(curve = purrr::pmap(
    .l = list(ec50_mu, slope_p, upper_p),
    function(ec50, slope, upper){
      data.frame(
        conc = seq(0,10, by = .1),
        surv = dose_response(conc = seq(0,10, by = .1),
                             ec50_mu = ec50,
                             slope_p = slope,
                             upper_p = upper,
                             lower_p = 0)
      )
    }
  ),
  curve_no = row_number()) |>
  unnest(curve) |>
  ggplot() +
  geom_line(aes(x = conc, y = surv, group = curve_no))

library(tidyverse)

ggplot() +
  geom_point(data = Data,
             aes(x = total_thi, y = prop.surv)) +
  # geom_line(
  #   data = samples[[1]] |>
  #     as_tibble() |>
  #     slice(1:100) |>
  #     mutate(curve = purrr::pmap(
  #       .l = list(ec50_mu, slope_p, upper_p),
  #       function(ec50, slope, upper){
  #         data.frame(
  #           conc = seq(0,10, by = .1),
  #           surv = dose_response(conc = seq(0,10, by = .1),
  #                                ec50_mu = ec50,
  #                                slope_p = slope,
  #                                upper_p = upper,
  #                                lower_p = 0)
  #         )
  #       }
  #     ),
  #     curve_no = row_number()) |>
  #     unnest(curve),
  #   aes(x = conc, y = surv, group = curve_no),
  #   alpha = .2
  # ) +
  geom_ribbon(
    data = samples[[1]] |>
      as_tibble() |>
      summarize(
        ech50_2.5 = quantile(ec50_mu, 0.025),
        ech50_97.5 = quantile(ec50_mu, 0.975),
        slope_2.5 = quantile(slope_p, 0.025),
        slope_97.5 = quantile(slope_p, 0.975),
        upper_2.5 = quantile(upper_p, 0.025),
        upper_97.5 = quantile(upper_p, 0.975)
      )  |>
      bind_cols(data.frame(conc = seq(0,10, by = .1))) |>
      mutate(
        ci_2.5 = dose_response(conc, ech50_2.5, slope_2.5, 1, 0),
        ci_97.5 = dose_response(conc, ech50_97.5, slope_97.5, 1, 0)
      ),
    aes(x = conc,
        ymin = ci_2.5,
        ymax = ci_97.5)
  ) +
  geom_line(data = data.frame(thiamine = seq(0,10, by = .1)) |>
              mutate(survival = dose_response(thiamine, ec50_mu = 2.7, upper_p = 1, lower_p = 0, slope_p = 2.8)),
            aes(x = thiamine, y = survival),
            linewidth = 1, colour = "red") +
  scale_x_continuous(limits = c(0,10))

