atlantic <- readRDS(
  "inst/fitted_models_2025-09-11/Posterior_Atlantic_2025_09_11_MD.rds"
)
chinook <- readRDS(
  "inst/fitted_models_2025-09-11/Posterior_Chinook_2025_09_11_MD.rds"
)
coho <- readRDS(
  "inst/fitted_models_2025-09-11/Posterior_Coho_2025_09_11_MD.rds"
)
laketrout <- readRDS(
  "inst/fitted_models_2025-09-11/Posterior_LakeTrout_2025_09_11_MD.rds"
)
steelhead <- readRDS(
  "inst/fitted_models_2025-09-11/Posterior_Steelhead_2025_09_11_MD.rds"
)


dose_response_params <-
  bind_rows(
    atlantic$samples |>
      rename(
        overall = ec50_mu,
        baltic = "c50_region[1]",
        greatLakes = "c50_region[2]",
        species = Species
      ) |>
      group_by(species) |>
      summarize(across(
        where(is.numeric),
        list(
          median = median,
          CI_2.5 = ~ quantile(., 0.025),
          CI_97.5 = ~ quantile(., 0.975)
        ),
        .names = "{.col}__{.fn}"
      )) |>
      tidyr::pivot_longer(
        starts_with("overall") |
          starts_with("baltic") |
          starts_with("greatLakes"),
        names_to = c("region", ".value"),
        names_sep = "__"
      ) |>
      rename(
        ec50_mu__median = median,
        ec50_mu__CI_2.5 = CI_2.5,
        ec50_mu__CI_97.5 = CI_97.5
      ) |>
      select(species, region, starts_with("ec50_mu"), everything()) |>
      tidyr::pivot_longer(
        -c(species, region),
        names_to = c("parameter", "statistic"),
        names_sep = "__"
      ),
    chinook$samples |>
      rename(
        overall = ec50_mu,
        greatLakes = "c50_region[1]",
        pacific = "c50_region[2]",
        species = Species
      ) |>
      group_by(species) |>
      summarize(across(
        where(is.numeric),
        list(
          median = median,
          CI_2.5 = ~ quantile(., 0.025),
          CI_97.5 = ~ quantile(., 0.975)
        ),
        .names = "{.col}__{.fn}"
      )) |>
      tidyr::pivot_longer(
        starts_with("overall") |
          starts_with("pacific") |
          starts_with("greatLakes"),
        names_to = c("region", ".value"),
        names_sep = "__"
      ) |>
      rename(
        ec50_mu__median = median,
        ec50_mu__CI_2.5 = CI_2.5,
        ec50_mu__CI_97.5 = CI_97.5
      ) |>
      select(species, region, starts_with("ec50_mu"), everything()) |>
      tidyr::pivot_longer(
        -c(species, region),
        names_to = c("parameter", "statistic"),
        names_sep = "__"
      ),
    coho$samples |>
      rename(species = Species) |>
      mutate(region = "greatLakes") |>
      group_by(species, region) |>
      summarize(across(
        where(is.numeric),
        list(
          median = median,
          CI_2.5 = ~ quantile(., 0.025),
          CI_97.5 = ~ quantile(., 0.975)
        ),
        .names = "{.col}__{.fn}"
      )) |>
      select(species, region, starts_with("ec50_mu"), everything()) |>
      tidyr::pivot_longer(
        -c(species, region),
        names_to = c("parameter", "statistic"),
        names_sep = "__"
      ),
    laketrout$samples |>
      rename(species = Species) |>
      mutate(region = "greatLakes") |>
      group_by(species, region) |>
      summarize(across(
        where(is.numeric),
        list(
          median = median,
          CI_2.5 = ~ quantile(., 0.025),
          CI_97.5 = ~ quantile(., 0.975)
        ),
        .names = "{.col}__{.fn}"
      )) |>
      select(species, region, starts_with("ec50_mu"), everything()) |>
      tidyr::pivot_longer(
        -c(species, region),
        names_to = c("parameter", "statistic"),
        names_sep = "__"
      ),
    steelhead$samples |>
      rename(species = Species) |>
      mutate(region = "greatLakes") |>
      group_by(species, region) |>
      summarize(across(
        where(is.numeric),
        list(
          median = median,
          CI_2.5 = ~ quantile(., 0.025),
          CI_97.5 = ~ quantile(., 0.975)
        ),
        .names = "{.col}__{.fn}"
      )) |>
      select(species, region, starts_with("ec50_mu"), everything()) |>
      tidyr::pivot_longer(
        -c(species, region),
        names_to = c("parameter", "statistic"),
        names_sep = "__"
      )
  )

saveRDS(dose_response_params, "inst/misc_data/dose_response_params.rds")
usethis::use_data(dose_response_params, overwrite = TRUE)
