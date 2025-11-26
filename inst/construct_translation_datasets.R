library(googledrive)
library(dplyr)
googledrive::drive_deauth()

# Download xlsx file from Google Drive
googledrive::drive_download(
  file = "https://docs.google.com/spreadsheets/d/14frMz_SEpjdOCE8BhsQeIgz645Cxx6OPUpA829wehN4/edit?usp=sharing",
  path = "inst/misc_data/vanishing_vitamin_translations.xlsx",
  # type = "xlsx",
  overwrite = TRUE
)


sheet_names <- openxlsx::getSheetNames(
  "inst/misc_data/vanishing_vitamin_translations.xlsx"
)

purrr::walk(
  sheet_names,
  ~ {
    translations <- openxlsx::read.xlsx(
      "inst/misc_data/vanishing_vitamin_translations.xlsx",
      sheet = .x
    )

    if (.x == "en_rev") {
      translations <-
        translations |>
        dplyr::mutate(
          en_rev = dplyr::case_when(
            type == "link" ~ en,
            .default = stringi::stri_reverse(en)
          )
        )
    }

    translations |>
      dplyr::select(1:2) |>
      readr::write_csv(
        file = paste0("www/translations/translation_", .x, ".csv")
      )
  }
)
