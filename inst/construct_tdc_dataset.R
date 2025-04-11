library(googledrive)
library(dplyr)
googledrive::drive_deauth()

googledrive::drive_download(file = "https://docs.google.com/spreadsheets/d/1TX5lkpAsdurQlWQoNAmKWHv4WBoPwjmq/edit?usp=sharing&ouid=106506252335393186387&rtpof=true&sd=true",
                            path = "data/LC50_EC50_salmon.xlsx",
                            # type = "xlsx",
                            overwrite = TRUE)

tdc_data <-
  purrr::map_dfr(c(5:9),
                 ~ readxl::read_xlsx("data/LC50_EC50_salmon.xlsx",
                                     sheet = .x,
                                     col_types = c("text", "text", "numeric", "numeric",
                                                   "text", "text", "text",
                                                   "numeric", "text",
                                                   "numeric", "numeric", "numeric",
                                                   "numeric", "numeric",
                                                   "text", "text", "text",
                                                   "numeric", "numeric", "text", "text", "text")))

locations_by_doi <-
  tdc_data |>
  distinct(DOI, Location, Latitude_DD, Longitude_DD)

only_location_given <-
  locations_by_doi |>
  filter(!is.na(Location) & is.na(Latitude_DD)) |>
  mutate(latlong = tidygeocoder::geo_combine(address = Location,
                                             global_params = list(address = "address"),
                                             queries = list(list(method = "osm"),
                                                            list(method = "census"),
                                                            list(method = "arcgis")))) |>
  tidyr::unnest(latlong) |>
  mutate(Latitude_DD = lat,
         Longitude_DD = long) |>
  select(DOI,Location, Latitude_DD, Longitude_DD)

locations_by_doi_combined <-
  bind_rows(
    locations_by_doi |>
      filter(!(!is.na(Location) & is.na(Latitude_DD))) |>
      mutate(location_type = ifelse(is.na(Latitude_DD), "missing", "provided")),
    only_location_given |>
      mutate(location_type = "approximated")
  )

tdc_data_cleaned <-
  tdc_data |>
  mutate(location_type =
           case_when(!is.na(Latitude_DD) ~ "provided",
                     is.na(Location) & is.na(Latitude_DD) ~ "missing",
                     !is.na(Location) & is.na(Latitude_DD) ~ "approximated")) |>
  select(-c(Latitude_DD, Longitude_DD)) |>
  left_join(locations_by_doi_combined,
            by = c("DOI", "Location","location_type")) |>
  mutate(
    Location_label = ifelse(is.na(Location),
                            "MISSING", toupper(Location)) |>
      factor() |>
      forcats::fct_relevel("MISSING", after = Inf),
    Species_label = ifelse(is.na(Species),
                           "MISSING", toupper(Species)) |>
      factor(),
    Run_label = ifelse(is.na(Run),
                       "MISSING", toupper(Run)) |>
      factor() |>
      forcats::fct_relevel("MISSING", after = Inf),
    Tissue_label = ifelse(is.na(Tissue),
                          "MISSING", toupper(Tissue)) |>
      factor(),
    Title_label = ifelse(is.na(Title),
                         "MISSING", toupper(Title)) |>
      factor() |>
      forcats::fct_relevel("MISSING", after = Inf),
    Study_Date_start_label =
      case_when(Study_Date == "1994-2010" ~ "1994-01-01",
                Study_Date == "2001, 2012" ~ "2001-01-01",
                Study_Date == "2000, 2002" ~ "2000-01-01",
                Study_Date == "1998, 1999" ~ "1998-01-01",
                stringr::str_detect(Study_Date, "^[0-9]{4}$") ~ paste0(Study_Date,"-01-01"),
                stringr::str_detect(Study_Date, "^[0-9]{2}/[0-9]{2}/[0-9]{4}$") ~ Study_Date),
    Study_Date_end_label =
      case_when(Study_Date == "1994-2010" ~ "2010-12-31",
                Study_Date == "2001, 2012" ~ "2012-12-31",
                Study_Date == "2000, 2002" ~ "2002-12-31",
                Study_Date == "1998, 1999" ~ "1999-12-31",
                stringr::str_detect(Study_Date, "^[0-9]{4}$") ~ paste0(Study_Date,"-12-31"),
                stringr::str_detect(Study_Date, "^[0-9]{2}/[0-9]{2}/[0-9]{4}$") ~ Study_Date)
  ) |>
  rename(Thiamin_conc = Thiamine_conc)

readr::write_csv(tdc_data_cleaned,
                 file = "data/tdc_data.csv")

