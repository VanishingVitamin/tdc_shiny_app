library(googledrive)
library(dplyr)
googledrive::drive_deauth()

# Download xlsx file from Google Drive
googledrive::drive_download(file = "https://docs.google.com/spreadsheets/d/1TX5lkpAsdurQlWQoNAmKWHv4WBoPwjmq/edit?usp=sharing&ouid=106506252335393186387&rtpof=true&sd=true",
                            path = "inst/misc_data/LC50_EC50_salmon.xlsx",
                            # type = "xlsx",
                            overwrite = TRUE)

# Loop over pages in Excel file that contain data
tdc_data <-
  purrr::map_dfr(c(5:9),
                 ~ readxl::read_xlsx("inst/misc_data/LC50_EC50_salmon.xlsx",
                                     sheet = .x,
                                     col_types = c("text", "text", "numeric", "numeric",
                                                   "text", "text", "text",
                                                   "numeric", "text",
                                                   "numeric", "numeric", "numeric",
                                                   "numeric", "numeric",
                                                   "text", "text", "text",
                                                   "numeric", "numeric", "text", "text", "text"))) |>
  dplyr::mutate(DOI = tolower(DOI),
                # Some observations are missing lat/long coordinates. Some of
                # these do provide a location name
                location_type =
                  case_when(!is.na(Latitude_DD) ~ "provided",
                            is.na(Location) & is.na(Latitude_DD) ~ "missing",
                            !is.na(Location) & is.na(Latitude_DD) ~ "approximated")) |>
  dplyr::rename(
    Thiamin_conc = Thiamine_conc,
    Thiamin_units = Thiamine_units
  )

# Extract all locations/coordinates per DOI
locations_by_doi <-
  tdc_data |>
  distinct(DOI, Location, Latitude_DD, Longitude_DD)

# Filter to cases where exact coordinates are not provided, attempt to fill in
# coordinates using geocoding services
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
         Longitude_DD = long,
         # Add Wellsboro lab (176 Straight Run Rd) coordinates manually
         Latitude_DD = ifelse(is.na(Latitude_DD) & Location == "Wellsboro lab", 41.77745120634784, Latitude_DD),
         Longitude_DD = ifelse(is.na(Longitude_DD) & Location == "Wellsboro lab", -77.39841596441796, Longitude_DD)) |>
  select(DOI,Location, Latitude_DD, Longitude_DD)

tdc_data_approx_location <-
  tdc_data |>
  filter(location_type == "approximated") |>
  select(-c(Latitude_DD, Longitude_DD)) |>
  left_join(only_location_given, by = c("DOI", "Location"))

tdc_data_cleaned <-
  # combine data for which coordinates are given and data with approximate
  # coordinates
  bind_rows(
    tdc_data |> filter(location_type != "approximated"),
    tdc_data_approx_location
  ) |>
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
                stringr::str_detect(Study_Date, "^[0-9]{2}/[0-9]{2}/[0-9]{4}$") ~ Study_Date),
    marker_label = purrr::pmap_chr(list(Location_label, Species_label, Run_label, Tissue_label, DOI, location_type),
                                   ~
                                     paste0("<strong>Location:</strong> ",..1,
                                            ifelse(..6 == "approximated", " (Approximate)", ""),"</br>",
                                            "<strong>Species:</strong> ",..2,"</br>",
                                            "<strong>Run:</strong> ",..3,"</br>",
                                            "<strong>Tissue:</strong> ",..4,"</br>",
                                            "<strong>DOI:</strong> <a href='",..5,"' target='_blank'>",..5,"</a>")
    )
  )

readr::write_csv(tdc_data_cleaned,
                 file = "inst/misc_data/tdc_data.csv")

