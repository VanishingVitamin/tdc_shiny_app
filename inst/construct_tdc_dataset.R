library(googledrive)
library(dplyr)
googledrive::drive_deauth()

# Download xlsx file from Google Drive
googledrive::drive_download(
  file = "https://docs.google.com/spreadsheets/d/1TX5lkpAsdurQlWQoNAmKWHv4WBoPwjmq/edit?usp=sharing&ouid=106506252335393186387&rtpof=true&sd=true",
  path = "inst/misc_data/LC50_EC50_salmon.xlsx",
  # type = "xlsx",
  overwrite = TRUE
)

# Loop over pages in Excel file that contain data
tdc_data_unclean <-
  purrr::map_dfr(
    c(5:9),
    ~ readxl::read_xlsx(
      "inst/misc_data/LC50_EC50_salmon.xlsx",
      sheet = .x,
      col_types = c(
        # Study date, year, location, river
        "text",
        "text",
        "text",
        "text",
        # Lat and Long
        "numeric",
        "numeric",
        # Species, run, tissue
        "text",
        "text",
        "text",
        # Thiamin conc, units, treated
        "numeric",
        "text",
        "text",
        # N, N_survive, N_mortality
        "numeric",
        "numeric",
        "numeric",
        # Percent survived, percent mortality
        "numeric",
        "numeric",
        # Time of mortality, follow-up period, time units
        "text",
        "text",
        "text",
        # Reported LC50, Reported EC50
        "numeric",
        "numeric",
        # Title, DOI, Notes
        "text",
        "text",
        "text"
      )
    )
  ) |>
  dplyr::mutate(
    DOI = tolower(DOI),
    Link = ifelse(stringr::str_detect(DOI, "doi|github"), DOI, NA),
    DOI = ifelse(stringr::str_detect(DOI, "doi\\.org"), DOI, NA) |>
      stringr::str_remove("https?:\\/\\/(?:dx\\.)?doi\\.org\\/"),
    # Create a unique ID that is either a study's DOI or the Location + River of data collection (for unpublished data)
    unique_id = ifelse(
      is.na(DOI),
      paste0("Unpublished, ", Location, ", ", River),
      DOI
    ),
    # Some observations are missing lat/long coordinates. Some of
    # these do provide a location name
    location_type = case_when(
      !is.na(Latitude_DD) ~ "provided",
      is.na(Location) & is.na(Latitude_DD) ~ "missing",
      !is.na(Location) & is.na(Latitude_DD) ~ "approximated"
    ),
    Longitude_DD = round(Longitude_DD, 7),
    Latitude_DD = round(Latitude_DD, 7),
    Thiamine_conc = case_when(
      Thiamine_units == "pmol/g" ~ Thiamine_conc / 1000,
      .default = Thiamine_conc
    ),
    Thiamine_units = "nmol/g",
    Treated = tolower(Treated),
    published = !is.na(DOI),
    has_thiamine = !is.na(Thiamine_conc),
    region = case_when(
      Location == "Baltic Sea" ~ "BALTIC SEA",
      Location %in%
        c(
          "Lake Champlain",
          "Maple Lake",
          "Cayuga Lake",
          "Hinchenbrooke",
          "Wellsboro lab",
          "Charleston Lake",
          "Seneca Lake",
          "Lake Ontario",
          "Lake Huron",
          "Lake Erie",
          "Lake Michigan",
          "Lake Superior"
        ) ~
        "GREAT LAKES BASIN",
      Location == "Pacific Ocean" ~ "PACIFIC OCEAN"
    )
  )

# Extract all locations/coordinates per DOI
locations_by_doi <-
  tdc_data_unclean |>
  distinct(DOI, Location, Latitude_DD, Longitude_DD)

# Filter to cases where exact coordinates are not provided, attempt to fill in
# coordinates using geocoding services
only_location_given <-
  locations_by_doi |>
  filter(!is.na(Location) & is.na(Latitude_DD)) |>
  mutate(
    latlong = tidygeocoder::geo_combine(
      address = Location,
      global_params = list(address = "address"),
      queries = list(
        list(method = "osm"),
        list(method = "census"),
        list(method = "arcgis")
      )
    )
  ) |>
  tidyr::unnest(latlong) |>
  mutate(
    Latitude_DD = lat,
    Longitude_DD = long,
    # Add Wellsboro lab (176 Straight Run Rd) coordinates manually
    Latitude_DD = ifelse(
      is.na(Latitude_DD) & Location == "Wellsboro lab",
      41.77745120634784,
      Latitude_DD
    ),
    Longitude_DD = ifelse(
      is.na(Longitude_DD) & Location == "Wellsboro lab",
      -77.39841596441796,
      Longitude_DD
    )
  ) |>
  select(DOI, Location, Latitude_DD, Longitude_DD)

tdc_data_approx_location <-
  tdc_data_unclean |>
  filter(location_type == "approximated") |>
  select(-c(Latitude_DD, Longitude_DD)) |>
  left_join(only_location_given, by = c("DOI", "Location"))

tdc_data <-
  # combine data for which coordinates are given and data with approximate
  # coordinates
  bind_rows(
    tdc_data_unclean |> filter(location_type != "approximated"),
    tdc_data_approx_location
  ) |>
  mutate(
    Location_label = ifelse(is.na(Location), "MISSING", toupper(Location)) |>
      factor() |>
      forcats::fct_relevel("MISSING", after = Inf),
    River_label = ifelse(is.na(River), "MISSING", toupper(River)) |>
      factor() |>
      forcats::fct_relevel("MISSING", after = Inf),
    Species_label = ifelse(is.na(Species), "MISSING", toupper(Species)) |>
      factor(),
    Run_label = ifelse(is.na(Run), "MISSING", toupper(Run)) |>
      factor() |>
      forcats::fct_relevel("MISSING", after = Inf),
    Tissue_label = ifelse(is.na(Tissue), "MISSING", toupper(Tissue)) |>
      factor(),
    Title_label = ifelse(is.na(Title), "MISSING", toupper(Title)) |>
      factor() |>
      forcats::fct_relevel("MISSING", after = Inf),
    Study_Date_start_label = case_when(
      Study_Date == "1994-2010" ~ "1994-01-01",
      Study_Date == "2001, 2012" ~ "2001-01-01",
      Study_Date == "2000, 2002" ~ "2000-01-01",
      Study_Date == "1998, 1999" ~ "1998-01-01",
      stringr::str_detect(Study_Date, "^[0-9]{4}$") ~
        paste0(Study_Date, "-01-01"),
      stringr::str_detect(Study_Date, "^[0-9]{2}/[0-9]{2}/[0-9]{4}$") ~
        Study_Date
    ),
    Study_Date_end_label = case_when(
      Study_Date == "1994-2010" ~ "2010-12-31",
      Study_Date == "2001, 2012" ~ "2012-12-31",
      Study_Date == "2000, 2002" ~ "2002-12-31",
      Study_Date == "1998, 1999" ~ "1999-12-31",
      stringr::str_detect(Study_Date, "^[0-9]{4}$") ~
        paste0(Study_Date, "-12-31"),
      stringr::str_detect(Study_Date, "^[0-9]{2}/[0-9]{2}/[0-9]{4}$") ~
        Study_Date
    ),
    marker_label = purrr::pmap_chr(
      list(
        Location_label,
        River_label,
        Species_label,
        Run_label,
        Tissue_label,
        DOI,
        Link,
        location_type
      ),
      ~ paste0(
        "<strong>Location:</strong> ",
        ..1,
        ifelse(..8 == "approximated", " (Approximate)", ""),
        "</br>",
        "<strong>River:</strong> ",
        ..2,
        "</br>",
        "<strong>Species:</strong> ",
        ..3,
        "</br>",
        "<strong>Run:</strong> ",
        ..4,
        "</br>",
        "<strong>Tissue:</strong> ",
        ..5,
        "</br>",
        "<strong>Link:</strong> ",
        case_when(
          is.na(..6) & is.na(..7) ~ "Unpublished (no link)",
          is.na(..6) & !is.na(..7) ~
            paste0("<a href='", ..7, "' target='_blank'>", ..7, "</a>"),
          .default = paste0("<a href='", ..7, "' target='_blank'>", ..6, "</a>")
        )
      )
    )
  )

# Create a character string of what to show in the Dataset table details
table_details <-
  tdc_data |>
  dplyr::group_by(unique_id) |>
  dplyr::group_split() |>
  purrr::map_dfr(function(dat) {
    dat |>
      dplyr::distinct(unique_id) |>
      dplyr::mutate(
        collection_locations = purrr::map2_chr(
          dat$Location,
          dat$River,
          function(loc, riv) {
            # if there's not a named river, just return the location
            if (is.na(riv) | riv == "NA") {
              return(
                toupper(loc)
              )
            } else {
              # otherwise, return "location (river)"
              return(
                paste0(toupper(loc), " (", toupper(riv), ")")
              )
            }
          }
        ) |>
          unique() |>
          paste0(collapse = ", "),
        table_details = paste0(
          "Collection location(s): ",
          collection_locations,
          "</br>",
          paste0(
            "Species: ",
            ifelse(
              all(is.na(dat$Species)),
              "MISSING",
              paste0(
                toupper(unique(dat$Species[!is.na(dat$Species)])),
                collapse = ", "
              )
            )
          ),
          "</br>",
          paste0(
            "Study year(s): ",
            ifelse(
              all(is.na(dat$Study_Year)),
              "MISSING",
              paste0(
                sort(unique(as.integer(dat$Study_Year[
                  !is.na(dat$Study_Year)
                ]))),
                collapse = ", "
              )
            )
          ),
          "</br>",
          paste0(
            "Run(s): ",
            ifelse(
              all(is.na(dat$Run)),
              "MISSING",
              paste0(toupper(unique(dat$Run[!is.na(dat$Run)])), collapse = ", ")
            )
          ),
          "</br>",
          paste0(
            "Tissue(s): ",
            ifelse(
              all(is.na(dat$Tissue)),
              "MISSING",
              paste0(
                toupper(unique(dat$Tissue[!is.na(dat$Tissue)])),
                collapse = ", "
              )
            )
          )
        ),
      )
  })

tdc_data <- tdc_data |>
  left_join(table_details, by = "unique_id") |>
  arrange(unique_id)

readr::write_csv(tdc_data, file = "inst/misc_data/tdc_data.csv")
usethis::use_data(tdc_data, overwrite = TRUE)
