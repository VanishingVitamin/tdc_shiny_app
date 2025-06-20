#' Thiamin concentration vs. Survivability data
#'
#' @format ## `tdc_data`
#' A tibble object with 1,088 rows and 31 columns:
#' \describe{
#'  \item{Study_Date}{date, year, or range of years when observation was collected.}
#'  \item{Location}{human-readable description of location where data were collected.}
#'  \item{Species}{species of fish from which observations originated.}
#'  \item{Run}{run/season of data collection.}
#'  \item{Tissue}{tissue that was used to estimate concentration.}
#'  \item{Thiamine_conc}{concentration of the tissue used to estimate mortality or survival.}
#'  \item{Thiamine_units}{units of the concentration.}
#'  \item{N}{sample size of collected data.}
#'  \item{N_survive}{number of survivors.}
#'  \item{N_mortality}{number of mortalities.}
#'  \item{Percent_survive}{percent of survivors relative to sample size.}
#'  \item{Percent_mortality}{percent of mortalities relative to sample size.}
#'  \item{Time_of_mortality}{time of death information (if available).}
#'  \item{Follow_up_period}{time between follow-up (if available).}
#'  \item{Time_units}{units of time (days, weeks, etc.)}
#'  \item{Reported_LC50}{estimated LC50, aka the lethal concentration of thiamine for 50% of the population calculated by the paper.}
#'  \item{Reported_EC50}{estimated EC50, aka the effect concentration of thiamine for 50% of the population calculated by the paper. The EC50 includes any non-lethal endpoint, but is often behavior.}
#'  \item{Title}{title of the paper or source of information.}
#'  \item{DOI}{DOI link for building bibliography.}
#'  \item{Notes}{Additional notes, including assumptions you made in filling out the table.}
#'  \item{location_type}{indicator of whether the Latitude_DD, Longitude_DD columns contain exact coordinates or approximate coordinates based on the place named in the Location column.}
#'  \item{Latitude_DD}{latitude coordinate of data collection location.}
#'  \item{Longitude_DD}{longitude coordinate of data collection location.}
#'  \item{Location_label}{processed version of Location column for display in the app.}
#'  \item{Species_label}{processed version of Species column for display in the app.}
#'  \item{Run_label}{processed version of Run column for display in the app.}
#'  \item{Tissue_label}{processed version of Tissue column for display in the app.}
#'  \item{Title_label}{processed version of Title column for display in the app.}
#'  \item{Study_Date_start_label}{processed version of the first date/year listed Study Date column for display in the app.}
#'  \item{Study_Date_end_label}{processed version of the last date/year listed in the Study Date column for display in the app.}
#'  \item{marker_label}{a text string containing HTML code. Used for text pop-ups in the Data Collection Location map in the app's Data tab.}
#' }
#'
#' @source <https://docs.google.com/spreadsheets/d/1TX5lkpAsdurQlWQoNAmKWHv4WBoPwjmq/edit?usp=sharing&ouid=106506252335393186387&rtpof=true&sd=true>
"tdc_data"

#' Citation information for published data
#'
#' @format ## `citations`
#' A tibble object with 15 rows and 4 columns
#' \describe{
#'  \item{DOI}{URL associated with the published data. May be a URL or "unpublished" for unpublished data.}
#'  \item{DOI_num}{DOI associated with the published data. May be a URL or "unpublished" for unpublished data.}
#'  \item{metadata}{A nested column. Each element contains metadata about the associated publication. Has value "NA" for unpublished data.}
#'  \item{formatted_metadata}{A formatted character string containing information stored in the metadata column. Used for display in the app.}
#'}
"citations"
