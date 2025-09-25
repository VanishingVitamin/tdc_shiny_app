#' Launch Vanishing Vitamin app
#'
#' @param options list; app launch options passed to the \code{options} argument
#'   of shiny::shinyApp()
#'
#' @seealso [shiny::shinyApp()]
#'
#' @examples
#' \dontrun{
#'   launch_app()
#' }
#'
#' @export

launch_app <- function(
  default_language = "en",
  options = list(launch.browser = TRUE)
) {
  translator <- shiny.i18n::Translator$new(
    translation_csvs_path = "www/translations/"
  )

  translator$set_translation_language(default_language)

  citations <- vanishingVitamin::citations
  tdc_data <- vanishingVitamin::tdc_data |>
    dplyr::mutate(
      DOI_join = tolower(DOI),
      Location_label = paste0(
        Location_label,
        ifelse(River_label == "MISSING", "", paste0(" (", River_label, ")"))
      )
    )

  dose_response_params <- vanishingVitamin::dose_response_params

  shiny::shinyApp(
    ui = app_ui(tdc_data, translator),
    server = app_server(tdc_data, citations, dose_response_params, translator),
    options = options
  )
}
