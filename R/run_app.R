#' Launch Vanishing Vitamin app
#'
#' @param options list; app launch options passed to the \code{options} argument
#'   of shiny::shinyApp()
#'
#' @seealso [shiny::shinyApp()]
#'
#' @examples
#' launch_app()
#'
#'
#' @export

launch_app <- function(options = list(launch.browser = TRUE)){

  citations <- vanishingVitamin::citations
  tdc_data <- vanishingVitamin::tdc_data |>
    dplyr::mutate(DOI_join = tolower(DOI)) |>
    dplyr::left_join(citations |>
                       dplyr::select(DOI, data_collection_region),
                     by = c("DOI_join" = "DOI")) |>
    dplyr::select(-DOI_join)

  shiny::shinyApp(ui = app_ui(tdc_data),
                  server = app_server(tdc_data, citations),
                  options = options)

}
