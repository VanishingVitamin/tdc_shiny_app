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

launch_app <- function(options = list()){

  tdc_data <- vanishingVitamin::tdc_data
  citations <- vanishingVitamin::citations

  shiny::shinyApp(ui = app_ui(tdc_data),
                  server = app_server(tdc_data, citations),
                  options = options)

}
