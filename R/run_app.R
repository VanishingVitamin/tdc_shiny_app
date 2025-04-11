#' Launch Vanishing Vitamin app
#'
#' @param options list; app launch options passed to \code{options} argument of
#'   shiny::shinyApp()
#'
#' @seealso [shiny::shinyApp()]
#'
#' @examples
#' \dontrun{
#'   launch_app()
#' }
#'
#'
#' @export

launch_app <- function(options = list()){

  tdc_data <- vanishingVitamin::tdc_data
  citations <- vanishingVitamin::citations
  lc50_curve <- vanishingVitamin::lc50_curve

  shiny::shinyApp(ui = app_ui(),
                  server = app_server(),
                  options = options)

}
