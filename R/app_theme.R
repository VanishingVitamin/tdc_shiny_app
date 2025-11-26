#' Defines the dashboard theme for the Vanishing Vitamin Shiny app
#'
#' This is an internal function that is used within the exported launch_app()
#' function.
#'
#' @return the result of \code{fresh::create_theme}
#' @keywords internal
#' @noRd

app_theme <- function() {
  fresh::create_theme(
    theme = "spacelab",
    fresh::bs4dash_status(
      light = "#7C9EC1"
    ),
    fresh::bs4dash_sidebar_light(color = "#FFFFFF"),
    fresh::bs4dash_vars(
      navbar_light_color = NULL,
      navbar_light_active_color = "#000000",
      navbar_light_hover_color = NULL
    )
  )
}
