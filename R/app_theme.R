#' Defines the dashboard theme for the Vanishing Vitamin Shiny app
#'
#' This is an internal function that is used within the exported launch_app()
#' function.
#'
#' @value the result of \code{fresh::create_theme}

app_theme <- function(){
  fresh::create_theme(
    fresh::bs4dash_vars(
      navbar_light_color = NULL,
      navbar_light_active_color = NULL,
      navbar_light_hover_color = NULL
    ),
    fresh::bs4dash_yiq(
      contrasted_threshold = 10,
      text_dark = "#000000",
      text_light = "#FFFFFF"
    ),
    fresh::bs4dash_layout(
      main_bg = NULL
    ),
    fresh::bs4dash_sidebar_light(
      bg = NULL,
      color = NULL,
      active_color = NULL,
      hover_color = NULL,
      submenu_bg = NULL,
      submenu_color = NULL,
      submenu_hover_color = NULL
    ),
    fresh::bs4dash_status(
      primary = "#B8C9FF",
      danger = NULL,
      light = NULL
    ),
    fresh::bs4dash_color(
    )
  )
}
