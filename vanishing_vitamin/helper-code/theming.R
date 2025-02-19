library(bs4Dash)
library(fresh)

color_theme <-
  create_theme(
  bs4dash_vars(
    navbar_light_color = NULL,
    navbar_light_active_color = NULL,
    navbar_light_hover_color = NULL
  ),
  bs4dash_yiq(
    contrasted_threshold = 10,
    text_dark = "#000000",
    text_light = "#FFFFFF"
  ),
  bs4dash_layout(
    main_bg = NULL
  ),
  bs4dash_sidebar_light(
    bg = NULL,
    color = NULL,
    active_color = NULL,
    hover_color = NULL,
    submenu_bg = NULL,
    submenu_color = NULL,
    submenu_hover_color = NULL
  ),
  bs4dash_status(
    primary = "#B8C9FF",
    danger = NULL,
    light = NULL
  ),
  bs4dash_color(
  )
  )
