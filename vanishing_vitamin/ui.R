library(shiny)

library(bs4Dash)
library(fresh)

library(DT)

source('helper-code/theming.R')

bs4Dash::dashboardPage(
  scrollToTop = TRUE,
  freshTheme = color_theme,
  dashboardHeader(title = "Vanishing Vitamin"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(text = "Welcome!", tabName = "welcome", icon = icon("fish-fins")),
      menuItem(text = "Data", tabName = "data", icon = icon("table")),
      menuItem(text = "Visualize", tabName = "visualize", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "welcome",
              h3("Welcome to Vanishing Vitamin!"),
              h5("This tab helps users get started. It summarizes the app's primary functionality and directs users to where they can find more information.")),
      tabItem(tabName = "data",
              DT::dataTableOutput(outputId = "tdc_data"))
    )
  )
)
