library(shiny)
library(readr)
library(dplyr)

library(bs4Dash)
library(fresh)

library(leaflet)
library(reactable)
library(gt)

source('helper-code/theming.R')
tdc_data <- read_csv("data/tdc_data.csv")

bs4Dash::dashboardPage(
  scrollToTop = TRUE,
  freshTheme = color_theme,
  help = NULL,
  dark = NULL,
  # controlbar = dashboardControlbar(disable = TRUE,width = 0, overlay = TRUE),
  header = dashboardHeader(title = h5("Vanishing Vitamin", style = "padding-left:10px;"),
                           navbarMenu(
                             id = "navmenu",
                             navbarTab(tabName = "welcome", text = tags$span(icon("fish-fins"), "Welcome!")),
                             navbarTab(tabName = "data", text = tags$span(icon("table"), "Data")),
                             navbarTab(tabName = "visualize", text = tags$span(icon("chart-line"), "Visualize"))
                           )),
  sidebar = dashboardSidebar(disable = TRUE,width = 10),
  # dashboardSidebar(collapsed = TRUE,
  #   sidebarMenu(
  #     menuItem(text = "Welcome!", tabName = "welcome", icon = icon("fish-fins")),
  #     menuItem(text = "Data", tabName = "data", icon = icon("table")),
  #     menuItem(text = "Visualize", tabName = "visualize", icon = icon("chart-line"))
  #   )
  # ),
  body = dashboardBody(
    tags$style(type = "text/css",
               "#tdc_data_map {height: calc(100vh - 57px) !important;
                    width: calc(80vh) !important;
                    overflow-x: hidden;
                    overflow-y: hidden;}"),
    tabItems(
      tabItem(tabName = "welcome",
              h3("Welcome to Vanishing Vitamin!"),
              h5("This tab helps users get started.
                 It summarizes the app's primary functionality and directs users to where they can find more information."),
              br(),
              h5(strong("Click on one of the buttons at the top to get started!"))),
      tabItem(tabName = "data",
              sidebarLayout(
                sidebarPanel = div(id = "tdc_table_filter_sidebar",
                                   class = "col-sm-2",
                                   sidebarPanel(
                                     width = 12,
                                     h6("Filter by:"),
                                                selectizeInput(inputId = "tdc_table_filter_location",
                                                               label = "Collection Location",
                                                               multiple = TRUE,
                                                               choices = c("",
                                                                           rev(unique(tdc_data$Location_label)))
                                                ),
                                                selectInput(inputId = "tdc_table_filter_species",
                                                            label = "Species",
                                                            multiple = TRUE,
                                                            choices = c("",
                                                                        rev(unique(tdc_data$Species_label)))
                                                ),
                                                selectInput(inputId = "tdc_table_filter_run",
                                                            label = "Run",
                                                            multiple = TRUE,
                                                            choices = c("",
                                                                        rev(unique(tdc_data$Run_label)))
                                                ),
                                                selectInput(inputId = "tdc_table_filter_tissue",
                                                            label = "Tissue",
                                                            multiple = TRUE,
                                                            choices = c("",
                                                                        rev(unique(tdc_data$Tissue_label)))
                                                )
                                                # ,sliderInput(inputId = "tdc_table_filter_date_range",
                                                #             label = "Date Collected",
                                                #             choices = c("",
                                                #                         unique(tdc_data$Title_label))
                                                #             )
                                   )),
                mainPanel = mainPanel(width = 10,
                                      fluidRow(column(width = 6,
                                                      bs4Dash::box(width = 12,title = "Datasets",
                                                                   reactable::reactableOutput(outputId = "tdc_data_table"),
                                                                   collapsible = FALSE,closable = FALSE,maximizable = TRUE,
                                                                   headerBorder = FALSE,solidHeader = FALSE,
                                                                   style = 'height: calc(100vh - 80px); overflow-y:scroll'
                                                      )),
                                               column(width = 6,
                                                      leaflet::leafletOutput("tdc_data_map",width = "100%"))
                                      )
                )
              )
      ),
    tabItem(tabName = "visualize",
            h5("This tab will contain EC50 curves for mortality vs. thiamin concentration."))
    )
  )
)
