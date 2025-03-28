library(shiny)
library(shinyjs)
library(readr)
library(dplyr)

library(bs4Dash)
library(bslib)
library(fresh)

library(leaflet)
library(reactable)
library(gt)

library(plotly)
library(ggplot2)

source('helper-code/theming.R')
tdc_data <- read_csv("data/tdc_data.csv")

bs4Dash::dashboardPage(
  scrollToTop = TRUE,
  freshTheme = color_theme,
  help = NULL,
  dark = NULL,
  # controlbar = dashboardControlbar(disable = TRUE,width = 0, overlay = TRUE),
  header = dashboardHeader(title = h5("Vanishing Vitamin", style = "padding-left:10px;"),
                           sidebarIcon = shiny::icon("caret-square-left",
                                                     style = "font-size:24px;",
                                                     id = "header_toggle"),
                           navbarMenu(
                             id = "navmenu",
                             useShinyjs(),
                             navbarTab(tabName = "welcome", text = tags$span(icon("fish-fins"), "Welcome!")),
                             navbarTab(tabName = "data", text = tags$span(icon("table"), "Data")),
                             navbarTab(tabName = "visualize", text = tags$span(icon("chart-line"), "Visualize"))
                           )),
  sidebar = dashboardSidebar(disable = FALSE,
                             elevation = 2,
                             collapsed = FALSE,
                             minified = FALSE,
                             expandOnHover = TRUE,
                             fixed = TRUE,
                             # text = "Dashboard filters",
                             # icon = shiny::icon("filter"),
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
  ),
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
              h5("Note to future self: link to ",
                 a(href = "https://sites.google.com/ucdavis.edu/salmonintheclassroomresources/home",
                   target = '_blank',
                   "https://sites.google.com/ucdavis.edu/salmonintheclassroomresources/home"),
                 " for resources, data, and other documentation."),
              br(),
              h5(strong("Click on one of the buttons at the top to get started!"))),
      tabItem(tabName = "data",
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
      ),
      tabItem(tabName = "visualize",
              fluidRow(
                column(width = 3,
                  bs4Dash::accordion(id = "visualize_accordion",
                                     accordionItem(collapsed = FALSE, status = "primary",
                                                   title = "Visualize your own data",
                                                   icon = bsicons::bs_icon("plus"),
                                                   shiny::selectInput(inputId = "visualize_add_data_choice",
                                                                      label = "Choose how to add data:",
                                                                      choices = c("Upload .csv", "Manual entry")),
                                                   conditionalPanel(
                                                     condition = "input.visualize_add_data_choice == 'Manual entry'",
                                                     DT::DTOutput("visualize_add_data_manual"),
                                                     actionButton(inputId = "visualize_add_data_new_row",
                                                                  label = "New row",
                                                                  icon = shiny::icon("plus"))
                                                     ),
                                                   conditionalPanel(
                                                     condition = "input.visualize_add_data_choice == 'Upload .csv'",
                                                     shiny::fileInput(inputId = "visualize_add_data_file",
                                                                      label = "Select a file", accept = ".csv", multiple = FALSE),
                                                     selectInput(inputId = "visualize_add_data_file_thiamin_col",
                                                                 label = "Thiamin Concentration column",
                                                                 choices = ""),
                                                     selectInput(inputId = "visualize_add_data_file_survive_col",
                                                                 label = "(Optional) % Survived column",
                                                                 choices = "")
                                                   ),
                                                   br(),
                                                   actionButton(inputId = "visualize_add_data_plot",
                                                                label = "Plot data",
                                                                icon = shiny::icon("chart-line"))
                                     )
                                     # ,accordionItem(
                                     #   title = "Plot style", status = "primary",
                                     #   icon = bsicons::bs_icon("palette"),
                                     #   selectInput(inputId = "visualize_point_color",
                                     #               label = "Point color",
                                     #               choices = c("","Species", "Tissue")),
                                     #   selectInput(inputId = "visualize_plot_theme")
                                     # )
                  )
                ),
                bs4Dash::box(
                  collapsible = FALSE, maximizable = TRUE, title = "Scatterplot", width = 9,
                  plotly::plotlyOutput("ec50_curve", height = "550px", width = '100%')
                )
              )
      )
    )
  )
)
