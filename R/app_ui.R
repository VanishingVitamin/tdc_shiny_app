#' Defines the User Interface elements for the Vanishing Vitamin Shiny app
#'
#' This is an internal function that is used within the exported launch_app()
#' function.
#'
#' @param tdc_data data set containing Thiamin by Survivability data. Should be
#'   the data set exported by the vanishingVitamin package,
#'   vanishingVitamin::tdc_data
#'
#' @return \code{bs4Dash::dashboardPage} object containing UI elements

app_ui <- function(tdc_data){
  bs4Dash::dashboardPage(
    scrollToTop = TRUE,
    freshTheme = app_theme(),
    help = NULL,
    dark = NULL,
    # controlbar = dashboardControlbar(disable = TRUE,width = 0, overlay = TRUE),
    header = bs4Dash::dashboardHeader(title = shiny::h5("Vanishing Vitamin", style = "padding-left:10px;"),
                                      sidebarIcon = shiny::icon("caret-square-left",
                                                                style = "font-size:24px;",
                                                                id = "header_toggle"),
                                      bs4Dash::navbarMenu(
                                        id = "navmenu",
                                        shinyjs::useShinyjs(),
                                        bs4Dash::navbarTab(tabName = "welcome", text = shiny::tags$span(shiny::icon("fish-fins"), "Welcome!")),
                                        bs4Dash::navbarTab(tabName = "data", text = shiny::tags$span(shiny::icon("table"), "Data")),
                                        bs4Dash::navbarTab(tabName = "visualize", text = shiny::tags$span(shiny::icon("chart-line"), "Visualize"))
                                      )),
    sidebar = bs4Dash::dashboardSidebar(disable = FALSE,
                                        elevation = 2,
                                        collapsed = TRUE,
                                        minified = FALSE,
                                        expandOnHover = TRUE,
                                        fixed = TRUE,
                                        # text = "Dashboard filters",
                                        # icon = shiny::icon("filter"),
                                        shiny::h6("Filter by:"),
                                        id = "filter_sidebar",
                                        shiny::selectizeInput(inputId = "tdc_table_filter_location",
                                                              label = "Collection Location",
                                                              multiple = TRUE,
                                                              options = list('plugins' = list('remove_button'), 'create' = TRUE, 'persist'
                                                                             = FALSE),
                                                              choices = c("",
                                                                          rev(unique(tdc_data$Location_label)))
                                        ),
                                        shiny::selectizeInput(inputId = "tdc_table_filter_species",
                                                              label = "Species",
                                                              multiple = TRUE,
                                                              options = list('plugins' = list('remove_button'), 'create' = TRUE, 'persist'
                                                                             = FALSE),
                                                              choices = c("",
                                                                          rev(unique(tdc_data$Species_label)))
                                        ),
                                        shiny::selectizeInput(inputId = "tdc_table_filter_run",
                                                              label = "Run",
                                                              multiple = TRUE,
                                                              options = list('plugins' = list('remove_button'), 'create' = TRUE, 'persist'
                                                                             = FALSE),
                                                              choices = c("",
                                                                          rev(unique(tdc_data$Run_label)))
                                        ),
                                        shiny::selectizeInput(inputId = "tdc_table_filter_tissue",
                                                              label = "Tissue",
                                                              multiple = TRUE,
                                                              options = list('plugins' = list('remove_button'), 'create' = TRUE, 'persist'
                                                                             = FALSE),
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
    body = bs4Dash::dashboardBody(
      shiny::tags$style(type = "text/css",
                        "#tdc_data_map {height: calc(80vh) !important;
                    /* width: calc(80vh) !important; */
                    overflow-x: hidden;
                    overflow-y: hidden;}"),
      shiny::tags$head(
        shiny::tags$script(
          "$(function() {
          $('[data-card-widget=\"maximize\"]').on('click', function() {
            $('#tdc_data_map').trigger('resize');
          });
        });
        "
        )
      ),
      bs4Dash::tabItems(
        bs4Dash::tabItem(tabName = "welcome",
                         shiny::h3("Welcome to Vanishing Vitamin!"),
                         shiny::h5("This tab helps users get started.
                 It summarizes the app's primary functionality and directs users to where they can find more information."),
                         shiny::h5("Note to future self: link to ",
                                   shiny::a(href = "https://sites.google.com/ucdavis.edu/salmonintheclassroomresources/home",
                                            target = '_blank',
                                            "https://sites.google.com/ucdavis.edu/salmonintheclassroomresources/home"),
                                   " for resources, data, and other documentation."),
                         shiny::br(),
                         shiny::h5(shiny::strong("Click on one of the buttons at the top to get started!"))),
        bs4Dash::tabItem(tabName = "data",
                         shiny::fluidRow(shiny::column(width = 6,
                                                       bs4Dash::box(
                                                         reactable::reactableOutput(outputId = "tdc_data_table"),
                                                         width = 12,title = "Datasets",
                                                         collapsible = FALSE,closable = FALSE,maximizable = TRUE,
                                                         headerBorder = FALSE,solidHeader = FALSE,
                                                         style = 'height: calc(84.5vh); overflow-y:scroll'
                                                       )),
                                         shiny::column(width = 6,
                                                       bs4Dash::box(id = "tdc_data_map_box",
                                                                    leaflet::leafletOutput("tdc_data_map",width = "100%"),
                                                                    width = 12,title = "Data Collection Locations",
                                                                    collapsible = FALSE,closable = FALSE,maximizable = TRUE,
                                                                    headerBorder = FALSE,solidHeader = FALSE
                                                       ))
                         )
        ),
        bs4Dash::tabItem(tabName = "visualize",
                         shiny::fluidRow(
                           shiny::column(width = 3,
                                         bs4Dash::accordion(id = "visualize_accordion",
                                                            bs4Dash::accordionItem(collapsed = FALSE, status = "primary",
                                                                                   title = "Visualize your own data",
                                                                                   style = "height: calc(80vh); overflow-y:scroll",
                                                                                   icon = bsicons::bs_icon("plus"),
                                                                                   shiny::selectInput(inputId = "visualize_add_data_choice",
                                                                                                      label = "Choose how to add data:",
                                                                                                      choices = c("Manual entry",
                                                                                                                  "Copy + paste",
                                                                                                                  "Upload data file")),
                                                                                   shiny::conditionalPanel(
                                                                                     condition = "input.visualize_add_data_choice == 'Manual entry'",
                                                                                     shiny::numericInput(inputId = "visualize_add_data_manual_thiamin",
                                                                                                         label = "Thiamin Concentration (nmol/g):",min = 0,
                                                                                                         value = NULL),
                                                                                     shiny::numericInput(inputId = "visualize_add_data_manual_survival",
                                                                                                         label = "(Optional) % Survived:",
                                                                                                         min = 0, max = 100,
                                                                                                         value = NULL),
                                                                                     shiny::actionButton(inputId = "visualize_add_data_new_row",
                                                                                                         label = "Add data",
                                                                                                         icon = shiny::icon("plus"))
                                                                                   ),
                                                                                   shiny::conditionalPanel(
                                                                                     condition = "input.visualize_add_data_choice == 'Copy + paste'",
                                                                                     shiny::textAreaInput(inputId = "visualize_add_data_clipboard",
                                                                                                          label = "Copy + paste data below",
                                                                                                          placeholder = "Copy + paste data with a header row",
                                                                                                          resize = "vertical"),
                                                                                     shinyjs::hidden(
                                                                                       shiny::wellPanel(id = "visualize_add_data_clipboard_panel",
                                                                                                        width = 12,
                                                                                                        shiny::selectInput(inputId = "visualize_add_data_clipboard_thiamin_col",
                                                                                                                           label = "Thiamin Concentration column",
                                                                                                                           choices = ""),
                                                                                                        shiny::selectInput(inputId = "visualize_add_data_clipboard_survive_col",
                                                                                                                           label = "(Optional) % Survived column",
                                                                                                                           choices = ""),
                                                                                                        shiny::br(),
                                                                                                        shiny::actionButton(inputId = "visualize_add_data_clipboard_button",
                                                                                                                            label = "Add data",
                                                                                                                            icon = shiny::icon("plus"))
                                                                                       )
                                                                                     )
                                                                                   ),
                                                                                   shiny::conditionalPanel(
                                                                                     condition = "input.visualize_add_data_choice == 'Upload data file'",
                                                                                     shiny::fileInput(inputId = "visualize_add_data_file",
                                                                                                      label = "Select a file",
                                                                                                      accept = c(".csv",".xlsx"),
                                                                                                      placeholder = "Select a csv or xlsx file",
                                                                                                      multiple = FALSE),
                                                                                     shinyjs::hidden(
                                                                                       shiny::wellPanel(id = "visualize_add_data_file_panel",
                                                                                                        width = 12,
                                                                                                        shiny::selectInput(inputId = "visualize_add_data_file_thiamin_col",
                                                                                                                           label = "Thiamin Concentration column",
                                                                                                                           choices = ""),
                                                                                                        shiny::selectInput(inputId = "visualize_add_data_file_survive_col",
                                                                                                                           label = "(Optional) % Survived column",
                                                                                                                           choices = ""),
                                                                                                        shiny::br(),
                                                                                                        shiny::actionButton(inputId = "visualize_add_data_upload",
                                                                                                                            label = "Add data",
                                                                                                                            icon = shiny::icon("plus"))
                                                                                       )
                                                                                     )
                                                                                   )
                                                            )
                                         )
                           ),
                           bs4Dash::box(
                             collapsible = FALSE, maximizable = TRUE, title = "Thiamin Concentration vs. % Survived", width = 9,
                             plotly::plotlyOutput("ec50_curve", height = "550px", width = '100%'),
                             reactable::reactableOutput("visualize_add_data")
                           )
                         )
        )
      )
    )
  )

}
