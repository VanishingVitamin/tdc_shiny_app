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
#' @keywords internal
#' @noRd

app_ui <- function(tdc_data){

  location_info <- tdc_data |>
    dplyr::distinct(data_collection_region,
                    Location_label) |>
    dplyr::filter(data_collection_region != "UNCATEGORIZED") |>
    dplyr::arrange(data_collection_region, Location_label)

  location_select_list <-
    location_info |>
    dplyr::group_by(data_collection_region) |>
    dplyr::group_split() |>
    purrr::map(~ {

      .x$Location_label
      # |>
      #   purrr::set_names(.x$Location_label |>
      #                      stringr::str_wrap(width = 14) |>
      #                      stringr::str_replace_all("\n","</br>")
      #                    )

    }) |>
    purrr::set_names(unique(location_info$data_collection_region))
  # purrr::set_names(stringr::str_wrap(unique(location_info$data_collection_region), width = 10))
  bs4Dash::dashboardPage(
    scrollToTop = TRUE,
    freshTheme = app_theme(),
    help = NULL,
    dark = NULL,
    # controlbar = dashboardControlbar(disable = TRUE,width = 0, overlay = TRUE),
    header = bs4Dash::dashboardHeader(title = shiny::h5("Vanishing Vitamin", style = "padding-left:10px;"),
                                      sidebarIcon = shiny::icon("square-plus",
                                                                style = "font-size:22px;",
                                                                id = "header_toggle"),
                                      bs4Dash::navbarMenu(
                                        id = "navmenu",
                                        shinyjs::useShinyjs(),
                                        bs4Dash::navbarTab(tabName = "welcome", text = shiny::tags$span(shiny::icon("fish-fins"), "Welcome!")),
                                        bs4Dash::navbarTab(tabName = "data", text = shiny::tags$span(shiny::icon("table"), "Data")),
                                        bs4Dash::navbarTab(tabName = "visualize", text = shiny::tags$span(shiny::icon("chart-line"), "Visualize")),
                                        bs4Dash::navbarTab(tabName = "disclaimer", text = shiny::tags$span(shiny::icon("triangle-exclamation"), "Disclaimer"))
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
                                        # tags$style(    type = 'text/css',
                                        #                ".vscomp-dropdown-container { word-wrap : break-word;}
                                        #                 .vscomp-dropdown-container { word-break : break-word;}
                                        #                 .vscomp-dropdown-container { whitespace : normal;}"
                                        # ),
                                        shinyWidgets::virtualSelectInput(inputId = "tdc_table_filter_location",
                                                                         label = "Collection Location",
                                                                         multiple = TRUE,
                                                                         showValueAsTags = TRUE,
                                                                         search = TRUE,
                                                                         showSelectedOptionsFirst = TRUE,
                                                                         allowNewOption = FALSE,
                                                                         hideClearButton = FALSE,
                                                                         autoSelectFirstOption = FALSE,
                                                                         disableSelectAll = TRUE,
                                                                         dropboxWidth = "350px",
                                                                         maxWidth = "350px",
                                                                         zIndex = 10000
                                                                         , choices = location_select_list
                                                                         # ,choices = sort(unique(tdc_data$Location_label))
                                        ),
                                        shinyWidgets::virtualSelectInput(inputId = "tdc_table_filter_species",
                                                                         label = "Species",
                                                                         multiple = TRUE,
                                                                         showValueAsTags = TRUE,
                                                                         search = FALSE,
                                                                         showSelectedOptionsFirst = TRUE,
                                                                         allowNewOption = FALSE,
                                                                         hideClearButton = FALSE,
                                                                         autoSelectFirstOption = FALSE,
                                                                         disableSelectAll = TRUE,
                                                                         dropboxWidth = "350px",
                                                                         maxWidth = "350px",
                                                                         zIndex = 10000
                                                                         ,choices = sort(unique(tdc_data$Species_label))
                                        ),
                                        shinyWidgets::virtualSelectInput(inputId = "tdc_table_filter_run",
                                                                         label = "Run",
                                                                         multiple = TRUE,
                                                                         showValueAsTags = TRUE,
                                                                         search = FALSE,
                                                                         showSelectedOptionsFirst = TRUE,
                                                                         allowNewOption = FALSE,
                                                                         hideClearButton = FALSE,
                                                                         autoSelectFirstOption = FALSE,
                                                                         disableSelectAll = TRUE,
                                                                         dropboxWidth = "350px",
                                                                         maxWidth = "350px",
                                                                         zIndex = 10000
                                                                         ,choices = sort(unique(tdc_data$Run_label))
                                        ),
                                        shinyWidgets::virtualSelectInput(inputId = "tdc_table_filter_tissue",
                                                                         label = "Tissue",
                                                                         multiple = TRUE,
                                                                         showValueAsTags = TRUE,
                                                                         search = FALSE,
                                                                         showSelectedOptionsFirst = TRUE,
                                                                         allowNewOption = FALSE,
                                                                         hideClearButton = FALSE,
                                                                         autoSelectFirstOption = FALSE,
                                                                         disableSelectAll = TRUE,
                                                                         dropboxWidth = "350px",
                                                                         maxWidth = "350px",
                                                                         zIndex = 10000
                                                                         ,choices = sort(unique(tdc_data$Tissue_label))
                                        )
    ),
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
      shiny::tags$style(HTML(tabset_styling())),
      bs4Dash::tabItems(
        bs4Dash::tabItem(tabName = "welcome",
                         shiny::h3("Welcome to Vanishing Vitamin!"),
                         shiny::h5("This tab helps users get started.
                 It summarizes the app's primary functionality and directs users to where they can find more information."),
                         bs4Dash::tabsetPanel(id = "splash_tabset", type = "tabs",
                                              shiny::tabPanel(title = "Background",
                                                              shiny::uiOutput("splash_page_background")),
                                              shiny::tabPanel(title = "What causes TDC?",
                                                              shiny::uiOutput("splash_page_what_causes_tdc")),
                                              shiny::tabPanel(title = "Vitamers",
                                                              shiny::uiOutput("splash_page_vitamers"))
                         )
        ),
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
                                                                                                          label = "Copy + paste data below (separted by space)",
                                                                                                          placeholder = "Thiamin_conc\tPercent_survive\n1.234\t56.78",
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
                                                                                     shiny::tags$style(type = "text/css", "#visualize_add_data_template {color: black; text-decoration: underline;}"),
                                                                                     shiny::tags$style(type = "text/css", "#visualize_add_data_template:hover {font-weight: bold;}"),
                                                                                     shiny::downloadLink(outputId = "visualize_add_data_template",
                                                                                                         label = "Download template data file"),
                                                                                     shiny::br(),shiny::br(),
                                                                                     shiny::fileInput(inputId = "visualize_add_data_file",
                                                                                                      label = "Select a file",
                                                                                                      accept = c(".csv",".xlsx"),
                                                                                                      placeholder = "Upload a csv or xlsx file",
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
                             shiny::br(),
                             reactable::reactableOutput("visualize_add_data")
                           )
                         )
        ),
        bs4Dash::tabItem(tabName = "disclaimer",
                         shiny::wellPanel(
                           shiny::h2("Disclaimer"),
                           shiny::tags$strong("This software is preliminary or provisional and is subject to revision. It is
being provided to meet the need for timely best science. The software has not
received final approval by the U.S. Geological Survey (USGS). No warranty,
expressed or implied, is made by the USGS or the U.S. Government as to the
functionality of the software and related material nor shall the fact of release
constitute any such warranty. The software is provided on the condition that
neither the USGS nor the U.S. Government shall be held liable for any damages
resulting from the authorized or unauthorized use of the software.
")
                         )
        )
      )
    )
  )

}

tabset_styling <- function(){

  ".inside-tabs {
    margin-left: -32px;
    margin-top: -16px;
}

.inside-tabs > .nav {
    margin-left: -30px;
    margin-right: -30px;
}

.tab-content {
    margin-left: 30px;
    margin-right: 30px;
}

.nav-tabs > li > a {
    color:#00000050;
    border: 1px solid #ddd;
    padding-left: 75px;
    padding-right: 75px;
    background-color: #fcfcfc;
}

.nav-tabs > li.active > a {
    color:#000000;
    border: 1px solid #ddd;
    padding-left: 75px;
    padding-right: 75px;
    background-color: #ffffff;
    border-left: 1px solid #ddd;
    border-right: 1px solid #ddd;
    border-bottom: none;
    border-top: none;
    box-shadow: none;
    color: #30256c;
    font-weight: 300;
}

.nav-tabs>li.active>a:hover, .nav-tabs>li.active>a:focus:hover {
    border: none;
    color: #ddd;
    color: #30256c;
}

.nav-tabs>li.active>a, .nav-tabs>li.active>a:focus {
    border: none;
    -webkit-box-shadow: none;
    box-shadow: none;
    color: #30256c;
}"

}
