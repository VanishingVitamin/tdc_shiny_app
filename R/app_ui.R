#' Defines the User Interface elements for the Vanishing Vitamin Shiny app
#'
#' This is an internal function that is used within the exported launch_app()
#' function.
#'
#' @param tdc_data data set containing Thiamin by Survivability data. Should be
#'   the data set exported by the vanishingVitamin package,
#'   vanishingVitamin::tdc_data
#'
#' @value \code{bs4Dash::dashboardPage} object containing UI elements

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

    }) |>
    purrr::set_names(unique(location_info$data_collection_region))

  bs4Dash::dashboardPage(
    scrollToTop = TRUE,
    freshTheme = app_theme(),
    help = NULL,
    dark = NULL,
    # controlbar = dashboardControlbar(disable = TRUE,width = 0, overlay = TRUE),
    header = bs4Dash::dashboardHeader(title = shiny::h5("Vanishing Vitamin", style = "padding-left:10px;"),
                                      sidebarIcon = shiny::icon("caret-square-right",
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
                                                                         # dropboxWidth = "500px",
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
                                                                         # dropboxWidth = "500px",
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
                                                                         # dropboxWidth = "500px",
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
                                                                         # dropboxWidth = "500px",
                                                                         zIndex = 10000
                                                                         ,choices = sort(unique(tdc_data$Tissue_label))
                                        )
                                        # shiny::selectizeInput(inputId = "tdc_table_filter_location",
                                        #                       label = "Collection Location",
                                        #                       multiple = TRUE,
                                        #                       options = list('plugins' = list('remove_button'), 'create' = TRUE, 'persist'
                                        #                                      = FALSE),
                                        #                       choices = c("",
                                        #                                   sort(unique(tdc_data$Location_label)))
                                        # ),
                                        # shiny::selectizeInput(inputId = "tdc_table_filter_species",
                                        #                       label = "Species",
                                        #                       multiple = TRUE,
                                        #                       options = list('plugins' = list('remove_button'), 'create' = TRUE, 'persist'
                                        #                                      = FALSE),
                                        #                       choices = c("",
                                        #                                   sort(unique(tdc_data$Species_label)))
                                        # ),
                                        # shiny::selectizeInput(inputId = "tdc_table_filter_run",
                                        #                       label = "Run",
                                        #                       multiple = TRUE,
                                        #                       options = list('plugins' = list('remove_button'), 'create' = TRUE, 'persist'
                                        #                                      = FALSE),
                                        #                       choices = c("",
                                        #                                   sort(unique(tdc_data$Run_label)))
                                        # ),
                                        # shiny::selectizeInput(inputId = "tdc_table_filter_tissue",
                                        #                       label = "Tissue",
                                        #                       multiple = TRUE,
                                        #                       options = list('plugins' = list('remove_button'), 'create' = TRUE, 'persist'
                                        #                                      = FALSE),
                                        #                       choices = c("",
                                        #                                   sort(unique(tdc_data$Tissue_label)))
                                        # )
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
                         shiny::h5("This tab will contain EC50 curves for survival % vs. thiamin concentration."),
                         shiny::sidebarLayout(
                           sidebarPanel = shiny::div(id = "tdc_table_filter_sidebar",
                                                     class = "col-sm-2",
                                                     shiny::sidebarPanel(width = 10,
                                                                         "This sidebar will let users filter the scatterplot.")),
                           mainPanel = shiny::mainPanel(width = 10,
                                                        plotly::plotlyOutput("ec50_curve",height = "800px"))
                         ))
      )
    )
  )
}
