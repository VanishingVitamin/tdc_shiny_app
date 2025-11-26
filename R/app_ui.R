#' Defines the User Interface elements for the Vanishing Vitamin Shiny app
#'
#' This is an internal function that is used within the exported launch_app()
#' function.
#'
#' @param tdc_data data set containing Thiamine by Survivability data. Should be
#'   the data set exported by the vanishingVitamin package,
#'   vanishingVitamin::tdc_data
#'
#' @return \code{bs4Dash::dashboardPage} object containing UI elements
#' @keywords internal
#' @noRd

app_ui <- function(tdc_data, translator) {

  # initial data processing
  location_info <- tdc_data |>
    dplyr::distinct(region, collection_locations) |>
    dplyr::arrange(region, collection_locations)

  location_select_list <-
    location_info |>
    dplyr::group_by(region) |>
    dplyr::group_split() |>
    purrr::map(
      ~ {
        .x$collection_locations
      }
    ) |>
    purrr::set_names(unique(location_info$region))

  # definition of app UI:
  bs4Dash::dashboardPage(
    scrollToTop = TRUE,
    freshTheme = app_theme(),
    help = NULL,
    dark = NULL,
    footer = bs4Dash::dashboardFooter(
      right = shiny::includeHTML("www/footer.html")
    ),
    # header: bar at top of page
    header = bs4Dash::dashboardHeader(fixed = TRUE,
      title = shiny::h5("VanishingVitamin", style = "padding-left:10px;"),
      sidebarIcon = shiny::icon(
        "square-plus",
        style = "font-size:22px;",
        id = "header_toggle"
      ),
      # elements in top-right corner of screen:
      rightUi = shiny::tags$li(
        class = "dropdown",
        shiny::div(
          style = "display: flex; align-items: center; gap: 20px; padding-right: 20px;",

          # Help button
          shiny::actionButton(
            inputId = "help",
            label = shiny::uiOutput("help_button_translation", inline = TRUE),
            icon = shiny::icon("question")
          ),

          # Language selector
          shiny::HTML('
      <div class="form-group shiny-input-container" style="margin: 0; width: 80px;">
        <div style="display: flex; align-items: center;">
          <i class="fas fa-language" style="font-size: 20px;"></i>
          <select id="selected_language" class="shiny-input-select" style="margin-left: 5px;">
            <option selected value="en">en</option>
            <option value="es">es</option>
            <option value="fi">fi</option>
            <option value="sv">sv</option>
          </select>
        </div>
      </div>'
          )
        )
      ),
      # USGS logo in top-left of screen
      shiny::a(
        href = "https://www.usgs.gov",
        target = "_blank",
        shiny::img(
          src = "https://earthquake.usgs.gov/data/comcat/logos/us.svg",
          height = "50"
        )
      ),
      # Menu elements
      bs4Dash::navbarMenu(
        id = "navmenu",
        skin = "light",
        shinyjs::useShinyjs(),
        shiny::includeCSS("www/stylesheets/common.css"),
        bs4Dash::navbarTab(
          tabName = "welcome",
          text = shiny::tags$span(shiny::icon("fish-fins"), shiny::uiOutput(outputId = "welcome_tabname",inline = TRUE))
        ),
        bs4Dash::navbarTab(
          tabName = "data",
          text = shiny::tags$span(shiny::icon("table"), shiny::uiOutput(outputId = "data_tabname",inline = TRUE))
        ),
        bs4Dash::navbarTab(
          tabName = "visualize",
          text = shiny::tags$span(shiny::icon("chart-line"), shiny::uiOutput(outputId = "visualize_tabname",inline = TRUE))
        ),
        bs4Dash::navbarTab(
          tabName = "about",
          text = shiny::tags$span(
            shiny::icon("info"),
            shiny::uiOutput(outputId = "about_tabname",inline = TRUE)
          )
        )
      )
    ),
    # Sidebar elements (app filters)
    sidebar = bs4Dash::dashboardSidebar(
      disable = FALSE,
      elevation = 2,
      collapsed = TRUE,
      minified = FALSE,
      expandOnHover = TRUE,
      fixed = TRUE,
      shiny::h6(shiny::uiOutput("sidebar_title_translation", inline = TRUE)),
      id = "filter_sidebar",
      shinyWidgets::virtualSelectInput(
        inputId = "tdc_table_filter_species",
        label = shiny::uiOutput("sidebar_species_translation", inline = TRUE),
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
        zIndex = 10000,
        choices = sort(unique(tdc_data$Species_label))
      ),
      shinyWidgets::virtualSelectInput(
        inputId = "tdc_table_filter_location",
        label = shiny::uiOutput("sidebar_collection_location_translation", inline = TRUE),
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
        zIndex = 10000,
        choices = location_select_list
      ),
      # Hide these UI elements (for now)
      shinyjs::hidden(
        shiny::tagList(
          shinyWidgets::virtualSelectInput(
            inputId = "tdc_table_filter_run",
            label = shiny::uiOutput("sidebar_run_translation", inline = TRUE),
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
            zIndex = 10000,
            choices = sort(unique(tdc_data$Run_label))
          ),
          shinyWidgets::virtualSelectInput(
            inputId = "tdc_table_filter_tissue",
            label = shiny::uiOutput("sidebar_tissue_translation", inline = TRUE),
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
            zIndex = 10000,
            choices = sort(unique(tdc_data$Tissue_label))
          )
        )
      ),
      shiny::checkboxInput(
        inputId = "tdc_table_filter_unpublished",
        label = shiny::uiOutput("sidebar_unpublished_data_translation", inline = TRUE),
        value = TRUE
      )
    ),
    # The main screen of the app
    body = bs4Dash::dashboardBody(
      # adds a spinner animation if app is loading:
      shinybusy::add_busy_spinner(position = "bottom-right",
                                  color = "#00000066"),
      # needed to translate app content:
      shiny.i18n::usei18n(translator),
      # controls size of map in Data tab:
      shiny::tags$style(
        type = "text/css",
        "#tdc_data_map {height: calc(80vh) !important;
                    /* width: calc(80vh) !important; */
                    overflow-x: hidden;
                    overflow-y: hidden;}"
      ),
      # Resizes Data tab map if the "fullscreen" button is clicked (can't
      # remember where I found this JavaScript code):
      shiny::tags$head(
        shiny::tags$script(
          "$(function() {
          $('[data-card-widget=\"maximize\"]').on('click', function() {
            $('#tdc_data_map').trigger('resize');
          });
        });
        "
        ),
        shiny::includeHTML("www/google-analytics-header.html")
      ),
      # Start of per-tab content
      bs4Dash::tabItems(
        bs4Dash::tabItem(
          tabName = "welcome", #NOTE: tabName must match name given in navbarMenu above

          # The Welcome page content is entirely rendered on the server side,
          # since the user can pick different languages. See
          # translation_server.R
          shiny::uiOutput(outputId = "welcome_page_content")
        ),
        # Start of "Data" tab UI content:
        bs4Dash::tabItem(
          tabName = "data",
          shiny::fluidRow(
            # Left side of page is table (reactable) summarizing avaiable data
            # sets by publication:
            shiny::column(
              width = 6,
              bs4Dash::box(
                reactable::reactableOutput(outputId = "tdc_data_table"),
                width = 12,
                title = shiny::tagList(shiny::uiOutput("data_datasets_translation", inline = TRUE)),
                # Button dropdown lets user download *published* data sets
                dropdownMenu = bs4Dash::boxDropdown(
                  bs4Dash::boxDropdownItem(
                    shiny::actionButton(
                      "dataset_download_popup",
                      label = shiny::uiOutput("data_download_data_translation", inline = TRUE),
                      icon = shiny::icon("download")
                    )
                  )
                ),
                collapsible = FALSE,
                closable = FALSE,
                maximizable = TRUE,
                headerBorder = FALSE,
                solidHeader = FALSE,
                style = 'height: calc(84.5vh); overflow-y:scroll'
              )
            ),
            # Right side of page is leaflet map showing collection locations:
            shiny::column(
              width = 6,
              bs4Dash::box(
                id = "tdc_data_map_box",
                leaflet::leafletOutput("tdc_data_map", width = "100%"),
                width = 12,
                title = shiny::uiOutput("data_collection_locations_translation", inline = TRUE),
                collapsible = FALSE,
                closable = FALSE,
                maximizable = TRUE,
                headerBorder = FALSE,
                solidHeader = FALSE
              )
            )
          )
        ),
        # Beginning of "Visualize" tab UI content:
        bs4Dash::tabItem(
          tabName = "visualize",
          shiny::fluidRow(
            # First column contains "add your own data" UI elements:
            shiny::column(
              width = 3,
              bs4Dash::accordion(
                id = "visualize_accordion",
                bs4Dash::accordionItem(
                  collapsed = FALSE,
                  status = "white",
                  title = shiny::uiOutput("visualize_your_own_data_translation", inline = TRUE),
                  style = "height: calc(80vh); overflow-y:scroll",
                  icon = bsicons::bs_icon("plus"),
                  # hide the ability to change data upload option (for
                  # foreseeable future). The copy + paste and file upload
                  # options need some updated server logic updates to select
                  # specific Species
                  shinyjs::hidden(
                    shiny::selectInput(
                      inputId = "visualize_add_data_choice",
                      label = "Choose how to add data:",
                      choices = c(
                        "Manual entry",
                        "Copy + paste",
                        "Upload data file"
                      )
                    )
                  ),
                  shiny::conditionalPanel(
                    condition = "input.visualize_add_data_choice == 'Manual entry'",
                    shinyWidgets::virtualSelectInput(
                      inputId = "visualize_add_data_manual_species",
                      label = shiny::uiOutput("visualize_select_a_species_translation", inline = TRUE),
                      multiple = FALSE,
                      showValueAsTags = TRUE,
                      search = FALSE,
                      showSelectedOptionsFirst = TRUE,
                      allowNewOption = FALSE,
                      hideClearButton = FALSE,
                      autoSelectFirstOption = FALSE,
                      disableSelectAll = TRUE,
                      dropboxWidth = "350px",
                      maxWidth = "350px",
                      zIndex = 10000,
                      choices = sort(unique(tdc_data$Species_label))
                    ),
                    shiny::numericInput(
                      inputId = "visualize_add_data_manual_thiamin",
                      label = shiny::uiOutput("visualize_thiamine_concentration_translation", inline = TRUE),
                      min = 0,
                      value = NULL
                    ),
                    shiny::numericInput(
                      inputId = "visualize_add_data_manual_survival",
                      label = shiny::uiOutput("visualize_percent_survived_translation", inline = TRUE),
                      min = 0,
                      max = 100,
                      value = NULL
                    ),
                    shiny::actionButton(
                      inputId = "visualize_add_data_new_row",
                      label = shiny::uiOutput("visualize_add_data_translation", inline = TRUE),
                      icon = shiny::icon("plus")
                    )
                  ),
                  ## NOTE: no longer relevant now that "Manual entry" is the
                  ## only available option:

                  # shiny::conditionalPanel(
                  #   condition = "input.visualize_add_data_choice == 'Copy + paste'",
                  #   shiny::textAreaInput(
                  #     inputId = "visualize_add_data_clipboard",
                  #     label = "Copy + paste data below (separated by space)",
                  #     placeholder = "Thiamine_conc\tPercent_survive\n1.234\t56.78",
                  #     resize = "vertical"
                  #   ),
                  #   # hide the ability to change data upload option for now. They
                  #   # need some server logic updates to select specific Species
                  #   shinyjs::hidden(
                  #     shiny::wellPanel(
                  #       id = "visualize_add_data_clipboard_panel",
                  #       width = 12,
                  #       shiny::selectInput(
                  #         inputId = "visualize_add_data_clipboard_thiamine_col",
                  #         label = "Thiamine Concentration column",
                  #         choices = ""
                  #       ),
                  #       shiny::selectInput(
                  #         inputId = "visualize_add_data_clipboard_survive_col",
                  #         label = "(Optional) % Survived column",
                  #         choices = ""
                  #       ),
                  #       shiny::br(),
                  #       shiny::actionButton(
                  #         inputId = "visualize_add_data_clipboard_button",
                  #         label = "Add data",
                  #         icon = shiny::icon("plus")
                  #       )
                  #     )
                  #   )
                  # ),
                  # shiny::conditionalPanel(
                  #   condition = "input.visualize_add_data_choice == 'Upload data file'",
                  #   shiny::tags$style(
                  #     type = "text/css",
                  #     "#visualize_add_data_template {color: black; text-decoration: underline;}"
                  #   ),
                  #   shiny::tags$style(
                  #     type = "text/css",
                  #     "#visualize_add_data_template:hover {font-weight: bold;}"
                  #   ),
                  #   shiny::downloadLink(
                  #     outputId = "visualize_add_data_template",
                  #     label = "Download template data file"
                  #   ),
                  #   shiny::br(),
                  #   shiny::br(),
                  #   shiny::fileInput(
                  #     inputId = "visualize_add_data_file",
                  #     label = "Select a file",
                  #     accept = c(".csv", ".xlsx"),
                  #     placeholder = "Upload a csv or xlsx file",
                  #     multiple = FALSE
                  #   ),
                  #   shinyjs::hidden(
                  #     shiny::wellPanel(
                  #       id = "visualize_add_data_file_panel",
                  #       width = 12,
                  #       shiny::selectInput(
                  #         inputId = "visualize_add_data_file_thiamine_col",
                  #         label = "Thiamine Concentration column",
                  #         choices = ""
                  #       ),
                  #       shiny::selectInput(
                  #         inputId = "visualize_add_data_file_survive_col",
                  #         label = "(Optional) % Survived column",
                  #         choices = ""
                  #       ),
                  #       shiny::br(),
                  #       shiny::actionButton(
                  #         inputId = "visualize_add_data_upload",
                  #         label = "Add data",
                  #         icon = shiny::icon("plus")
                  #       )
                  #     )
                  #   )
                  # )
                )
              )
            ),
            # To the right of "add your own data" are thiamine vs. survival %
            # scatterplot and table:
            bs4Dash::box(
              collapsible = FALSE,
              maximizable = TRUE,
              title = shiny::uiOutput("visualize_thiamine_concentration_vs_percent_survived_translation", inline = TRUE),
              width = 9,
              plotly::plotlyOutput(
                "ec50_curve",
                height = "550px",
                width = '100%'
              ),
              shiny::br(),
              shiny::tagList(
                # User-entered thiamin vs. survival % table summary:
                reactable::reactableOutput("visualize_add_data"),
                shiny::br(),
                # user can download table content, but we'll keep the button
                # hidden at first. Only show once the table renders (see
                # visualize_tab_server.R)
                shinyjs::hidden(
                  shiny::tagList(
                    csvDownloadButton(button_id = "visualize_data_download",
                                      table_id = "visualize_add_data",
                                      filename = "data.csv",
                                      label = shiny::uiOutput("visualize_download_table_translation", inline = TRUE))
                  )
                )
              )
            )
          )
        ),
        # Beginning of "About" tab content
        bs4Dash::tabItem(
          tabName = "about",
          shiny::wellPanel(
            shiny::fluidRow(
              shiny::column(width = 4,
                            shiny::div(
                              style = "text-align: center;",
                              shiny::a(href = "https://www.nceas.ucsb.edu/",
                                       target = "_blank",
                                       shiny::img(
                                         src = "www/nceas_logo_full.png",
                                         height = "110"
                                       ))
                            )
              ),
              # shiny::column(width = 3,
              #               shiny::div(
              #                 style = "text-align: center;",
              #                 shiny::a(href = "https://www.usgs.gov",
              #                          target = "_blank",
              #                          shiny::img(
              #                            src = "https://earthquake.usgs.gov/data/comcat/logos/us.svg",
              #                            height = "110"
              #                          ))
              #               )
              # ),
              shiny::column(width = 4,
                            shiny::div(
                              style = "text-align: center;",
                              shiny::a(href = "https://www.usgs.gov/john-wesley-powell-center",
                                       target = "_blank",
                                       shiny::img(
                                         src = "www/powell_center_logo.png",
                                         height = "110"
                                       ))
                            )
              ),
              shiny::column(width = 4,
                            shiny::div(
                              style = "text-align: center;",
                              shiny::a(href = "https://www.nsf.gov/",
                                       target = "_blank",
                                       shiny::img(
                                         src = "www/nsf_logo.png",
                                         height = "110"
                                       ))
                            )
              )
            ),
            shiny::h2("About the app"),
            shiny::p("This work resulted from funding for the Vanishing Vitamin Working Group from the National Center for Ecological Analysis and Synthesis Morpho program at the University of California, Santa Barbara; the John Wesley Powell Center for Analysis and Synthesis; and the National Science Foundation."),
            shiny::p("Contact Freya Rowland (",shiny::a(href = 'mailto:frowland@usgs.gov', "frowland@usgs.gov"),") with questions about the app."),
            shiny::p("View the source code for this app at: ", shiny::a(href = "https://github.com/VanishingVitamin/tdc_shiny_app", target='_blank', "https://github.com/VanishingVitamin/tdc_shiny_app")),
            # shiny::p(shiny::strong("Authors: "),"Joe Zemmels (maintainer), Matthew Futia, Freya Rowland, Miles Daniels"),
            # shiny::p(shiny::strong("Translators: "), "(Swedish) Elin Boalt, Caroline Ek, Samuel Hylander, (Spanish) Rebecca VanArnam, (Finnish) Jenni Prokkola"),
            # shiny::p(shiny::strong("Contributors (alphabetical by last name): "), "Marc Hauber, Dale Honeyfield, Kathrine Howard, Rachel Johnson, Shannon Kelly, Clifford Kraft, Nathaniel Mantua, Jacques Rinchard, Christopher Suffridge, Donald Tillitt, Vittoria Todisco, David Walters, Abigail Ward"),
            shiny::h2("Disclaimer"),
            "This software is preliminary or provisional and is subject to revision. It is
being provided to meet the need for timely best science. The software has not
received final approval by the U.S. Geological Survey (USGS). No warranty,
expressed or implied, is made by the USGS or the U.S. Government as to the
functionality of the software and related material nor shall the fact of release
constitute any such warranty. The software is provided on the condition that
neither the USGS nor the U.S. Government shall be held liable for any damages
resulting from the authorized or unauthorized use of the software.
"
          )
        )
      )
    )
  )

}

#' Creates a download button for Reactable table
#'
#' @param button_id button element ID
#' @param table_id ID of table to be downloaded
#' @param filename output filename
#' @param label button label
#'
#' @noRd
#' @keywords internal
#' @source https://glin.github.io/reactable/articles/examples.html?q=download#csv-download-button-in-shiny
csvDownloadButton <- function(button_id, table_id, filename = "data.csv", label = "Download as CSV") {
  shiny::tags$button(
    id = button_id,
    shiny::tagList(shiny::icon("download"), label),
    onclick = sprintf("Reactable.downloadDataCSV('%s', '%s')", table_id, filename)
  )
}
