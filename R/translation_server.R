#' Server logic for translating app content
#'
#' @noRd
#' @keywords internal
translation_server <- function(input, output, session, translator){
  shiny::observe({
    shiny.i18n::update_lang(input$selected_language, session)
  })

  output$welcome_page_content <- shiny::renderUI({
    shiny::HTML(readLines(
      warn = FALSE,
      paste0("www/welcome_page/welcome_page_", input$selected_language, ".html")
    ))
  })

  # navbar content
  output$welcome_tabname <- shiny::renderUI({translator$translate("Welcome!")})
  output$data_tabname <- shiny::renderUI({translator$translate("Data")})
  output$visualize_tabname <- shiny::renderUI({translator$translate("Visualize")})
  output$about_tabname <- shiny::renderUI({translator$translate("About")})
  output$help_button_translation <- shiny::renderUI({translator$translate("Help")})
  output$help_popup_title_translation <- shiny::renderUI({translator$translate("Help")})
  output$help_popup_data_translation <- shiny::renderUI({translator$translate("Data")})
  output$help_popup_visualize_translation <- shiny::renderUI({translator$translate("Visualize")})

  # sidebar content
  output$sidebar_title_translation <- shiny::renderUI({translator$translate("Filter data")})
  output$sidebar_species_translation <- shiny::renderUI({translator$translate("Species")})
  output$sidebar_collection_location_translation <- shiny::renderUI({translator$translate("Collection Location")})
  output$sidebar_run_translation <- shiny::renderUI({translator$translate("Run")})
  output$sidebar_tissue_translation <- shiny::renderUI({translator$translate("Tissue")})
  output$sidebar_unpublished_data_translation <- shiny::renderUI({translator$translate("Show unpublished data")})

  # data tab content
  output$data_datasets_translation <- shiny::renderUI({translator$translate("Datasets")})
  output$data_collection_locations_translation <- shiny::renderUI({translator$translate("Data Collection Locations")})
  output$data_download_data_translation <- shiny::renderUI({translator$translate("Download Data")})

  # visualize tab content
  output$visualize_your_own_data_translation <- shiny::renderUI({translator$translate("Visualize your own data")})
  output$visualize_select_a_species_translation <- shiny::renderUI({translator$translate("Select a species")})
  output$visualize_thiamine_concentration_translation <- shiny::renderUI({translator$translate("Thiamine Concentration (nmol/g)")})
  output$visualize_percent_survived_translation <- shiny::renderUI({translator$translate("(Optional) % Survived")})
  output$visualize_add_data_translation <- shiny::renderUI({translator$translate("Add data")})
  output$visualize_thiamine_concentration_vs_percent_survived_translation <- shiny::renderUI({translator$translate("Thiamine Concentration vs. % Survived")})
  output$visualize_download_table_translation <- shiny::renderUI({translator$translate("Download table to CSV")})
}

#' Server logic for translating Help pop-up message content
#'
#' @noRd
#' @keywords internal
help_message_server <- function(input, output, session){

  shiny::observe({

    shiny::showModal(
      ui = shiny::modalDialog(
        title = shiny::uiOutput("help_popup_title_translation"), size = "xl", easyClose = TRUE,footer = NULL,
        bs4Dash::bs4TabCard(id = "help_tabs",width = 12,collapsible = FALSE,
                            status = "white",
                            solidHeader = TRUE,background = NULL,headerBorder = TRUE,footer = NULL,
                            shiny::tabPanel(
                              title = shiny::uiOutput("help_popup_data_translation"),
                              shiny::HTML(readLines(
                                warn = FALSE,
                                paste0("www/help_message/help_message_data_", input$selected_language, ".html")
                              ))
                            ),
                            shiny::tabPanel(
                              title = shiny::uiOutput("help_popup_visualize_translation"),
                              shiny::HTML(readLines(
                                warn = FALSE,
                                paste0("www/help_message/help_message_visualize_", input$selected_language, ".html")
                              ))
                            ))
      )
    )

  }) |>
    shiny::bindEvent(input$help)

  shiny::observe({

    if(input$navmenu == "visualize"){
      shiny::updateTabsetPanel(session = session, inputId = "help_tabs",
                               selected = "Visualize")
    } else{
      shiny::updateTabsetPanel(session = session, inputId = "help_tabs",
                               selected = "Data")
    }

  })

}
