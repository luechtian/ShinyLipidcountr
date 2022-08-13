#' 5_rf UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_5_rf_ui <- function(id, tabName){
  ns <- NS(id)

  shinydashboard::tabItem(tabName = tabName,

                          fluidRow(
                            shinydashboard::box(
                              actionButton(ns("calc_rf"),"Use Response factors"),
                              actionButton(ns("omit_rf"),"Omit Response factors"),
                              title = "Response factors"
                            )
                          ),

                          fluidRow(
                            shinydashboard::box(
                              DT::dataTableOutput(ns("rf_check")),
                              width = 12
                            )
                          )
  )
}

#' 5_rf Server Functions
#'
#' @noRd
mod_5_rf_server <- function(id, data_blank, input_blank){

  moduleServer(id, function(input, output, session){
    ns <- session$ns

    rf_data <- reactiveValues()

    observeEvent(input$calc_rf, {

      rf_data$data <- data_blank  %>%
        dplyr::mutate(pmol_corr2 = pmol_corr * rf_values) %>%
        dplyr::select(-rf_values)

      output$rf_check <- DT::renderDataTable({

        # validate(
        #   need(input_blank, "Please proceed with blank subtraction first.")
        # )

        rf_data$data

      })

    })

    observeEvent(input$omit_rf, {

      rf_data$data <- data_blank %>%
        dplyr::select(-rf_values)

    })

    list(data = reactive(rf_data$data))

  })
}

## To be copied in the UI
# mod_5_rf_ui("rf_ui_1", tabName)

## To be copied in the server
# mod_5_rf_server("rf_ui_1", data_blank)
