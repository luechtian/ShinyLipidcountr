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
                              rhandsontable::rHandsontableOutput(ns("rf_values")),
                              textOutput(ns("rf_warning")),
                              width = 12
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

    output$rf_values <- rhandsontable::renderRHandsontable({

      validate(
        need(data_blank, "Please proceed with blank subtraction first."),
        need(rf_data$data, ""),
      )

      rhandsontable::rhandsontable(rf_data$data %>%
                                     dplyr::select(class, rf_values) %>%
                                     dplyr::mutate(rf_values =
                                                     round(rf_values,
                                                           digits = 2)
                                     ) %>%
                                     unique() %>%
                                     t(),
                                   readOnly = TRUE
      )

    })

    rf_data <- reactiveValues()

    observeEvent(input$calc_rf, {

      rf_data$data <- data_blank %>%
        impute_rf(blank_sub = TRUE, kon = "Kon") %>%
        apply_rf()

      output$rf_check <- DT::renderDataTable({

        rf_data$data

      })

      output$rf_warning <- renderText({

        if(any(is.na(rf_data$data$rf_values))){
          stop("Some response factors are missing!")
        }

      })

    })

    observeEvent(input$omit_rf, {

      rf_data$data <- data_blank %>%
        dplyr::mutate(rf_values = as.numeric(rf_values))

      output$rf_check <- DT::renderDataTable({

        rf_data$data

      })

      output$rf_warning <- renderText({

        "No response factors were used."

      })

    })

    list(data = reactive(rf_data$data))

  })
}

## To be copied in the UI
# mod_5_rf_ui("rf_ui_1", tabName)

## To be copied in the server
# mod_5_rf_server("rf_ui_1", data_blank)
