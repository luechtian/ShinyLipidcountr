#' 6_uM UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_6_uM_ui <- function(id, tabName){
  ns <- NS(id)

  shinydashboard::tabItem(tabName = tabName,

                          fluidRow(
                            shinydashboard::box(
                              actionButton(ns("calc_uM"),"Calculate uM-values"),
                              checkboxInput(inputId = ns("sum_same_species"),
                                            label = "Sum same species (for DAG/TAG from LipidView only)",
                                            value = FALSE),
                              title = "pmol to uM"
                            )
                          ),

                          fluidRow(
                            shinydashboard::box(
                              DT::dataTableOutput(ns("uM_check")),
                              width = 12
                            )
                          )
  )
}

#' 6_uM Server Functions
#'
#' @noRd
mod_6_uM_server <- function(id, data_rf){

  moduleServer(id, function(input, output, session){
    ns <- session$ns

    uM_data <- eventReactive(input$calc_uM,{

      if(input$sum_same_species == FALSE){

        data_rf %>%
          pmol_to_uM()

      } else if(input$sum_same_species == TRUE){

        data_rf %>%
          pmol_to_uM() %>%
          sum_same_species()

      }

    })

    output$uM_check <- DT::renderDataTable({uM_data()})

    list(data = uM_data)

  })
}

## To be copied in the UI
# mod_6_uM_ui("uM_ui_1")

## To be copied in the server
# mod_6_uM_server("uM_ui_1")
