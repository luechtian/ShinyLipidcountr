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

      data_rf %>%
        pmol_to_uM()

    })

    output$uM_check <- DT::renderDataTable({uM_data()})

    list(data = uM_data,
         input_calc_pmol = reactive(input$calc_uM))

  })
}

## To be copied in the UI
# mod_6_uM_ui("uM_ui_1")

## To be copied in the server
# mod_6_uM_server("uM_ui_1")
