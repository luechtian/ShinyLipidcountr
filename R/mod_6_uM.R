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
                              actionButton(ns("calc_um"),"Use Response factors"),
                              title = "Calculate micromolar"
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

#' 6_uM Server Functions
#'
#' @noRd
mod_6_uM_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_6_uM_ui("6_uM_ui_1")

## To be copied in the server
# mod_6_uM_server("6_uM_ui_1")
