#' 3_calc UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_3_calc_ui <- function(id, tabName){
  ns <- NS(id)

  shinydashboard::tabItem(tabName = tabName,

                          fluidRow(
                            h2("pmol tab content"),
                            actionButton(ns("switchtab"), 'Switch tab')
                          )
  )


}

#' 3_calc Server Functions
#'
#' @noRd
mod_3_calc_server <- function(id, tab_id, switch_to, parent){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$switchtab, {
      shinydashboard::updateTabItems(session = parent, tab_id, switch_to)
    })

  })
}

## To be copied in the UI
# mod_3_calc_ui("calc_ui_1")

## To be copied in the server
# mod_3_calc_server("calc_ui_1")
