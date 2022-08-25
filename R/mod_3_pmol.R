#' 3_pmol UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_3_pmol_ui <- function(id, tabName){
  ns <- NS(id)

  shinydashboard::tabItem(tabName = tabName,

                          fluidRow(
                            shinydashboard::box(
                              actionButton(ns("calc_pmol"),"Calculate pmol values"),
                              title = "Intensity to pmol"
                            )
                          ),

                          fluidRow(
                            shinydashboard::box(
                              DT::dataTableOutput(ns("pmol_check")),
                              width = 12
                            )
                          )
  )
}

#' 3_pmol Server Functions
#'
#' @noRd
mod_3_pmol_server <- function(id, data_meta, r6){

  moduleServer(id, function(input, output, session){
    ns <- session$ns

    pmol_data <- eventReactive(input$calc_pmol,{
      gargoyle::watch("update_meta")
      # data_meta %>%
      r6$data %>%
        join_istd_means() %>%
        intensity_to_pmol()

    })

    output$pmol_check <- DT::renderDataTable({pmol_data()})

    list(data = pmol_data,
         input_calc_pmol = reactive(input$calc_pmol))

  })
}

## To be copied in the UI
# mod_3_pmol_ui("pmol_ui_1", tabName)

## To be copied in the server
# mod_3_pmol_server("pmol_ui_1", data_meta)
