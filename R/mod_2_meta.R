#' 2_meta UI Function
#'
#' @description This modules handles the infrastructure of tab "2 Meta data".
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_2_meta_ui <- function(id, tabName){
  ns <- NS(id)

  shinydashboard::tabItem(tabName = tabName,

                          fluidRow(
                            shinydashboard::box(
                              rhandsontable::rHandsontableOutput(ns("meta_data")),
                              hr(),
                              fileInput(ns("meta_file"), label = NULL, multiple = F,
                                        accept = c(".csv"),
                                        placeholder = "meta_data.csv", width = 300,
                                        buttonLabel = "Select meta data"),
                              hr(),
                              actionButton(ns("calc_meta"),"Add meta data"),
                              width = 12
                            )
                          ),

                          fluidRow(
                            shinydashboard::box(DT::dataTableOutput(ns("meta_check")), width = 12)
                          )
  )
}

#' meta Server Functions
#'
#' @noRd
mod_2_meta_server <- function(id, data_raw, input_raw) {

  moduleServer(id, function(input, output, session) {

    ns = session$ns

    rv <- reactiveValues(data = data.frame())

    output$meta_data <- rhandsontable::renderRHandsontable({

      validate(
        need(input_raw, "Please upload files in tab '1 Raw data'.")
      )

      rhandsontable::rhandsontable(#width = 800, height = 500,
        meta_data_template(data_raw)) #%>%
      #hot_col(col = "class", readOnly = TRUE)

    })

    observeEvent(input$meta_data$changes$changes,{

      rv$data <- rhandsontable::hot_to_r(input$meta_data)

    })

    observeEvent(input$meta_file,{

      rv$data <- read.csv(input$meta_file$datapath) %>% janitor::clean_names()

      output$meta_data <- rhandsontable::renderRHandsontable({

        rhandsontable::rhandsontable(read.csv(input$meta_file$datapath) %>%
                                       janitor::clean_names()) %>%
          rhandsontable::hot_col(col = "class", readOnly = TRUE)

      })

    })

    raw_plus_meta <- eventReactive(input$calc_meta,{

      validate(
        need(input_raw, "Please upload files in tab '1 Raw data'."),
        need(length(rv$data) > 0, "Please provide metadata (upload via csv or type in manually after uploading rawdata).")
      )

      data_raw %>%
        join_metadata(rv$data)

    })

    output$meta_check <- DT::renderDataTable(raw_plus_meta())

    list(rawdata = raw_plus_meta)

  })

}

## To be copied in the UI
# mod_2_meta_ui("meta_ui_1")

## To be copied in the server
# mod_2_meta_server("meta_ui_1")
