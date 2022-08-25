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
                              rhandsontable::rHandsontableOutput(ns("metadata")),
                              hr(),
                              fileInput(ns("meta_file"), label = NULL, multiple = F,
                                        accept = c(".csv"),
                                        placeholder = "metadata.csv", width = 300,
                                        buttonLabel = "Select metadata"),
                              downloadButton(ns("metadata_dl"), "Download metadata"),
                              hr(),
                              actionButton(ns("calc_meta"), "Add metadata"),
                              textOutput(ns("check_raw_meta")),
                              width = 12
                            )
                          ),

                          fluidRow(
                            shinydashboard::box(
                              DT::dataTableOutput(ns("meta_check")),
                              width = 12
                            )
                          )
  )
}

#' meta Server Functions
#'
#' @noRd
mod_2_meta_server <- function(id, data_raw, input_raw, r6) {

  moduleServer(id, function(input, output, session) {

    ns = session$ns

    rv <- reactiveValues(data = data.frame())

    output$metadata <- rhandsontable::renderRHandsontable({

      validate(
        need(input_raw, "Please upload files in tab '1 Raw data'.")
      )

      rhandsontable::rhandsontable(#width = 800, height = 500,
        metadata_template(data_raw)) #%>%
      #hot_col(col = "class", readOnly = TRUE)

    })

    observeEvent(input$metadata$changes$changes,{

      rv$data <- rhandsontable::hot_to_r(input$metadata)

    })

    observeEvent(input$meta_file,{

      rv$data <- read.csv(input$meta_file$datapath, check.names = FALSE) #%>%
        #janitor::clean_names()                                                 # reactivate if clean_txt_files(clean_samples = TRUE)

      output$metadata <- rhandsontable::renderRHandsontable({

        rhandsontable::rhandsontable(read.csv(input$meta_file$datapath,
                                              check.names = FALSE) #%>%
                                     #janitor::clean_names()                    # reactivate if clean_txt_files(clean_samples = TRUE)
                                     ) #%>%
          #rhandsontable::hot_col(col = "class", readOnly = TRUE)

      })

    })

    raw_plus_meta <- eventReactive(input$calc_meta,{

      validate(
        need(input_raw, "Please upload files in tab '1 Raw data'."),
        need(length(rv$data) > 0, "Please provide metadata (upload via csv or type in manually after uploading rawdata).")
      )

      r6$data <- data_raw %>%
        join_metadata(rv$data)
      gargoyle::trigger("update_meta")

      data_raw %>%
        join_metadata(rv$data)


    })

    observeEvent(input$calc_meta,{

      # If sample names from metadata and rawdata are not the same
      # "check_raw_meta"-function shows a warning!
      output$check_raw_meta <- renderText({ check_raw_meta(data_raw, rv$data) })

    })

    output$meta_check <- DT::renderDataTable(raw_plus_meta())

    output$metadata_dl <- downloadHandler(
      filename = function() {
        paste("metadata", "csv", sep = ".")
      },
      content = function(file2) {
        write.csv(rv$data, file2, row.names = FALSE)
      }
    )

    list(data = raw_plus_meta,
         metadata = reactive(rv$data))

  })

}

## To be copied in the UI
# mod_2_meta_ui("meta_ui_1")

## To be copied in the server
# mod_2_meta_server("meta_ui_1", data_raw, input_raw)
