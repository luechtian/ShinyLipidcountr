#' excel UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_excel_ui <- function(id, tabName){
  ns <- NS(id)

  shinydashboard::tabItem(tabName = tabName,

                          fluidRow(
                            shinydashboard::box(
                              numericInput(ns("plot_threshold"), "mol%-threshold",
                                           value = 0.5, step = 0.1, width = '30%'),
                              actionButton(ns("start_excel"), "Build Excel-workbook"),
                              downloadButton(ns("excel"), "Download Excel-Workbook"),
                              title = "Prepare and download Excel workbook"
                            )
                          ),

                          fluidRow(
                            shinydashboard::box(
                              DT::dataTableOutput(ns("excel_check")),
                              width = 12
                            )
                          )
  )

}

#' excel Server Functions
#'
#' @noRd
mod_excel_server <- function(id, uM_data, metadata){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$start_excel, {
      #### Excel_data-list #####
      funs <- c(sum = sum_table,
                ether = ether_table,
                species = species_table,
                species_mean = species_mean_table,
                species_plot = species_plot)

      args <- list(data = uM_data(),
                   threshold = input$plot_threshold)

      withProgress(message = 'Building barplots', style = "notification", value = 0.5, {
        excel_data <-
          unique(uM_data()$class) %>%
          purrr::map(~ funs %>% purrr::map(rlang::exec, .x, !!!args)) %>%
          setNames(unique(uM_data()$class))
      })

      ##### Initiate Excel workbook #####
      wb <- openxlsx::createWorkbook()
      openxlsx::modifyBaseFont(wb, fontSize = 10, fontColour = "black", fontName = "Arial")

      ##### Add metadata sheet #####
      openxlsx::addWorksheet(wb, "metadata")

      openxlsx::setColWidths(wb, "metadata", col = 1:99, widths = 10)
      openxlsx::setRowHeights(wb, "metadata", rows = c(1:99), heights = 15)

      openxlsx::writeData(wb, "metadata", metadata(), startCol = 1, startRow = 1)

      #### Add lipidclass sheets #####
      withProgress(message = 'Building workbook', style = "notification", value = 0.5, {

        unique(uM_data()$class) %>%
          purrr::map(~add_lipidclass_sheet(., excel_data, workbook = wb))

      })


      ##### Add summary sheet #####
      uM_data() %>%
        add_summary_sheet(workbook = wb)
      #
      ##### Add uM sheet #####
      uM_data() %>%
        add_uM_sheet(workbook = wb)

      ##### Excel sheet order #####
      excel_sheet_order <- c("metadata",
                             lipid_class_order[lipid_class_order %in% openxlsx::sheets(wb)],
                             "summary",
                             "uM_table"
      )

      openxlsx::worksheetOrder(wb) <- match(excel_sheet_order, openxlsx::sheets(wb))

      openxlsx::saveWorkbook(wb, paste0("inst/dl/wb",".xlsx"), overwrite = TRUE)

      ####Check-df to visualise calculations are finished

      output$excel_check <- DT::renderDataTable({

        uM_data()

      })
    })

    output$excel <- downloadHandler(
      filename = function() {
        paste("output", "xlsx", sep = ".")
      },
      content = function(file2) {
        file.copy("inst/dl/wb.xlsx", file2)
      }
    )

  })
}

## To be copied in the UI
# mod_excel_ui("excel_ui_1", tabName)

## To be copied in the server
# mod_excel_server("excel_ui_1", uM_data, metadata)
