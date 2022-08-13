#' 4_blank UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_4_blank_ui <- function(id, tabName){
  ns <- NS(id)

  shinydashboard::tabItem(tabName = tabName,

                          fluidRow(
                            shinydashboard::box(
                              actionButton(ns("calc_blank"),"Subtract blank"),
                              actionButton(ns("omit_blank"),"Omit blank subtraction"),
                              textOutput(ns("check_blank")),
                              title = "Blank subtraction"
                            )
                          ),

                          fluidRow(
                            shinydashboard::box(
                              rhandsontable::rHandsontableOutput(ns("blanks")),
                              width = 12
                            )
                          ),

                          fluidRow(
                            shinydashboard::box(
                              DT::dataTableOutput(ns("blank_sub_check")),
                              width = 12
                            )
                          )
  )
}

#' 4_blank Server Functions
#'
#' @noRd
mod_4_blank_server <- function(id, data_pmol, input_pmol){

  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$blanks <- rhandsontable::renderRHandsontable({

      rhandsontable::rhandsontable(data_pmol %>%
                                     dplyr::select(sample, blank) %>%
                                     unique() %>%
                                     t(),
                                   readOnly = TRUE
      )

    })

    blank_data <- reactiveValues()

    observeEvent(input$calc_blank, {

      output$check_blank <- renderText({ check_blank(data_pmol) })

      blank_data$data <- data_pmol %>%
        blank_sub()

      output$blank_sub_check <- DT::renderDataTable({

        validate(
          need(input_pmol, "Please calculate pmol-values first.")
        )

        blank_data$data

      })

    })

    observeEvent(input$omit_blank, {

      blank_data$data <- data_pmol %>%
        dplyr::mutate(pmol_corr = pmol) %>%
        dplyr::select(-blank)

      output$check_blank <- renderText({ "Blank subtraction omitted." })

    })



    list(data = reactive(blank_data$data),
         input_calc_blank = reactive(input$calc_blank),
         input_omit_blank = reactive(input$omit_blank))

  })
}

## To be copied in the UI
# mod_4_blank_ui("blank_ui_1", tabName)

## To be copied in the server
# mod_4_blank_server("blank_ui_1", data_pmol)
