#' 1_raw UI Function
#'
#' @description That module contains all code working in the first tab "1 Raw data".
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_1_raw_ui <- function(id, tabName){
  ns <- NS(id)

  shinydashboard::tabItem(tabName = tabName,

                          fluidRow(
                            shinydashboard::box(
                              fileInput(ns("files"), "Select raw data",
                                        multiple = TRUE, accept = c(".txt", ".csv"),
                                        placeholder = "raw data"),

                              # Placeholder for dynamic FileInput-element (comb LV & LX files)
                              div(id = "lipidxplorer_upload_for_comb"),

                              radioButtons(ns("radio"), label = NULL,
                                           choices = list("LipidView" = 1,
                                                          "LipidXplorer" = 2,
                                                          "LipidView & LipidXplorer combined" = 3),
                                           selected = 1, inline = TRUE),

                              hr(),

                              actionButton(ns("calc"),"Start Upload"),

                              actionButton(ns("rmv_is"), "Confirm Internal standards")
                            ),

                            shinydashboard::box(
                              rhandsontable::rHandsontableOutput(ns("is_check"), width = 300, height = "195"),
                              width = 3
                            )
                          ),

                          fluidRow(
                            shinydashboard::box(DT::dataTableOutput(ns("calc_check")), width = 12)
                          )
  )

}

#' raw Server Functions
#'
#' @noRd
mod_1_raw_server <- function(id){
  moduleServer(id, function(input, output, session) {

    ns = session$ns

    # Processing of file input ----
    raw <- eventReactive(input$calc,{

      #LipidView files
      if(input$radio == 1){

        files <- input$files$datapath

        filenames <- input$files$name %>%
          purrr::map(name_lipid_file) %>%
          unlist()

        names(files) <- names(filenames)

        ##### main data #####
        data <- files %>%
          read_txt_files() %>%
          clean_txt_files() %>%
          tidyr::pivot_longer(
            cols = c(-species, -class, -scan_name),
            names_to = "sample",
            values_to = "intensity")

      }

      #LipidXplorer file
      else if(input$radio == 2){

        data <- input$files$datapath %>%
          read_lipidxplorer_files() %>%
          clean_lipidxplorer_files()
      }

      # LV / LX files combined
      else if(input$radio == 3){


        ## LipidXplorer files
        lx_data <-  input$file_lx$datapath %>%
          read_lipidxplorer_files() %>%
          clean_lipidxplorer_files() %>%
          dplyr::rename(scan_name = molecular_species) %>%
          dplyr::select(class, species, scan_name, sample, intensity)


        ## LipidView files
        files <- input$files$datapath

        filenames <- input$files$name %>%
          purrr::map(name_lipid_file) %>%
          unlist()

        names(files) <- names(filenames)

        ##### main data #####
        lv_data <- files %>%
          read_txt_files() %>%
          clean_txt_files() %>%
          tidyr::pivot_longer(
            cols = c(-species, -class, -scan_name),
            names_to = "sample",
            values_to = "intensity")

        data <- rbind(lv_data, lx_data)
      }

    })

    # Dynamic File input appears, if radio button 3 is active ----
    active_elements <- reactiveVal(value = NULL)

    observeEvent(input$radio, {

      if(input$radio == 3){
        # Insert FileInput element
        ## update the list of currently shown elements
        current_id <- paste0("id_", input$radio)
        active_elements(c(current_id, active_elements()))

        insertUI(
          selector = "#lipidxplorer_upload_for_comb",
          ui = div(
            id = current_id,
            fileInput(ns("file_lx"), "Select LipidXplorer output to combine with LipidView dataset",
                      multiple = TRUE, accept = c(".csv"),
                      placeholder = "LipidXplorer output file (.csv)")
          )
        )
      } else if(input$radio != 3){
        # Remove FileInput element
        ## only remove a element if there is at least one element shown
        if (length(active_elements()) > 0) {
          current_id <- active_elements()[1]
          removeUI(
            selector = paste0("#", current_id)
          )

          # update the list of currently shown modules
          active_elements(active_elements()[-1])
        }
      }
    })

    # Internal standard selection ----
    is <- reactiveValues(data = data.frame())

    output$is_check <- rhandsontable::renderRHandsontable({

      rhandsontable::rhandsontable(
        raw() %>%
          dplyr::select(class, species) %>%
          dplyr::filter(stringr::str_detect(species, "IS")) %>%
          unique() %>%
          dplyr::mutate(include = TRUE)
      )

    })

    observeEvent(input$is_check$changes$changes,{

      is$data <- rhandsontable::hot_to_r(input$is_check)

    })

    # Processed file input new filtered by IS selection ----
    raw1 <- eventReactive(input$rmv_is,{

      if(all(is$data$include)) { raw() }
      else {
        ignore_is <- is$data %>%
          dplyr::filter(include == FALSE) %>%
          dplyr::pull(species)

        raw() %>% dplyr::filter(!species %in% ignore_is)
      }
    })

    output$calc_check <- DT::renderDataTable(raw1())

    # Make IS filtered & processed File input available outside the module ----
    #
    # Example outside the module, assign the module and call with ...$data()
    # raw <- raw_server(id)
    # raw$data()
    list(data = raw1,
         input_rmv_is = reactive(input$rmv_is))

  })
}

## To be copied in the UI
# mod_1_raw_ui("raw_ui_1", tabName)

## To be copied in the server
# mod_1_raw_server("raw_ui_1")
