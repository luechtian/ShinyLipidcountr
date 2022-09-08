#' 7_filter UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_7_filter_ui <- function(id, tabName){
  ns <- NS(id)

  shinydashboard::tabItem(tabName = tabName,

                          fluidRow(
                            shinydashboard::box(
                              actionButton(ns("refresh"), "Reload / Reset"),
                              hr(),
                              actionButton(ns("edit_data"), "Confirm selection"),
                              hr(),
                              selectInput(ns("class"),
                                          "Lipid class:",
                                          choices = NULL,
                                          selected = "PC"),
                              hr(),
                              numericInput(ns("plot_threshold"), "mol%-threshold",
                                           value = 0.5, step = 0.1),
                              hr(),
                              numericInput(ns("point_size"), "Point size",
                                           value = 4, step = 1),
                              hr(),
                              checkboxInput(inputId = ns("errorbar"),
                                            label = "Show error bars [stdev]",
                                            value = TRUE),
                              width = 2, height = 550, title = "Data filtering"
                            ),
                            shinydashboard::box(
                              rhandsontable::rHandsontableOutput(ns("sample_check"),  height = 480),
                              width = 2, height = 550, title = "Samples"
                            ),
                            shinydashboard::box(
                              rhandsontable::rHandsontableOutput(ns("class_check"), height = 480),
                              width = 2, height = 550, title = "Lipid classes"
                            ),
                            shinydashboard::box(
                              rhandsontable::rHandsontableOutput(ns("species_check"), height = 480),
                              width = 2, height = 550, title = "Lipid species"
                            )
                          ),

                          fluidRow(
                            shinydashboard::box(
                              plotly::plotlyOutput(ns("species_plot"), height = 550),
                              width = 12
                            ),
                            shinydashboard::box(
                              plotly::plotlyOutput(ns("class_plot"), height = 550),
                              width = 12
                            )
                          )
  )
}

#' 7_filter Server Functions
#'
#' @noRd
mod_7_filter_server <- function(id, uM_data){

  moduleServer( id, function(input, output, session){
    ns <- session$ns

    uM_data_2 <- reactiveValues()

    observeEvent(input$refresh,{

      updateSelectInput(session = session,
                        inputId = "class",
                        choices = unique(uM_data()$class))

      uM_data_2$data <- uM_data()
    })

    # Sample selection ----
    samples <- reactiveValues(data = data.frame())

    output$sample_check <- rhandsontable::renderRHandsontable({

      rhandsontable::rhandsontable(
        uM_data() %>%
          dplyr::select(sample, group) %>%
          unique() %>%
          dplyr::mutate(include = TRUE,
                        sample = as.character(sample),
                        group = as.character(group))
      ) %>%
        rhandsontable::hot_cols(manualColumnResize = TRUE)

    })

    observeEvent(input$sample_check$changes$changes,{

      samples$data <- rhandsontable::hot_to_r(input$sample_check)

    })

    # Class selection ----
    classes <- reactiveValues(data = data.frame())

    output$class_check <- rhandsontable::renderRHandsontable({

      rhandsontable::rhandsontable(
        uM_data() %>%
          dplyr::select(class) %>%
          unique() %>%
          dplyr::mutate(include = TRUE)
      )

    })

    observeEvent(input$class_check$changes$changes,{

      classes$data <- rhandsontable::hot_to_r(input$class_check)

    })

    # Lipid species selection ----
    lipid_species <- reactiveValues(data = data.frame())

    output$species_check <- rhandsontable::renderRHandsontable({

      rhandsontable::rhandsontable(
        uM_data() %>%
          dplyr::select(class, species) %>%
          dplyr::filter(class == input$class) %>%
          unique() %>%
          dplyr::mutate(include = TRUE,
                        species = as.character(species))
      )

    })

    observeEvent(input$species_check$changes$changes,{

      lipid_species$data <- rhandsontable::hot_to_r(input$species_check)

    })

    # edit_data logic ----
    observeEvent(input$edit_data,{

      if(all(samples$data$include & classes$data$include & lipid_species$data$include)) { uM_data_2$data }
      else {
        ignore_samples <- samples$data %>%
          dplyr::filter(include == FALSE) %>%
          dplyr::pull(sample)

        ignore_classes <- classes$data %>%
          dplyr::filter(include == FALSE) %>%
          dplyr::pull(class)

        ignore_lipid_species <- lipid_species$data %>%
          dplyr::filter(include == FALSE) %>%
          dplyr::pull(species)

        uM_data_2$data <- uM_data_2$data %>%
          dplyr::filter(!sample %in% ignore_samples,
                        !class %in% ignore_classes,
                        !species %in% ignore_lipid_species)

        return(uM_data_2$data)
      }
    })


    #---- Plots ----
    output$species_plot <- plotly::renderPlotly({
      req(input$edit_data)

      plot_data <- as.data.frame(uM_data_2$data)

      species_plot(plot_data, input$class)

      plotly::ggplotly(species_plot(plot_data, input$class, input$plot_threshold, input$errorbar), tooltip = "text") %>%
        plotly::style(
          marker.size = input$point_size,
          marker.line = list(width = 0.5)) %>%
        plotly::layout(
          xaxis = list(tickfont = list(size = 15)),
          yaxis = list(tickfont = list(size = 15))) %>%
        plotly::config(displayModeBar = FALSE)

    })

    output$class_plot <- plotly::renderPlotly({
      req(input$edit_data)

      plot_data2 <- as.data.frame(uM_data_2$data)

        plotly::ggplotly(class_plot(plot_data2, input$errorbar), tooltip = "text") %>%
          plotly::style(
            marker.size = input$point_size,
            marker.line = list(width = 0.5)) %>%
          plotly::layout(
            xaxis = list(tickfont = list(size = 15)),
            yaxis = list(tickfont = list(size = 15))) %>%
          plotly::config(displayModeBar = FALSE)

    })

    list(data = reactive(uM_data_2$data))

})
}

## To be copied in the UI
# mod_7_filter_ui("filter_ui_1")

## To be copied in the server
# mod_7_filter_server("filter_ui_1")

