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
                              width = 3, height = 550, title = "Samples"
                            ),
                            shinydashboard::box(
                              rhandsontable::rHandsontableOutput(ns("class_check"), height = 480),
                              width = 2, height = 550, title = "Lipid classes"
                            ),
                            shinydashboard::box(
                              rhandsontable::rHandsontableOutput(ns("species_check"), height = 480),
                              width = 3, height = 550, title = "Lipid species"
                            ),
                            shinydashboard::box(
                              rhandsontable::rHandsontableOutput(ns("excluded_species"), height = 480),
                              width = 2, height = 550, title = "Excluded species"
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

    # Reactive values ----
    uM_data_2 <- reactiveValues()

    samples <- reactiveValues(data = data.frame())

    classes <- reactiveValues(data = data.frame())

    lipid_species <- reactiveValues(data = data.frame())

    # Reload/Reset logic ----
    observeEvent(input$refresh,{

      updateSelectInput(session = session,
                        inputId = "class",
                        choices = unique(uM_data()$class))

      uM_data_2$data <- uM_data() %>%
        dplyr::mutate(include = TRUE)

      uM_data_2$species <- uM_data() %>%
        dplyr::mutate(include = TRUE)

      # Sample selection ----
      output$sample_check <- rhandsontable::renderRHandsontable({

        rhandsontable::rhandsontable(
          uM_data_2$data %>%
            dplyr::select(sample, group, include) %>%
            unique() %>%
            dplyr::mutate(sample = as.character(sample),
                          group = as.character(group))
        ) %>%
          rhandsontable::hot_cols(manualColumnResize = TRUE) %>%
          rhandsontable::hot_col("sample", readOnly = TRUE) %>%
          rhandsontable::hot_col("group", readOnly = TRUE)

      })

      samples$data <- uM_data_2$data %>%
        dplyr::select(sample, group, include) %>%
        unique() %>%
        dplyr::mutate(sample = as.character(sample),
                      group = as.character(group))

      # Class selection ----
      output$class_check <- rhandsontable::renderRHandsontable({

        rhandsontable::rhandsontable(
          uM_data_2$data %>%
            dplyr::select(class, include) %>%
            unique()
        ) %>%
          rhandsontable::hot_col("class", readOnly = TRUE)

      })

      classes$data <- uM_data_2$data %>%
        dplyr::select(class, include) %>%
        unique()

      # Lipid species selection ----
      output$species_check <- rhandsontable::renderRHandsontable({

        rhandsontable::rhandsontable(
          uM_data_2$species %>%
            dplyr::select(class, species, include) %>%
            dplyr::filter(class == input$class) %>%
            unique() %>%
            dplyr::mutate(species = as.character(species))
        ) %>%
          rhandsontable::hot_col("class", readOnly = TRUE) %>%
          rhandsontable::hot_col("species", readOnly = TRUE)

      })

      lipid_species$data <- uM_data_2$species %>%
        dplyr::select(class, species, include) %>%
        dplyr::filter(class == input$class) %>%
        unique() %>%
        dplyr::mutate(species = as.character(species))

    })

    # Changes in rhandsontables ----
    observeEvent(input$sample_check$changes$changes,{

      samples$data <- rhandsontable::hot_to_r(input$sample_check)

    })

    observeEvent(input$class_check$changes$changes,{

      classes$data <- rhandsontable::hot_to_r(input$class_check)

    })

    observeEvent(input$species_check$changes$changes,{

      lipid_species$data <- rhandsontable::hot_to_r(input$species_check)

    })

    plot_data <- eventReactive(input$edit_data,{

        ignore_samples <- samples$data %>%
          dplyr::filter(include == FALSE) %>%
          dplyr::pull(sample)

        ignore_classes <- classes$data %>%
          dplyr::filter(include == FALSE) %>%
          dplyr::pull(class)

        ignore_lipid_species <- uM_data_2$species %>%
          dplyr::filter(include == FALSE) %>%
          dplyr::pull(species)

        uM_data_2$data %>%
          dplyr::filter(!sample %in% ignore_samples,
                        !class %in% ignore_classes,
                        !species %in% ignore_lipid_species)

    })

    observeEvent(input$edit_data,{

      uM_data_2$species <- uM_data_2$species %>%
        dplyr::left_join(lipid_species$data, by = c("class", "species")) %>%
        dplyr::mutate(include.y = ifelse(is.na(include.y), "", include.y),
                      include = ifelse(include.y != "", include.y, include.x),
                      include = as.logical(include),
                      species = as.factor(species)) %>%
        dplyr::select(-include.x, -include.y)

      output$excluded_species <- rhandsontable::renderRHandsontable({

        rhandsontable::rhandsontable(
          uM_data_2$species %>%
            dplyr::filter(include == FALSE) %>%
            dplyr::select(class, species) %>%
            unique() %>%
            dplyr::mutate(species = as.character(species))
        ) %>%
          rhandsontable::hot_col("class", readOnly = TRUE) %>%
          rhandsontable::hot_col("species", readOnly = TRUE)

      })

    })


    #---- Plots ----
    output$species_plot <- plotly::renderPlotly({
      req(input$edit_data)

      plotly::ggplotly(species_plot(plot_data(), input$class, input$plot_threshold, input$errorbar),
                       tooltip = "text") %>%
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

        plotly::ggplotly(class_plot(plot_data(), input$errorbar), tooltip = "text") %>%
          plotly::style(
            marker.size = input$point_size,
            marker.line = list(width = 0.5)) %>%
          plotly::layout(
            xaxis = list(tickfont = list(size = 15)),
            yaxis = list(tickfont = list(size = 15))) %>%
          plotly::config(displayModeBar = FALSE)

    })

    list(data = plot_data) #reactive(uM_data_2$data)

})
}

## To be copied in the UI
# mod_7_filter_ui("filter_ui_1")

## To be copied in the server
# mod_7_filter_server("filter_ui_1")

