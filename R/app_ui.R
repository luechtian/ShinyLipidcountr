#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {

  shinydashboard::dashboardPage(

    shinydashboard::dashboardHeader(title = "ShinyLipidcountr"),

    shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        id = "tabs",
        shinydashboard::menuItem("1 Raw data", tabName = "raw", icon = icon("file-import")),
        shinydashboard::menuItem("2 Meta data", tabName = "meta", icon = icon("weight")),
        shinydashboard::menuItem("3 Calculations", tabName = "cal", icon = icon("calculator"),
                                 shinydashboard::menuSubItem("intensity to pmol",
                                                             tabName = "pmol"),
                                 shinydashboard::menuSubItem("Blank subtraction",
                                                             tabName = "blank"))
      )
    ),

    shinydashboard::dashboardBody(
      shinydashboard::tabItems(
        #---- raw data-tab content ----
        mod_1_raw_ui("raw_ui_1", tabName = "raw"),

        #---- meta data-tab content ----
        mod_2_meta_ui("meta_ui_1", tabName = "meta"),

        #---- calculations-tab content ----
        mod_3_calc_ui("calc_ui_1", tabName = "pmol"),
        shinydashboard::tabItem(tabName = "blank",
                                fluidRow(
                                  h2("blank tab content")
                                )
        )

        #----  ----
      )
    )

  )

}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){

  add_resource_path(
    'www', app_sys('app/www')
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'ShinyLipidcountr'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

