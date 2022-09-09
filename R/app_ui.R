#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {

  shinydashboard::dashboardPage(

    shinydashboard::dashboardHeader(
      title = "ShinyLipidcountr",
      shinydashboard::dropdownMenu(
        type       = "notification",
        badgeStatus = NULL,
        icon       = icon("question-circle"),
        headerText = "Need Help?",
        # shinydashboard::notificationItem(text   = "Documentation",
        #                                  href   = "christian.luechtenborg@bzh.uni-heidelberg.de",
        #                                  icon   = icon("book"),
        #                                  status = "success"),
        # shinydashboard::notificationItem(text   = "Report bug",
        #                                  href   = "https://github.com/luechtian/ShinyLipidcountr/issues/new",
        #                                  status = "success",
        #                                  icon   = icon("bug")),
        tags$li(a(onclick = "onclick =window.open('https://github.com/luechtian/ShinyLipidcountr')",
                  p(icon("github"), " GitHub repo"),
                  href = NULL,
                  title = "GitHub",
                  style = "cursor: pointer;"),
                class = "dropdown"),
        tags$li(a(onclick = "onclick =window.open('https://github.com/luechtian/ShinyLipidcountr/issues/new')",
                  p(icon("bug"), " Report bug"),
                  href = NULL,
                  title = "GitHub issues",
                  style = "cursor: pointer;"),
                class = "dropdown"),
        tags$li(a(#onclick = "onclick =window.open('mailto:christian.luechtenborg@bzh.uni-heidelberg.de')",
                  p(icon("envelope"), "Email"),
                  href = "mailto:christian.luechtenborg@bzh.uni-heidelberg.de?subject=Need help with ShinyLipidcountr!",
                  title = "Send me a message",
                  style = "cursor: pointer;"),
                class = "dropdown")
    )),

    shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        id = "tabs",
        shinydashboard::menuItem("1 Raw data", tabName = "raw", icon = icon("file-import")),
        shinydashboard::menuItem("2 Meta data", tabName = "meta", icon = icon("weight")),
        shinydashboard::menuItem("3 Calculations", tabName = "cal", icon = icon("calculator"),
                                 shinydashboard::menuSubItem("Intensity to pmol",
                                                             tabName = "pmol"),
                                 shinydashboard::menuSubItem("Blank subtraction",
                                                             tabName = "blank"),
                                 shinydashboard::menuSubItem("Response factors",
                                                             tabName = "rf"),
                                 shinydashboard::menuSubItem("pmol to uM",
                                                             tabName = "uM")),

        shinydashboard::menuItem("4 Data Filtering", tabName = "filter", icon = icon("filter")),

        shinydashboard::menuItem("5 Excel", tabName = "excel", icon = icon("file-excel"))
      )
    ),

    shinydashboard::dashboardBody(
      shinydashboard::tabItems(
        #---- raw data-tab content ----
        mod_1_raw_ui("raw_ui_1", tabName = "raw"),

        #---- meta data-tab content ----
        mod_2_meta_ui("meta_ui_1", tabName = "meta"),

        #---- calculations-tab content ----
        ##--- pmol-subtab content ----
          mod_3_pmol_ui("pmol_ui_1", tabName = "pmol"),

        ##--- blank subtraction-subtab content ----
          mod_4_blank_ui("blank_ui_1", tabName = "blank"),

        ##--- Response factors-subtab content ----
          mod_5_rf_ui("rf_ui_1", tabName = "rf"),

        ##--- uM-subtab content ----
          mod_6_uM_ui("uM_ui_1", tabName = "uM"),

        #---- filter-tab content ----
        mod_7_filter_ui("filter_ui_1", tabName = "filter"),

        #---- excel-tab content ----
        mod_excel_ui("excel_ui_1", tabName = "excel")
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

