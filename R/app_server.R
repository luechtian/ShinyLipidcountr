#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {

  #---- Raw data ----
  raw <- mod_1_raw_server("raw_ui_1")

  #---- Meta data ----
  meta <- mod_2_meta_server("meta_ui_1", raw$data(), raw$input_rmv_is())

  #---- Calculations ----
  mod_3_calc_server("calc_ui_1", "tabs", "blank", session)

}
