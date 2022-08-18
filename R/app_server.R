#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {

  LipidomicsData <- R6::R6Class("LipidomicsData",
                                public = list(
                                  data = NULL,
                                  rawdata = NULL,
                                  metadata = NULL
                                )


  )

  data_r6 <- LipidomicsData$new()
  gargoyle::init("update_meta")

  #---- Raw data ----
  raw <- mod_1_raw_server("raw_ui_1")

  #---- Meta data ----
  meta <- mod_2_meta_server("meta_ui_1", raw$data(), raw$input_rmv_is(), r6 = data_r6)

  #---- Calculations ----
  ## intensity to pmol
  pmol <- mod_3_pmol_server("pmol_ui_1", meta$data(), r6 = data_r6)

  ## blank sub
  pmol_blank <- mod_4_blank_server("blank_ui_1", pmol$data(), pmol$input_calc_pmol())

  ## response factors
  pmol_rf <- mod_5_rf_server("rf_ui_1", pmol_blank$data(), pmol_blank$input_calc_blank())

  ## pmol to uM
  uM <- mod_6_uM_server("uM_ui_1", pmol_rf$data())

}
