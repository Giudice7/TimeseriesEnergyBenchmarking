#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  perform_button <- mod_load_data_server("load_data1")

  mod_pre_processing_server("pre_processing1", perform_button$analyze)
}
