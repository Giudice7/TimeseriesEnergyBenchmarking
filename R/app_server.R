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

  mod_peer_identification_server("peer_identification1", perform_button$analyze)

  mod_KPI_server("KPI1", perform_button$analyze)

  mod_benchmarking_server("benchmarking1", perform_button$analyze)

  mod_result_summary_server("summary1", perform_button$analyze)

  # Tab visit tracking
  visited_tabs <- reactiveValues()

  observeEvent(input$tabs, {
    tab_id <- input$tabs

    if (is.null(visited_tabs[[tab_id]]) || !visited_tabs[[tab_id]]) {
      tab_info <- tab_descriptions[[tab_id]]
      if (!is.null(tab_info)) {
        showModal(modalDialog(
          title = tab_info$title,
          tab_info$content,
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
      }
      visited_tabs[[tab_id]] <- TRUE
    }
  })

}

