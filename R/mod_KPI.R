#' KPI UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_KPI_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' KPI Server Functions
#'
#' @noRd 
mod_KPI_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_KPI_ui("KPI_1")
    
## To be copied in the server
# mod_KPI_server("KPI_1")
