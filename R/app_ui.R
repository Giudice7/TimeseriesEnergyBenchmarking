#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    includeScript("https://code.highcharts.com/highcharts.js"),
    # App title
    div(
      id = "header-title",
      div(
        id = "title",
        p(
          "External Energy Benchmarking"
        )
      ),
      div(
        id = "container-logos",
        div(
          class = "logo",
          id = "logo_baeda",
          img(
            src = "www/logo.png"
          )

        ),
        div(
          class = "logo",
          id = "logo_polito",
          img(
            src = "www/logo_polito.jpg"
          )
        )
      )
    ),
    shinydashboard::dashboardPage(
      sidebar = sidebar,
      header = header,
      body = shinydashboard::dashboardBody(
        tabItems(
        #   tabItem(tabName = "about",
        #           mod_about_ui("about1")),
          tabItem(tabName = "load_data",
                  mod_load_data_ui("load_data1"))
        #   tabItem(tabName = "pre-processing",
        #           mod_pre_processing_ui("pre_processing1")),
        #   tabItem(tabName = "peer_identification",
        #           mod_peer_identification_ui("peer_identification1")),
        #   tabItem(tabName = "kpi",
        #           mod_EPI_ui("KPI1")),
        #   tabItem(tabName = "benchmarking")
        )
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
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "TimeseriesEnergyBenchmarking"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
