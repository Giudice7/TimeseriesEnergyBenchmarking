#' benchmarking UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_benchmarking_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      selectizeInput(inputId = ns("load_condition_benchmarking"),
                     label = "Select a load condition to inspect benchmarking results",
                     choices = c("Winter workdays", "Summer workdays",
                                 "Winter weekends", "Summer weekends"))
    ),
    fluidRow(
      box(
        width = 6,
        solidHeader = TRUE,
        title="Energy Use Intensity",
        htmlOutput(outputId = ns("plot_density_EUI"))
      ),
      box(
        width=6,
        solidHeader = TRUE,
        title = "Operational schedules efficiency",
        div(
          style = "display: flex; justify-content: space-between; gap: 10px;",
          div(style = "flex: 1;", htmlOutput(outputId = ns("plot_density_OFF_impact"))),
          div(style = "flex: 1;", htmlOutput(outputId = ns("plot_density_WEEKEND_impact")))
        )
      )
    ),
    fluidRow(
      box(
        width = 6,
        solidHeader = TRUE,
        title="Load volatility",
        htmlOutput(outputId = ns("plot_density_load_volatility"))
      ),
      box(
        width=6,
        solidHeader = TRUE,
        title = "Anomalies in the energy consumption",
        htmlOutput(outputId = ns("plot_density_anomaly_rate"))
      )
    ),
    fluidRow(
      box(width=6,
          solidHeader = TRUE,
          title = "Frequency of the load pattern",
          htmlOutput(outputId = ns("plot_density_pattern_frequency"))
          )
    )

  )
}

#' benchmarking Server Functions
#'
#' @noRd
mod_benchmarking_server <- function(id, button){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    kpi_lc_end_use <- reactive({
      req(data_clean(), end_use())

      data <- read.csv(file = file.path("data",  paste0("KPI_load_condition_", tolower(end_use()), ".csv")), sep = ";") %>%
        subset(load_condition == input$load_condition_benchmarking)
    })

    kpi_season_end_use <- reactive({
      req(data_clean(), end_use())

      data <- read.csv(file = file.path("data",  paste0("KPI_season_", tolower(end_use()), ".csv")), sep = ";") %>%
        subset(load_condition ==  gsub(' .*', '', input$load_condition_benchmarking))
    })


    kpi_lc_peers <- reactive({
      req(data_clean(), peers())

      data <- read.csv(file = file.path("data",  paste0("KPI_load_condition_", tolower(end_use()), ".csv")), sep = ";") %>%
        subset(load_condition == input$load_condition_benchmarking) %>%
        subset(building_id %in% peers()$building_id)
    })

    kpi_season_peers <- reactive({
      req(data_clean(), peers())

      data <- read.csv(file = file.path("data",  paste0("KPI_season_", tolower(end_use()), ".csv")), sep = ";") %>%
        subset(load_condition == gsub(' .*', '', input$load_condition_benchmarking)) %>%
        subset(building_id %in% peers()$building_id)

    })

    output$plot_density_EUI <- renderUI({
      req(KPIs())

      if ((gsub(' .*', '', input$load_condition_benchmarking)) == "Winter") {
      kpi <- KPIs()$EUI_Winter
    } else {
      kpi <- KPIs()$EUI_Summer
    }

      tags$div(HTML(
        sprintf(
          '<script>plot_density(%s, %s, "benchmarking1-plot_density_EUI", %s, "%s", "%s")</script>',
          jsonlite::toJSON(round(kpi_season_peers()$EUI, 2), dataframe = "rows"),
          jsonlite::toJSON(round(kpi_season_end_use()$EUI, 2), dataframe = "rows"),
          kpi,
          end_use(),
          "EUI [kWh/m2]"
        )
      ))

    })

    output$plot_density_OFF_impact <- renderUI({
      req(KPIs())

      if ((gsub(' .*', '', input$load_condition_benchmarking)) == "Winter") {
        kpi <- KPIs()$off_impact_Winter
      } else {
        kpi <- KPIs()$off_impact_Summer
      }


      tags$div(HTML(
        sprintf(
          '<script>plot_density(%s, %s, "benchmarking1-plot_density_OFF_impact", %s, "%s", "%s")</script>',
          jsonlite::toJSON(round(kpi_season_peers()$off_impact, 2), dataframe = "rows"),
          jsonlite::toJSON(round(kpi_season_end_use()$off_impact, 2), dataframe = "rows"),
          kpi,
          end_use(),
          "OFF impact [%]"
        )
      ))

    })

    output$plot_density_WEEKEND_impact <- renderUI({
      req(KPIs())

      if ((gsub(' .*', '', input$load_condition_benchmarking)) == "Winter") {
        kpi <- KPIs()$weekend_impact_Winter
      } else {
        kpi <- KPIs()$weekend_impact_Summer
      }

      # TODO: Adjust weekend_impact

      tags$div(HTML(
        sprintf(
          '<script>plot_density(%s, %s, "benchmarking1-plot_density_WEEKEND_impact", %s, "%s", "%s")</script>',
          jsonlite::toJSON(round(kpi_season_peers()$weekend_impact, 2), dataframe = "rows", na = "null"),
          jsonlite::toJSON(round(kpi_season_end_use()$weekend_impact, 2), dataframe = "rows", na = "null"),
          kpi,
          end_use(),
          "WEEKEND impact [%]"
        )
      ))

    })

    output$plot_density_load_volatility <- renderUI({
      req(KPIs())

      if (input$load_condition_benchmarking == "Winter workdays") {
        kpi <- KPIs()$volatility_Winter_workdays
      } else if (input$load_condition_benchmarking == "Winter weekends") {
        kpi <- KPIs()$volatility_Winter_weekends
      } else if (input$load_condition_benchmarking == "Summer workdays") {
        kpi <- KPIs()$volatility_Summer_workdays
      } else if (input$load_condition_benchmarking == "Summer weekends") {
        kpi <- KPIs()$volatility_Summer_weekends
      }

      tags$div(HTML(
        sprintf(
          '<script>plot_density(%s, %s, "benchmarking1-plot_density_load_volatility", %s, "%s", "%s")</script>',
          jsonlite::toJSON(round(kpi_lc_peers()$distance_volatility, 2), dataframe = "rows"),
          jsonlite::toJSON(round(kpi_lc_end_use()$distance_volatility, 2), dataframe = "rows"),
          kpi,
          end_use(),
          "Load volatility [%]"
        )
      ))

    })

    output$plot_density_anomaly_rate <- renderUI({
      req(KPIs())

      if (input$load_condition_benchmarking == "Winter workdays") {
        kpi <- KPIs()$ar_Winter_workdays
      } else if (input$load_condition_benchmarking == "Winter weekends") {
        kpi <- KPIs()$ar_Winter_weekends
      } else if (input$load_condition_benchmarking == "Summer workdays") {
        kpi <- KPIs()$ar_Summer_workdays
      } else if (input$load_condition_benchmarking == "Summer weekends") {
        kpi <- KPIs()$ar_Summer_weekends
      }

      tags$div(HTML(
        sprintf(
          '<script>plot_density(%s, %s, "benchmarking1-plot_density_anomaly_rate", %s, "%s", "%s")</script>',
          jsonlite::toJSON(round(kpi_lc_peers()$anomaly_rate, 2), dataframe = "rows"),
          jsonlite::toJSON(round(kpi_lc_end_use()$anomaly_rate, 2), dataframe = "rows"),
          kpi,
          end_use(),
          "Anomaly Rate [%]"
        )
      ))

    })

    output$plot_density_pattern_frequency <- renderUI({
      req(KPIs())

      if (input$load_condition_benchmarking == "Winter workdays") {
        kpi <- KPIs()$shape_frequency_Winter_workdays
      } else if (input$load_condition_benchmarking == "Winter weekends") {
        kpi <- KPIs()$shape_frequency_Winter_weekends
      } else if (input$load_condition_benchmarking == "Summer workdays") {
        kpi <- KPIs()$shape_frequency_Summer_workdays
      } else if (input$load_condition_benchmarking == "Summer weekends") {
        kpi <- KPIs()$shape_frequency_Summer_weekends
      }

      tags$div(HTML(
        sprintf(
          '<script>plot_density(%s, %s, "benchmarking1-plot_density_pattern_frequency", %s, "%s", "%s")</script>',
          jsonlite::toJSON(round(kpi_lc_peers()$EPI_freq, 2), dataframe = "rows"),
          jsonlite::toJSON(round(kpi_lc_end_use()$EPI_freq, 2), dataframe = "rows"),
          kpi,
          end_use(),
          "Frequency of load patterns [%]"
        )
      ))

    })

  })
}

## To be copied in the UI
# mod_benchmarking_ui("benchmarking_1")

## To be copied in the server
# mod_benchmarking_server("benchmarking_1")
