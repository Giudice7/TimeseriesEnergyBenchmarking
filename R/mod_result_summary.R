#' result_summary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import DT
mod_result_summary_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      selectizeInput(inputId = ns("load_condition_benchmarking"),
                     label = "Select a season to inspect benchmarking results",
                     choices = c("Winter", "Summer"))
    ),
    fluidRow(
      box(
        title = "Performance Score - Season",
        width = 6,
        solidHeader = T,
        withSpinner(uiOutput(outputId = ns("performance_score_season")))
      ),
      box(
        title = "Performance score - Load condition",
        width = 6,
        solidHeader = TRUE,
        div(
          style = "display: flex; justify-content: space-between; gap: 10px;",
          div(style = "flex: 1;", withSpinner(uiOutput(outputId = ns("performance_score_workdays")))),
          div(style = "flex: 1;", withSpinner(uiOutput(outputId = ns("performance_score_weekends"))))
        )
      )
    ),
    fluidRow(
      box(
        title = "Key Performance Indicator table - Season",
        widht = 6,
        solidHeader = T,
        DT::dataTableOutput(outputId = ns("kpi_table_season"))
      ),
      box(
        title = "Key Performance Indicator table - Load condition",
        widht = 6,
        solidHeader = T,
        DT::dataTableOutput(outputId = ns("kpi_table_load_condition"))
      )
    )

  )
}

#' result_summary Server Functions
#'
#' @noRd
mod_result_summary_server <- function(id, button){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    uom_eui_Winter <- reactive({
      req(thermal_correlation())

      corr <- thermal_correlation() %>%
        filter(load_condition == "Winter workdays")

      if (corr$result == "Thermal sensitive") {
        uom <- "kWh/(m2*°C)"
      } else {
        uom <- "kWh/m2"
      }

      uom
    })

    uom_eui_Summer <- reactive({
      req(thermal_correlation())

      corr <- thermal_correlation() %>%
        filter(load_condition == "Summer workdays")

      if (corr$result == "Thermal sensitive") {
        uom <- "kWh/(m2*°C)"
      } else {
        uom <- "kWh/m2"
      }

      uom
    })

    kpi_season <- reactive({

      df <- data.frame(
        Value = c(KPIs()$EUI_Winter, KPIs()$EUI_Summer, KPIs()$off_impact_Winter, KPIs()$off_impact_Summer, KPIs()$weekend_impact_Winter, KPIs()$weekend_impact_Summer),
        Unit_of_Measure = c(uom_eui_Winter(), uom_eui_Summer(), "%", "%", "%", "%"),
        Name = c("Energy Use Intensity", "Energy Use Intensity", "OFF Impact", "OFF Impact", "Weekend Impact", "Weekend Impact"),
        Season = c("Winter", "Summer", "Winter", "Summer", "Winter", "Summer")
      )

    })

    kpi_load_condition <- reactive({

      df <- data.frame(
        Value = c(KPIs()$ar_Winter_workdays, KPIs()$ar_Summer_workdays, KPIs()$ar_Winter_weekends, KPIs()$ar_Summer_weekends,
                KPIs()$volatility_Winter_workdays, KPIs()$volatility_Summer_workdays, KPIs()$volatility_Winter_weekends, KPIs()$volatility_Summer_weekends,
                KPIs()$shape_frequency_Winter_workdays, KPIs()$shape_frequency_Summer_workdays, KPIs()$shape_frequency_Winter_weekends, KPIs()$shape_frequency_Summer_weekends),
        Unit_of_Measure = c("%", "%", "%", "%", "%", "%", "%", "%", "%", "%", "%", "%"),
        Name = c("Anomaly Rate", "Anomaly Rate", "Anomaly Rate", "Anomaly Rate",
                 "Load volatility",  "Load volatility",  "Load volatility",  "Load volatility",
                 "Load patterns frequency", "Load patterns frequency", "Load patterns frequency", "Load patterns frequency"),
        Load_Condition = c("Winter workdays", "Summer workdays", "Winter weekends", "Summer weekends",
                           "Winter workdays", "Summer workdays", "Winter weekends", "Summer weekends",
                           "Winter workdays", "Summer workdays", "Winter weekends", "Summer weekends")

      )

    })

    kpi_lc_end_use <- reactive({
      data <- read.csv(file = file.path("data",  paste0("KPI_load_condition_", tolower(end_use()), ".csv")), sep = ";") %>%
        subset(load_condition %in% c(paste(input$load_condition_benchmarking, "workdays", sep=" "), paste(input$load_condition_benchmarking, "weekends", sep=" ")))
    })

    kpi_season_end_use <- reactive({
      data <- read.csv(file = file.path("data",  paste0("KPI_season_", tolower(end_use()), ".csv")), sep = ";") %>%
        subset(load_condition == input$load_condition_benchmarking)
    })


    kpi_lc_peers <- reactive({
      req(peers())

      data <- read.csv(file = file.path("data",  paste0("KPI_load_condition_", tolower(end_use()), ".csv")), sep = ";") %>%
        subset(load_condition %in% c(paste(input$load_condition_benchmarking, "workdays", sep=" "), paste(input$load_condition_benchmarking, "weekends", sep=" "))) %>%
        subset(building_id %in% peers()$building_id)
    })

    kpi_season_peers <- reactive({
      req(peers())

      data <- read.csv(file = file.path("data",  paste0("KPI_season_", tolower(end_use()), ".csv")), sep = ";") %>%
        subset(load_condition == input$load_condition_benchmarking) %>%
        subset(building_id %in% peers()$building_id)
    })

    ps_season <- reactive({
      req(KPIs(), peers())

      if (input$load_condition_benchmarking == "Winter")  {
        eui <- KPIs()$EUI_Winter
        off_impact <- KPIs()$off_impact_Winter
        weekend_impact <- KPIs()$weekend_impact_Winter

      } else {
        eui <- KPIs()$EUI_Summer
        off_impact <- KPIs()$off_impact_Summer
        weekend_impact <- KPIs()$weekend_impact_Summer
      }

      data.frame(
        KPI = c("Energy Use Intensity", "Off impact", "Weekend impact"),
        Value = c(percentile_rank(kpi_season_peers()$EUI, eui),
                  percentile_rank(kpi_season_peers()$off_impact, off_impact),
                  percentile_rank(kpi_season_peers()$weekend_impact, weekend_impact)
                  )
      )

    })

    ps_workdays <- reactive({
      req(KPIs(), peers())

      if (input$load_condition_benchmarking == "Winter")  {
        ar <- KPIs()$ar_Winter_workdays
        lv <- KPIs()$volatility_Winter_workdays
        shape_frequency <- KPIs()$shape_frequency_Winter_workdays

      } else {
        ar <- KPIs()$ar_Summer_workdays
        lv <- KPIs()$volatility_Summer_workdays
        shape_frequency <- KPIs()$shape_frequency_Summer_workdays
      }

      data.frame(
        KPI = c("Anomaly Rate", "Load Volatility", "Frequency of load patterns"),
        Value = c(percentile_rank(kpi_lc_peers()$anomaly_rate, ar),
                  percentile_rank(kpi_lc_peers()$distance_volatility, lv),
                  percentile_rank(kpi_lc_peers()$EPI_freq, shape_frequency)
        )
      )
    })

    ps_weekends <- reactive({
      req(KPIs(), peers())

      if (input$load_condition_benchmarking == "Winter")  {
        ar <- KPIs()$ar_Winter_weekends
        lv <- KPIs()$volatility_Winter_weekends
        shape_frequency <- KPIs()$shape_frequency_Winter_weekends

      } else {
        ar <- KPIs()$ar_Summer_weekends
        lv <- KPIs()$volatility_Summer_weekends
        shape_frequency <- KPIs()$shape_frequency_Summer_weekends
      }

      data.frame(
        KPI = c("Anomaly Rate", "Load Volatility", "Frequency of load patterns"),
        Value = c(percentile_rank(kpi_lc_peers()$anomaly_rate, ar),
                  percentile_rank(kpi_lc_peers()$distance_volatility, lv),
                  percentile_rank(kpi_lc_peers()$EPI_freq, shape_frequency)
        )
      )
    })


    output$performance_score_season <- renderUI({
      tags$div(
        HTML(sprintf(
          '<script>radar_plot(%s, "summary1-performance_score_season", "%s")</script>',
          jsonlite::toJSON(ps_season(), dataframe = "rows"),
          paste(input$load_condition_benchmarking, "season")
        ))
      )
    })


    output$performance_score_workdays <- renderUI({
      tags$div(
        HTML(sprintf(
          '<script>radar_plot(%s, "summary1-performance_score_workdays", "%s")</script>',
          jsonlite::toJSON(ps_workdays(), dataframe = "rows"),
          paste(input$load_condition_benchmarking, "workdays")
        ))
      )
    })

    output$performance_score_weekends <- renderUI({
      tags$div(
        HTML(sprintf(
          '<script>radar_plot(%s, "summary1-performance_score_weekends", "%s")</script>',
          jsonlite::toJSON(ps_weekends(), dataframe = "rows"),
          paste(input$load_condition_benchmarking, "weekends")
        ))
      )
    })

    output$kpi_table_season <- DT::renderDataTable({

      # Apply conditional formatting based on 'Value'
      datatable(kpi_season(),
                 options = list(
                   # columnDefs = list(
                   #   list(targets = "_all", className = "dt-center")  # Center align all columns
                   # ),
                   rownames = FALSE,
                   searching = FALSE,
                   paging = FALSE,      # Disable pagination
                   info = FALSE,        # Disable page information
                   ordering = FALSE,    # Disable column ordering
                   dom = 't')
      #   rowCallback = JS(
      #     "function(row, data) {",
      #     "var value = parseFloat(data[2]);",  # Index 2 corresponds to 'Value' column
      #     "if (value <= 25) {",
      #     "$(row).css('background-color', '", colors[1], "');",  # Red for values <= 25
      #     "} else if (value > 25 && value <= 50) {",
      #     "$(row).css('background-color', '", colors[2], "');",  # Yellow for values > 25 and <= 50
      #     "} else if (value > 50 && value < 75) {",
      #     "$(row).css('background-color', '", colors[3], "');",  # Light green for values > 50 and < 75
      #     "} else {",
      #     "$(row).css('background-color', '", colors[4], "');",  # Dark green for values >= 75
      #     "}",
      #     "}"
      #   )
      # )
      )
    })

    output$kpi_table_load_condition <- DT::renderDataTable({

      # Apply conditional formatting based on 'Value'
      datatable(kpi_load_condition(),
                options = list(
                  columnDefs = list(
                    list(targets = "_all", className = "dt-center")  # Center align all columns
                  ),
                  rownames = FALSE,
                  searching = FALSE,
                  paging = FALSE,      # Disable pagination
                  info = FALSE,        # Disable page information
                  ordering = FALSE,    # Disable column ordering
                  dom = 't')
                #   rowCallback = JS(
                #     "function(row, data) {",
                #     "var value = parseFloat(data[3]);",  # Index 2 corresponds to 'Value' column
                #     "if (value <= 25) {",
                #     "$(row).css('background-color', '", colors[1], "');",  # Red for values <= 25
                #     "} else if (value > 25 && value <= 50) {",
                #     "$(row).css('background-color', '", colors[2], "');",  # Yellow for values > 25 and <= 50
                #     "} else if (value > 50 && value < 75) {",
                #     "$(row).css('background-color', '", colors[3], "');",  # Light green for values > 50 and < 75
                #     "} else {",
                #     "$(row).css('background-color', '", colors[4], "');",  # Dark green for values >= 75
                #     "}",
                #     "}"
                #   )
                # )
      )
    })



  })
}

## To be copied in the UI
# mod_result_summary_ui("result_summary_1")

## To be copied in the server
# mod_result_summary_server("result_summary_1")
