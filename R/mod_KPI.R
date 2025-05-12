#' KPI UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_KPI_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        width = 10,
        title = "Operational schedules",
        solidHeader = TRUE,
        withSpinner(htmlOutput(outputId = ns("schedule_plot")))
      ),
      box(
        width = 2,
        title = "KPIs Operational schedules efficiency",
        solidHeader = TRUE,
        withSpinner(uiOutput(
          outputId = ns("kpi_operational_schedules")
        ))
      )
    ),
    fluidRow(
      box(
        width = 10,
        title = "Volatility of energy consumption",
        solidHeader = TRUE,
        div(
          id="select_date_lv",
          selectizeInput(
            inputId = ns("load_condition_lv"),
            label = "Select a load condition to inspect",
            choices = c("Winter workdays", "Summer workdays",
                        "Winter weekends", "Summer weekends")
          ),
          selectizeInput(
            inputId = ns("date_lv"),
            label = "Select a date to inspect",
            choices = c()
          )
        ),
        withSpinner(uiOutput(
          outputId = ns("volatility_profiles")
        ))
      ),
      box(
        width = 2,
        title = "KPIs Load volatility",
        solidHeader = TRUE,
        withSpinner(uiOutput(
          outputId = ns("kpi_load_volatility")))
      )
    ),
    fluidRow(
      box(
        width = 10,
        title = "Anomalies in energy consumption",
        solidHeader = TRUE,
        div(
          id = "anomaly_profiles",
          htmlOutput(
            outputId = ns("anomaly_profiles_Winter_workdays")
          ),
          htmlOutput(
            outputId = ns("anomaly_profiles_Winter_weekends")
          ),
          htmlOutput(
            outputId = ns("anomaly_profiles_Summer_workdays")
          ),
          htmlOutput(
            outputId = ns("anomaly_profiles_Summer_weekends")
          ),
        )
      ),
      box(
        width = 2,
        title = "KPIs Anomalies in energy consumption",
        solidHeader = TRUE,
        withSpinner(uiOutput(
          outputId = ns("kpi_anomalies")))
      )
    )  # ,
    # fluidRow(
    #   box(
    #     width = 12,
    #     title = "Frequency of load pattern",
    #     solidHeader = TRUE
    #   )
    # )
  )
}

#' KPI Server Functions
#'
#' @noRd
mod_KPI_server <- function(id, button) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    output$schedule_plot <- renderUI({
      req(schedules())

      df <- schedules() %>%
        mutate(date = as.Date(timestamp),
               hour = format(as.POSIXct(timestamp), "%H:%M")) %>%
        select(date, hour, power, on_hour)

      tags$div(HTML(
        sprintf(
          '<script>plot_heatmap(%s, "KPI1-schedule_plot")</script>',
          jsonlite::toJSON(df, dataframe = "rows")
        )
      ))
    })

    output$kpi_operational_schedules <- renderUI({
      req(schedules())
      HTML(
        paste(
          "<p class='value'>",
          KPIs()$off_impact_Winter,
          " %</p>",
          "<p> OFF-impact Winter season </p>",
          "<p class='value'>",
          KPIs()$weekend_impact_Winter,
          " %</p>",
          "<p> WEEKEND impact Winter season </p>",
          "<p class='value'>",
          KPIs()$off_impact_Summer,
          " %</p>",
          "<p> OFF-impact Summer season </p>",
          "<p class='value'>",
          KPIs()$weekend_impact_Winter,
          " %</p>",
          "<p> WEEKEND impact Summer season </p>"
        )
      )
    })

    data_lc <- reactive({
      req(data_clean())

      data <- data_clean() %>%
        mutate(
          time = strftime(timestamp, format = "%H:%M", tz = 'UTC'),
          date = as.Date(timestamp, format = '%Y-%m-%d'),
          load_condition = get_load_condition()$load_condition
        ) %>%
        select(c(date, time, power, load_condition))
    })


    observeEvent(input$load_condition_lv, {
      req(data_clean())
      # Get the selected load condition
      selected_condition <- input$load_condition_lv

      # Call your function to get available dates for that condition
      available_dates <- get_dates(selected_condition)

      # Update the selectizeInput for dates
      updateSelectizeInput(
        session = session,
        inputId = "date_lv",
        choices = available_dates,
        server = TRUE
      )
    })

    output$volatility_profiles <- renderUI({
      req(data_lc(), input$load_condition_lv, input$date_lv)

      selected_date <- as.Date(input$date_lv)
      neighbor_row <- get_neighbor_days(input$load_condition_lv) %>%
        filter(date == selected_date)

      # Extract neighbor dates as a vector (excluding the first column)
      neighbor_dates <- neighbor_row %>%
        select(-date) %>%
        unlist(use.names = FALSE) %>%
        as.Date()

      data <- data_lc() %>%
        filter(load_condition == input$load_condition_lv) %>%
        mutate(
          type = case_when(
            date == selected_date ~ "Selected",
            date %in% neighbor_dates ~ "Neighbor",
            TRUE ~ "Normal"
          )
        )

      tags$div(HTML(
        sprintf(
          '<script>plot_volatility_profiles(%s, "KPI1-volatility_profiles", "%s", "%s")</script>',
          jsonlite::toJSON(data, dataframe = "rows"),
          input$load_condition_lv,
          max(data$power)
        )
      ))
    })

    output$kpi_load_volatility <- renderUI({
      req(schedules())
      HTML(
        paste(
          "<p class='value'>",
          KPIs()$volatility_Winter_workdays,
          " %</p>",
          "<p> Load Volatility in Winter workdays </p>",
          "<p class='value'>",
          KPIs()$volatility_Winter_weekends,
          " %</p>",
          "<p> Load volatility in Winter weekends </p>",
          "<p class='value'>",
          KPIs()$volatility_Summer_workdays,
          " %</p>",
          "<p> Load volatility in Summer workdays </p>",
          "<p class='value'>",
          KPIs()$volatility_Summer_weekends,
          " %</p>",
          "<p> Load volatility in Summer weekends </p>"
        )
      )
    })


    output$anomaly_profiles_Winter_workdays <- renderUI({
      req(data_clean())
      anomaly_dates <- get_anomaly_dates("Winter workdays")

      data <- data_lc() %>%
        subset(load_condition == "Winter workdays") %>%
        mutate(type = ifelse(date %in% as.Date(anomaly_dates), "Anomalous", "Normal"))

      tags$div(HTML(
        sprintf(
          '<script>plot_anomalies(%s, "KPI1-anomaly_profiles_Winter_workdays", "%s", "%s")</script>',
          jsonlite::toJSON(data, dataframe = "rows"),
          "Winter workdays",
          max(data_clean()$power)
        )
      ))

    })

    output$anomaly_profiles_Winter_weekends <- renderUI({
      req(data_clean())
      anomaly_dates <- get_anomaly_dates("Winter weekends")

      data <- data_lc() %>%
        subset(load_condition == "Winter weekends") %>%
        mutate(type = ifelse(date %in% as.Date(anomaly_dates), "Anomalous", "Normal"))

      tags$div(HTML(
        sprintf(
          '<script>plot_anomalies(%s, "KPI1-anomaly_profiles_Winter_weekends", "%s", "%s")</script>',
          jsonlite::toJSON(data, dataframe = "rows"),
          "Winter weekends",
          max(data_clean()$power)
        )
      ))

    })

    output$anomaly_profiles_Summer_workdays <- renderUI({
      req(data_clean())
      anomaly_dates <- get_anomaly_dates("Summer workdays")

      data <- data_lc() %>%
        subset(load_condition == "Summer workdays") %>%
        mutate(type = ifelse(date %in% as.Date(anomaly_dates), "Anomalous", "Normal"))

      tags$div(HTML(
        sprintf(
          '<script>plot_anomalies(%s, "KPI1-anomaly_profiles_Summer_workdays", "%s", "%s")</script>',
          jsonlite::toJSON(data, dataframe = "rows"),
          "Summer workdays",
          max(data_clean()$power)
        )
      ))

    })

    output$anomaly_profiles_Summer_weekends <- renderUI({
      req(data_clean())
      anomaly_dates <- get_anomaly_dates("Summer weekends")

      data <- data_lc() %>%
        subset(load_condition == "Summer weekends") %>%
        mutate(type = ifelse(date %in% as.Date(anomaly_dates), "Anomalous", "Normal"))

      tags$div(HTML(
        sprintf(
          '<script>plot_anomalies(%s, "KPI1-anomaly_profiles_Summer_weekends", "%s", "%s")</script>',
          jsonlite::toJSON(data, dataframe = "rows"),
          "Summer weekends",
          max(data_clean()$power)
        )
      ))

    })

    output$kpi_anomalies <- renderUI({
      req(schedules())
      HTML(
        paste(
          "<p class='value'>",
          KPIs()$ar_Winter_workdays,
          " %</p>",
          "<p> Anomaly Rate in Winter workdays </p>",
          "<p class='value'>",
          KPIs()$ar_Winter_weekends,
          " %</p>",
          "<p> Anomaly Rate in Winter weekends </p>",
          "<p class='value'>",
          KPIs()$ar_Summer_workdays,
          " %</p>",
          "<p> Anomaly Rate in Summer workdays </p>",
          "<p class='value'>",
          KPIs()$ar_Summer_weekends,
          " %</p>",
          "<p> Anomaly Rate in Summer weekends </p>"
        )
      )
    })

  })
}

## To be copied in the UI
# mod_KPI_ui("KPI_1")

## To be copied in the server
# mod_KPI_server("KPI_1")
