#' load_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
#' @import shinyWidgets
#' @import shinyjs
mod_load_data_ui <- function(id) {
  ns <- NS(id)
  includeScript("inst/app/www/plot_ts_raw.js")
  includeScript("inst/app/www/plot_weather_raw.js")
  tagList(fluidRow(
    box(
      width = 12,
      solidHeader = T,
      title = "Load data",
      div(
        id = "toggle-description",
        tags$span("Use the"),
        tags$b("toogle button"),
        tags$span(
          "below to decide if upload your building data or inspect the application using default buildings"
        )
      ),
      materialSwitch(
        inputId = ns("switch"),
        label = "Inspect/Load your data",
        value = FALSE,
        status = "success"
      ),
      uiOutput(outputId = ns("description_ui"))
      ,
      uiOutput(outputId = ns("toggle_ui")),
      actionButton(inputId = ns("analyze"), label = "Perform the analysis")
    )
  ), fluidRow(
    box(
      width = 12,
      title = "Metadata",
      solidHeader = T,
      withSpinner(uiOutput(outputId = ns("metadata")))
    )
  ), fluidRow(
    box(
      width = 6,
      title = "Electric energy consumption time series",
      solidHeader = TRUE,
      withSpinner(htmlOutput(ns("energy_raw")))
    ),
    box(
      width = 6,
      title = "Outdoor air temperature time series",
      solidHeader = TRUE,
      withSpinner(htmlOutput(ns(
        "temperature_raw"
      )))
    )
  ))
}

#' load_data Server Functions
#'
#' @noRd
#'
#' @import shiny
#' @import shinyWidgets
#' @importFrom magrittr %>%
#' @import dplyr
#' @import jsonlite
mod_load_data_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$description_ui <- renderUI({
      if (input$switch == FALSE) {
        ui <- NULL
      } else {
        ui <- tags$div(
          id = "upload-description",
          tags$p(
            "To test the energy benchmarking system you should upload the following files:"
          ),
          tags$ol(
            tags$li(
              "A .csv file that contains the one-year hourly energy consumption time series of your building. The file should contain:"
            ),
            tags$ul(
              tags$li(
                tags$span("The column"),
                tags$i("timestamp"),
                tags$span(
                  ", which contains a string with the timestamp in the format %Y-%m-%d %H:%M:%S;"
                )
              ),
              tags$li(
                tags$span("The column"),
                tags$i("power"),
                tags$span(
                  ", which contains a generic float number with the power of the meter in kW."
                )
              )
            ),
            tags$li(
              "A .csv file that contains the  one-year hourly  weather data of the location of your building. The file should contain:"
            ),
            tags$ul(
              tags$li(
                tags$span("The column"),
                tags$i("timestamp"),
                tags$span(
                  ", which contains a string with the timestamp in the format %Y-%m-%d %H:%M:%S;"
                )
              ),
              tags$li(
                tags$span("The column"),
                tags$i("airTemperature"),
                tags$span(
                  ", which contains a generic float number with the external air temperature in °C."
                )
              )
            ),
            tags$li("The gross floor area of your building in m2."),
            tags$li(
              "The Primary Space Usage (PSU) category. Currently, only Office and Education buildings are supported."
            ),
            tags$li(
              "Additional: the State of the building in order to eliminate holidays from the data analyzed"
            )
          ),
          tags$span("Once you have insert all the data, click the"),
          tags$b("Perform the analysis"),
          tags$span("button and see the results in the other tabs!")
        )
      }

      return(ui)
    })

    output$toggle_ui <- renderUI({
      if (input$switch == FALSE) {
        selectizeInput(
          inputId = ns("file_raw"),
          label = "Select a building to analyze",
          choices = gsub(
            pattern = ".csv",
            replacement = "",
            x = list.files(
              file.path("data", "default_files", "electric_consumption")
            )
          )
        )
      } else {
        tagList(
          fileInput(
            inputId = ns("ts_raw"),
            label = "Upload the energy consumption time series",
            accept = ".csv"
          ),
          fileInput(
            inputId = ns("weather_raw"),
            label = "Upload the weather data",
            accept = ".csv"
          ),
          numericInput(
            inputId = ns("sqm"),
            label = "Insert the floor area of your building in m2",
            value = 0
          ),
          selectInput(
            inputId = ns("end_use"),
            label = "Primary Space usage (PSU) category",
            choices = c("Office", "Education")
          ),
          selectInput(
            inputId = ns("state"),
            label = "Insert the state (Optional)",
            choices = append("None", gsub(
              ".csv", "", gsub(".*_", "", list.files(file.path(
                "data", "calendar"
              )))
            ))
          )
        )
      }

    })

    data_raw <<- reactive({
      if (input$switch == FALSE) {
        req(input$file_raw)
        df <- read.csv(
          file = file.path(
            "data",
            "default_files",
            "electric_consumption",
            paste0(input$file_raw, ".csv")
          ),
          header = TRUE
        )
      } else {
        req(input$ts_raw)
        file <- input$ts_raw
        if (is.null(file))
          return(NULL)
        df <- read.csv(file$datapath, header = TRUE)
      }
      return(df)
    })

    weather <<- reactive({
      if (input$switch == FALSE) {
        req(input$file_raw)
        code_state <- gsub(pattern = "_.*",
                           replacement = "",
                           input$file_raw)
        df <- read.csv(
          file = file.path(
            "data",
            "default_files",
            "weather",
            paste0("weather_", code_state, ".csv")
          ),
          header = TRUE
        )
      } else {
        req(input$weather_raw)
        file <- input$weather_raw
        if (is.null(file))
          return(NULL)
        df <- read.csv(file$datapath, header = TRUE)
      }
      return(df)
    })

    metadata <- reactive({
      if (input$switch == FALSE) {
        metadata <- read.csv(file = file.path("data", "metadata.csv")) %>%
          subset(building_id == input$file_raw)
      } else {
        metadata <- NULL
      }
      return(metadata)
    })

    state <<- reactive({
      if (input$switch == FALSE) {
        state <- metadata()$state
      } else {
        req(input$state)
        state <- input$state
      }
      return(state)
    })

    end_use <<- reactive({
      if (input$switch == FALSE) {
        end_use <- metadata()$primaryspaceusage
      } else {
        req(input$end_use)
        end_use <- input$end_use
      }
      return(end_use)
    })

    sqm <<- reactive({
      if (input$switch == FALSE) {
        sqm <- metadata()$sqm
      } else {
        req(input$end_use)
        sqm <- input$sqm
      }
      return(sqm)
    })

    output$metadata <- renderUI({
      req(input$file_raw)

      HTML(
        paste(
          "<div class='card'> <p class='value'>",
          end_use(),
          "</p> <p> Primary Space Usage category </p> </div>",
          "<div class='card'> <p class='value'>",
          round(sqm(), 1),
          "</p> <span> <p> Gross floor area ",
          withMathJax("\\([m^2]\\)"),
          " </p> </span> </div>",
          "<div class='card'> <p class='value'>",
          state(),
          "</p> <p> State/Region </p> </div>"
        )
      )
    })

    output$energy_raw <- renderUI({
      data <- data_raw() %>%
        mutate(timestamp = as.POSIXct(timestamp, tz = "UTC", format = "%Y-%m-%d %H:%M")) %>%
        mutate(datetime_ms = as.numeric(timestamp) * 1000)
      tags$div(HTML(
        sprintf(
          '<script>plot_ts_raw(%s, "load_data1-energy_raw")</script>',
          jsonlite::toJSON(data, dataframe = "rows")
        )
      ))
    })

    output$temperature_raw <- renderUI({
      data <- weather() %>%
        select(timestamp, airTemperature) %>%
        mutate(timestamp = as.POSIXct(timestamp, tz = "UTC", format = "%Y-%m-%d %H:%M")) %>%
        mutate(datetime_ms = as.numeric(timestamp) * 1000)
      tags$div(HTML(
        sprintf(
          '<script>plot_temperature(%s, "load_data1-temperature_raw")</script>',
          jsonlite::toJSON(data, dataframe = "rows")
        )
      ))
    })

    # Define reactive values to store the output of the functions
    results <- reactiveValues(
      data_clean = NULL,
      thermal_correlation = NULL,
      features = NULL,
      peers = NULL,
      operational_schedules = NULL
    )

    kpi <- reactiveValues(
      EUI_Winter = NULL,
      EUI_Summer = NULL,
      off_impact_Winter = NULL,
      off_impact_Summer = NULL,
      weekend_impact_Winter = NULL,
      weekend_impact_Summer = NULL,
      epi_schedules = NULL,
      ar_Winter_workdays = NULL,
      ar_Summer_workdays = NULL,
      ar_Winter_weekends = NULL,
      ar_Summer_weekends = NULL,
      volatility_Winter_workdays = NULL,
      volatility_Summer_workdays = NULL,
      volatility_Winter_weekends = NULL,
      volatility_Summer_weekends = NULL,
      shape_frequency_Winter_workdays = NULL,
      shape_frequency_Summer_workdays = NULL,
      shape_frequency_Winter_weekends = NULL,
      shape_frequency_Summer_weekends = NULL
    )

    observeEvent(input$analyze, {
      # Reset the progress indicator
      withProgress(message = "Analyzing the building: ", value = 0, {
        # PRE-PROCESSING
        incProgress(0.1, detail = "Data pre-processing")
        results$data_clean <- get_data_clean()

        # PEER IDENTIFICATION
        incProgress(0.2, detail = "Peer identification")
        features <- get_features()

        results$thermal_correlation <- features$thermal_correlation

        results$features <- features$features

        peers_winter <- get_peers(load_condition_string = "Winter workdays")
        peers_winter$load_condition = "Winter workdays"
        peers_summer <- get_peers(load_condition_string = "Summer workdays")
        peers_summer$load_condition = "Summer workdays"
        results$peers <- rbind(peers_winter, peers_summer)

        # ENERGY PERFORMANCE INDICATORS CALCULATION
        incProgress(0.1, detail = "Energy Use Intensity calculation")
        EUI <- get_eui()
        kpi$EUI_Winter <- EUI$EUI[EUI$load_condition == "Winter"]
        kpi$EUI_Summer <- EUI$EUI[EUI$load_condition == "Summer"]

        incProgress(0.2, detail = "Operational schedules extraction")
        results$operational_schedules <- get_operational_schedules()

        kpi_schedules <- get_epi_schedules(results$operational_schedules$schedule)
        off_impact <- kpi_schedules$off_impact
        weekend_impact <- kpi_schedules$weekend_impact
        kpi$off_impact_Winter <- off_impact$off_impact[off_impact$load_condition == "Winter workdays"]
        kpi$off_impact_Summer <- off_impact$off_impact[off_impact$load_condition == "Summer workdays"]
        kpi$weekend_impact_Winter <- weekend_impact$weekend_impact[weekend_impact$season == "Winter"]
        kpi$weekend_impact_Summer <- weekend_impact$weekend_impact[weekend_impact$season == "Summer"]

        incProgress(0.1, detail = "Analysis of anomalies in energy consumption")
        kpi$ar_Winter_workdays <- get_anomaly_rate("Winter workdays")
        kpi$ar_Summer_workdays <- get_anomaly_rate("Summer workdays")
        kpi$ar_Winter_weekends <- get_anomaly_rate("Winter weekends")
        kpi$ar_Summer_weekends <- get_anomaly_rate("Summer weekends")

        incProgress(0.2, detail = "Volatility of energy consumption calculation")
        kpi$volatility_Winter_workdays <- get_volatility("Winter workdays")
        kpi$volatility_Summer_workdays <- get_volatility("Summer workdays")
        kpi$volatility_Winter_weekends <- get_volatility("Winter weekends")
        kpi$volatility_Summer_weekends <- get_volatility("Summer weekends")

        incProgress(0.1, detail = "Obtaining the frequency of load profiles")
        load_shape_frequency <- get_load_shape_frequency()
        kpi$shape_frequency_Winter_workdays <- load_shape_frequency$freq[load_shape_frequency$load_condition == "Winter workdays"]
        kpi$shape_frequency_Summer_workdays <- load_shape_frequency$freq[load_shape_frequency$load_condition == "Summer workdays"]
        kpi$shape_frequency_Winter_weekends <- load_shape_frequency$freq[load_shape_frequency$load_condition == "Winter weekends"]
        kpi$shape_frequency_Summer_weekends <- load_shape_frequency$freq[load_shape_frequency$load_condition == "Summer weekends"]

      })
    })

    data_clean <<- reactive({
      results$data_clean

    })

    features <<- reactive({
      results$features
    })

    peers <<- reactive({
      results$peers
    })

    thermal_correlation <<- reactive({
      results$thermal_correlation
    })

    schedules <<- reactive({
      results$operational_schedules$schedule
    })

    KPIs <<- reactive({
      list(
        "EUI_Winter" = round(kpi$EUI_Winter, 2),
        "EUI_Summer" = round(kpi$EUI_Summer, 2),
        "off_impact_Winter" = kpi$off_impact_Winter,
        "off_impact_Summer" = kpi$off_impact_Summer,
        "weekend_impact_Winter" = kpi$weekend_impact_Winter,
        "weekend_impact_Summer" = kpi$weekend_impact_Summer,
        "ar_Winter_workdays" = kpi$ar_Winter_workdays,
        "ar_Summer_workdays" = kpi$ar_Summer_workdays,
        "ar_Winter_weekends" = kpi$ar_Winter_weekends,
        "ar_Summer_weekends" = kpi$ar_Summer_weekends,
        "volatility_Winter_workdays" = round(mean(kpi$volatility_Winter_workdays$distance_volatility), 2),
        "volatility_Summer_workdays" = round(mean(kpi$volatility_Summer_workdays$distance_volatility), 2),
        "volatility_Winter_weekends" = round(mean(kpi$volatility_Winter_weekends$distance_volatility), 2),
        "volatility_Summer_weekends" = round(mean(kpi$volatility_Summer_weekends$distance_volatility), 2),
        "shape_frequency_Winter_workdays" = kpi$shape_frequency_Winter_workdays,
        "shape_frequency_Summer_workdays" =  kpi$shape_frequency_Summer_workdays,
        "shape_frequency_Winter_weekends" = kpi$shape_frequency_Winter_weekends,
        "shape_frequency_Summer_weekends" = kpi$shape_frequency_Summer_weekends
      )
    })


    return(list(analyze = reactive({
      input$analyze
    })))


  })
}

## To be copied in the UI
# mod_load_data_ui("load_data_1")

## To be copied in the server
# mod_load_data_server("load_data_1")
