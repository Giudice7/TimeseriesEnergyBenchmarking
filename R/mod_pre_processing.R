#' pre_processing UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinycssloaders withSpinner
mod_pre_processing_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        width = 9,
        title = "Time series pre-processing",
        solidHeader = TRUE,
        withSpinner(htmlOutput(outputId = ns("plot_ts_clean")))
      ),
      box(
        width = 3,
        title = "Data corruptance",
        solidHeader = TRUE,
        withSpinner(uiOutput(outputId = ns("corruptance")))
      )
    ) # ,
    # fluidRow(
    #   box(
    #     width = 12,
    #     title = "Multi-Seasonal and Trend Decomposition by Loess",
    #     solidHeader = TRUE,
    #     withSpinner(htmlOutput(outputId = ns("mstl_plot")))
    #   )
    # )
  )
}

#' pre_processing Server Functions
#'
#' @noRd
#'
#' @importFrom dplyr mutate
#' @import highcharter
#' @import rlang
#' @import mathjaxr
mod_pre_processing_server <- function(id, button){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Calculate the number of outliers, missing values, high values and low values
    data_corruptance <- reactiveValues(
      outliers = 0,
      missing = 0,
      constant = 0,
      hv = 0,
      hv_threshold = 0,
      lv = 0,
      lv_threshold = 0,
      percentage = 0
    )

    pre_processing_results <- observeEvent(button(), {
      req(data_raw())

      data <- data_raw() %>%
        mutate(
          type = ifelse(is.na(power), "Missing value", "No")
        )

      # Indentify very low values and very high values
      low_threshold <- quantile(data$power, 0.05, na.rm = TRUE) / 2

      high_threshold <- quantile(data$power, 0.95, na.rm = TRUE) * 2

      idx_lv <- which(data$power < low_threshold)
      idx_hv <- which(data$power > high_threshold)

      data$type[idx_lv] <- "Low value"
      data$type[idx_hv] <- "High value"

      data$power[idx_lv] <- NA
      data$power[idx_hv] <- NA


      # Find consecutive values and their position
      list <- rle(data_raw()$power)

      # Find the position of duplicated values > 6
      pos <- which(list$lengths > 6)

      constant_vector <- c()

      if (length(pos) >= 1) {

        for (idx_pos in c(1:length(pos))) {
          # Starting position
          start <- sum(list$lengths[1:pos[idx_pos]]) - list$lengths[pos[idx_pos]] + 1
          # End position
          end <- sum(list$lengths[1:pos[idx_pos]])
          # Index of dataframe with consecutive values
          constant_vector <- append(constant_vector, c(start:end))
        }

      }

      index_outliers <- get_outlier_index(data)

      data$type[index_outliers] <- "Outlier"
      data$type[constant_vector] <- "Constant value"

      data_corruptance$outliers <- length(data$type[data$type == "Outlier"])
      data_corruptance$missing <- length(data$type[data$type == "Missing value"])
      data_corruptance$constant <- length(data$type[data$type == "Constant value"])
      data_corruptance$lv <- length(data$type[data$type == "Low value"])
      data_corruptance$hv <- length(data$type[data$type == "High value"])
      data_corruptance$percentage <- round((data_corruptance$outliers + data_corruptance$missing + data_corruptance$constant + data_corruptance$hv + data_corruptance$lv) /8760 * 100, 1)
      data_corruptance$index <-  which(data$type != "No"& data$type != "Missing value")
      data_corruptance$data_raw_tagged <- data
      data_corruptance$hv_threshold <- round(high_threshold, 1)
      data_corruptance$lv_threshold <- round(low_threshold, 1)
    })


    output$corruptance <- renderUI({
      req(data_raw(), data_clean())
      HTML(
        paste(
          "<p class='value'> <span class='highlight'>",
          data_corruptance$percentage,
          "</span> <span class='highlight'> % </span> </p>",
          "<p> Time series corruptance </p>",
          "<p class='value'>",
          data_corruptance$outliers,
          "</p>",
          "<p> Outliers </p>",
          "<p class='value'>",
          data_corruptance$missing,
          "</p>",
          "<p> Missing values </p>",
          "<p class='value'>",
          data_corruptance$constant,
          "</p>",
          "<p> Continuous constant values </p>",
          "<div class='inline-div'> <div class='inline-value'> <p class='value'>",
          data_corruptance$hv,
          "</p>",
          "<p> Very high value </p> </div>",
          "<div class='inline-value'> <p class='value'>",
          data_corruptance$hv_threshold,
          " kW</p>",
          "<p> Very high value threshold </p> </div> </div>",
          "<div class='inline-div'> <div class='inline-value'> <p class='value'>",
          data_corruptance$lv,
          "</p>",
          "<p> Very low value </p> </div>",
          "<div class='inline-value'> <p class='value'>",
          data_corruptance$lv_threshold,
          " kW</p>",
          "<p> Very low value </p> </div> </div>"

        )
      )
    })


    output$plot_ts_clean <- renderUI({
      req(data_raw(), data_clean())

      data <- data_raw() %>%
        setNames(c("timestamp", "power_raw")) %>%
        merge(data_clean(),
              by = "timestamp") %>%
        merge(
          data_corruptance$data_raw_tagged[, c("timestamp", "type")],
          by = "timestamp") %>%
        mutate(timestamp = as.POSIXct(timestamp,
                                      tz = "UTC",
                                      format = "%Y-%m-%d %H:%M")) %>%
        mutate(datetime_ms = as.numeric(timestamp) * 1000)

      tags$div(
        HTML(sprintf(
          '<script>plot_ts_clean(%s, %s, "pre_processing1-plot_ts_clean")</script>',
          jsonlite::toJSON(data, dataframe = "rows"),
          jsonlite::toJSON(data_corruptance$index, dataframe = "rows")
        ))
      )


    })

    # output$mstl_plot <- renderUI({
    #   data <- data_raw() %>%
    #     mutate(
    #       time = strftime(timestamp, format = "%H:%M:%S", tz = 'UTC'),
    #       date = as.Date(timestamp, format = '%Y-%m-%d'),
    #       dayofweek = weekdays(date, abbreviate = TRUE),
    #       month = as.character(format(date, "%b"))
    #     )
    #
    #   # Calculate threshold of low values and high values
    #   low_threshold <- quantile(data$power, 0.05, na.rm = TRUE) / 2
    #
    #   high_threshold <- quantile(data$power, 0.95, na.rm = TRUE) * 2
    #
    #   # Substituting very low values with na
    #   data <- data %>%
    #     mutate(power = ifelse(power < low_threshold |
    #                             power > high_threshold, NA, power))
    #
    #   x <- msts(data$power[49:8569], seasonal.periods = c(24, 24 * 7))
    #
    #   decomposition <- data.frame(mstl(x, s.window = "periodic"))
    #   decomposition$timestamp <- data$timestamp[49:8569]
    #   decomposition <- decomposition %>%
    #     mutate(timestamp = as.POSIXct(timestamp, tz = "UTC")) %>%
    #     mutate(datetime_ms = as.numeric(timestamp) * 1000) %>%
    #     select(-c(timestamp))
    #
    #   tags$div(HTML(
    #     sprintf(
    #       '<script>mstl_plot(%s, "pre_processing1-mstl_plot")</script>',
    #       jsonlite::toJSON(decomposition, dataframe = "rows")
    #     )
    #   ))
    #
    #
    # })


  })
}

## To be copied in the UI
# mod_pre_processing_ui("pre_processing_1")

## To be copied in the server
# mod_pre_processing_server("pre_processing_1")
