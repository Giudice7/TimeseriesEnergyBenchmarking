#' peer_identification UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_peer_identification_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        title = "Load condition segmentation",
        width = 12,
        solidHeader = TRUE,
        div(
          id = "load_condition_segmentation",
          htmlOutput(outputId = ns("Winter_workdays_profiles")),
          htmlOutput(outputId = ns("Summer_workdays_profiles")),
          htmlOutput(outputId = ns("Winter_weekends_profiles")),
          htmlOutput(outputId = ns("Summer_weekends_profiles")),
          htmlOutput(outputId = ns("non_workdays_profiles")),
          htmlOutput(outputId = ns("Holidays_profiles"))
        )
      )
    ),
    fluidRow(
      selectizeInput(inputId = ns("load_condition_peer_identification"),
                     label = "Select a load condition to inspect peer buildings",
                     choices = c("Winter workdays", "Summer workdays"))
    ),
    fluidRow(
      box(
        title = "Thermal sensitivity analysis",
        width = 6,
        solidHeader = T,
        withSpinner(uiOutput(outputId = ns("thermal_correlation_results"))),
        withSpinner(htmlOutput(outputId = ns("plot_energy_signature")))
      ),
      box(
        title = "Identification of peer buildings",
        width = 6,
        solidHeader = T,
        withSpinner(uiOutput(outputId = ns("features"))),
        withSpinner(htmlOutput(outputId = ns("plot_peers")))
      )
    )
  )
}

#' peer_identification Server Functions
#'
#' @noRd
mod_peer_identification_server <- function(id, button){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

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

    output$Summer_workdays_profiles <- renderUI({
      req(data_lc())

      tags$div(
        HTML(sprintf(
          '<script>load_condition_plot(%s, "peer_identification1-Summer_workdays_profiles", "%s", "%s")</script>',
          jsonlite::toJSON(data_lc() %>%
                             subset(load_condition == "Summer workdays"),
                           dataframe = "rows"),
          "Summer workdays",
          max(data_clean()$power)
        ))
      )
    })

    output$Winter_workdays_profiles <- renderUI({
      req(data_lc())

      tags$div(
        HTML(sprintf(
          '<script>load_condition_plot(%s, "peer_identification1-Winter_workdays_profiles", "%s", "%s")</script>',
          jsonlite::toJSON(data_lc() %>% subset(load_condition == "Winter workdays"), dataframe = "rows"),
          "Winter workdays",
          max(data_clean()$power)
        ))
      )
    })


    output$Summer_weekends_profiles <- renderUI({
      req(data_lc())

      tags$div(
        HTML(sprintf(
          '<script>load_condition_plot(%s, "peer_identification1-Summer_weekends_profiles", "%s", "%s")</script>',
          jsonlite::toJSON(data_lc() %>%
                             subset(load_condition == "Summer weekends"),
                           dataframe = "rows"),
          "Summer weekends",
          max(data_clean()$power)
        ))
      )
    })

    output$Winter_weekends_profiles <- renderUI({
      req(data_lc())

      tags$div(
        HTML(sprintf(
          '<script>load_condition_plot(%s, "peer_identification1-Winter_weekends_profiles", "%s", "%s")</script>',
          jsonlite::toJSON(data_lc() %>%
                             subset(load_condition == "Winter weekends"),
                           dataframe = "rows"),
          "Winter weekends",
          max(data_clean()$power)
        ))
      )
    })

    output$non_workdays_profiles <- renderUI({
      req(data_lc())

      tags$div(
        HTML(sprintf(
          '<script>load_condition_plot(%s, "peer_identification1-non_workdays_profiles", "%s", "%s")</script>',
          jsonlite::toJSON(data_lc() %>% subset(load_condition == "Non-working days"), dataframe = "rows"),
          "Non-working days",
          max(data_clean()$power)
        ))
      )
    })

    output$Holidays_profiles <- renderUI({
      req(data_lc())

      tags$div(
        HTML(sprintf(
          '<script>load_condition_plot(%s, "peer_identification1-Holidays_profiles", "%s", "%s")</script>',
          jsonlite::toJSON(data_lc() %>% subset(load_condition == "Holidays"), dataframe = "rows"),
          "Holidays",
          max(data_clean()$power)
        ))
      )
    })




    output$thermal_correlation_results <- renderUI({
      req(thermal_correlation())

      correlation <- thermal_correlation()

      spearman <- correlation$spearman[correlation$load_condition == input$load_condition_peer_identification]
      slope <- correlation$slope[correlation$load_condition == input$load_condition_peer_identification]
      result <- correlation$result[correlation$load_condition == input$load_condition_peer_identification]

      HTML(
        paste(
          "<div class='card'> <p class='value'>",
          result,
          "</p> <p> Sensitiveness </p> </div>",
          "<div class='card'> <p class='value'>",
          round(spearman, 1),
          "</p> <p> Spearman coefficient </p> </div>",
          "<div class='card'> <p class='value'>",
          slope,
          "</p> <p> Normalized slope </p> </div>"
        )
      )
    })

    output$plot_energy_signature <- renderUI({
      req(data_clean(), weather())
      data <- merge(
        x = weather(),
        y = data_clean() %>%
          mutate(date = as.Date(timestamp, format = '%Y-%m-%d')) %>%
          mutate(load_condition = get_load_condition()$load_condition),
        by = "timestamp",
        all = TRUE
      ) %>%
        mutate(airTemperature = imputeTS::na_interpolation(airTemperature, option = "linear"))

      data <- data[!duplicated(data),]

      data <- data %>%
        subset(load_condition == input$load_condition_peer_identification) %>%
        dplyr::group_by(date, load_condition) %>%
        dplyr::summarise(energy = sum(power),
                         temperature = round(mean(airTemperature), 1)) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(temperature) %>%
        dplyr::slice(1:90)

      model <- lm(energy ~ temperature, data = data)
      slope <- coef(model)["temperature"]
      intercept <- coef(model)["(Intercept)"]

      tags$div(
        HTML(sprintf(
          '<script>energy_signature_plot(%s, "peer_identification1-plot_energy_signature", %s, %s)</script>',
          jsonlite::toJSON(data, dataframe = "rows"),
          slope, intercept
        ))
      )

    })

    output$features <- renderUI({
      req(thermal_correlation())

      correlation <- thermal_correlation()
      result <- correlation$result[correlation$load_condition == input$load_condition_peer_identification]

      HTML(
        paste(
          "<div class='card'> <p class='value'>",
          result,
          "</p> <p> Sensitiveness </p> </div>",
          "<div class='card'> <p class='value'>",
          end_use(),
          "</p> <p> PSU </p> </div>",
          "<div class='card'> <p class='value'>",
          features()$mean_ec[features()$load_condition == input$load_condition_peer_identification],
          "kWh</p> <p> Mean daily energy consumption </p> </div>",
          "<div class='card'> <p class='value'>",
          features()$shape_factor[features()$load_condition == input$load_condition_peer_identification],
          "</p> <p> Shape factor </p> </div>"
        )
      )
    })

    output$plot_peers <- renderUI({
      req(peers())

      peers <- peers() %>%
        subset(load_condition == input$load_condition_peer_identification)

      correlation <- thermal_correlation()
      result <- correlation$result[correlation$load_condition == input$load_condition_peer_identification]

      features_all <- read.csv2(
        file.path("data", paste0("features_", tolower(end_use()), ".csv"))
      ) %>%
        subset(!(building_id %in% gsub(".csv", "", list.files(path = file.path("data", "default_files", "electric_consumption"), pattern = tolower(end_use()))))) %>%
        subset(temperature_correlation == result) %>%
        subset(load_condition == input$load_condition_peer_identification) %>%
        mutate(type = ifelse(
          building_id %in% peers$building_id, "Peers", end_use()
        ))

      end_use_features <- features_all %>%
        subset(type == end_use()) %>%
        mutate(mean_ec = as.numeric(mean_ec),
               shape_factor = as.numeric(shape_factor))

      peers_features <- features_all %>%
        subset(type == "Peers") %>%
        mutate(mean_ec = as.numeric(mean_ec),
               shape_factor = as.numeric(shape_factor))

      building_features <- features() %>%
        subset(load_condition == input$load_condition_peer_identification)

      end_use_name <- end_use()

      tags$div(
        HTML(sprintf(
          '<script>scatter_plot_peers("peer_identification1-plot_peers", %s, %s, %s, %s)</script>',
          jsonlite::toJSON(end_use_features, dataframe = "rows"),
          jsonlite::toJSON(peers_features, dataframe = "rows"),
          jsonlite::toJSON(building_features, dataframe = "rows"),
          jsonlite::toJSON(end_use_name, dataframe = "rows")
        ))
      )

    })






  })
}

## To be copied in the UI
# mod_peer_identification_ui("peer_identification_1")

## To be copied in the server
# mod_peer_identification_server("peer_identification_1")
