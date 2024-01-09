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
        width = 12,
        title = "About Peer identification",
        solidHeader = TRUE,
        collapsible = TRUE,
        withMathJax(HTML(
          paste0(
            "<p> The peer identification process is a step performed to identify a group of similar buildings against which compare your building. </p>",
            "<p> This identification process is performed taking into account several aspects,
            such as: </p>",
            "<ul>",
            "<li> The end-use category (or Primary Space Usage PSU) of the building: when a new building is benchmarked, its peers are searched among buildings with the same end-use category. </li>",
            "<li> Reference load conditions among the yearly electrical energy consumption time series: when a new building is benchmarked its yearly energy consumption time series is firstly chunked into sub-sequences of daily length and all the obtained daily load profiles are then grouped in so-called ”Load conditions” a-priori identified with a domain-expert approach (i.e., winter workdays, winter weekends and holidays, summer workdays, summer weekends and holidays).
            In particular, non-working days are identified as days with a low standard deviation of the energy consumption.</li>",
            "<li>  The sensitiveness of the electric load to the external air temperature: when a new building is benchmarked, for some of the pre-identified load conditions (i.e., Winter workdays, Summer workdays) the electrical load is classified, by means of a statistical correlation analysis, as thermal sensitive or not.
            In particular, the Spearman's rank correlation coefficient is used to identify a potential correlation, and for those with a Sperman > 0.4, a regression analysis is performed. </li>",
            "<li> The mean daily energy consumption: when a new building is benchmarked, for some of the pre-identified load conditions (i.e., Winter workdays, Summer workdays) is calculated the mean daily energy consumption. </li>",
            "<li> A load shape factor F : when a new building is benchmarked, for some of the pre-identified load conditions (i.e., Winter workdays, Summer workdays) is calculated an indicator that is representative of the shape of daily load profiles. </li>",
            "</ul>",
            "<p> In particular, once the load condition of the building are identified, the thermal correlation analysis is performed, which can return 'Thermal sensitive' or 'Non-thermal sensitive' for the two load conditions 'Winter workdays' and 'Summer workdays'.
            Subsequently, the identification of the most similar peers to the building under analysis is performed considering two different metrics: the mean daily energy consumption and the load shape factor F, calculated as: ",
            withMathJax("\\(F = \\frac{E_{night}\\:[kWh]}{E_{day}\\:[kWh]}\\)"),
            ", where ",
            withMathJax("\\(E_{night}\\)"),
            " is the amount of energy consumption in the interval [20:00-07:00] and ",
            withMathJax("\\(E_{day}\\)"),
            " is the amount of energy consumption in the interval [08:00-19:00] separately evaluated for all the days in the load conditions 'Winter Workday' and 'Summer Workday'.
            Finally, the 30 nearest neighbors are extracted from the dataset using the Euclidean distance as a similarity measure in the geometrical space. In particular, the calculated Euclidean distance is differently weighted among the two metrics, giving the 70% of the importance to the peer similarity on E and the remaining 30% of the importance to the peer similarity on the load shape factor F. </p>"
          )
        )
        )
      )
    ),
    fluidRow(
      box(
        title = "Load condition segmentation",
        width = 12,
        solidHeader = TRUE,
        withSpinner(htmlOutput(outputId = ns("plot_load_condition")))
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

    output$plot_load_condition <- renderUI({
      req(data_clean())

      data <- data_clean() %>%
        mutate(
          time = strftime(timestamp, format = "%H:%M", tz = 'UTC'),
          date = as.Date(timestamp, format = '%Y-%m-%d'),
          load_condition = get_load_condition()$load_condition
          ) %>%
        select(c(date, time, power, load_condition))

      tags$div(
        HTML(sprintf(
          '<script>load_condition_plot(%s, "peer_identification1-plot_load_condition")</script>',
          jsonlite::toJSON(data, dataframe = "rows")
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




  })
}

## To be copied in the UI
# mod_peer_identification_ui("peer_identification_1")

## To be copied in the server
# mod_peer_identification_server("peer_identification_1")
