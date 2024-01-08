#' This script contains all the util functions to perform the analysis of the building

#' @description : function that returns the load condition tag for each timestamp

#' @return a dataframe with timestamp and load condition

#' @noRd

#' @importFrom dplyr mutate group_by summarise
#' @importFrom lubridate month wday
get_load_condition <- function() {

  if (state() != "None") {

    calendar_file <- list.files(path = file.path("data", "calendar"),
                                pattern = state())

    df_date <- read.csv(file = file.path("data", "calendar", calendar_file))

    data <- data_clean() %>%
      mutate(
        time = strftime(timestamp, format = "%H:%M:%S", tz = 'UTC'),
        date = as.Date(timestamp, format = '%Y-%m-%d'),
        dayofweek = wday(date, label = TRUE, locale="EN-us"),
        month = as.character(month(date, label = TRUE, abbr = TRUE, locale = "EN-us"))
      ) %>%
      mutate(
        load_condition = ifelse(
          as.character(date) %in% df_date$date, "Holidays",
          ifelse(
            month %in% c("Jan", "Feb", "Mar", "Oct", "Nov", "Dec") &
              dayofweek %in% c("Sun", "Sat"),
            "Winter Weekend",
            ifelse(
              month %in% c("Jan", "Feb", "Mar", "Oct", "Nov", "Dec") &
                !(dayofweek %in% c("Sun", "Sat")),
              "Winter Weekday",
              ifelse(
                month %in% c("Apr", "May", "Jun", "Jul", "Aug", "Sep") &
                  dayofweek %in% c("Sun", "Sat"),
                "Summer Weekend","Summer Weekday")
            )
          ))
      )

  } else {
    data <- data_clean() %>%
      mutate(
        time = strftime(timestamp, format = "%H:%M:%S", tz = 'UTC'),
        date = as.Date(timestamp, format = '%Y-%m-%d'),
        dayofweek = weekdays(date, abbreviate = TRUE),
        month = as.character(format(date, "%b"))
      ) %>%
      mutate(
        load_condition = ifelse(
          month %in% c("Jan", "Feb", "Mar", "Oct", "Nov", "Dec") &
            dayofweek %in% c("Sun", "Sat"),
          "Winter Weekend",
          ifelse(
            month %in% c("Jan", "Feb", "Mar", "Oct", "Nov", "Dec") &
              !(dayofweek %in% c("Sun", "Sat")),
            "Winter Weekday",
            ifelse(
              month %in% c("Apr", "May", "Jun", "Jul", "Aug", "Sep") &
                dayofweek %in% c("Sun", "Sat"),
              "Summer Weekend","Summer Weekday")
          )
        )
      )
  }

  getOutlierBoxPlot <- function(x) {
        #' @description function that returns that return the value of a vector that are outlier based on boxplot method.

    idx_outliers <- (x < quantile(x, .25) - 1.5*IQR(x))

    return(which(idx_outliers))
  }

  data_sd <- data %>%
    subset(load_condition %in% c("Winter Weekday", "Summer Weekday")) %>%
    group_by(date, load_condition) %>%
    summarise(sd = sd(power))

  data_Winter <- data_sd[data_sd$load_condition == "Winter Weekday",]
  data_outliers_Winter <- data_Winter[getOutlierBoxPlot(data_Winter$sd),]

  data_Summer <- data_sd[data_sd$load_condition == "Summer Weekday",]
  data_outliers_Summer <- data_Summer[getOutlierBoxPlot(data_Summer$sd),]

  data_outliers <- rbind(data_outliers_Winter, data_outliers_Summer)

  data <- data %>%
    mutate(load_condition = ifelse(date %in% data_outliers$date, "Non-working days", load_condition)) %>%
    mutate(load_condition = ifelse(load_condition == "Winter Weekday", "Winter workdays",
                                   ifelse(load_condition == "Summer Weekday", "Summer workdays",
                                          ifelse(load_condition == "Winter Weekend", "Winter weekends",
                                                 ifelse(load_condition == "Summer Weekend", "Summer weekends", load_condition)))
    ))

  return(data[, c("timestamp", "load_condition")])

  # Extract holidays

  return(data)
}


#' @description Functions that return the statistical mode of a vector
#'
#' @param x: a vector of number or characters
#'
#' @return the mode
#'
#' @noRd
get_mode <- function(x) {

  ux <- unique(x)

  return(ux[which.max(tabulate(match(x, ux)))])

}


#' @description Function that returns the dates into a load condition
#' @param load_condition_string the load condition string (e.g. "Winter Workday")

#' @return dates: vector of dates in character format

#' @noRd

#' @importFrom dplyr mutate
get_dates <- function(load_condition_string) {

  data <- data_clean() %>%
    mutate(
      time = strftime(timestamp, format = "%H:%M:%S", tz = 'UTC'),
      date = as.Date(timestamp, format = '%Y-%m-%d'),
      dayofweek = weekdays(date, abbreviate = TRUE),
      month = as.character(format(date, "%b"))
    ) %>%
    mutate(
      load_condition = get_load_condition()$load_condition
    ) %>%
    subset(load_condition == load_condition_string)

  dates <- unique(as.character(data$date))

  return(dates)
}


#' @description Function that calculates the distance matrix among load profiles in a load condition

#' @param load_condition_string the load condition string (e.g. "Winter Workday")

#' @return dist_matrix

#' @noRd

#' @importFrom dplyr mutate select
#' @importFrom tidyr pivot_wider
get_distance_matrix_power <- function(load_condition_string) {

  data <- data_clean() %>%
    mutate(
      time = strftime(timestamp, format = "%H:%M:%S", tz = 'UTC'),
      date = as.Date(timestamp, format = '%Y-%m-%d'),
      dayofweek = weekdays(date, abbreviate = TRUE),
      month = as.character(format(date, "%b"))
    ) %>%
    mutate(
      load_condition = get_load_condition()$load_condition
    )

  data_wide <- data %>%
    subset(load_condition == load_condition_string) %>%
    select(timestamp, date, time, power) %>%
    pivot_wider(id_cols = !timestamp, names_from = "time", values_from = "power")

  dist_matrix  <- as.matrix(dist(data_wide[2:25], method = "euclidean"))

  return(dist_matrix)
}


#' @description Function that calculates the distance matrix among temperature profiles in a load condition

#' @param load_condition_string the load condition string (e.g. "Winter Workday")

#' @return dist_matrix

#' @noRd

#' @importFrom dplyr mutate select
#' @importFrom tidyr pivot_wider
#' @importFrom imputeTS na_interpolation
get_distance_matrix_temperature <- function(load_condition_string) {

  data <- merge(
    x = weather(),
    y = data_clean() %>%
      mutate(
        time = strftime(timestamp, format = "%H:%M:%S", tz = 'UTC'),
        date = as.Date(timestamp, format = '%Y-%m-%d'),
        dayofweek = weekdays(date),
        month = as.character(format(date, "%b"))
      ) %>%
      mutate(
        load_condition = get_load_condition()$load_condition
      ),
    by = "timestamp",
    all = TRUE
  ) %>%
    mutate(airTemperature = imputeTS::na_interpolation(airTemperature, option = "linear"))

  # Eliminate duplicates
  data <- data[!duplicated(data),]

  data_wide <- data %>%
    subset(load_condition == load_condition_string) %>%
    select(timestamp, date, time, airTemperature) %>%
    pivot_wider(id_cols = !timestamp, names_from = "time", values_from = "airTemperature")

  dist_matrix  <- as.matrix(dist(data_wide[2:25], method = "euclidean"))

  return(dist_matrix)
}

