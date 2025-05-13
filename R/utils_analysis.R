#' This script contains all the util functions to perform the analysis of the building

#' @description : function that returns the load condition tag for each timestamp
#'
#' @return a dataframe with timestamp and load condition
#'
#' @noRd
#'
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
        dayofweek = wday(date, label = TRUE, locale="en_US.UTF-8"),
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
        dayofweek = wday(date, label = TRUE, locale="EN-us"),
        month = as.character(month(date, label = TRUE, abbr = TRUE, locale = "EN-us"))
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
#'
#' @param load_condition_string the load condition string (e.g. "Winter Workday")
#'
#' @return dates: vector of dates in character format
#'
#' @noRd
#'
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
#'
#' @param load_condition_string the load condition string (e.g. "Winter Workday")
#'
#' @return dist_matrix
#'
#' @noRd
#'
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
#'
#' @param load_condition_string the load condition string (e.g. "Winter Workday")
#'
#' @return dist_matrix
#'
#' @noRd
#'
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


#' @description Function that returns the outliers date based on distance calculation
#'
#' @param load_condition_string the load condition string (e.g. "Winter Workday")
#'
#' @return date_outliers: the date of outliers identified
#'
#' @noRd
#'
#' @importFrom dplyr mutate group_by summarise
#' @importFrom stats setNames
get_anomaly_dates <- function(load_condition_string) {


  # Load dataframe of distance among neighbor load profiles
  df_neighbor_distance <- get_neighbor_distance(load_condition_string)
  df_neighbor_distance$mean_dist <- rowMeans(df_neighbor_distance[, 2:length(df_neighbor_distance)], na.rm = TRUE)

  # Outlier detection for distances
  outliers_box_plot <- get_outlier_boxplot(df_neighbor_distance$mean_dist)
  outliers_zscore <- get_outlier_zscore(df_neighbor_distance$mean_dist)
  outliers_MAD <- get_outlier_mad(df_neighbor_distance$mean_dist)

  all_outliers <- unlist(list(outliers_box_plot, outliers_zscore, outliers_MAD))
  all_outliers_date <- df_neighbor_distance$date[all_outliers]

  outliers_distance <- as.data.frame(all_outliers_date) %>%
    setNames(c("date")) %>%
    mutate(date = as.character(date)) %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(anomaly_distance_score = n())

  date_outliers <- outliers_distance$date[outliers_distance$anomaly_distance_score == 3]

  return(date_outliers)

}


#' @description Returns the clustering assignment for the daily load profiles of a building
#'
#' @return date with cluster labels
#'
#' @noRd
#'
#' @importFrom dplyr mutate select
get_cluster_labels <- function() {

  data <- data_clean() %>%
    mutate(
      time = strftime(timestamp, format = "%H:%M:%S", tz = 'UTC'),
      date = as.Date(timestamp, format = '%Y-%m-%d')
    ) %>%
    mutate(load_condition = get_load_condition()$load_condition) %>%
    tidyr::pivot_wider(id_cols = !timestamp, names_from = "time", values_from = "power") %>%
    select(-load_condition, load_condition) %>%
    subset(!load_condition %in% c("Holidays", "Non-working days"))
  colnames(data) <- as.character(colnames(data))

  # Normalization
  max <- apply(data[, 2:25], 1, max)
  data_norm <- data
  data_norm[, 2:25] <- data_norm[, 2:25]/max

  centroids <- read.csv(file.path("data", paste0("centroids_",tolower(end_use()), ".csv")), header = TRUE)

  data_clustered <- data.frame(matrix(nrow = 0, ncol = 2)) %>%
    setNames(c("date", "cluster"))

  for (load_condition_string in unique(data_norm$load_condition)) {

    centroids_lc <- centroids %>%
      subset(load_condition == load_condition_string)

    max_centroids <- apply(centroids_lc[, 2:25], 1, max)
    centroids_norm <- centroids_lc
    centroids_norm[, 2:25] <- centroids_norm[, 2:25]/max_centroids

    data_norm_lc <- data_norm %>%
      subset(load_condition == load_condition_string)

    data_norm_lc$cluster <- NA

    for (idx_day in c(1:length(data_norm_lc$date))) {

      dist <- c()
      for (idx_cluster in c(1:nrow(centroids_norm))) {

        dist <- append(dist, euclidean_distance(data_norm_lc[idx_day, 2:25], centroids_norm[idx_cluster, 2:25], weights = c(1,1)))
      }

      data_norm_lc$cluster[idx_day] <- which.min(dist)

    }

    data_norm_lc$cluster<- paste("Shape", data_norm_lc$cluster)

    data_clustered <- rbind(data_clustered, data_norm_lc[, c("date", "cluster", "load_condition")])

  }

  return(data_clustered)

}
