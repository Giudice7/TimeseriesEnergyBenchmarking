#' @description Function that returns the neighbor dates for each day

#' @param load_condition_string the load condition string (e.g. "Winter Workday")

#' @return df_neighbor_date

#' @noRd

#' @importFrom dplyr pull arrange
get_neighbor_days <- function(load_condition_string) {


  # Obtaining the number of dates in the load condition
  dates <- get_dates(load_condition_string)

  load_condition_thermal <- gsub(" .*", " Workday", load_condition_string)

  temperature_correlation <- thermal_correlation() %>%
    subset(load_condition == load_condition_thermal) %>%
    pull(result)

  # Distance matrix calculation: if thermal sensitive the neighbors are founded in temperature profiles
  if (temperature_correlation == "Non-thermal sensitive") {
    dist_matrix  <- get_distance_matrix_power(load_condition_string)
  } else {
    dist_matrix  <- get_distance_matrix_temperature(load_condition_string)
  }

  n_neighbor <- round(0.1 * length(rownames(dist_matrix)))

  # Obtaining the dataframe with neighbor index days for each day
  df_neighbor <- data.frame(matrix(ncol = n_neighbor + 1, nrow = length(dates))) %>%
    setNames(append("idx_day", c(1:n_neighbor)))

  for (idx_day in rownames(dist_matrix)) {

    dist_day <- data.frame(
      dist_matrix[idx_day, colnames(dist_matrix)[!colnames(dist_matrix) %in% c(idx_day)]]) %>%
      setNames(c("dist")) %>%
      arrange(dist)

    idx_neighbor <- rownames(dist_day)[1:n_neighbor]

    df_neighbor[idx_day, ]<- c(idx_day, idx_neighbor)

  }

  df_neighbor_date <- data.frame(matrix(ncol = n_neighbor + 1, nrow = length(dates))) %>%
    setNames(append("date", c(1:n_neighbor)))

  for (ii in c(1:length(df_neighbor$idx_day))){

    for (jj in c(2:length(df_neighbor))) {

      df_neighbor_date[ii, jj] <- as.character(dates[as.numeric(df_neighbor[ii, jj])])

    }
  }

  df_neighbor_date$date <- dates

  return(df_neighbor_date)
}


#' @description Function that returns the distance dataframe for each day with its neighbor load profiles

#' @param state the state where the building is located
#' @param load_condition_string the load condition string (e.g. "Winter Workday")

#' @return df_neighbor_distance

#' @noRd

#' @importFrom dplyr slice pull select
get_neighbor_distance <- function(load_condition_string) {

  # Obtaining the neighbor days:
  df_neighbor_date <- get_neighbor_days(load_condition_string)

  # Obtaining the number of dates in the load condition
  dates <- get_dates(load_condition_string)

  # Obtaining the distance matrix among load profiles
  dist_matrix  <- get_distance_matrix_power(load_condition_string)
  rownames(dist_matrix) <- df_neighbor_date$date
  colnames(dist_matrix) <- df_neighbor_date$date

  n_neighbor <- round(0.1 * length(rownames(dist_matrix)))

  # Calculate the distance for each day
  df_neighbor_distance <- data.frame(matrix(ncol = n_neighbor + 1, nrow = length(dates))) %>%
    setNames(append("idx_day", paste("dist", c(1:n_neighbor), sep = "")))

  for (idx_day in 1:length(rownames(dist_matrix))) {

    dist_day <- data.frame(
      dist_matrix[idx_day, colnames(dist_matrix)[!colnames(dist_matrix) %in% c(idx_day)]]) %>%
      setNames(c("dist")) %>%
      subset(rownames(.) %in% df_neighbor_date[idx_day, 2:(n_neighbor + 1)]) %>%
      slice(match(df_neighbor_date[idx_day, 2:(n_neighbor + 1)], rownames(.))) %>%
      pull(dist)

    df_neighbor_distance[idx_day, ]<- c(idx_day, as.numeric(dist_day))

  }

  df_neighbor_distance[, 2:length(df_neighbor_distance)] <- sapply(df_neighbor_distance[, 2:length(df_neighbor_distance)], as.numeric)

  df_neighbor_distance$date <- dates

  df_neighbor_distance <- select(df_neighbor_distance, date, paste("dist", c(1:n_neighbor), sep = ""))

  return(df_neighbor_distance)
}


#' @description Function that returns a dataframe containing the daily energy consumption for each
#' neighbor load profile of each day

#' @param load_condition_string the load condition string (e.g. "Winter Workday")

#' @return df_neighbor_energy

#' @noRd

#' @importFrom dplyr mutate group_by summarise
get_neighbor_energy <- function(load_condition_string) {

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

  data_daily <- data %>%
    subset(load_condition == load_condition_string) %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(energy = sum(power))

  df_neighbor_date <- get_neighbor_days(load_condition_string)

  n_neighbor <- round(0.1 * length(rownames(df_neighbor_date)))

  df_neighbor_energy <- data.frame(matrix(ncol = n_neighbor + 1, nrow = length(data_daily$date))) %>%
    setNames(append("date", paste("energy", c(1:n_neighbor), sep = "")))
  df_neighbor_energy$date <- as.character(data_daily$date)

  for (idx_column in c(2:length(df_neighbor_date))) {

    idx_match <- matchAll(df_neighbor_date[, idx_column], as.character(data_daily$date))
    df_neighbor_energy[, idx_column] <- data_daily$energy[idx_match]

  }

  df_neighbor_energy <- merge(
    x = data_daily %>%
      mutate(date = as.character(date)),
    y = df_neighbor_energy,
    by = "date",
    all = TRUE
  )

  return(df_neighbor_energy)

}
