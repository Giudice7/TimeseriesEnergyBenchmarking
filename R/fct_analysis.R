#' This script contains the functions needed to perform the analysis of the building

#' @description function that returns that return the index of power column that are outlier based on
#' boxplot method on MSTL remainder.

#' @param data: the vector that must be detected

#' @return idx_outliers: outliers detected

#' @importFrom forecast msts na.interp
#' @importFrom dplyr na_if

#' @noRd
get_outlier_index <- function(data) {
  # Substituting 0s with NA to not influence the distribution
  data$power <- na_if(data$power, 0)
  # Temporary interpolating NA to obtain mstl
  data$power <- na.interp(data$power)

  # Transmorming power into a multi-seasonal time series
  x <- msts(data$power[49:8569], seasonal.periods=c(24, 24*7))

  decomposition <- data.frame(mstl(x, s.window = "periodic"))
  remainder <- decomposition$Remainder
  condition_outliers <- (remainder > quantile(remainder, .75) + 5*IQR(remainder)) | (remainder < quantile(remainder, .15) - 5*IQR(remainder))

  return(which(condition_outliers) + 48)
}


#' @description Makes data cleaning detecting outlier with Multi Seasonal-Trend decomposition by Loess (MSTL) and
#' eliminating NA and zeros filling them with a lookup table or linear interpolation

#' @return data  Dataset cleaned by na, zeros and outliers

#' @importFrom magrittr %>%
#' @importFrom dplyr mutate group_by summarise intersect
#' @importFrom imputeTS na_interpolation
#' @importFrom forecast na.interp mstl

#' @noRd
get_data_clean <- function() {

  # Read data and extract features such as time, date, dayofweek, month and load_condition
  data <- data_raw() %>%
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
        "Winter weekends",
        ifelse(
          month %in% c("Jan", "Feb", "Mar", "Oct", "Nov", "Dec") &
            !(dayofweek %in% c("Sun", "Sat")),
          "Winter workdays",
          ifelse(
            month %in% c("Apr", "May", "Jun", "Jul", "Aug", "Sep") &
              dayofweek %in% c("Sun", "Sat"),
            "Summer weekends",
            "Summer workdays"
          )
        ))
    )

  # Calculate threshold of low values
  baseload <- quantile(data$power, 0.05, na.rm = TRUE)
  peakload <- quantile(data$power, 0.95, na.rm = TRUE)
  low_threshold <- baseload / 2
  high_threshold <- peakload * 2

  # Substituting very low values with zeros
  data <- data %>%
    mutate(power = ifelse(power < low_threshold | power > high_threshold, 0, power))

  # Detecting outliers with MSTl method and putting them to 0s
  index_outliers <- get_outlier_index(data)
  data$power[index_outliers] <- 0

  data$power[is.na(data$power)] <- 0

  # Searching for continuous constant values
  list <- rle(data$power)
  idx_zero <- which(list$values==0)
  idx_constant <- intersect(which(list$values!=0), which(list$lengths > 12))
  idx_consecutive_zero <- idx_zero[list$lengths[idx_zero] > 2]
  idx_nan <- idx_zero[list$lengths[idx_zero] <= 2]

  # Interpolating consecutive NaN if they are maximum two
  if (length(idx_nan) >0) {
    for (i in 1:length(idx_nan)) {
      idx_nan_real_start <- sum(list$lengths[1:idx_nan[i]]) - list$lengths[idx_nan][i] + 1
      idx_nan_real_end <- sum(list$lengths[1:idx_nan[i]])

      data$power[idx_nan_real_start:idx_nan_real_end] <- NA
    }
    data$power <- na_interpolation(data$power)
  }

  # Putting at 0 continuous constant values higher than 12
  if (length(idx_constant) > 0) {
    for (i in 1:length(idx_constant)) {
      idx_constant_real_start <- sum(list$lengths[1:idx_constant[i]]) - list$lengths[idx_constant][i] + 1
      idx_constant_real_end <- sum(list$lengths[1:idx_constant[i]])

      data$power[idx_constant_real_start:idx_constant_real_end] <- 0
    }
  }

  # Extracting only zeros data to interpolate with lookup table next

  data_na <- data %>%
    subset(power == 0) %>%
    mutate(power = NA)

  # Extracting clean data
  data_clean <- data %>%
    subset(power != 0)

  # Look up table
  lookup <- data_clean %>%
    group_by(time, dayofweek, load_condition) %>%
    summarise(mean = mean(power, na.rm = TRUE))

  # Obtain the values to be substitute
  data_interp <- merge(data_na, lookup, by=c("time", "dayofweek", "load_condition"), all.x = TRUE)
  data_interp <- data_interp[order(data_interp$timestamp),]

  # Concatenate dataframe
  data$power[data$timestamp %in% data_interp$timestamp] <- data_interp$mean

  # If NA remain perform linear interpolation
  data$power <- na.interp(data$power)

  # Return only timestamp and power
  return(data[, c("timestamp", "power")])


}


#' @description function that returns the features of the building selected

#' @return dataframe with the features for the building
#'
#' @importFrom dplyr mutate group_by summarise across select full_join
#' @importFrom purrr reduce

#' @noRd
get_features <- function() {

  # Import dataset
  data <- data_clean() %>%
    mutate(
      time = strftime(timestamp, format = "%H:%M:%S", tz = 'UTC'),
      date = as.Date(timestamp, format = '%Y-%m-%d'),
      dayofweek = wday(date, label = TRUE, locale="EN-us"),
      month = as.character(month(date, label = TRUE, abbr = TRUE, locale = "EN-us"))
    ) %>%
    mutate(load_condition = get_load_condition()$load_condition
    )

  # Calculation of the mean daily energy consumption
  mean_ec <- data %>%
    subset(load_condition %in% c("Winter workdays", "Summer workdays")) %>%
    dplyr::group_by(date, load_condition) %>%
    dplyr::summarise(energy = sum(power)) %>%
    dplyr::group_by(load_condition) %>%
    dplyr::summarise(mean_ec = mean(energy)) %>%
    mutate(across(where(is.numeric), round, 2))

  # Correlation analysis with temperature
  thermal_correlation <- get_thermal_correlation()

  temperature_correlation <- thermal_correlation %>%
    select(load_condition, result) %>%
    setNames(c("load_condition", "temperature_correlation"))

  # Shape factors
  night_period <- c("00", "01", "02", "03", "04", "05", "06", "07", "20", "21", "22", "23")
  night_period <- paste(night_period, ":00:00", sep = "")

  shape_factor <- data %>%
    subset(load_condition %in% c("Winter workdays", "Summer workdays")) %>%
    mutate(period = ifelse(time %in% night_period, "night", "day")) %>%
    dplyr::group_by(date, load_condition) %>%
    dplyr::summarise(F_ratio = sum(power[period == "night"]) / sum(power[period == "day"])) %>%
    dplyr::group_by(load_condition) %>%
    dplyr::summarise(shape_factor = mean(F_ratio)) %>%
    mutate(across(where(is.numeric), round, 2))

  # Merge all data frames together
  features_list <- list(mean_ec, shape_factor, temperature_correlation)

  features <- features_list %>%
    reduce(full_join, by='load_condition') %>%
    mutate(building_id = "Your building") %>%
    select(building_id, everything()) %>%
    mutate(across(where(is.numeric), round, 2))

  return(list(
    "features" = features,
    "thermal_correlation" = thermal_correlation))
}


#' @description Perform the thermal correlation analysis

#' @return the correlation analysis in a dataframe
#'
#' @importFrom dplyr group_by summarise ungroup arrange slice select
#' @importFrom imputeTS na_interpolation
#'
#' @noRd
get_thermal_correlation <- function() {


  data <- merge(
    x = weather(),
    y = data_clean() %>%
      mutate(date = as.Date(timestamp, format = '%Y-%m-%d')) %>%
      mutate(load_condition = get_load_condition()$load_condition),
    by = "timestamp",
    all = TRUE
  ) %>%
    mutate(airTemperature = imputeTS::na_interpolation(airTemperature, option = "linear"))

  # Eliminates duplicates
  data <- data[!duplicated(data),]

  # Calculate top 90 days for Winter and Summer
  sliced_Winter_Workday <- data %>%
    subset(load_condition == "Winter workdays") %>%
    dplyr::group_by(date, load_condition) %>%
    dplyr::summarise(energy = sum(power),
                     temperature = mean(airTemperature)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(temperature) %>%
    dplyr::slice(1:90)

  sliced_Summer_Workday <- data %>%
    subset(load_condition == "Summer workdays") %>%
    dplyr::group_by(date, load_condition) %>%
    dplyr::summarise(energy = sum(power),
                     temperature = mean(airTemperature)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(desc(temperature)) %>%
    dplyr::slice(1:90)

  # Performing correlation analysis
  correlation <- data %>%
    subset(load_condition %in% c("Winter workdays", "Summer workdays")) %>%
    dplyr::group_by(date, load_condition) %>%
    dplyr::summarise(energy = sum(power),
                     temperature = mean(airTemperature)) %>%
    dplyr::mutate(slice = ifelse((load_condition == "Winter workdays") &
                                   (date %in% sliced_Winter_Workday$date),
                                 "Yes",
                                 ifelse((load_condition == "Summer workdays") &
                                          (date %in% sliced_Summer_Workday$date),
                                        "Yes",
                                        "No"
                                 ))
    ) %>%
    subset(slice == "Yes") %>%
    select(-slice) %>%
    dplyr::group_by(load_condition) %>%
    dplyr::summarise(
      spearman = cor.test(energy, temperature, method = "spearman", exact = FALSE)$estimate,
      p_value = cor.test(energy, temperature, method = "spearman", exact = FALSE)$p.value
    )

  # Obtaining result of the analysis
  correlation <- correlation %>%
    mutate(result = ifelse(
      p_value > 0.05,
      "Non-thermal sensitive",
      ifelse(
        abs(spearman) < 0.4,
        "Non-thermal sensitive",
        "Potentially thermal sensitive"
      )
    ))

  correlation$slope <- "NaN"

  if (correlation$result[correlation$load_condition == "Winter workdays"] == "Potentially thermal sensitive") {

    # Normalize data
    normalized_data <- data %>%
      dplyr::group_by(date, load_condition) %>%
      dplyr::summarise(energy = sum(power),
                       temperature = mean(airTemperature)) %>%
      subset(load_condition %in% c("Winter workdays")) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(energy = energy / max(energy)) %>%
      dplyr::arrange(temperature) %>%
      dplyr::slice(1:90)

    model <- lm(energy ~ temperature, data = normalized_data)
    slope_Winter <- round(abs(model$coefficients[2]), 2)

    if (slope_Winter < 0.005) {
      correlation$result[correlation$load_condition == "Winter workdays"] <- "Non-thermal sensitive"
    } else {
      correlation$result[correlation$load_condition == "Winter workdays"] <- "Thermal sensitive"
      correlation$slope[correlation$load_condition == "Winter workdays"] <- slope_Winter
    }
  }


  if (correlation$result[correlation$load_condition == "Summer workdays"] == "Potentially thermal sensitive") {

    # Normalize data
    normalized_data <- data %>%
      dplyr::group_by(date, load_condition) %>%
      dplyr::summarise(energy = sum(power),
                       temperature = mean(airTemperature)) %>%
      subset(load_condition %in% c("Summer workdays")) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(energy = energy / max(energy)) %>%
      dplyr::arrange(temperature) %>%
      dplyr::slice(1:90)

    model <- lm(energy ~ temperature, data = normalized_data)
    slope_Summer <- round(abs(model$coefficients[2]), 2)

    if (slope_Summer < 0.005) {
      correlation$result[correlation$load_condition == "Summer workdays"] <- "Non-thermal sensitive"
    } else {
      correlation$result[correlation$load_condition == "Summer workdays"] <- "Thermal sensitive"
      correlation$slope[correlation$load_condition == "Summer workdays"] <- slope_Summer
    }

  }

  return(correlation)

}



#' @description Function that perform the EUI calculation

#' @return EUI: a dataframe with EUI for Winter and Summer season

#' @importFrom dplyr mutate group_by summarise filter ungroup pull
#' @importFrom lubridate wday month

#' @noRd
get_eui <- function() {

  # Obtaining data using two season in place of the usual load_condition
  data <- merge(
    x = weather(),
    y = data_clean(),
    by = "timestamp"
  ) %>%
    mutate(
      time = strftime(timestamp, format = "%H:%M:%S", tz = 'UTC'),
      date = as.Date(timestamp, format = '%Y-%m-%d'),
      dayofweek = wday(date, label = TRUE, locale="EN-us"),
      month = as.character(month(date, label = TRUE, abbr = TRUE, locale = "EN-us"))
    ) %>%
    mutate(load_condition = ifelse(
      date >= as.Date("2017-04-01") &
        date <= as.Date("2017-09-30"),
      "Summer",
      "Winter"
    ))

  GG_Winter <- data %>%
    subset(load_condition == "Winter") %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(temperature = mean(airTemperature)) %>%
    dplyr::filter(temperature < 18.3) %>%
    dplyr::summarise(GG = sum(18.3 - temperature)) %>%
    dplyr::pull(GG)

  GG_Summer <- data %>%
    subset(load_condition == "Summer") %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(temperature = mean(airTemperature)) %>%
    dplyr::filter(temperature > 18.3) %>%
    dplyr::summarise(GG = sum(temperature - 18.3))%>%
    dplyr::pull(GG)

  temperature_correlation <- get_thermal_correlation() %>%
    mutate(load_condition = gsub(" workdays", "", load_condition))

  data <- merge(x = data,
                y = temperature_correlation[, c("load_condition", "result")],
                by = "load_condition")

  # Obtaining EUI; if thermal sensitive use GG
  EUI <- data %>%
    dplyr::group_by(date, load_condition, result) %>%
    dplyr::summarise(energy = sum(power)) %>%
    dplyr::ungroup(date) %>%
    dplyr::group_by(load_condition, result) %>%
    dplyr::summarise(EUI = ifelse((unique(result) == "Thermal sensitive" &
      unique(load_condition) == "Winter"),
                                  sum(energy) / sqm() / GG_Winter,
                                  ifelse((unique(result) == "Thermal sensitive" &
                                    unique(load_condition) == "Summer"),
                                         sum(energy) / sqm() / GG_Summer,
                                         sum(energy) / sqm()
                                  )
    ))

  return(EUI)
}


#' @description function that returns the operational schedules from BreakoutDetection analysis using
#' Twitter's library BreakoutDetection

#' @return dataframe encoded with operational hour and the number of breakouts detected
#'
#' @importFrom dplyr mutate group_by summarise ungroup pull n
#' @import BreakoutDetection
#' @importFrom lubridate wday month
#'
#' @noRd
get_operational_schedules <- function() {

  data <- data_clean() %>%
    mutate(
      time = strftime(timestamp, format = "%H:%M:%S", tz = 'UTC'),
      date = as.Date(timestamp, format = '%Y-%m-%d'),
      dayofweek = wday(date, label = TRUE, locale="EN-us"),
      month = as.character(month(date, label = TRUE, abbr = TRUE, locale = "EN-us"))
    ) %>%
    mutate(load_condition = get_load_condition()$load_condition)

  # Running E-divise with medians algorithm
  location <- tryCatch(
    {
      breakout_detection <- breakout(data$power, min.size = 24*21, method = 'multi', beta=0.008, degree=1, plot=FALSE)
      breakout_detection$loc
    }, error = function(err){
      integer(0)
    }
  )

  # Extracting idnex of changepoints and tagging the dataframe
  breakout_indexes <- sort(append(location, c(0, 2160, 6552, 8760)))
  data$breakout <- NA
  breakout_length <- c(1:length(breakout_indexes))

  for (idx_breakout in breakout_length[1:(length(breakout_indexes)-1)]) {
    data$breakout[(breakout_indexes[idx_breakout] + 1):breakout_indexes[idx_breakout + 1]] <- idx_breakout

  }

  # Calculating load parameters
  load_parameters <- data %>%
    subset(load_condition %in% c("Winter workdays", "Summer workdays")) %>%
    dplyr::group_by(breakout, load_condition) %>%
    dplyr::summarise(baseload = quantile(power, 0.15),
                     peakload = quantile(power, 0.95)) %>%
    mutate(delta_load = peakload - baseload)

  # Obtaining on-off hours
  schedules <- data %>%
    merge(load_parameters[, c("breakout", "delta_load")], by = "breakout", all.x = TRUE) %>%
    dplyr::group_by(date, breakout, load_condition) %>%
    mutate(
      on_hour_threshold = if_else(
        load_condition %in% c("Winter workdays", "Summer workdays"),
        quantile(power, 0.15) + 0.25 * delta_load,
        NA_real_
      )
    ) %>%
    mutate(
      on_hour = case_when(
        load_condition %in% c("Winter weekends", "Summer weekends") ~ "weekend",
        power > on_hour_threshold ~ "on hour",
        TRUE ~ "off hour"
      )
    ) %>%
    ungroup() %>%
    dplyr::group_by(breakout, load_condition) %>%
    mutate(
      baseload = if_else(
        load_condition %in% c("Winter workdays", "Summer workdays"),
        quantile(power, 0.15),
        NA_real_
      )
    ) %>%
    ungroup()

  # Obtaining full on or full of days
  dates <- schedules %>%
    group_by(date, on_hour) %>%
    summarise(hours = n()) %>%
    subset(on_hour == "off hour") %>%
    subset(hours == 24) %>%
    pull(date)

  # Adjusting full on-off days
  schedules <- schedules %>%
    group_by(date, breakout) %>%
    mutate(on_hour = ifelse(date %in% dates & mean(power) >= baseload + 0.25*delta_load, "on hour",
                            ifelse(date %in% dates & mean(power) < baseload + 0.25*delta_load, "off hour", on_hour
                            ))) %>%
    ungroup()



  return(
    list(
      "n_breakout" = length(location),
      "schedule" = schedules[, c("timestamp", "power", "on_hour")])
  )

}


#' @description Function that performs the calculation of schedule KPIs, e.g. off impact and weekend impact

#' @param operational_schedules: the operational schedules encoding

#' @return EPI_sch: a dataframe with building schedule indicators

#' @importFrom dplyr mutate group_by summarise ungroup n distinct
#' @importFrom lubridate wday month

#' @noRd
get_epi_schedules <- function(operational_schedules) {

  data <- merge(
    x = as.data.frame(operational_schedules),
    y = as.data.frame(get_load_condition()),
    by = "timestamp"
  ) %>%
    mutate(date = as.Date(timestamp))

  operational_hour <- data %>%
    dplyr::group_by(date, load_condition) %>%
    dplyr::summarise(operational_hour = sum(on_hour == "on hour")) %>%
    dplyr::group_by(load_condition) %>%
    dplyr::summarise(operational_hour = round(get_mode(operational_hour)))

  off_impact <- merge(x = data,
                      y =  data %>%
                        distinct(date) %>%
                        mutate(day_index = seq_along(date)),
                      by = "date") %>%
    dplyr::group_by(day_index, load_condition) %>%
    dplyr::summarise(total_energy = sum(power),
                     on_energy = sum(power[on_hour == "on hour"]),
                     off_energy = sum(power[on_hour == "off hour"])) %>%
    ungroup() %>%
    mutate(off_impact = round((off_energy - on_energy) / on_energy * 100, 2)) %>%
    # mutate(off_impact = ifelse(off_impact > 100, 100, off_impact)) %>%
    dplyr::group_by(load_condition) %>%
    dplyr::summarise(off_impact = median(off_impact))

  weekend_impact <- merge(
    x = data_clean(),
    y = operational_schedules[, c("timestamp", "on_hour")],
    by = "timestamp",
    all = TRUE
  ) %>%
    mutate(
      time = strftime(timestamp, format = "%H:%M:%S", tz = 'UTC'),
      date = as.Date(timestamp, format = '%Y-%m-%d'),
      dayofweek = wday(date, label = TRUE, locale="EN-us"),
      month = as.character(month(date, label = TRUE, abbr = TRUE, locale = "EN-us"))
    ) %>%
    mutate(load_condition = get_load_condition()$load_condition) %>%
    mutate(n_week = lubridate::week(lubridate::ymd(date))) %>%
    mutate(season = ifelse(month %in% c("Jan", "Feb", "Mar", "Oct", "Nov", "Dec"), "Winter", "Summer")) %>%
    subset(!(load_condition %in% c("Holiday", "Non-working days"))) %>%
    dplyr::group_by(n_week, season) %>%
    mutate(day_in_week = n()) %>%
    ungroup() %>%
    subset(day_in_week == 168) %>%
    dplyr::group_by(n_week, season) %>%
    dplyr::summarise(weekly_energy = sum(power[dayofweek %in% c("Mon", "Tue", "Wed", "Thu", "Fri") & on_hour == "on hour"]),
                     weekend_energy = sum(power[dayofweek %in% c("Sun", "Sat")])) %>%
    ungroup()%>%
    filter(!is.na(weekly_energy) & weekly_energy != 0) %>%
    mutate(weekend_impact = round((weekend_energy - weekly_energy) / weekly_energy * 100, 2)) %>%
    dplyr::group_by(season) %>%
    summarise(weekend_impact = median(weekend_impact))

  return(
    list(
      "off_impact" = off_impact,
      "weekend_impact" = weekend_impact
    )
  )
}


#' @description Function that returns load volatility indicators

#' @param load_condition_string the load condition string (e.g. "Winter Workday")

#' @return df_neighbor_energy

#' @noRd

#' @importFrom dplyr mutate group_by summarise
#' @importFrom lubridate wday month
get_volatility <- function(load_condition_string) {

  data_daily <- data_clean() %>%
    mutate(
      time = strftime(timestamp, format = "%H:%M:%S", tz = 'UTC'),
      date = as.Date(timestamp, format = '%Y-%m-%d'),
      dayofweek = wday(date, label = TRUE, locale="EN-us"),
      month = as.character(month(date, label = TRUE, abbr = TRUE, locale = "EN-us"))
    ) %>%
    mutate(load_condition = get_load_condition()$load_condition) %>%
    subset(load_condition == load_condition_string) %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(energy = sum(power))


  # Calculate Distance Volatility
  #dist_matrix <- get_distance_matrix_power(load_condition_string)
  df_neighbor_distance <- get_neighbor_distance(load_condition_string)
  df_neighbor_distance$mean_dist <- rowMeans(df_neighbor_distance[, 2:length(df_neighbor_distance)], na.rm = TRUE)
  df_neighbor_distance$distance_volatility <- df_neighbor_distance$mean_dist/mean(data_daily$energy, na.rm = TRUE)*100

  # # Calculate Relative Energy
  # df_neighbor_energy <- getNeighborEnergy(file, load_condition_string)
  # df_neighbor_energy$mean_energy <- rowMeans(df_neighbor_energy[, 2:length(df_neighbor_energy)], na.rm = TRUE)
  # df_neighbor_energy$energy_volatility <- abs(data_daily$energy - df_neighbor_energy$mean_energy)/df_neighbor_energy$mean_energy*100

  return(df_neighbor_distance)

}


#' @description Function that returns the anomaly rate for a building in a load condition
#'
#' @param load_condition_string the load condition string (e.g. "Winter Workday")
#'
#' @return the anomaly rate in percentage
#'
#' @noRd
get_anomaly_rate <- function(load_condition_string) {



  data_daily <- data_clean() %>%
    mutate(
      time = strftime(timestamp, format = "%H:%M:%S", tz = 'UTC'),
      date = as.Date(timestamp, format = '%Y-%m-%d'),
      dayofweek = weekdays(date, abbreviate = TRUE),
      month = as.character(format(date, "%b"))
    ) %>%
    mutate(load_condition = get_load_condition()$load_condition) %>%
    subset(load_condition == load_condition_string) %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(energy = sum(power))

  # Calculate Anomaly Rate
  date_outliers <- get_anomaly_dates(load_condition_string)
  anomaly_rate <- length(date_outliers)/length(data_daily$date)*100

  # # Calculate Anomaly Energy
  # anomaly_energy <- data_daily %>%
  #   subset(as.character(date) %in% date_outliers) %>%
  #   pull(energy) %>%
  #   sum()/sum(data_daily$energy)*100

  return(round(anomaly_rate, 2))
}


#' @description Returns the percentage of frequent load profiles in the clusters
#'
#' @return dataframe with frequency indicators
#'
#' @noRd
#'
#' @importFrom dplyr group_by summarise ungroup
get_load_shape_frequency <- function() {

  data_cluster <- get_cluster_labels()

  metrics <- read.csv(file.path("data", paste0("metrics_", tolower(end_use()), ".csv")), header = TRUE)

  data_cluster <- merge(
    x = data_cluster,
    y = metrics[,c("cluster", "frequency", "load_condition")],
    by = c("cluster", "load_condition"),
    all.x = TRUE
  )

  load_shape_freq <- data_cluster %>%
    subset(!load_condition %in% c("Non-working days", "Holidays")) %>%
    group_by(date, load_condition) %>%
    summarise(frequency = unique(frequency)) %>%
    ungroup() %>%
    group_by(load_condition) %>%
    summarise(freq = round(sum(frequency == "Frequent load shape") / n() * 100, 2))

  return(load_shape_freq)

}

#' Calculate the percentile rank of a value within a numeric vector
#'
#' This function computes the percentile position of a value in a numeric vector
#' using the empirical cumulative distribution function (`ecdf`). The percentile
#' is returned as a value between 0 and 100.
#'
#' @param vec A numeric vector of values.
#' @param value A single numeric value whose percentile rank is to be computed.
#'
#' @return A numeric value indicating the percentile rank (from 0 to 100).
#' Returns NA if `vec` is empty or contains no finite values.
#'
#' @examples
#' percentile_rank(c(10, 20, 30, 40, 50), 35)
#' # Returns 60
#'
#' @noRd
#' @export
percentile_rank <- function(vec, value) {
  vec <- vec[is.finite(vec)]  # Remove NA, Inf, -Inf
  if (length(vec) == 0) return(NA_real_)
  F <- ecdf(vec)
  return(F(value) * 100)
}


# perform_analysis <- function(input) {
#   # Define reactive values to store the output of the functions
#   results <- reactiveValues(data_clean = NULL,
#                             thermal_correlation = NULL,
#                             features = NULL,
#                             EUI = NULL,
#                             operational_schedules = NULL,
#                             epi_schedules = NULL,
#                             volatility_Winter_workdays = NULL,
#                             volatility_Summer_workdays = NULL,
#                             volatility_Winter_weekends = NULL,
#                             volatility_Summer_weekends = NULL)
#
#   observeEvent(input, {
#     # Reset the progress indicator
#     withProgress(message = "Analyzing the building: ", value = 0, {
#
#       # PRE-PROCESSING
#       incProgress(0.2, detail = "Data pre-processing")
#       results$data_clean <- get_data_clean()
#
#       # PEER IDENTIFICATION
#       incProgress(0.2, detail = "Peer identification")
#       features <- get_features()
#
#       results$thermal_correlation <- features$thermal_correlation
#
#       results$features <- features$features
#
#       # KEY PERFORMANCE INDICATOR CALCULATION
#       incProgress(0.1, detail = "Energy Use Intensity calculation")
#       results$EUI <- get_eui()
#
#       incProgress(0.2, detail = "Operational schedules extraction")
#       results$operational_schedules <- get_operational_schedules()
#
#       results$epi_schedules <- get_epi_schedules(results$operational_schedules$schedule)
#
#       incProgress(0.1, detail = "Volatility of energy consumption calculation")
#       results$volatility_Winter_workdays <- get_volatility("Winter workdays")
#       results$volatility_Summer_workdays <- get_volatility("Summer workdays")
#       results$volatility_Winter_weekends <- get_volatility("Winter weekends")
#       results$volatility_Summer_weekends <- get_volatility("Summer weekends")
#
#
#     })
#   })
#
#   data_clean <<- reactive({
#     results$data_clean
#   })
#
#   features <<- reactive({
#     results$features
#   })
#
#   thermal_correlation <<- reactive({
#     results$thermal_correlation
#   })
#
#   EUI <<- reactive({
#     results$EUI
#   })
#
#   schedules <<- reactive({
#     results$operational_schedules$schedule
#   })
#
#   off_impact <<- reactive({
#     results$epi_schedules$off_impact
#   })
#
#   weekend_impact <<- reactive({
#     results$epi_schedules$weekend_impact
#   })
#
#   # volatility <<- reactive(
#   #   list(
#   #     "volatility_Winter_workdays" = results$volatility_Winter_Workday,
#   #     "volatility_Summer_workdays" = results$volatility_Summer_Workday,
#   #     "volatility_Winter_workdays" = results$volatility_Winter_Weekend,
#   #     "volatility_Summer_workdays" = results$volatility_Summer_Weekend
#   #   )
#   # )
# }
