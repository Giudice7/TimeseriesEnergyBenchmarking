#' peer_identification
#'
#' @description Perform peer identification for a load condition in an end use

#' @param load_condition the load condition (e.g. Winter workdays, Summer workdays)
#'
#' @return Peer buildings identified
#'
#' @noRd
#'
#' @importFrom dplyr select mutate
#'
get_peers <- function(load_condition_string) {

  # Load features dataframe
  features_all <- read.csv2(
    file.path("data", paste0("features_", tolower(end_use()), ".csv"))
  ) %>%
    subset(!(building_id %in% gsub(".csv", "", list.files(path = file.path("data", "default_files", "electric_consumption"), pattern = tolower(end_use()))))) %>%
    subset(temperature_correlation == features()$temperature_correlation) %>%
    subset(load_condition == load_condition_string) %>%
    select(-c(load_condition, temperature_correlation))

  features_all$mean_ec = as.numeric(features_all$mean_ec)
  features_all$shape_factor = as.numeric(features_all$shape_factor)

  features_building <- features() %>%
    subset(load_condition == load_condition_string) %>%
    select(-c(load_condition, temperature_correlation))

  mean_ec <- features_building$mean_ec[1]
  shape_factor <- features_building$shape_factor[1]

  max <- apply(features_all[2:(length(features_all) - 1)], 2, max)
  normalized_features <- features_all

  normalized_features[2:length(normalized_features)] <-
    mapply(min_max_normalization,
           normalized_features[2:length(normalized_features)])

  normalized_mean_ec <- min_max_normalization_test(features_all$mean_ec, mean_ec)
  normalized_shape_factor <- min_max_normalization_test(features_all$shape_factor, shape_factor)

  normalized_features_building <- data.frame(
    building_id = "Your_building",
    mean_ec = normalized_mean_ec,
    shape_factor = normalized_shape_factor
  )

  weight_mean_ec <- 0.7
  weight_shape_factor <- 0.3
  weights <- c(weight_mean_ec, weight_shape_factor)

  dist_vector <- c()
  for (building in features_all$building_id) {

    dist_vector <- append(
      dist_vector,
      euclidean_distance(normalized_features[features_all$building_id == building, 2:ncol(normalized_features)],
                         normalized_features_building[1, 2: ncol(normalized_features_building)],
                         weights))
  }

  dist_vector_norm <- min_max_normalization(c(dist_vector, 0))[1:length(dist_vector)]

  features_all$distance <- dist_vector

  n_neighbors <- 30
  peers <- features_all[order(features_all$distance), ][1:n_neighbors, ]

  peers <- peers %>%
    mutate(across(where(is.numeric), ~ round(., 2)))

  rownames(peers) <- c(1:length(peers$building_id))

  return(peers)
}


#' @description Return the KPI values of the peer buildings

#' @param load_condition the load condition (e.g. Winter workdays, Summer workdays)
#' @param peers the list of peers of the building for the load condition
#'
#' @return Peer buildings identified
#'
#' @noRd
#'
#' @importFrom dplyr select mutate
get_KPI_peers <- function(load_condition_string, peers) {

  KPI_season <- read.csv2(
    file.path("data", paste0("KPI_season_", tolower(end_use()), ".csv"))) %>%
    subset(building_id %in% peers$building_id)

  KPI_load_condition <- read.csv2(
    file.path("data", paste0("KPI_load_condition_", tolower(end_use()), ".csv"))
  ) %>%
    subset(building_id %in% peers$building_id)


  return(
    list(
      "KPI_season" = KPI_season,
      "KPI_load_condition" = KPI_load_condition
    )
  )
}
