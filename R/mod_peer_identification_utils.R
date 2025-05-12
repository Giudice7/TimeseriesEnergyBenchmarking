#' @description Performs Min-max normalization
#'
#' @param x vector to be normalized
#'
#' @return Vector normalized
#'
#' @noRd
min_max_normalization <- function(x, na.rm = TRUE) {

  return((x- min(x)) /(max(x)-min(x)))

}

#' @description Performs Min-max normalization of a test vector
#'
#' @param x vector to normalize with respect to
#' @param y vector to be normalized
#'
#' @return Vector normalized
#'
#' @noRd
min_max_normalization_test <- function(x, y, na.rm = TRUE) {

  return((y- min(x)) /(max(x)-min(x)))

}


#' @description Performs the euclidean distance between two points
#'
#' @param x point
#' @param y point
#' @param weights weights to apply to the points
#'
#' @return distance among the points
#'
#' @noRd
euclidean_distance <- function(x, y, weights) {

  dist = sqrt(sum((x - y)^2*weights))

  return(dist)
}
