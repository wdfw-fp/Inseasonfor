#' Get default local storage path for flow data
#'
#' @return Full path to the default flow data CSV
#' @export
get_default_flow_path <- function() {
  file.path(tools::R_user_dir("Inseasonfor", which = "data"), "flow_temp_dat.csv")
}

#' Get default local storage path for count data
#'
#' @return Full path to the default flow data CSV
#' @export
get_default_count_path <- function() {
  file.path(tools::R_user_dir("Inseasonfor", which = "data"), "fish_counts.csv")
}


#' Get default local storage path for ocean data
#'
#' @return Full path to the default flow data CSV
#' @export
get_default_ocean_path <- function() {
  file.path(tools::R_user_dir("Inseasonfor", which = "data"), "PDO_NPGO.csv")
}

#' Get default local storage path for model result data
#'
#' @return Full path to the default flow data CSV
#' @export
get_default_model_result_path <- function() {
  file.path(tools::R_user_dir("Inseasonfor", which = "data"), "forecast_results.csv")
}
