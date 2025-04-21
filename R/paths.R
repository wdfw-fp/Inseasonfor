#' Get default local storage path for flow data
#'
#' @return Full path to the default flow data CSV
#' @export
get_default_flow_path <- function() {
  here::here("inst", "data-cache",  "flow_temp_dat.csv")
}

#' Get default local storage path for count data
#'
#' @return Full path to the default flow data CSV
#' @export
get_default_count_path <- function() {
  here::here("inst", "data-cache",  "fish_counts.csv")
}


#' Get default local storage path for ocean data
#'
#' @return Full path to the default flow data CSV
#' @export
get_default_ocean_path <- function() {
  here::here("inst", "data-cache", "PDO_NPGO.csv")
}

#' Get default local storage path for model result data
#'
#' @return Full path to the default model result CSV
#' @export
get_default_model_result_path <- function() {
  if (nzchar(Sys.getenv("GITHUB_ACTIONS"))) {
    # Use a writable top-level folder in GitHub Actions
    path <- file.path("data-cache", "forecast_results.csv")
  } else {
    # Use inst/ for local development
    path <- here::here("inst", "data-cache", "forecast_results.csv")
  }
  # Ensure the directory exists
  if (!dir.exists(dirname(path))) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  }
  return(path)
}
