# test_csv_write.R

test_csv_write <- function() {
  # Define the output directory and file
  output_dir <- "inst/data-cache"
  output_file <- file.path(output_dir, "test_forecast_results.csv")

  # Create the directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    message("Created directory: ", output_dir)
  }

  # Check if the directory is writable
  if (file.access(output_dir, 2) != 0) {
    stop("Directory is not writable: ", output_dir)
  }

  # Create a dummy data frame
  dummy_data <- data.frame(
    Date = Sys.Date() + 0:4,
    Forecast = c(100, 200, 150, 250, 300)
  )

  # Write the dummy data to a CSV file
  tryCatch({
    vroom::vroom_write(dummy_data, output_file)
    message("Successfully wrote file: ", output_file)
  }, error = function(e) {
    stop("Failed to write file: ", e$message)
  })

  return(TRUE)
}

# Run the function (uncomment to test locally)
# test_csv_write()
