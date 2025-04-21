


setwd("C:/Users/sorelmhs/Documents/R packages/Inseasonfor")

devtools::load_all(".")


sink("log.txt", append = TRUE, split = TRUE)
tryCatch({
  library(Inseasonfor)
  Inseasonfor::render_and_deploy(params = list(use_dev_version = TRUE))
}, error = function(e) {
  cat("Error: ", e$message, "\n")
})
sink()
