
#' render_page_fun
#'
#' @param output_file
#' @param output_dir
#' @param params
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
render_page_fun<-function(
    output_file = NULL,
    output_dir = NULL,
    params = NULL,

     # output_file = "Forcast report.docx",
    # output_dir = getwd(),
    ...
){




  template_path <- system.file("rmarkdown","Inseason-forecast.Rmd", package = "Inseasonfor")

  if (template_path == "") {
    stop("Template file not found. Ensure it exists in the package.")
  }

  rmarkdown::render(
    input = template_path,
    output_file = output_file,
    output_dir = output_dir,
    params = params,
    # output_file = file.path(output_dir, output_file),
    # params = list(
    #   # forecasts=param_forecasts,
    #   # mod_name=mod_name,
    #   # stocks=stocks_out,
    #   # forecast_year=forecast_year_out
    # )

    envir = new.env() # Avoids variable conflicts
  )

}



