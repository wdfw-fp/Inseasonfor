
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
    output_file = "index.html",
    output_dir = "site",
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
  # return the path of the rendered HTML file
  return(file.path(output_dir, output_file))
}



#' Deploy rendered report to GitHub Pages
#'
#' Automatically switches to `gh-pages` branch, copies the HTML report from the
#' `site/` folder to the root directory, commits the update, and pushes to GitHub.
#'
#' @param site_dir Directory containing the rendered `index.html` (default: `"site"`).
#' @param commit_message A message to use for the Git commit.
#'
#' @return Invisibly returns the path to the deployed `index.html`.
#' @export
deploy_site <- function(site_dir = "site", commit_message = "Update GitHub Pages site") {
  html_file <- "index.html"
  rendered_path <- file.path(site_dir, html_file)
  deploy_path <- file.path(".", html_file)

  # Check if file exists
  if (!file.exists(rendered_path)) {
    stop("Rendered file not found at: ", rendered_path)
  }

  # Check current branch
  current_branch <- system("git branch --show-current", intern = TRUE)

  if (current_branch != "gh-pages") {
    message("Switching from branch '", current_branch, "' to 'gh-pages'...")
    switch_status <- system("git checkout gh-pages")
    if (switch_status != 0) {
      stop("❌ Failed to switch to 'gh-pages' branch. Please check your Git setup.")
    }
  }

  # Copy file
  message("Copying ", rendered_path, " to root directory...")
  success <- file.copy(rendered_path, deploy_path, overwrite = TRUE)
  if (!success) stop("Failed to copy HTML file to root.")

  # Commit and push
  message("Committing and pushing to gh-pages branch...")
  system("git add index.html")
  system(paste0("git commit -m \"", commit_message, "\""))
  system("git push origin gh-pages")

  message("✅ Deployment complete.")
  invisible(deploy_path)
}


#' Render and deploy your site to GitHub Pages
#'
#' This convenience function runs `render_page_fun()` and then `deploy_site()`.
#'
#' @param ... Additional arguments passed to `render_page_fun()` (e.g., `params`).
#' @param commit_message A message to use for the Git commit.
#'
#' @return Invisibly returns the path to the deployed `index.html`.
#' @export
render_and_deploy <- function(..., commit_message = "Update GitHub Pages site") {
  render_page_fun(output_file = "index.html", output_dir = "site", ...)
  deploy_site(commit_message = commit_message)
}

