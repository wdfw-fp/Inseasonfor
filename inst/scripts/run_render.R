
pkgload::load_all()
rmarkdown::render("inst/rmarkdown/Inseason-forecast.Rmd",     output_file = "index.html",
                  output_dir = "site",params=list(use_dev_version=FALSE))
