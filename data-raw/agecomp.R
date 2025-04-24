## code to prepare `agecomp` dataset goes here

agecomp<-read.csv(here::here("data-raw","agecomp.csv"))
usethis::use_data(agecomp, overwrite = TRUE)
