## code to prepare `SnakeWild` dataset goes here

snake_wild<-read.csv(here::here("data-raw","snake_wild.csv"))
usethis::use_data(snake_wild, overwrite = TRUE)
