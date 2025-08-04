## code to prepare `ladder_cnts_historic` dataset goes here

cur_date<-lubridate::today()
cur_year<-lubridate::year(cur_date)

ladder_cnts<-fpc_laddersplit(cur_date,
                             sdate=as.Date("1999-08-01"))

ladder_cnts_historic<-ladder_cnts |>
  dplyr::filter(lubridate::year(CountDate)<cur_year,
                dplyr::between(lubridate::month(CountDate),8,11))

usethis::use_data(ladder_cnts_historic, overwrite = TRUE)
