

#' Calculates predictions based on 5 and 10 year average run timing.
#'
#' @param dat
#'
#' @return
#' @export
#'
#' @examples
Bon_ch_fun<-function(pred_date=NULL,
                     dat){


  current_season<-chk_season(pred_date)

  #
  #   if(lubridate::month(pred_date)<6|lubridate::month(pred_date)==6&lubridate::day(pred_date)<=15){
  #   "spring"
  # }else{
  #   if(lubridate::month(pred_date)<8){
  #     "summer"
  #   }else{
  #     "fall"
  #   }
  # }
  season_dat<-dat  |>
    dplyr::filter(.data$season==current_season) |>
    dplyr::ungroup()


  dat2<- season_dat|>
    dplyr::right_join(season_dat |> dplyr::select(year,month,mday) |> tidyr::expand(year,tidyr::nesting(month,mday))) |>
    dplyr::mutate( CountDate=as.Date(ifelse(is.na(CountDate),as.Date(paste(year,month,mday,sep="-")),CountDate))) |>
    dplyr::mutate(AdultChinook=tidyr::replace_na(.data$AdultChinook,0))|>

    # dplyr::left_join(tidyr::expand(dat,.data$year,tidyr::nesting("month","mday")),dat) |>
    dplyr::group_by(.data$year) |>
    dplyr::arrange(.data$year,.data$month,.data$mday) |>
    dplyr::mutate(
      total=cumsum(.data$AdultChinook),
      total=ifelse(.data$CountDate>pred_date,NA,total),
      prop=.data$total/sum(.data$AdultChinook)
    ) |>
    dplyr::group_by(.data$month,.data$mday) |>
    dplyr::mutate(Ave_5yr=dplyr::lag(zoo::rollapply(.data$prop,width=5,FUN=\(x)mean(x,na.rm=T),align="right",fill=NA_real_),1),
                  Ave_10yr=dplyr::lag(zoo::rollapply(.data$prop,width=10,FUN=\(x)mean(x,na.rm=T),align="right",fill=NA_real_),1),
                  Ave_10yr_daily_cnt=dplyr::lag(zoo::rollapply(.data$AdultChinook,width=10,FUN=\(x)mean(x,na.rm=T),align="right",fill=NA_real_),1)
                  ) |>
    dplyr::ungroup() |>
    dplyr::filter(year>=lubridate::year(pred_date)-30)

  dat3<-dat2 |>
    dplyr::group_by(year) |>
    dplyr::mutate(
      #lags
      !!!purrr::set_names(
        purrr::list_flatten(
          purrr::map(c("Ave_5yr","Ave_10yr"), function(col) {
            purrr::map(1:10, function(k) {
              dplyr::expr(dplyr::lag(!!dplyr::sym(col), !!k))
            })
          })
        ),
        purrr::flatten_chr(purrr::map(c("Ave_5yr","Ave_10yr"), ~ paste0(.x,"_", 1:10,"_days_late")))
      ),
      #leads
      !!!purrr::set_names(
        purrr::list_flatten(
          purrr::map(c("Ave_5yr","Ave_10yr"), function(col) {
            purrr::map(1:10, function(k) {
              dplyr::expr(dplyr::lead(!!dplyr::sym(col), !!k))
            })
          })
        ),
        purrr::flatten_chr(purrr::map(c("Ave_5yr","Ave_10yr"), ~ paste0(.x,"_", 1:10,"_days_early")))
      )
    ) |>
    dplyr::ungroup()


  dat3 |> dplyr::mutate(dplyr::across(Ave_5yr:dplyr::last_col(),\(x){.data$total/x}, .names = "pred_{.col}")) |>
    dplyr::group_by(year) |>
    dplyr::mutate(obs_tot=tail(.data$total,1),
                  er_5= obs_tot-pred_Ave_5yr,
                  er_10 =obs_tot-pred_Ave_10yr,
                  log_er_5= log(obs_tot/pred_Ave_5yr),
                  log_er_10 =log(obs_tot/pred_Ave_10yr),
                  APE_5=abs(er_5)/obs_tot,
                  APE_10=abs(er_10)/obs_tot
    ) |>

    dplyr::arrange(.data$year,.data$month,.data$mday) |>
    dplyr::group_by(month ,mday) |>
    dplyr::mutate(MAPE_5yr=dplyr::lag(zoo::rollmean(.data$APE_5,k=15,align="right",fill=NA_real_),1),
                  MAPE_10yr=dplyr::lag(zoo::rollmean(.data$APE_10,k=15,align="right",fill=NA_real_),1),
                  log_sd_5yr=dplyr::lag(zoo::rollapply(.data$log_er_5,width=15,FUN=\(x){sqrt(mean(x^2))},align="right",fill=NA_real_),1),
                  ,
                  log_sd_10yr=dplyr::lag(zoo::rollapply(.data$log_er_10,width=15,FUN=\(x){sqrt(mean(x^2))},align="right",fill=NA_real_),1),
                  logit_prop_sd_5yr=dplyr::lag(zoo::rollapply(qlogis(.data$Ave_5yr),width=15,FUN=sd,align="right",fill=NA_real_),1),
                  ,
                  logit_prop_sd_10yr=dplyr::lag(zoo::rollapply(qlogis(.data$Ave_5yr),width=15,FUN=sd,align="right",fill=NA_real_),1)

                  ) |>
  dplyr::arrange(desc(CountDate))


}



