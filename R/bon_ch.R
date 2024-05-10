
#' bon_ch
#'
#' Reads window count data from fish passage center. Calculates predictions based on 5 and 10 year average run timing.
#'
#' @return tibble with daily counts, proportions of runs complete, and forecasts
#' @export
#'
#' @examples
#'
#' bon_ch()
#'
bon_ch<-function(){

  today<-Sys.Date()
  current_season<-if(lubridate::month(today)<6|lubridate::month(today)==6&lubridate::day(today)<=15){
    "spring"
  }else{
    if(lubridate::month(today)<8){
      "summer"
    }else{
      "fall"
    }
  }


  dat<-fpcDamCounts::fpc_runsum(dam="BON",rpt="salmon",sdate="1980-01-01",edate="2050-12-31")|>
    dplyr::select(CountDate,AdultChinook)

  dat |>
    dplyr::mutate(year=lubridate::year(CountDate),
                  yday=lubridate::yday(CountDate),
                  month=lubridate::month(CountDate),mday=lubridate::mday(CountDate),
                  season=dplyr::case_when(
                    month>=8~"fall",
                    (month==7|(month==6&mday>15))~"summer",
                    month>=3~"spring",
                    TRUE~"Winter?"
                  )) |>
    dplyr::filter(season==current_season) |>
    dplyr::group_by(year) |>
    dplyr::arrange(CountDate) |>
    dplyr::mutate(
      total=cumsum(AdultChinook),
      prop=total/sum(AdultChinook)
    ) |>
    dplyr::group_by(month,mday) |>
    dplyr::mutate(Ave_5yr=dplyr::lag(zoo::rollmean(prop,k=5,align="right",fill=NA_real_),1),
                  Ave_10yr=dplyr::lag(zoo::rollmean(prop,k=10,align="right",fill=NA_real_),1),
                  Pred_5=total/Ave_5yr,
                  Pred_10=total/Ave_10yr)
}
