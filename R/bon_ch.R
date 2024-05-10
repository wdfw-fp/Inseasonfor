
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
    dplyr::select("CountDate","AdultChinook")

  dat<-dat |>
    dplyr::mutate(year=lubridate::year(.data$CountDate),
                  yday=lubridate::yday(.data$CountDate),
                  month=lubridate::month(.data$CountDate),
                  mday=lubridate::mday(.data$CountDate),
                  season=dplyr::case_when(
                    .data$month>=8~"fall",
                    (.data$month==7|(.data$month==6&.data$mday>15))~"summer",
                    .data$month>=3~"spring",
                    TRUE~"Winter?"
                  )) |>
    dplyr::filter(.data$season==current_season)

    dplyr::left_join(tidyr::expand(dat,.data$year,tidyr::nesting("month","mday")),dat) |>
    dplyr::group_by(.data$year) |>
    dplyr::arrange(.data$year,.data$month,.data$mday) |>
    dplyr::mutate(
      total=cumsum(.data$AdultChinook),
      prop=.data$total/sum(.data$AdultChinook)
    ) |>
    dplyr::group_by(.data$month,.data$mday) |>
    dplyr::mutate(Ave_5yr=dplyr::lag(zoo::rollmean(.data$prop,k=5,align="right",fill=NA_real_),1),
                  Ave_10yr=dplyr::lag(zoo::rollmean(.data$prop,k=10,align="right",fill=NA_real_),1),
                  Pred_5=.data$total/.data$Ave_5yr,
                  Pred_10=.data$total/.data$Ave_10yr) |>
    dplyr::ungroup()
}

