
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

  dat2<-dat |>
    dplyr::mutate(year=lubridate::year(CountDate),
                  yday=lubridate::yday(CountDate),
                  month=lubridate::month(CountDate),mday=lubridate::mday(CountDate),
                  season=dplyr::case_when(
                    month>=8~"fall",
                    (month==7|(month==6&mday>15)),
                  )
                  ) |>
    dplyr::group_by(year) |>
    dplyr::arrange(yday) |>
    dplyr::mutate(
      total=cumsum(AdultChinook),
      prop=total/sum(AdultChinook)
    )


}
