
#' bon_dat_fun
#'
#' Reads window count data from fish passage center.
#'
#' @param pred_date  the last date of counts for which predictions are to be based
#' @param count_file the file where fish counts are stored
#' @param url the fishpassage center url where the data are pulled from.
#'
#' @return tibble with daily counts, proportions of runs complete, and forecasts
#' @export
#'
#' @examples
#'
#' bon_dat_fun()
#'
bon_dat_fun<-function(pred_date=NULL,
                 count_file="fish_counts.csv",
                 url = "https://www.fpc.org/adults/R_coeadultcount_runsum"){




  #fetch data

  if (file.exists(count_file)) {
    local_data <- readr::read_csv(count_file) |> tidyr::drop_na()

    sdate <- max(local_data$date)+1

  } else {
    local_data<-NULL
    sdate<-"1980-01-01"
  }

  if(is.null(pred_date)){
    edate<-Sys.Date()-1
  } else{
    edate<-pred_date
  }

  if(edate>=sdate){
    new_dat<-readr::read_csv(glue::glue("{url}_salmon_getresults.php?dam=BON&sdate={sdate}&edate={edate}"),
                  col_types=readr::cols(CountDate=readr::col_date(format="%m/%d/%Y"))) |>
      dplyr::select(CountDate,AdultChinook,JackChinook) |>
      dplyr::mutate(year=lubridate::year(.data$CountDate),
                    yday=lubridate::yday(.data$CountDate),
                    month=lubridate::month(.data$CountDate),
                    mday=lubridate::mday(.data$CountDate),
                    season=dplyr::case_when(
                      .data$month>=8~"fall",
                      (.data$month==7|(.data$month==6&.data$mday>15))~"summer",
                      .data$month>=3~"spring",
                      TRUE~"Winter?"
                    ))

    dat<-dplyr::bind_rows(local_data, new_dat)
    readr::write_csv(dat,count_file)
    return(dat)
  }
  else{
    return(local_data)
  }
}






#' Calculates predictions based on 5 and 10 year average run timing.
#'
#' @param dat
#'
#' @return
#' @export
#'
#' @examples
Bon_ch_fun<-function(pred_date=NULL){

  current_season<-if(lubridate::month(pred_date)<6|lubridate::month(pred_date)==6&lubridate::day(pred_date)<=15){
    "spring"
  }else{
    if(lubridate::month(pred_date)<8){
      "summer"
    }else{
      "fall"
    }
  }

  dat2<-dat  |>
    dplyr::filter(.data$season==current_season) |>

    # dplyr::left_join(tidyr::expand(dat,.data$year,tidyr::nesting("month","mday")),dat) |>
    dplyr::group_by(.data$year) |>
    dplyr::arrange(.data$year,.data$month,.data$mday) |>
    dplyr::mutate(
      total=cumsum(.data$AdultChinook),
      prop=.data$total/sum(.data$AdultChinook)
    ) |>
    dplyr::group_by(.data$month,.data$mday) |>
    dplyr::mutate(Ave_5yr=dplyr::lag(zoo::rollmean(.data$prop,k=5,align="right",fill=NA_real_),1),
                  Ave_10yr=dplyr::lag(zoo::rollmean(.data$prop,k=10,align="right",fill=NA_real_),1)) |>
    dplyr::ungroup() |>
    filter(year>=lubridate::year(pred_date)-16)

  dat3<-dat2 |>
    group_by(year) |>
    mutate(
      #lags
      !!!set_names(
        list_flatten(
          map(c("Ave_5yr","Ave_10yr"), function(col) {
            map(1:10, function(k) {
              expr(lag(!!sym(col), !!k))
            })
          })
        ),
        flatten_chr(map(c("Ave_5yr","Ave_10yr"), ~ paste0(.x,"_", 1:10,"_days_late")))
      ),
      #leads
      !!!set_names(
        list_flatten(
          map(c("Ave_5yr","Ave_10yr"), function(col) {
            map(1:10, function(k) {
              expr(lead(!!sym(col), !!k))
            })
          })
        ),
        flatten_chr(map(c("Ave_5yr","Ave_10yr"), ~ paste0(.x,"_", 1:10,"_days_early")))
      )
    ) |>
    ungroup()


  dat3 |> mutate(across(Ave_5yr:dplyr::last_col(),\(x){.data$total/x}, .names = "pred_{.col}"))


}
