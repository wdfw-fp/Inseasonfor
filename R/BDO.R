

BDO_dat<-function(){


  bdo_con <- DBI::dbConnect(
    RPostgres::Postgres(),
    dbname = Sys.getenv("dbname"),
    host = Sys.getenv("host"),
    port = as.integer(Sys.getenv("port")),
    user = Sys.getenv("user"),
    password = Sys.getenv("password")
  )

  bdo_dat <-  DBI::dbGetQuery(bdo_con, 'SELECT * FROM "bdo"."raw_data"')  |>
    dplyr::filter(!is.na(fish_count)) |>
    dplyr::group_by(count_date,ladder,stock,stage) |>
    dplyr::summarise(fish_count=sum(fish_count)) |>
    dplyr::ungroup() |>
    tidyr::complete(count_date,
                    tidyr::nesting(stock, stage), fill = list(fish_count = 0)) |>
    dplyr::mutate(fish_count=ifelse(fish_count=="NaN",NA_integer_,as.integer(fish_count)),
           fish_count=ifelse(fish_count<0,0,fish_count),
           is_jack=stage=="Jack",is_jack) |>
    dplyr::group_by(count_date,ladder,is_jack) |>
    dplyr::mutate(stage_prop=proportions(fish_count)) |>
    dplyr::ungroup() |>
    dplyr::rename(CountDate=count_date,Ladder=ladder) |>
    dplyr::select(-fish_count,-is_jack) |>
    tidyr::pivot_wider(names_from = c(stock,stage),values_from = stage_prop)

  bdo_dat
}

#' Get laddersplit count report from FPC (from Ben's fpcDamCounts)
#'
#' @param pred_date
#' @param url
#'
#' @return
#' @export
#'
#' @examples
fpc_laddersplit<-function (pred_date=NULL,
                           url = "https://www.fpc.org/adults/R_adultcoequeries_laddersplitreport_results_get"
)
{

  if(is.null(pred_date)){
    edate<-Sys.Date()+2
  } else{
    edate<-pred_date
  }

  suppressWarnings(readr::read_csv(glue::glue("{url}salmon.php?sdate=1999-01-01&edate={edate}"),
                                   col_types = readr::cols(CountDate = readr::col_date(format = "%m/%d/%Y")))) |>
    dplyr::filter(!is.na(CountDate)) |>
    dplyr::filter(Location =="Bonneville") |>
    dplyr::select(CountDate,Ladder,AdultChinook,JackChinook)
}



tule_bright_split<-function(pred_date){
  BDO_dat_obs<-BDO_dat()
  ladder_cnts<-fpc_laddersplit(pred_date)

ladder_cnts |>
    dplyr::mutate(
    year=lubridate::year(.data$CountDate),
  yday=lubridate::yday(.data$CountDate),
  month=lubridate::month(.data$CountDate),
  mday=lubridate::mday(.data$CountDate)
    ) |>
    dplyr::filter(dplyr::between(month,8,11)) |>
    dplyr::left_join(BDO_dat_obs,relationship="one-to-one") |>
  dplyr::group_by(year,Ladder) |>
  dplyr::arrange(CountDate) |>
    dplyr::mutate(
      dplyr::across(c(AdultChinook,JackChinook),\(x)tidyr::replace_na(x,0)),
   #interpolate missing days
         dplyr::across(Bright_Adult :Tule_Stubby ,\(x)zoo::na.approx(x,na.rm=F,rule=2)),
   #multiply proportions by total counts
      dplyr::across(c(Bright_Adult,Tule_Adult,Tule_Stubby) ,\(x)x*AdultChinook),
   dplyr::across(c(Bright_Jack,Tule_Jack) ,\(x)x*JackChinook)
      ) |>
  dplyr::group_by(CountDate,year,month,mday) |>
  dplyr::summarise(dplyr::across(Bright_Adult :Tule_Stubby ,sum)) |>
  dplyr::mutate(season="fall")


}
