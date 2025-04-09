


#' Title
#'
#' @param day
#'
#' @return
#' @export
#'
#' @examples
chk_season<-function(day){

  month=lubridate::month(day)
  mday=lubridate::mday(day)
  ifelse(
    month>=8,"fall",
    ifelse(
      (month==7|(month==6 & mday>15)),"summer",
      ifelse(
        month>=3,"spring",
        "Winter?")
    )
    )
}


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
    local_data <-
      readr::read_csv(count_file) |> tidyr::drop_na()

    sdate <- max(local_data$CountDate  )+1
    #
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




#' Function to calculate exponential moving avg on a time series (x)
#'
#' @param x
#' @param ratio
#'
#' @return
#'
#' @examples
ema <- function(x, ratio){
  c(stats::filter(x * ratio, 1 - ratio, "recursive", init=x[1]))
}




# mean daily flow (cfs) at The Dalles from USGS- Begins 1/1/1939
get_usgs_flow_data <- function(forecastdate){
  # Download the data
  dataRetrieval::readNWISdv(
    # Columbia River at The Dalles, Site 14105700
    siteNumbers = "14105700",
    startDate = "1939-1-1",
    endDate = forecastdate,
    #"00060 is flow, 00010 is temp
    parameterCd = c("00060", "00010"),
    statCd = "00003"
  ) %>%
    # pull relevant columns, rename
    dplyr::select(date=Date,
                  # Dollar-sign is regex for ends with. Don't want the _cd columns
                  cfs = testthat::matches("00060_00003$")

    ) %>%
    # create the other columns
    dplyr::mutate(Year=year(date)) %>%
    dplyr::transmute(
      Year = Year,
      flw_date=date,
      cfs = cfs
    )

}





#' get flow data from CRB DART
#'
#' @param forecastdate
#' @param start_year
#'
#' @return
#' @export
#'
#' @examples
get_CBR_DART_flow_temp_data<-function(forecastdate,start_year=1960){
  readr::read_csv(paste0("https://www.cbr.washington.edu/dart/cs/php/rpt/mg.php?sc=1&mgconfig=river&outputFormat=csvSingle",paste0(paste0("&year%5B%5D=",lubridate::year(forecastdate):start_year),collapse = ""),"&loc%5B%5D=BON&data%5B%5D=Outflow&data%5B%5D=Temp+%28Scroll+Case%29&startdate=1%2F1&enddate=12%2F1&avgyear=0&consolidate=1&grid=1&y1min=0&y1max=&y2min=&y2max=&size=large")) %>%
    dplyr::select(-c(unit,datatype )) |>
    tidyr::pivot_wider(names_from = parameter,values_from = value) |>
    dplyr::mutate(flw_date=lubridate::ymd(paste0( year,`mm-dd`,sep="-"))) %>%
    #make date column names match
    dplyr::mutate(cfs_CBR=(round(outflow ,0)*1000),#convert from kcfs to cfs and round to nearest throusandto match usgs data
                  temp_f_CBR=round((tempscr*.55556)+32),
                  Year=as.numeric(year)) %>%
    dplyr::select(Year,flw_date,cfs_CBR,temp_f_CBR)#select columns of interest
}




#' ean daily flow (kcfs) at Bonneville from US Army Corps of Engineers
#'
#' @param forecastdate
#' @param startdate
#' @param dam
#'
#' @return
#' @export
#'
#' @examples
get_usace_flow_temp_data <- function(forecastdate,startdate,dam="BON") {

  # Format dates as "MM/DD/YYYY 07:00", then URL encode
  start_str <- URLencode(paste0(format(startdate, "%m/%d/%Y"), " 07:00"))
  end_str <- URLencode(paste0(format(forecastdate, "%m/%d/%Y"), " 07:00"))

  url <- paste0(
    "https://www.nwd-wc.usace.army.mil/dd/common/web_service/webexec/ecsv?id=",
    dam,
    ".Flow-Out.Ave.~1Day.1Day.CBT-REV%3Aunits%3Dkcfs%7CBON-ScrollCase.Temp-Water.Inst.~1Day.0.CBT-RAW%3Aunits%3DF&headers=true&filename=&timezone=PST",
    "&lookback=6220w2d10h50m&lookforward=-11h50m",
    "&startdate=", start_str,
    "&enddate=", end_str
  )


  readr::read_csv(
    file=url,

    # Rename columns, skip 1st row so the old colnames aren't put into the df
    col_names = c("date", "kcfs","temp"),
    col_types = readr::cols(date = readr::col_datetime("%d-%b-%Y %H:%M"),
                            kcfs = readr::col_double(),
                            temp= readr::col_double()),
    skip = 1 ) %>%

    dplyr::transmute(Date = (date),
                     Year = lubridate::year(date),
                     day_nu = lubridate::yday(date),
                     `Flow (kcfs)`=kcfs,
                     `River temp. (F)`=temp)# %>%

}

#----------------------------------------------------------


get_flow_data<-function(forecastdate=NULL,start_month=4,flow_file="flow_temp_dat.csv"){



  if (file.exists(flow_file)) {
    local_data <-
      readr::read_csv(flow_file) |> tidyr::drop_na()

    sdate <- max(local_data$flw_date)+1
    #
  } else {
    local_data<-NULL
    sdate<-"1980-01-01"
  }

  if(is.null(forecastdate)){
    edate<-Sys.Date()-1
  } else{
    edate<-forecastdate
  }

  if(edate>=sdate){
    #obtain the data from multile web sources
    ##usgs
    # usgs_dat<- get_usgs_flow_data(forecastdate)
    ##CBR
    CBR_DAT_Dat<- try( get_CBR_DART_flow_temp_data(forecastdate,start_year = lubridate::year(sdate))|>
                         dplyr::filter(flw_date>=sdate))


    ##USACoE
    USACoE_dat<-get_usace_flow_temp_data(forecastdate,startdate=as.Date(sdate),dam="BON") %>%
      dplyr::rename(flw_date=  Date    ) %>% #make date column names match
      dplyr::mutate(cfs_USACoE=(round(`Flow (kcfs)`,0)*1000),
                    temp_f_ASACE=`River temp. (F)`) %>% #convert from kcfs to cfs and round to nearest throusandto match usgs data
      dplyr::select(Year,flw_date,cfs_USACoE,temp_f_ASACE)#select columns of interest

    #merge them where there is a column for each sources
    if(is.null(dim(CBR_DAT_Dat))) {
      all_dat<-USACoE_dat %>%
        dplyr::mutate(cfs_mean=cfs_USACoE,temp_mean=temp_f_ASACE)
    }else{
      all_dat<-USACoE_dat %>% dplyr::left_join(CBR_DAT_Dat) %>%
        dplyr::mutate(cfs_mean=dplyr::select(.,c(cfs_USACoE,cfs_CBR)) %>% rowMeans(na.rm=T),
                      temp_mean=dplyr::select(.,c(temp_f_ASACE,temp_f_CBR)) %>% rowMeans(na.rm=T)
        )
    }


    dat<-dplyr::bind_rows(local_data, all_dat |> dplyr::filter(flw_date >max(local_data$flw_date)) )
    readr::write_csv(dat,flow_file)
    return(dat)

  }
  else{
    return(local_data)
  }
}



flow_ema_fun<-function(data){
  dat %>%
    mutate(Year=lubridate::year(flw_date),
           month=lubridate::month(flw_date),
           md=lubridate::mday(flw_date)) %>%
    dplyr::filter(month>=start_month) |>
    dplyr::group_by(Year) %>% dplyr::arrange(flw_date) %>%

    # calculate exponential moving avg of flow using ema fn above
    dplyr::mutate(cfs_mean_ema=ema(cfs_mean, ratio=0.1)) %>%
    dplyr::ungroup()
}

get_flow_day<-function(dat,day){
  dat%>% dplyr::mutate(
    month=lubridate::month(flw_date),
    monthday=lubridate::mday(flw_date)
  ) |>
    dplyr::filter(month ==lubridate::month(day) & monthday == lubridate::mday(day))  %>%


    dplyr::mutate(logCflow_mean_ema = (log(cfs_mean_ema) - mean(log(cfs_mean_ema), na.rm=T))/sd(log(cfs_mean_ema), na.rm=T))
}
