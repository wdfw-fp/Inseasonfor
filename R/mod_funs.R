

#' fit three models and save results to csv.
#'
#' @param forecastdate
#' @param dat
#' @param forecast
#' @param forecast_log_sd
#' @param joint_like_data_file
#'
#' @return
#' @export
#'
#' @examples
mod_results<-function(forecastdate,
                      Count_dat = Bon_cnts,
                      River_dat = flow_temp_dat,
                      Ocean_dat = ocean_cov,
                      forecast=122500,
                      forecast_log_sd=0.28,
                      mod_result_file=NULL){


  # if (is.null(mod_result_file)) {
  #   mod_result_file <- get_default_model_result_path()
  # }

file_path<-here::here("inst", "data-cache", "forecast_results.csv")

  if (file.exists(file_path)) {
    local_data <-
      readr::read_csv(file_path)

    sdate <- max(local_data$date)+1
    #
  } else {
    local_data<-NULL
    sdate<-  as.Date(paste0(lubridate::year(forecastdate),"-04-05"))
  }


  if(sdate<=forecastdate){
    new_dat<-data.frame()
    for (i in seq.Date(from=sdate,to=forecastdate,by=1)){

      forecast_year<-lubridate::year(as.Date(i))
      forecast_month<-lubridate::month(as.Date(i))
      forecast_mday<-lubridate::mday(as.Date(i))
      forecast_season<-chk_season(as.Date(i))

      fish_river_ocean_i<-cnts_for_mod_fun(as.Date(i),Bon_cnts=Count_dat) |>
        dplyr::left_join(River_dat |>
                           dplyr::filter(month==forecast_month,
                                         md==forecast_mday) |>
                           dplyr::select(year=Year,cfs_mean_ema,temp_mean_ema),
        ) |>
        dplyr::left_join(
          Ocean_dat
        ) |>
        dplyr::mutate(
          cnt_by_flow= cfs_mean_ema*log_cum_cnt,
          cnt_by_temp= temp_mean_ema *log_cum_cnt,
        )


      #ARIMA
      ARIMA_for<-do_salmonForecasting_fun(fish_river_ocean_i,cov_vec=c("log_cum_cnt","cnt_by_flow"))
      #
      #   #DLM
        DLM_for<-do_sibregresr_fun(fish_river_ocean_i,cov_vec=c("log_cum_cnt","cnt_by_flow"))
      #Joint_like
      joint_likelihood_fit<-fit_joint_likelihood(fish_river_ocean_i,forecast = forecast,forecast_log_sd = forecast_log_sd)

      #combined
      comb_for<-   dplyr::bind_rows(
        DLM_for,
      ARIMA_for,
      joint_likelihood_fit
      ) |>
        dplyr::mutate(
          date=as.Date(i),
          dplyr::across(dplyr::where(is.numeric),\(x)round(x,3))
        )

      new_dat<-
        dplyr::bind_rows(new_dat,
                         comb_for
                    )


  }


    tryCatch({
      readr::write_csv(data, file_path)
    }, error = function(e) {
      message("Error writing file: ", e)
      dir.create("data-cache", showWarnings = FALSE)
      Sys.chmod("data-cache", mode = "0777", use_umask = TRUE)
      stop(e) # Rethrow the error after handling
    })

    return(dat)
}else{
  return(local_data)
}

  }















#' make forecast using sibregresr
#'
#' @param fish_river_ocean data
#'
#' @return
#' @export
#'
#' @examples
do_sibregresr_fun<-function(data,cov_vec=c("log_cum_cnt","cnt_by_flow")){#,"temp_mean_ema","lag2_Spr_NPGO","lag2_Spr_PDO"

  ## data for sibregresr package
  sib_reg_dat<-data |> dplyr::mutate(Stock=paste("Bon","Chk",sep="_")) |> dplyr::select(Stock,ReturnYear=year,Age3=tot_jack ,Age4=tot_adult) |>
    dplyr::filter(ReturnYear<max(ReturnYear))


  sib_reg_cov<-data |> dplyr::select(ReturnYear=year,all_of(cov_vec))



  pen_dlm_forecast_cov<-sibregresr::forecast_fun(
    df = sib_reg_dat,
    include = c("PenDlm"),
    transformation = log,
    inverse_transformation = exp,
    scale_x = TRUE,
    scale_y = TRUE,
    perf_yrs = 15,
    wt_yrs = 1,
    covariates = sib_reg_cov,
    penDLM_formula =formula(paste(c("y ~ x" , cov_vec),collapse=" + "))
  )

  sibregresr::make_table(pen_dlm_forecast_cov$forecasts,"PenDlm")


  forecast<-pen_dlm_forecast_cov$forecasts |> dplyr::filter(Age=="4",ReturnYear==max(ReturnYear),model_name=="PenDlm") |>
    dplyr::ungroup() |>
    dplyr::mutate(`Lo 95`=exp(qnorm(.025,log(Pred),log_sd)),
                  `Lo 50`=exp(qnorm(.25,log(Pred),log_sd)),
                  `Hi 50`=exp(qnorm(.75,log(Pred),log_sd)),
                  `Hi 95`=exp(qnorm(.975,log(Pred),log_sd))) |>
    dplyr::select(model=model_name,
                  predicted_abundance=Pred,
                  MAPE,RMSE,
                  `Lo 95`:`Hi 95`)







  info<-pen_dlm_forecast_cov$fits |> dplyr::filter(Age==4,n_years==-1)



  coefs<-c(unlist(tail(info$MLE[[1]]$result$obj$report()$coefs,1))) |> `names<-`(c("intercept","log_lag_jack",cov_vec[]))


  covs<-c(1,unlist(tail(info$xy_dat[[1]],1)[,c("x",cov_vec)]))|> `names<-`(c("intercept","log_lag_jack",cov_vec[]))

  mean_effects<-coefs*covs |> `names<-`(c("intercept","log_lag_jack",cov_vec[]))






  forecast|>
    dplyr::bind_cols(tibble::as_tibble_row(coefs)|>
                       dplyr::rename_with(~ paste0("coef_", .x))) |>
    dplyr::bind_cols(tibble::as_tibble_row(covs)|>
                       dplyr::rename_with(~ paste0("covar_", .x))) |>
    dplyr::bind_cols(tibble::as_tibble_row(mean_effects)|>
                       dplyr::rename_with(~ paste0("effect_", .x))) |>
    dplyr::mutate(mod_type="DLM",.before=dplyr::everything())

}




#' ARIMA (salmonForecasting) model forecast
#'
#' @param data
#' @param cov_vec
#'
#' @return
#' @export
#'
#' @examples
do_salmonForecasting_fun<-function(data,cov_vec=c("log_cum_cnt","cnt_by_flow")){


  salmonForecasting_dat<-data |> dplyr::mutate(species="Bon_Spr",period=1) |> dplyr::select(species,period,year,abundance=tot_adult,log_lag_jack,log_cum_cnt,cfs_mean_ema:dplyr::last_col()) |>
    tidyr::fill(c("log_lag_jack",cov_vec[])) |>
    dplyr::mutate(
      dplyr::across(c("log_lag_jack",cov_vec[]),\(x)c(scale(x)))
    )





  ARIMA_forecast<-SalmonForecasting::do_forecast(salmonForecasting_dat,
                                                 covariates =c("log_lag_jack",cov_vec[]),max_vars=2,n_cores=3,do_stacking = FALSE,TY_ensemble=15,write_model_summaries=FALSE,include_mod = TRUE)


  best_weighting<-ARIMA_forecast$ens$forecast_skill |> dplyr::filter(grepl("w",model)) |> dplyr::filter(MAPE==min(MAPE)) |> dplyr::pull(model)

  #best pred
  pred<-ARIMA_forecast$ens$ensembles |> dplyr::ungroup() |>  dplyr::filter(year==max(year),
                                                                           model== best_weighting) |>
    dplyr::left_join(ARIMA_forecast$ens$forecast_skill)


  #average coefficients
  coef_mat<-ARIMA_forecast$rp$top_mods |>
    dplyr::mutate(dplyr::across(-mod,unlist)) |>
    dplyr::filter(year==max(year)) |>
    dplyr::pull(mod) |>
    lapply(\(x)x[[1]]) |>
    dplyr::bind_rows()

  model_weights<-ARIMA_forecast$ens$final_model_weights |> dplyr::pull(substr(best_weighting,1,(nchar(best_weighting)-2)))


  ave_coefs<-colSums(coef_mat *
                       model_weights ,na.rm=T)

  ave_coefs_non_int_or_ARMA<-ave_coefs[!names(ave_coefs)%in%c("intercept",paste0("ar",1:10),paste0("ma",1:10),paste0("sar",1:10),paste0("sma",1:10))]


  covars<-salmonForecasting_dat[salmonForecasting_dat$year==max(salmonForecasting_dat$year),c("log_lag_jack",cov_vec[])]


  covar_effects<-ave_coefs_non_int_or_ARMA*covars[names(ave_coefs_non_int_or_ARMA)]




  pred |> dplyr::bind_cols(tibble::as_tibble_row(ave_coefs)|>
                             dplyr::rename_with(~ paste0("coef_", .x))) |>
    dplyr::bind_cols(tibble::as_tibble(covars)|>
                       dplyr::rename_with(~ paste0("covar_", .x))) |>
    dplyr::bind_cols(tibble::as_tibble(covar_effects)|>
                       dplyr::rename_with(~ paste0("effect_", .x))) |>
    dplyr::mutate(mod_type="ARIMA",.before=dplyr::everything())

}
