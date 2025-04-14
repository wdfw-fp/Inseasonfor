

#' joint_likelihood_results
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
joint_likelihood_results<-function(forecastdate,dat,forecast=122500,forecast_log_sd=0.28,
    joint_like_data_file="Joint_like_res.csv"){


  if (file.exists(joint_like_data_file)) {
    local_data <-
      readr::read_csv(joint_like_data_file)

    sdate <- max(local_data$date)+1
    #
  } else {
    local_data<-NULL
    sdate<-  as.Date(paste0(lubridate::year(forecastdate),"-04-05"))
  }


  if(sdate<=forecastdate){
    new_dat<-data.frame()
for (i in seq.Date(from=sdate,to=forecastdate,by=1)){

  dat_i <- dat |>

  new_dat<-dplyr::bind_rows(new_dat,
                            fit_joint_likelihood(fish_river_ocean,forecast = forecast,forecast_log_sd = forecast_log_sd)) |>
    dplyr::mutate(date=i,month=lubridate::month(i),mday=lubridate::mday(i),.before="L95")

}

    dat<-dplyr::bind_rows(local_data,new_dat)
    readr::write_csv(dat,joint_like_data_file)
    return(dat)
  }else{
    return(local_data)
  }

}







#' joint likelihhod data constructor
#'
#' @param dat
#' @param preseason_forecast
#' @param preseason_forecast_log_sd
#'
#' @return
#'
#' @examples
make_joint_likelihood_dat<-function(dat,
                                    preseason_forecast,
                                    preseason_forecast_log_sd){

## Data
list(
logCFlow= dat |> tidyr::fill(cfs_mean_ema) |> dplyr::pull(cfs_mean_ema) |> log() |> scale() |> c(),
InseasonCount  =dat$cum_cnt,
final_bon_log =log(head(dat$tot_adult,-1)),
log_pre_season_forecast = log(preseason_forecast),
preseason_log_sd = preseason_forecast_log_sd
)

}

#' make hoint likelihood parameters
#'
#' @param dat
#'
#' @return
#'
#' @examples
make_joint_like_params_fun<-function(mod_data){

#params
  list(
mu = qlogis(mean(head(mod_data$InseasonCount,-1)/(exp(mod_data$final_bon_log)))),
year_eff   = rep(0.05,length(mod_data$InseasonCount)),
phi        = .5,
tau_proc_err = -2,
B1  = -.2,
log_pred_sd = -.5 )

}





#' fit the joint likelihood model
#'
#' @param dat
#' @param forecast
#' @param forecast_log_sd
#'
#' @return
#' @export
#'
#' @examples
#'
fit_joint_likelihood<-function(dat,forecast,forecast_log_sd){

  RTMB_data<-make_joint_likelihood_dat(dat,forecast,forecast_log_sd)

  RTMB_params<-make_joint_like_params_fun(RTMB_data)

  RTMB_NLL<-Inseasonfor(RTMB_data)

  mod_obj<- RTMB::MakeADFun(RTMB_NLL,RTMB_params,random=c("year_eff"))

  opt <- nlminb(mod_obj$par, mod_obj$fn, mod_obj$gr)

  sdr <- RTMB::sdreport(mod_obj)

  adrep_est<-as.list(sdr, "Est", report=TRUE)
  adrep_sd<-as.list(sdr, "Std", report=TRUE)

  pred<-adrep_est$current_pred
  pred_sd<-adrep_sd$current_pred


  matrix(exp(qnorm(c(.025,.25,.5,.75,.975),pred,pred_sd)),nrow=1) |>
    `colnames<-`(c("L95","L50","Pred","U50","U95")) |> data.frame(logit_p= adrep_est$logitp |> tail(1),
                                                                  logit_p_sd= adrep_sd$logitp |> tail(1),
                                                                  B1= adrep_est$B1 |> tail(1),
                                                                  B1_sd= adrep_sd$B1 |> tail(1))

}



#' likelihood function for joint likelihood model in RTMB
#'
#' @param parms
#'
#' @return
#' @export
#'
#' @examples
Inseasonfor <- function(data_list) {

  function(parms){

  RTMB::getAll(parms,data_list, warn=FALSE)

  ## Derived quantities

  # proportion complete
  logitp <- mu + year_eff + B1 * logCFlow
  p <- 1/(1+exp(-logitp))


  ## Negative log-likelihood
  nll <- 0

  ## AR(1) year effect on proportion complere
  phi2 <- 2 / (1 + exp(-phi)) - 1
  tau_proc_err2 <- exp(tau_proc_err)
  nll <- nll - RTMB::dautoreg(year_eff,mu=0, phi=phi2, scale=tau_proc_err2,log=TRUE)

  ## Observation model
  old_pred<-log(head(InseasonCount,-1)/head(p,-1))
  current_pred<-log(tail(InseasonCount,1)/tail(p,1))

  #### previous years' total vs predictions
  nll <- nll - sum(RTMB::dnorm(final_bon_log, old_pred, exp(log_pred_sd), log = TRUE))
  #### current year's preseason forecast vs prediction
  nll <- nll - RTMB::dnorm(log_pre_season_forecast, current_pred, preseason_log_sd, log = TRUE)


  ## Reporting

  RTMB::REPORT(p)
  RTMB::REPORT(current_pred)
  RTMB::ADREPORT(logitp)
  RTMB::ADREPORT(current_pred)
  RTMB::ADREPORT(B1)

  return(nll)
  }
}
