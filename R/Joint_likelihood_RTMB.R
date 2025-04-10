




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

  list(adrep_est=adrep_est,
       adrep_sd=adrep_sd)
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
  RTMB::ADREPORT(p)
  RTMB::ADREPORT(current_pred)
  RTMB::ADREPORT(B1)

  return(nll)
  }
}
