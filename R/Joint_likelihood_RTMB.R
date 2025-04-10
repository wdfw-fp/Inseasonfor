
## Data
logjCK       <- as.vector(data$logjCK)
Delta_logjCK <- as.vector(data$Delta_logjCK)
lag_logCK4   <- as.vector(data$lag_logCK4)
logCK4       <- as.vector(data$logCK4)
logCK5_6     <- as.vector(data$logCK5_6)
logCFlow     <- as.vector(data$logCFlow)
InseasonCount <- as.numeric(data$InseasonCount)
obslogitp     <- as.vector(data$obslogitp)

## Parameters
beta       <- as.vector(par$beta)
alpha      <- as.vector(par$alpha)
tau_alpha  <- par$tau_alpha
alpha2     <- par$alpha2
mu         <- par$mu
year_eff   <- as.vector(par$year_eff)
phi        <- par$phi
tau_proc_err <- par$tau_proc_err
B1         <- par$B1
log_4_forecast <- par$log_4_forecast
log_56_forecast <- par$log_56_forecast
tau_logCK4     <- par$tau_logCK4
tau_logCK5_6   <- par$tau_logCK5_6
resid_err      <- par$resid_err

random_TMB<-c("alpha","year_eff","log_4_forecast","log_56_forecast")



get_age_data <- function(){
  read_csv("data/agecomp.csv",
           col_types=cols(`pAge 4`=col_number(),
                          `pAge 5`=col_number(),
                          `pAge 6`=col_number()),
           na="") %>%
    filter(Type=="Bonn Dam") %>%
    select(Year,`pAge 4`)
}

#' likelihood function for joint likelihood model in RTMB
#'
#' @param parms
#'
#' @return
#' @export
#'
#' @examples
Inseasonfor <- function(parms) {

  RTMB::getAll(parms,dat, warn=FALSE)



  ## Derived quantities
  N_years <- length(logjCK)

  # # Preseason model means
  # logCK4_mu <- alpha + beta[1] * logjCK + beta[2] * Delta_logjCK
  # logCK5_6_mu <- alpha2 + beta[3] * lag_logCK4

  # Inseason model
  logitp <- mu + year_eff + B1 * logCFlow
  p <- plogis(logitp)

  ## Negative log-likelihood
  nll <- 0

  # ## Random walk for alpha
  # sd_alpha <- exp(tau_alpha)
  # for (i in 2:length(alpha)) {
  #   nll <- nll - RTMB::dnorm(alpha[i] - alpha[i - 1], 0, sd_alpha, log = TRUE)
  # }

  ## AR(1) year effect
  phi2 <- 2 / (1 + exp(-phi)) - 1
  tau_proc_err2 <- exp(tau_proc_err)
  nll <- nll - RTMB::dautoreg(year_eff,mu=0, phi=phi2, scale=tau_proc_err2,log=TRUE)

  ## Observation model: preseason
  # sd_logCK4 <- exp(tau_logCK4)
  # sd_logCK5_6 <- exp(tau_logCK5_6)
  # for (i in seq_along(logCK4)) {
  #   nll <- nll - RTMB::dnorm(logCK4[i], logCK4_mu[i], sd_logCK4, log = TRUE)
  #   nll <- nll - RTMB::dnorm(logCK5_6[i], logCK5_6_mu[i], sd_logCK5_6, log = TRUE)
  # }

  # forecast <- exp(log_4_forecast) + exp(log_56_forecast)


  nll <- nll - RTMB::dnorm(log_4_forecast, logCK4_mu[N_years], sd_logCK4, log = TRUE)
  nll <- nll - RTMB::dnorm(log_56_forecast, logCK5_6_mu[N_years], sd_logCK5_6, log = TRUE)

  # Inseason obs model
  props <- numeric(N_years)
  props[1:(N_years - 1)] <- obslogitp
  props[N_years] <- qlogis(InseasonCount / forecast)  # logit

  nll <- nll - sum(RTMB::dnorm(logitp, props, exp(resid_err), log = TRUE))

  ## Reporting
  RTMB::REPORT(logitp = logitp,
         forecast = forecast,
         log_forecast = log(forecast),
         B1 = B1,
         phi2 = phi2,
         p = p,
         props = props)

  return(nll)
}
