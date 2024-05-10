/// @file Inseasonfor.hpp

#undef TMB_OBJECTIVE_PTR
#define TMB_OBJECTIVE_PTR obj


template<class Type>
Type Inseasonfor(objective_function<Type>* obj) {
  DATA_VECTOR(logjCK);
  DATA_VECTOR(Delta_logjCK);
  DATA_VECTOR(lag_logCK4);
  DATA_VECTOR(logCK4);
  DATA_VECTOR(logCK5_6);
  DATA_VECTOR(logCFlow);
  DATA_SCALAR(InseasonCount);
  // DATA_VECTOR(harvest);
  DATA_VECTOR(obslogitp);
  //pre-season process mod
  PARAMETER_VECTOR(beta);   //sibling regression coefficients
  PARAMETER_VECTOR(alpha);  // time varying intercepts
  PARAMETER(tau_alpha);     // process error on intercept
  PARAMETER(alpha2);
  //in-season process mod
  PARAMETER(mu);             // intercept for logit p
  PARAMETER_VECTOR(year_eff);// AR1 year effects
  PARAMETER(phi);             // autocorrelation coefficient
  PARAMETER(tau_proc_err);    // precess error for AR1 year effect
  PARAMETER(B1);             // effect of flow on
  //obs mods
  PARAMETER(log_4_forecast); //forecast for age 4 in 2023
  PARAMETER(log_56_forecast);//forecast for age 5+ in 2023
  PARAMETER(tau_logCK4);    //log chinook 4 obs sd
  PARAMETER(tau_logCK5_6);  // log chk 56 obs sd
  PARAMETER(resid_err);
  //==================================
  //       Pre-season process models
  //==================================

  // Age 4 sib regression
  vector<Type> logCK4_mu = alpha + beta(0)*logjCK +
    beta(1)*Delta_logjCK;

  // Age 5-6 sib regression
  vector<Type> logCK5_6_mu = alpha2 + beta(2)*lag_logCK4;

  //total abundance at Bon through end of season
  int N_years = logjCK.size();
  // vector<Type>CK(N_years);
  // CK.head(N_years-1) =  vector<Type>(exp(logCK4)) +
  //                       vector<Type>( exp(logCK5_6));
  //==================================
  //       in-season process models
  //==================================

  // Linear predictor for proportion complete on forecast date
  vector<Type> logitp = mu + year_eff + B1*logCFlow;

  vector<Type>  p = invlogit(logitp);
  //==================================
  //       Likelihood
  //==================================

  Type NLL = 0;  //negative log-likelihood intilize

  ////random effects
  // Age4 sib regression intercept (random walk)

  Type sd_alpha=exp(tau_alpha);

  // NLL -= dnorm(alpha(0,Type(0) ,
  //              sd_alpha,true);
  for(int i = 1; i<alpha.size(); i++){
    NLL -= dnorm(alpha(i)-alpha(i-1),Type(0) ,
                 sd_alpha,true);
  }


  // in-season AR1 year effect
  using namespace density;
  Type phi2=Type(2)/(Type(1) + exp(-Type(phi) * x)) - Type(1);
  Type tau_proc_err2 = exp(tau_proc_err);
  NLL += SCALE(AR1(phi2),tau_proc_err2)(year_eff);
  ////observation models

  // Preseason forecast models
  Type sd_logCK4 = exp(tau_logCK4);
  Type sd_logCK5_6 = exp(tau_logCK5_6);
  for(int i = 0; i<logCK4.size(); i++){
    NLL -= dnorm(logCK4(i),logCK4_mu(i) ,sd_logCK4,true);

    NLL -= dnorm(logCK5_6(i),logCK5_6_mu (i),sd_logCK5_6,true);
  }

  Type forecast=exp(log_4_forecast)+exp(log_56_forecast);
  // CK.tail(1)=forecast;
  // vector<Type> CK_bon =CK-harvest;

  NLL -= dnorm(log_4_forecast,logCK4_mu(N_years-1) ,sd_logCK4,true);
  NLL -= dnorm(log_56_forecast,logCK5_6_mu(N_years-1) ,sd_logCK5_6,true);

  // Inseason observation model
  // Bonneville runsize on doy
  // vector<Type> pred = p*CK_bon;
  // NLL -= dpois(InseasonCount,pred,true).sum();
  vector<Type> props(N_years);
  props.head(N_years-1)=obslogitp;
  props.tail(1)=logit(InseasonCount/forecast);

  NLL -= dnorm(logitp,props,exp(resid_err),true).sum();


  ADREPORT(logitp);
  ADREPORT(forecast);
  ADREPORT(log(forecast));
  ADREPORT(B1);
  ADREPORT(phi2);
  REPORT(p);
  REPORT(props);
  // REPORT(CK);
  return (NLL);
}

#undef TMB_OBJECTIVE_PTR
#define TMB_OBJECTIVE_PTR this
