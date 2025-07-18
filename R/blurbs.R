
inital_blurb_fun<-function(Bon_ch_day,season_dates){




  forecastdate<-Bon_ch_day$CountDate
  forecast_season<-Bon_ch_day$season
  forecast_year<-Bon_ch_day$year

  daily_10_yr_intervals<-
    (Bon_ch_day$total/
       plogis(qnorm(rev(c(.025,.25,.5,.75,.975)),qlogis(Bon_ch_day$Ave_10yr),Bon_ch_day$logit_prop_sd_10yr))) |>
    round() |>
    format(scientific=FALSE, big.mark=",")

  if (forecastdate <
      as.Date(ifelse(forecast_season=="spring",as.Date(paste0(forecast_year,"-03-01")),
             ifelse(forecast_season=="summer",as.Date(paste0(forecast_year,"-06-16")),
                    ifelse(forecast_season=="tule",as.Date(paste0(forecast_year,"-08-01")),
                           ifelse(forecast_season=="bright",as.Date(paste0(forecast_year,"-08-01")),NA)))))){

    cat(sprintf("**No counts yet for %s — the season is %s !**",forecast_year,
                chk_season_print(forecast_season, season_dates)))
  } else {
    cat(sprintf(
      "The cumulative %s season adult Chinook passage at Bonneville Dam through %s is **%s**. The 10-year (%s) average proportion of the count that has occurred at Bonneville Dam through %s is **%s%%**. Based on the cumulative counts to date and this proportion, the expected total %s (%s) season dam count would be %s (95%% prediction interval = %s and 50%% prediction interval = %s) adult Chinook.",
      forecast_season,
      format(forecastdate, "%B %d, %Y"),
      format(Bon_ch_day$total, scientific = FALSE, big.mark = ","),
      paste0((forecast_year - 10), "--", (forecast_year - 1)),
      format(forecastdate, "%B %d"),
      round(Bon_ch_day$Ave_10yr * 100, 1),
      forecast_season,
      chk_season_print(forecast_season, season_dates),
      format(round(Bon_ch_day$pred_Ave_10yr), scientific = FALSE, big.mark = ","),
      paste(daily_10_yr_intervals[c(1, 5)], collapse = " -- "),
      paste(daily_10_yr_intervals[c(2, 4)], collapse = " -- ")
    ))
  }
}

