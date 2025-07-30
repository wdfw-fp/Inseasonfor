

render_tab<-function(pred_date,counts,river_env,do_plots,seas_dats,seas_end,write_local=write_local,morph){

  if(morph!=""){
    counts2 <-counts  |>
      dplyr::rename(AdultChinook=paste(morph,"Adult",sep="_"),
                    JackChinook=paste(morph,"Jack",sep="_"))
if(morph=="Tule"){
  counts2 <-counts2  |>
    dplyr::mutate(JackChinook=JackChinook+Tule_Stubby )
}

  }else{
    counts2 <-counts
  }


  Bon_ch<-Bon_ch_fun(pred_date,counts2)
  Bon_ch_year<-Bon_ch |> dplyr::filter(year==forecast_year)|>
    dplyr::filter(month>=3) |>
    dplyr::mutate(
    AdultChinook=ifelse(CountDate>pred_date,NA_real_,AdultChinook),
)

  inital_blurb_fun(Bon_ch|> dplyr::filter(CountDate==forecastdate_i),
                   season_dates=season_dates,
                   morph=morph)


  cat("\n\n")


    summary_plot_tabs(flow_temp_dat2,Bon_ch,forecastdate_i)



    mod_wrapper_fun(pred_date,counts2,river_env,#ocean_cov,
                    Bon_ch_year,season_dates=seas_dats,
                    season_end_date=seas_end,
                    write_local=write_local,
                    morph)



    bon_ch_tabs(
      Bon_ch_year,
      forecast_year,
      forecastdate_i
    )


}
