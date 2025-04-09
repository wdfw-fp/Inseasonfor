
forecast_month<-lubridate::month(forecastdate)
forecast_mday<-lubridate::mday(forecastdate)
forecast_season

fish_dat<-Bon_cnts |>
  dplyr::filter(season ==forecast_season) |>
  dplyr::arrange(.data$year,.data$month,.data$mday) |>
  dplyr::group_by(.data$year) |>
  dplyr::mutate(
    cum_cnt=cumsum(.data$AdultChinook),
    tot_adult=tail(.data$cum_cnt,1),
    tot_jack=sum(.data$JackChinook)
    ) |>
  dplyr::ungroup() |>
dplyr::filter(month==forecast_month,mday==forecast_mday) |>
  dplyr::mutate(lag_jack=dplyr::lag(tot_jack,1)
                ) |>
  dplyr::select(year,cum_cnt,abundance=tot_adult,lag_jack)

# abundance=TOTAL,year=Year,species,period

cov_dat <-read_csv("cov_dat.csv")

#covatiates including lag1 hatchery and wild 1-salts
cov_dat_lag1<-cov_dat |>
  mutate(Year=Year+1) |>
  left_join(
    dat |> filter(Stock=="HatcheryA") |> select(Year,hatchA1_lag1=`1-salt`) |> mutate(Year=Year+1)
  ) |>
  left_join(
    dat |> filter(Stock=="WildA") |> select(Year,wildA1_lag1=`1-salt`) |> mutate(Year=Year+1)
  ) |>
  mutate(even_odd=rep(c(-1,1),times=nrow(cov_dat)/2)) |>
  filter(Year<=2025)|>
  rename(year=Year)


A_wild<-dat |> filter(Stock=="WildA") |> mutate(species="Steelhead",period=1)|>
  select(abundance=TOTAL,year=Year,species,period)

A_wild<-A_wild |> bind_rows(A_wild |> filter(year==2024) |> mutate(year=2025,abundance=NA)) |>
  left_join(cov_dat_lag1)

A_wild_mod<-SalmonForecasting::do_forecast(A_wild,covariates =c("wildA1_lag1","Spr_Nino3.4","Spr_NPGO","Spr_PDO","even_odd"),max_vars=2,n_cores=8,do_stacking = FALSE,TY_ensemble=min(15,round((nrow(A_wild)/2)-7))+1,slide=min(15,round((nrow(A_wild)/2)-7)),write_model_summaries=FALSE)


A_wild_mod$plots_and_tables$Table2
A_wild_mod$plots_and_tables$Table3
A_wild_mod$plots_and_tables$Table4
A_wild_mod$plots_and_tables$Figure1
A_wild_mod$plots_and_tables$Figure3
