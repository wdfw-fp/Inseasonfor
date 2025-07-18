

current_year_cnts_plot<-function(flow_temp_dat2,Bon_ch_year){


  p<-
    flow_temp_dat2 |>
    dplyr::mutate(`Flow (kcfs)`=`Flow (kcfs)`/1000) |>
    dplyr::inner_join(
      Bon_ch_year |>
        dplyr::select(Date=CountDate,`Current yr`=AdultChinook,`10yr ave.` =Ave_10yr_daily_cnt,month,md=mday) |>
        tidyr::pivot_longer(c(`Current yr`,`10yr ave.`), names_to = "type",values_to = "Adult count")
    ) |>
    tidyr::pivot_longer(cols=c(`Adult count`, `Flow (kcfs)`, `River temp. (F)`),names_to="Param",values_to="Value")  |>
    ggplot2::ggplot(ggplot2::aes(x=Date,y=Value,col=type))+ ggplot2::geom_point(size=2.5)+ggplot2::geom_line()+ggplot2::facet_wrap(~Param,ncol=1,scales="free_y")+
    ggplot2::scale_color_manual(values=c("#E69F00","#56B4E9"))+#c("#E69F00","#56B4E9","#CC79A7"))+
    ggplot2::labs(y="",
                  col=NULL,
                  shape=NULL)+
    ggplot2::theme(legend.key=ggplot2::element_blank())+ggplot2::theme_grey()+ggplot2::theme(axis.title.x = ggplot2::element_blank(),text = ggplot2::element_text(size=18))

  print(p)
  cat("\n\n")
  cat(paste("Daily adult chinook salmon counts, flow, and temperature measurements taken at Bonneville Dam."), "\n\n")

}


percent_complete<-function(Bon_ch,forecast_year,forecastdate){
  p<-
    Bon_ch |> dplyr::filter(dplyr::between(year,forecast_year-15,forecast_year-1)) |> dplyr::mutate(date=(as.Date(paste(forecast_year,month,mday,sep="-")))) |>
    dplyr::filter(month>=4) |>
    ggplot2::ggplot(ggplot2::aes(x=date,y=prop))+ggplot2::geom_vline(ggplot2::aes(xintercept = forecastdate),col="firebrick",lty=2,lwd=1)+ggplot2::geom_boxplot(ggplot2::aes(group = date))+ ggplot2::scale_x_date(
      date_breaks = "1 month",
      date_labels = "%b"
    )+ggplot2::ylab("Percent passage complete")+ ggplot2::scale_y_continuous(labels = scales::unit_format(suffix="%",scale = 100))+ggplot2::theme_grey()+ggplot2::theme(axis.title.x = ggplot2::element_blank(),text = ggplot2::element_text(size=18))


  print(p)

  cat("\n\n")

  cat(paste("<strong>Percent of the run complete by date, 1995--2024.</strong> Lower and upper hinges correspond to the first and third quartiles (the 25th and 75th percentiles).The upper whisker extends from the hinge to the largest value no further than 1.5 * IQR from the hinge (where IQR is the inter-quartile range, or distance between the first and third quartiles). The lower whisker extends from the hinge to the smallest value at most 1.5 * IQR of the hinge. Data beyond the end of the whiskers are called &quot;outlying&quot; points and are plotted individually."), "\n\n")
}



prediction_error<-function(Bon_ch_year){
  p<-
    Bon_ch_year |>
    dplyr::filter(month>=4) |> ggplot2::ggplot(ggplot2::aes(y = (MAPE_10yr),x=CountDate))+ggplot2::geom_col(fill="grey10")+ggplot2::geom_vline(ggplot2::aes(xintercept = forecastdate),col="firebrick",lty=2,lwd=1,alpha=.5)+ggplot2::ylab("Mean absolute percent error (MAPE)") + ggplot2::scale_y_continuous(labels = scales::unit_format(suffix="%",scale = 100))+ggplot2::theme_grey()+ggplot2::theme(axis.title.x = ggplot2::element_blank(),text = ggplot2::element_text(size=18))

  print(p)

cat("\n\n")

cat(paste("Mean absolute percent error  (over 15 year retrospectiv) of predictions based on cumulative counts and 10-year average run timing, by day of year."), "\n\n")
}



summary_plot_tabs<-function(flow_temp_dat2,Bon_ch,forecastdate){

    forecast_year<-lubridate::year(forecastdate)

  Bon_ch_year<-Bon_ch |> dplyr::filter(year==forecast_year)|>
    dplyr::filter(month>=3)


cat("### {.tabset}","\n\n")

cat("#### Current year","\n\n")
current_year_cnts_plot(flow_temp_dat2,Bon_ch_year)
cat("\n\n")

cat("#### Percent complete","\n\n")
(percent_complete(Bon_ch,forecast_year,forecastdate))
cat("\n\n")

cat("#### Prediction error","\n\n")
(prediction_error(Bon_ch_year))
cat("\n\n")
}


