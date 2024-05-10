#' plot bon
#'
#' @param dat dataframe containing bonneville counts, proportions of total run passed, and forecasts
#' @param forecast current best forecast of total season passage
#'
#' @return ggplot of observed and predicted daily counts
#' @export
#'
#' @examples
#' dat<-bon_ch()
#' plot_bon(dat,forecast =115000)
#'
plot_bon<-function(dat, forecast=114867){

  plot_dat<-dat |>
    dplyr::filter(.data$year>=max(.data$year)-1)

  plot_dat2<-plot_dat |>
    dplyr::mutate(year=as.factor(.data$year)) |>
    dplyr::bind_rows(
      plot_dat |>
        dplyr::filter(.data$year>=max(.data$year)) |>
        dplyr::arrange(.data$month,.data$mday) |>
        dplyr::mutate(year="Expected 5 yr",AdultChinook=c(.data$Ave_5yr[1],forecast*diff(.data$Ave_5yr)))
    )|>
    dplyr::bind_rows(
      plot_dat |>
        dplyr::filter(.data$year>=max(.data$year)) |>
        dplyr::arrange(.data$month,.data$mday) |>
        dplyr::mutate(year="Expected 10 yr",AdultChinook=c(.data$Ave_10yr[1],forecast*diff(.data$Ave_10yr)))
    ) |>
    dplyr::mutate(date=as.Date(paste(2000,.data$month,.data$mday,sep="-"))) |>
    dplyr::filter(.data$month>=4)



    ggplot2::ggplot(plot_dat2,mapping =ggplot2::aes(x=.data$date,y=.data$AdultChinook,color=.data$year,linetype=.data$year))+ggplot2::geom_line(lwd=1.5)+ggplot2::scale_color_manual(values=c("mediumpurple4","cadetblue","black","darkblue"))+ggplot2::theme_classic() +ggplot2::scale_x_date(date_labels = "%m/%d")+ggplot2::scale_linetype_manual(values = c(1,1,1,5))

}
