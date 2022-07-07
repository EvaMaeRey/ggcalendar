#' Title
#'
#' @param dates_df
#'
#' @return
#' @export
#'
#' @examples
#' library(lubridate)
#' library(dplyr)
#' library(ggplot2)
#'
#' ggcalendar() +
#' geom_text_calendar()
#'
#' ggcalendar() +
#' geom_text_calendar(color = "grey35") +
#' geom_point_calendar(data = . %>%
#' filter(wday(date) == 3),
#' color = "red",  alpha = .4, size = 5) +
#' labs(title = "When to do #TidyTuesday in 2022") +
#' geom_text_calendar(label = "X",
#'                   data = data.frame(date = seq(as.Date("2022/01/01"),
#'                   as.Date("2022/04/18"), "days")))
#'
ggcalendar <- function(dates_df = return_dates_year(2022)){

  ggplot2::ggplot(data = dates_df) +
    ggplot2::aes(date = date) +
    ggplot2::scale_y_reverse() +
    ggplot2::facet_wrap(~month(date, abbr = T, label = T)) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank())

}
