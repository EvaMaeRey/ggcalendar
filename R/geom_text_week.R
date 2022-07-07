#' Title
#'
#' @param date
#'
#' @return
#' @export
#'
#' @examples
#' fullweek("2022-01-03")
fullweek <- function(date = "2022-01-03"){

  date %>%
  lubridate::as_date() ->
  date

  lubridate::week(date) ->
  raw_week

  lubridate::wday(date) ->
  week_day

  lubridate::wday(lubridate::floor_date(date, unit = "year")) ->
  year_start_week_day

  raw_week + (week_day < year_start_week_day)
}


StatWeek <- ggplot2::ggproto(`_class` = "StatWeek",
                                 `_inherit` = ggplot2::Stat,
                                 required_aes = c("date"),
                                 compute_group = compute_group_calendar,
                                 default_aes = ggplot2::aes(x = after_stat(day_of_week),
                                                            y = after_stat(hour),
                                                            label = after_stat(hour)))

#' Title
#'
#' @param mapping
#' @param data
#' @param position
#' @param na.rm
#' @param show.legend
#' @param inherit.aes
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' library(tidyverse)
#' library(lubridate)
#'
#' data.frame(date = as.Date("2020-01-01") + hours(0:2000)) %>%
#'   ggplot() +
#'   aes(date = date) +
#'   aes(color = date) +
#'   geom_text_week() +
#'   facet_wrap(~fullweek(date)) +
#'   scale_y_reverse(limits = c(22, 7))
#'
#' data.frame(date = as.Date("2020-01-01") + hours(0:2000)) %>%
#'   ggplot() +
#'   aes(date = date) +
#'   aes(color = date) +
#'   geom_text_week() +
#'   facet_wrap(~fullweek(date)) +
#'   scale_y_reverse(limits = c(22, 7))
#'
#'
geom_text_week <- function(mapping = NULL, data = NULL,
                               position = "identity", na.rm = FALSE,
                               show.legend = NA,
                               inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatWeek, # proto object from Step 2
    geom = ggplot2::GeomText, # inherit other behavior
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


