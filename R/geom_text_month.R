compute_group_calendar <- function(data, scales){

  data %>%
    dplyr::mutate(day_of_week = lubridate::wday(.data$date, label = T, abbr = T)) %>%
    dplyr::mutate(week_of_month = (- lubridate::wday(.data$date) + lubridate::day(.data$date)) %/% 7 + 1) %>%
    dplyr::mutate(date_of_month = lubridate::day(.data$date)) %>%
    dplyr::mutate(which_year = lubridate::year(.data$date) - 2018) %>%
    dplyr::mutate(month = lubridate::month(.data$date, abbr = T, label = T)) %>%
    dplyr::mutate(academic_year =  lubridate::year(.data$date) +
             ifelse(lubridate::month(date) >
                      6, 1, 0)) %>%
    mutate(academic_month = .data$month %>%
             factor(levels = c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
                               "Jan", "Feb", "Mar", "Apr", "May", "Jun"))) %>%
    mutate(x = .data$day_of_week) %>%
    mutate(y = -.data$week_of_month) %>%
    mutate(label = .data$date_of_month)

}


StatCalendar <- ggplot2::ggproto(`_class` = "StatCalendar",
                                 `_inherit` = ggplot2::Stat,
                                 required_aes = c("date"),
                                 compute_group = compute_group_calendar,
                                 default_aes = ggplot2::aes(x = after_stat(x)))

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
#' data.frame(date = as.Date("2020-01-01") + days(0:365)) %>%
#'   ggplot() +
#'   aes(date = date) +
#'   aes(color = date) +
#'   geom_text_calendar() +
#'   facet_wrap(~month(date, label = T, abbr = T))
#'
#' data.frame(date = as.Date("2020-01-01") + days(0:800)) %>%
#'   ggplot() +
#'   aes(date = date) +
#'   aes(color = date) +
#'   geom_text_calendar() +
#'   facet_grid(year(date) ~ month(date, label = T, abbr = T))
#'
#'   data.frame(date = as.Date("2020-01-01") + days(0:800)) %>%
#'   ggplot() +
#'   aes(date = date) +
#'   aes(color = date) +
#'   geom_text_calendar() +
#'   scale_x_discrete() +
#'   facet_grid(lubridate::year(date) ~ lubridate::month(date, label = T, abbr = T))
#'
geom_text_calendar <- function(mapping = NULL, data = NULL,
                               position = "identity", na.rm = FALSE,
                               show.legend = NA,
                               inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatCalendar, # proto object from Step 2
    geom = ggplot2::GeomText, # inherit other behavior
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


