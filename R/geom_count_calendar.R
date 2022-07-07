compute_group_calendar_count <- function(data, scales){

  data %>%
    dplyr::count(date) %>%
    dplyr::mutate(day_of_week = lubridate::wday(.data$date, label = TRUE, abbr = TRUE)) %>%
    dplyr::mutate(week_of_month = (- lubridate::wday(.data$date) + lubridate::day(.data$date)) %/% 7 + 1) %>%
    dplyr::mutate(date_of_month = lubridate::day(.data$date)) %>%
    dplyr::mutate(which_year = lubridate::year(.data$date) - 2018) %>%
    dplyr::mutate(month = lubridate::month(.data$date, abbr = TRUE, label = TRUE)) %>%
    dplyr::mutate(academic_year =  lubridate::year(.data$date) +
                    ifelse(lubridate::month(date) >
                             6, 1, 0)) %>%
    dplyr::mutate(academic_month = .data$month %>%
             factor(levels = c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
                               "Jan", "Feb", "Mar", "Apr", "May", "Jun"))) %>%
    dplyr::mutate(x = .data$day_of_week) %>%
    dplyr::mutate(y = -.data$week_of_month) %>%
    dplyr::mutate(label = .data$date_of_month)

}

StatCalendarcount <- ggplot2::ggproto(`_class` = "StatCalendarcount",
                                 `_inherit` = ggplot2::Stat,
                                 required_aes = c("date"),
                                 compute_group = compute_group_calendar_count,
                                 default_aes = ggplot2::aes(x = after_stat(x),
                                                            size = after_stat(n))
                                 )

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
#' library(lubridate)
#' library(tidyverse)
#'
#' data.frame(date = as.Date("2020-01-01") + days(0:365) ) %>%
#'   sample_n(500, replace = TRUE) %>%
#'   ggplot() +
#'   aes(date = date) +
#'   geom_count_calendar() +
#'   scale_color_discrete() +
#'   facet_wrap(~month(date, label = TRUE, abbr = TRUE))

geom_count_calendar <- function(mapping = NULL, data = NULL,
                               position = "identity", na.rm = FALSE,
                               show.legend = NA,
                               inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatCalendarcount, # proto object from Step 2
    geom = ggplot2::GeomPoint, # inherit other behavior
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


