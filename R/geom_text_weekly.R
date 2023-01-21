
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
#'   facet_wrap(~month(date, label = TRUE, abbr = TRUE)) +
#'   scale_y_reverse()
#'
#' data.frame(date = as.Date("2020-01-01") + days(0:800)) %>%
#'   ggplot() +
#'   aes(date = date) +
#'   aes(color = date) +
#'   geom_text_calendar() +
#'   facet_grid(year(date) ~ month(date, label = TRUE, abbr = TRUE))
#'
#'   data.frame(date = as.Date("2020-01-01") + days(0:800)) %>%
#'   ggplot() +
#'   aes(date = date) +
#'   aes(color = date) +
#'   geom_text_calendar() +
#'   scale_x_discrete() +
#'   facet_grid(lubridate::year(date) ~ lubridate::month(date, label = TRUE, abbr = TRUE))
#'
geom_text_weekly <- function(mapping = NULL, data = NULL,
                               position = "identity", na.rm = FALSE,
                               show.legend = NA,
                               inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatWeekly, # proto object from Step 2
    geom = ggplot2::GeomText, # inherit other behavior
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


