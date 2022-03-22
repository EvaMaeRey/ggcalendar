StatCalendartile <- ggplot2::ggproto(`_class` = "StatCalendartile",
                                 `_inherit` = ggplot2::Stat,
                                 required_aes = c("date"),
                                 compute_group = compute_group_calendar,
                                 default_aes = ggplot2::aes(x = after_stat(num_day_of_week)))

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
#' data.frame(date = as.Date("2020-01-01") + days(0:365)) %>%
#'   ggplot() +
#'   aes(date = date) +
#'   aes(color = date) +
#'   geom_tile_calendar() +
#'   facet_wrap(~month(date, label = T, abbr = T))
#'
#' data.frame(date = as.Date("2020-01-01") + days(0:400)) %>%
#'   ggplot() +
#'   aes(date = date) +
#'   geom_tile_calendar() +
#'   facet_grid(year(date) ~ month(date, label = T, abbr = T))
geom_tile_calendar <- function(mapping = NULL, data = NULL,
                               position = "identity", na.rm = FALSE,
                               show.legend = NA,
                               inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatCalendartile, # proto object from Step 2
    geom = ggplot2::GeomTile, # inherit other behavior
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


