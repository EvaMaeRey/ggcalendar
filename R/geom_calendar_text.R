#' Title
#'
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
geom_calendar_text <- function(...){stat_calendar(geom = "text", ...)}

#' Title
#'
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
geom_calendar_point <- function(...){stat_calendar(geom = "point", ...)}

#' Title
#'
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
geom_calendar_tile <- function(...){stat_calendar(geom = "tile", ...)}



#' Title
#'
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
geom_calendar_tile <- function(...){stat_calendar(geom = "tile", ...)}

GeomTileFrame <- ggplot2::ggproto("GeomTileFrame", ggplot2::GeomTile,
                     default_aes = modifyList(ggplot2::GeomTile$default_aes, 
                                              ggplot2::aes(fill = NA, color = ggplot2::from_theme(accent),
                                                  linewidth = 1)))

#' @export
geom_calendar_tile_frame <- function(...){stat_calendar(geom = "tile_frame", ...)}


GeomTextRed <- ggplot2::ggproto("GeomTextRed", ggplot2::GeomText,
                     default_aes = modifyList(ggplot2::GeomText$default_aes, 
                                              ggplot2::aes(color = ggplot2::from_theme(accent),
                                                  alpha = .65)))

#' @export
geom_calendar_strikeoff <- function(label = "X", end_date = Sys.Date() - lubridate::days(1), ...){
  
  stat_calendar(geom = GeomTextRed,
                label = label, complete = T, end_date = end_date, show.legend = F,
                ...)
  }

#' @export
geom_calendar_today <- function(geom = GeomTileFrame, ...){
  
  stat_calendar(geom = geom, complete = T, 
                start_date = Sys.Date(), end_date = Sys.Date(), show.legend = F,
                ...)
  }

#' @export
geom_calendar_complete <- function(geom = "text", ...){
  
  stat_calendar(geom = geom, complete = T, ...)
  
  }


