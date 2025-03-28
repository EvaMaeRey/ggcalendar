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


geom_calendar_strikeoff <- function(label = "X", end_date = Sys.Date() - days(1), ...){
  
  stat_calendar(geom = ggexpress:::qproto_update(GeomText, aes(color = "darkred", alpha = .85)),
                label = label, complete = T, end_date = end_date, 
                ...)
  }



