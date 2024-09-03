#' Title
#'
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
theme_grey_calendar <- function(...){

  theme_grey(...) %+replace%
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.title = element_blank()) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank())
  
}


scale_y_calendar <- function(...){ggplot2::scale_y_reverse(breaks = 6:0, 
                             expand = c(0,0), 
                             limits = c(6.5, 0.5), ...)}

scale_x_calendar <- function(day_labels = c("M", "T", "W", "T", "F", "S", "S"), ...){
  
    ggplot2::scale_x_continuous(breaks = 1:7, 
                                labels = day_labels,
                                limits = c(.5, 7.5), 
                                expand = c(0,0), ...)}

facet_calendar <- function(...){
  
  ggplot2::facet_wrap(~lubridate::month(date, abbr = T, label = T), 
                      scales = "free",...)
  
}


geom_calendar_blank <- function(...){
  
  stat_calendar(geom = "blank", ...)
  
}


