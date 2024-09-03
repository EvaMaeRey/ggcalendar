#' Title
#'
#' @param day_labels 
#'
#' @return
#' @export
#'
#' @examples
defaults_calendar <- function(day_labels = c("M", "T", "W", "T", "F", "S", "S")){
  
  week_start <- getOption("lubridate.week.start", 7)
  
  if(week_start != 1){day_labels <- day_labels[c(week_start:7, 1:(week_start-1))]}

  list(scale_y_calendar(),
       scale_x_calendar(day_labels = day_labels),
       facet_calendar(),
       theme_grey_calendar(),
       stat_calendar(geom = "blank")
       )
  
}
