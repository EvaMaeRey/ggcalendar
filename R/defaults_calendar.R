#' @examples
defaults_calendar <- function(day_labels = c("M", "T", "W", "T", "F", "S", "S"),
                              .stat.calendar = stat_calendar(geom = "blank", complete = T),
                              .scale.y.calendar = scale_y_calendar(),
                              .scale.x.calendar = scale_x_calendar(day_labels = day_labels),
                              .facet.calendar = facet_calendar(),
                              .theme.calendar = theme_calendar()
                              ){
  
  week_start <- getOption("lubridate.week.start", 7)
  
  if(week_start != 1){day_labels <- day_labels[c(week_start:7, 1:(week_start-1))]}

  list(.stat.calendar, .scale.y.calendar, .scale.x.calendar, .facet.calendar, .theme.calendar)
  
}


stamp_calendar_text <- function(..., 
                                complete = T,
                                .defaults.calendar = defaults_calendar()
                              ){
  
  list(stat_calendar(..., geom = "text", alpha = .6, complete = complete),
       .defaults.calendar)
  
}

geom_calendar_text <- function(..., 
                              complete = F,
                              .defaults.calendar = defaults_calendar()
                              ){
  
  list(stat_calendar(..., geom = "text", complete = complete), .defaults.calendar)
  
}


stamp_calendar_tile <- function(..., 
                                complete = T,
                                .defaults.calendar = defaults_calendar()
                              ){
  
  list(stat_calendar(..., geom = "tile", alpha = .4, complete = complete), .defaults.calendar)
  
}

geom_calendar_tile <- function(..., 
                              complete = F,
                              .defaults.calendar = defaults_calendar()
                              ){
  
  list(stat_calendar(..., geom = "tile", complete = complete), .defaults.calendar)
  
}


stamp_calendar_text <- function(..., 
                                complete = T,
                                .defaults.calendar = defaults_calendar()
                              ){
  
  list(stat_calendar(..., geom = "text", alpha = .4, complete = complete), .defaults.calendar)
  
}

geom_calendar_text <- function(..., 
                              complete = F,
                              .defaults.calendar = defaults_calendar()
                              ){
  
  list(stat_calendar(..., geom = "text", complete = complete), .defaults.calendar)
  
}



