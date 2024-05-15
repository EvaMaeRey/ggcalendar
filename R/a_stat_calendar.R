stat_calendar <- function(mapping = NULL, 
                          data = NULL,
                          geom = "text",
                          position = "identity", 
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatCalendar, # proto object from Step 2
    geom = geom, # inherit other behavior
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


