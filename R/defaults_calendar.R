defaults_calendar <- function(day_labels = c("S", "M", "T", "W", "T", "F", "S")){
  
  list(
    ggplot2::scale_y_reverse(breaks = 5:0, 
                             expand = c(0,0), 
                             limits = c(5.5, -0.5)),
    ggplot2::scale_x_continuous(breaks = 1:7, 
                                labels = day_labels,
                                limits = c(.5, 7.5), 
                                expand = c(0,0)
                                ),
    ggplot2::facet_wrap(~lubridate::month(date, abbr = T, label = T), scales = "free"),
    ggplot2::labs(x = NULL, y = NULL),
    ggplot2::theme(axis.text.y = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank()) +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank()),
    ggplot2::geom_blank()
  )
  
}
