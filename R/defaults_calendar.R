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

  
  list(
    ggplot2::scale_y_reverse(breaks = 6:0, 
                             expand = c(0,0), 
                             limits = c(6.5, 0.5)),
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
