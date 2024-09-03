#' Title
#'
#' @param dates_df 
#' @param day_labels 
#' @param geom 
#' @param color 
#' @param size 
#' @param alpha 
#'
#' @return
#' @export
#'
#' @examples
ggcalendar <- function(dates_df = df_year(), 
                       day_labels = c("M", "T", "W", "T", "F", "S", "S"), 
                       geom = "text", 
                       color = "grey35",
                       size = 3,
                       alpha = 1){
  
    my_layer <- stat_calendar(geom = geom, 
                              color = color, 
                              ggplot2::aes(date = date), 
                              size = size, 
                              alpha = alpha, 
                              show.legend = F) 
    
  ggplot2::ggplot(data = dates_df) +
  defaults_calendar(day_labels = day_labels) +
  ggplot2::aes(date = date) +
  my_layer

  }
