#' Title
#'
#' @param dates_df 
#' @param day_labels 
#' @param labels_layer 
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
                       labels_layer = TRUE, 
                       color = "grey35",
                       size = 3,
                       alpha = 1){
  
  if(labels_layer){
    
    my_layer <- stat_calendar(
    color = color, ggplot2::aes(date = date), 
    size = size, alpha = alpha, show.legend = F) 
    
    } else { my_layer <- NULL}
  
  ggplot2::ggplot(data = dates_df) +
  defaults_calendar(day_labels = day_labels) +
  ggplot2::aes(date = date) +
  my_layer

  }
