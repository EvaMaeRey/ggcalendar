ggcalendar <- function(dates_df = return_df_dates_year(), 
                       day_labels = c("S", "M", "T", "W", "T", "F", "S"), 
                       labels_layer = TRUE, 
                       color = "grey35",
                       size = 3,
                       alpha = .5){

  if(labels_layer){my_layer <- stat_calendar(color = color, aes(date = date), size = size, show.legend = F)}else{my_layer <- NULL}
  
  ggplot2::ggplot(data = dates_df) +
  defaults_calendar(day_labels = day_labels) +
  ggplot2::aes(date = date) +
  my_layer

  }
