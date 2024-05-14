get_week_of_month <- function(x){
  
  (- lubridate::wday(x) + lubridate::day(x)) %/% 
    7 + 1 +
    ifelse(lubridate::wday(lubridate::floor_date(lubridate::as_date(x), "month")) == 1, -1, 0)
  
}
