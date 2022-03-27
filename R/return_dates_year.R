#' Title
#'
#' @param start_date
#' @param end_date
#'
#' @return
#' @export
#'
#' @examples
#' return_dates_interval("2020-01-01", "2020-01-05")
return_dates_interval <- function(start_date, end_date){

  lubridate::as_date(start_date):lubridate::as_date(end_date) %>%
    lubridate::as_date()  %>%
    data.frame(date = .)

}




#' Title
#'
#' @param month
#'
#' @return
#' @export
#'
#' @examples
#' return_dates_month("1999-02")
#' return_dates_month("1999-04")
return_dates_month <- function(month){

  paste0(month, "-01") %>%
    lubridate::as_date() ->
    start_date

  start_date %>% lubridate::ceiling_date(unit = "month") ->
    end_date

  return_dates_interval(start_date, end_date - lubridate::days(1)) %>%
    data.frame(date = .)

}


#' Title
#'
#' @param year
#'
#' @return
#' @export
#'
#' @examples
#' return_dates_year(1999)
return_dates_year <- function(year){

  paste0(year, "-01-01") %>%
    lubridate::as_date() ->
  start_date

  start_date %>% lubridate::ceiling_date(unit = "year") ->
    end_date

  return_dates_interval(start_date, end_date - lubridate::days(1)) %>%
    data.frame(date = .)

}
