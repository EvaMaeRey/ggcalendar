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
#' @param date
#'
#' @return
#' @export
#'
#' @examples
#' return_dates_week()
return_dates_week <- function(date = NULL){

  if(is.null(date)){date <- Sys.Date()}

  start_date <- lubridate::floor_date(date, unit = "week")
  end_date <- lubridate::ceiling_date(date, unit = "week")

  return_dates_interval(start_date, end_date - lubridate::days(1)) %>%
    data.frame(date = .)

}


#' Title
#'
#' @param date
#'
#' @return
#' @export
#'
#' @examples
#' return_hours_week()
return_hours_week <- function(date = NULL){

  if(is.null(date)){date <- Sys.Date()}

  start_date <- floor_date(date, unit = "week")

  (start_date + lubridate::hours(1:(24*7-1))) %>%
    data.frame(date = .)

}

# week("2022-08-26")
# week("2022-08-27")
# (as_date("2022-08-27") + hours(x = 1)) %>% week()

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




