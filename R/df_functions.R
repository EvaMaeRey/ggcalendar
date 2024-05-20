#' Title
#'
#' @return
#' @export
#'
#' @examples
df_today <- function(){

  data.frame(date = Sys.Date())

}

#' Title
#'
#' @param date 
#'
#' @return
#' @export
#'
#' @examples
df_day <- function(date = NULL){
  
  if(is.null(date)){date <- Sys.Date()}

  data.frame(date = date)

}


#' Title
#'
#' @param start_date 
#' @param end_date 
#'
#' @return
#' @export
#'
#' @examples
df_dates_interval <- function(start_date, end_date){

  data.frame(date = as.Date(start_date):as.Date(end_date) |>
    as.Date())

}

#' Title
#'
#' @param month 
#' @param year 
#'
#' @return
#' @export
#'
#' @examples
df_month <- function(month = NULL, year = NULL){

  if(is.null(month)){
    
    date <- Sys.Date()
    month <- lubridate::month(date) 
  }
   
  if(is.numeric(month)){ 
    
    month <- stringr::str_pad(month, width = 2, pad = "0")
    
  }
  
    if(is.null(year)){
    
    date <- Sys.Date()
    year <- lubridate::year(date)
    }

  
  paste0(year,"-", month, "-01") |>
    lubridate::as_date() ->
    start_date

  start_date |> lubridate::ceiling_date(unit = "month") ->
    end_date

    data.frame(date = 
                 df_dates_interval(start_date, 
                                       end_date - lubridate::days(1)))

}

#' Title
#'
#' @param date 
#'
#' @return
#' @export
#'
#' @examples
df_week <- function(date = NULL){

  if(is.null(date)){date <- Sys.Date()}

  start_date <- lubridate::floor_date(date, unit = "week")
  end_date <- lubridate::ceiling_date(date, unit = "week")

  data.frame(date = df_dates_interval(start_date, 
                        end_date - lubridate::days(1)) )

}

#' Title
#'
#' @param date 
#'
#' @return
#' @export
#'
#' @examples
return_df_hours_week <- function(date = NULL){

  if(is.null(date)){date <- Sys.Date()}

  start_date <- lubridate::floor_date(date, unit = "week")

  data.frame(date = (start_date + lubridate::hours(1:(24*7-1))))

}

#' Title
#'
#' @param year 
#'
#' @return
#' @export
#'
#' @examples
df_year <- function(year = NULL){

  if(is.null(year)){year <-  lubridate::year(Sys.Date())}

  paste0(year, "-01-01") |>
    lubridate::as_date() ->
  start_date

  start_date |> lubridate::ceiling_date(unit = "year") ->
    end_date

    data.frame(date = 
                 df_dates_interval(start_date, 
                                       end_date - lubridate::days(1)))
    
}
