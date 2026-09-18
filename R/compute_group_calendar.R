date_df_parse <- function(data){
  
  data |>
    dplyr::mutate(date = as.Date(date)) |>
    dplyr::mutate(wday = lubridate::wday(.data$date)) |>
    dplyr::mutate(wday_abbr = lubridate::wday(.data$date, label = TRUE, abbr = TRUE)) |>
    dplyr::mutate(week_of_month = get_week_of_month(.data$date)) |>
    dplyr::mutate(day = lubridate::day(.data$date)) |>
    dplyr::mutate(year = lubridate::year(.data$date) - 2018) |>
    dplyr::mutate(month_abbr = lubridate::month(.data$date, abbr = TRUE, label = TRUE)) |>
    dplyr::mutate(hour = lubridate::hour(.data$date)) |>
    dplyr::mutate(year_academic =  lubridate::year(.data$date) +
                    ifelse(lubridate::month(date) >
                             6, 1, 0)) |>
    dplyr::mutate(month_academic_abbr = .data$month_abbr |>
                    factor(levels = c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
                                      "Jan", "Feb", "Mar", "Apr", "May", "Jun")))
}


compute_panel_calendar <- function(data, scales, complete = F, start_date = NULL, end_date = NULL ){
  
  if(!complete){
    
      data |> date_df_parse() |> mutate(orginal = 1)

  }else{ # complete case; fill in all days of month
  
    data$date <- as.Date(data$date) 
    
    month_start <- lubridate::floor_date(min(data$date), unit = "month")
    month_end <- (lubridate::ceiling_date(max(data$date), unit = "month") - days(1))
                              
    df_dates_interval(month_start, month_end) |>
      date_df_parse() ->
    completed
    
    if(!is.null(start_date)){completed <- completed |> dplyr::filter(completed$date >= as.Date(start_date)) }
    if(!is.null(end_date)){completed <- completed |> dplyr::filter(completed$date <= as.Date(end_date)) }
    
    completed |>
      mutate(original = date %in% data$date)

   }
}
  
StatCalendar <- ggplot2::ggproto(`_class` = "StatCalendar",
                                 `_inherit` = ggplot2::Stat,
                                 required_aes = c("date"),
                                 compute_panel = compute_panel_calendar,
                                 default_aes = ggplot2::aes(x = ggplot2::after_stat(wday),
                                                            y = ggplot2::after_stat(week_of_month),
                                                            label = ggplot2::after_stat(day),
                                                            alpha = original))


StatWeekly <- ggplot2::ggproto(`_class` = "StatCalendar",
                               `_inherit` = ggplot2::Stat,
                               required_aes = c("date"),
                               compute_panel = compute_panel_calendar,
                               default_aes = ggplot2::aes(x = ggplot2::after_stat(wday),
                                                          y = ggplot2::after_stat(hour),
                                                          label = ggplot2::after_stat(hour)))
