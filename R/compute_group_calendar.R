compute_group_calendar <- function(data, scales){

  data |>
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

StatCalendar <- ggplot2::ggproto(`_class` = "StatCalendar",
                                 `_inherit` = ggplot2::Stat,
                                 required_aes = c("date"),
                                 compute_group = compute_group_calendar,
                                 default_aes = ggplot2::aes(x = ggplot2::after_stat(wday),
                                                            y = ggplot2::after_stat(week_of_month),
                                                            label = ggplot2::after_stat(day)))


StatWeekly <- ggplot2::ggproto(`_class` = "StatCalendar",
                               `_inherit` = ggplot2::Stat,
                               required_aes = c("date"),
                               compute_group = compute_group_calendar,
                               default_aes = ggplot2::aes(x = ggplot2::after_stat(wday),
                                                          y = ggplot2::after_stat(hour),
                                                          label = ggplot2::after_stat(hour)))
