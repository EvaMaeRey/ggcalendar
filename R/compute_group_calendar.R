#' Title
#'
#' @param data
#' @param scales
#'
#' @return
#' @export
#'
#' @examples
#' return_dates_year(1999) %>%
#' head() %>%
#' compute_group_calendar()
compute_group_calendar <- function(data, scales){

  data %>%
    dplyr::mutate(num_day_of_week = lubridate::wday(.data$date)) %>%
    dplyr::mutate(day_of_week = lubridate::wday(.data$date, label = T, abbr = T)) %>%
    dplyr::mutate(week_of_month = (- lubridate::wday(.data$date) + lubridate::day(.data$date)) %/% 7 + 1) %>%
    dplyr::mutate(date_of_month = lubridate::day(.data$date)) %>%
    dplyr::mutate(which_year = lubridate::year(.data$date) - 2018) %>%
    dplyr::mutate(month = lubridate::month(.data$date, abbr = T, label = T)) %>%
    dplyr::mutate(academic_year =  lubridate::year(.data$date) +
                    ifelse(lubridate::month(date) >
                             6, 1, 0)) %>%
    dplyr::mutate(academic_month = .data$month %>%
                    factor(levels = c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
                                      "Jan", "Feb", "Mar", "Apr", "May", "Jun"))) %>%
    dplyr::mutate(label = .data$date_of_month)

}
