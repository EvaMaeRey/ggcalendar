geom_text_calendar <- stat_calendar
geom_point_calendar <- function(...){stat_calendar(geom = "point", ...)}
geom_tile_calendar <- function(...){stat_calendar(geom = "tile", ...)}
