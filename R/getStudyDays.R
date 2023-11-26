#' getStudyDaysElements
#'
#' Getting study days
#' 
#'
#' @param x 함수의 input parameter
#' @import dplyr
#' @import lubridate
#' @import glue
#' @export
getStudyDaysElements <- function(s, e, i) {
    x <- dplyr::tibble(start_time = seq.Date(
        from = s,
        to = e,
        by = i
    ))
    x <- x %>%
        dplyr::mutate(isoweek = lubridate::isoweek(.data$start_time)) %>%
        dplyr::mutate(month = lubridate::month(.data$start_time)) %>%
        dplyr::mutate(quarter = quarters(.data$start_time)) %>%
        dplyr::mutate(year = lubridate::year(.data$start_time)) %>%
        dplyr::mutate(years = glue::glue("{year}")) %>%
        dplyr::mutate(months = dplyr::if_else(.data$month < 10,
                                              paste0(.data$year, "_0", .data$month),
                                              paste0(.data$year, "_", .data$month)
        )) %>%
        dplyr::mutate(quarters = glue::glue("{year}_{quarter}")) %>%
        dplyr::mutate(
            year =
                dplyr::if_else(.data$month == 1 & .data$isoweek > 50,
                               .data$year - 1,
                               .data$year
                )
        ) %>%
        dplyr::mutate(weeks = dplyr::if_else(.data$isoweek < 10,
                                             paste0(.data$year, "_0", .data$isoweek),
                                             paste0(.data$year, "_", .data$isoweek)
        ))
    return(x)
}
