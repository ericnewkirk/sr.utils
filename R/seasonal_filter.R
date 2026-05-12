#' Seasonal Filter
#'
#' Filters a data frame or similar object by date regardless of year
#'
#' @details
#' Start and end dates are inclusive, and filtering is performed on
#' julian day to correctly align filters with calendar dates. This means
#' one additional day may be included in leap years depending on whether
#' February 29 falls between the start and end dates specified. If
#' \code{start} and \code{end} are the same only records from that date
#' are returned. This function is timezone agnostic. Times are ignored
#' if \code{start} and/or \code{end} are provided as \code{POSIX} objects.
#'
#' @param x data object to filter
#' @param date_col column to use for filtering (unquoted a la \code{dplyr})
#' @param start \code{Date} or \code{POSIX} start date in any year
#' @param end \code{Date} or \code{POSIX} end date in any year
#'
#' @return an object of the same class as \code{x}
#' @export
#'
#' @examples
#'
#' library(tibble)
#'
#' df <- tibble(
#'   name = letters[1:5],
#'   timestamp = as.POSIXct(c(
#'     "2020-01-01 08:00:00",
#'     "2022-10-05 11:15:00",
#'     "2023-12-10 19:30:00",
#'     "2024-08-31 00:00:00",
#'     "2024-05-25 02:45:00"
#'   )),
#'   x = 42 + runif(5, 0, 1),
#'   y = -107 + runif(5, 0, 1)
#' )
#'
#' start <- as.Date("1900-10-01")
#' end <- as.Date("2030-05-01")
#'
#' df |>
#'   seasonal_filter(timestamp, start, end)
#'
seasonal_filter <- function(x, date_col, start, end) {

  if (nrow(x) == 0) {
    return(x)
  }

  # check that filter args are correct class
  stopifnot(
    "start must be a POSIX or Date" = inherits(start, c("POSIXt", "Date")),
    "end must be a POSIX or Date" = inherits(end, c("POSIXt", "Date"))
  )

  `!!` <- rlang::`!!`

  date_col <- rlang::enquo(date_col)

  # determine date order
  date_order <- diff(lubridate::yday(c(start, end)))

  # convert POSIX arguments to date
  start <- as.Date(start)
  end <- as.Date(end)

  if (date_order >= 0) {
    # if start comes before end, filter to records between start and end

    dplyr::filter(
      x,
      lubridate::yday(!!date_col) >=
        lubridate::yday(
          lubridate:::update.Date(start, year = lubridate::year(!!date_col))
        ),
      lubridate::yday(!!date_col) <=
        lubridate::yday(
          lubridate:::update.Date(end, year = lubridate::year(!!date_col))
        )
    )

  } else {
    # if start comes after end, filter to records after start or before end

    dplyr::filter(
      x,
      lubridate::yday(!!date_col) >=
        lubridate::yday(
          lubridate:::update.Date(start, year = lubridate::year(!!date_col))
        ) |
      lubridate::yday(!!date_col) <=
        lubridate::yday(
          lubridate:::update.Date(end, year = lubridate::year(!!date_col))
        )
    )

  }

}
