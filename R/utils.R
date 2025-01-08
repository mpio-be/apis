
.isOnline <- function(url) {
  tryCatch({
    curl_fetch_memory(url)
    return(TRUE)
  }, error = function(e) {
    message(url, ' is not online.')
    return(FALSE)
  })
}

#' Convert Between ISO 8601 Timestamp Strings and POSIXct Objects
#'
#' These functions facilitate the conversion between ISO 8601 timestamp strings and POSIXct objects.
#'
#' @details
#' - `from_timestamp()` converts an ISO 8601 timestamp string to a POSIXct object in the UTC timezone.
#' - `to_timestamp()` converts a POSIXct object to an ISO 8601 timestamp string in UTC format.
#'
#' @param x For `from_timestamp()`, a character string representing a timestamp in ISO 8601 format 
#' (e.g., "2024-12-07T06:01:00Z"). For `to_timestamp()`, a POSIXct object.
#'
#' @return 
#' - `from_timestamp()`: A POSIXct object representing the given timestamp in the UTC timezone.
#' - `to_timestamp()`: A character string in ISO 8601 format representing the given POSIXct object.
#'
#' @examples
#' # Convert a timestamp string to a POSIXct object
#' posix_time <- from_timestamp("2024-12-07T06:01:00Z")
#'
#' # Convert a POSIXct object back to an ISO 8601 timestamp string
#' iso_time <- to_timestamp(posix_time)
#'
#' # Check if conversion and reverse conversion match the original string
#' identical("2024-12-07T06:01:00Z", from_timestamp("2024-12-07T06:01:00Z") |> to_timestamp())
#'
#' @export
from_timestamp <- function(x) {
  with_tz(ymd_hms(x), "UTC")
}

#' @rdname from_timestamp
#' @export
to_timestamp <- function(x) {
  format(x, format = "%Y-%m-%dT%H:%M:%SZ")
}
