
# DRUID Ecotopia API

#' ecotopia login token
#'
#' @param un   username
#' @param pwd  password
#' @param verbose      print [httr2::request] string. Default to TRUE.
#'
#' @return a character string
#' @export
#' @examples 
#' crd = config::get(config = "druid_api")
#' logstring = ecotopia_login(crd$generic$un, crd$generic$pwd, crd$kw1,  crd$kw2)
#' 
ecotopia_login <- function(un, pwd, kw1, kw2, verbose = TRUE) {

  x <- request("https://www.ecotopiago.com/api/v2/login") |>
    req_body_json(list(
      username = un,
      password = sha256(glue("{un} + {kw1} + {pwd} + {kw2}")) |> as.character()
    )) 
  
  if(verbose) print(x)

  o = req_perform(x)

  o$headers$`x-druid-authentication`

}


#' List all devices for the given login profile
#'
#' @param logstring   login string as returned by [apis::ecotopia_login]
#' @param verbose      print [httr2::request] string. Default to TRUE.
#'
#' @return a data.table
#' @export
#' @examples
#' crd = config::get(config = "druid_api")
#' logstring <- ecotopia_login(crd$generic$un, crd$generic$pwd, crd$kw1, crd$kw2)
#' dl = ecotopia_devlist(logstring)
ecotopia_devlist <- function(logstring, verbose = TRUE) {

  x <- request("https://www.ecotopiago.com/api/v3/device/page") |>
    req_headers(
      "X-Druid-Authentication" = logstring,
      "X-result-limit" = 1000
    )

  if (verbose) print(x)

  req_perform(x) |>
    resp_body_json(simplifyVector = TRUE) |>
    as.data.table()


}


.ecotopia_gps <- function(logstring, id, datetime = "2000-01-01T00:00:00Z", verbose = TRUE) {
  x <- request("https://www.ecotopiago.com/api/v2/gps/device") |>
    req_headers(
      "X-Druid-Authentication" = logstring,
      "X-result-limit" = 1000
    ) |>
    req_url_path_append(glue("/{id}/page/")) |>
    req_url_path_append(datetime)

  if (verbose) print(x)

  o <- req_perform(x) |>
    resp_body_json(simplifyVector = TRUE) |>
    as.data.table()

  # remove unnecessary columns
  if (nrow(o) > 0 )
  o <- o[, .(device_id, uuid, updated_at, timestamp, latitude, longitude, altitude, hdop, vdop)]
  

  o
}

#' Download gps data of a given id after a given datetime
#'
#' @param logstring login string as returned by [apis::ecotopia_login]
#' @param cleanup   when FALSE (default) returns a cleaned up data.table removing all unnecessary columns. 
#' @param verbose   print [httr2::request] string. Default to TRUE.
#'
#' @return a data.table
#' @export
#' @examples
#' crd = config::get(config = "druid_api")
#' logstring <- ecotopia_login(crd$generic$un, crd$generic$pwd, crd$kw1, crd$kw2)
#' dl = ecotopia_devlist(logstring)
#' x =  ecotopia_gps(logstring, dl$id[100])
#' 
ecotopia_gps <- function(logstring, id, datetime = "2000-01-01T00:00:00Z", verbose = TRUE) {

  x = .ecotopia_gps(logstring = logstring, id = id, datetime = datetime, verbose = verbose)

  if(nrow(x) > 0) {
    last_timestamp = ymd_hms(x$timestamp) |> with_tz("UTC")
    last_timestamp = x[which(last_timestamp == max(last_timestamp)), timestamp]

    x_next = .ecotopia_gps(logstring = logstring, id = id, datetime = last_timestamp, verbose = FALSE)

    if(nrow(x_next == 0)) {
      break
    }

    x = rbind(x, x_next)
  }

  x

}
