
# DRUID Ecotopia API

#' ecotopia login token
#'
#' @param un   username
#' @param pwd  password
#' @param kw1  keyword 1
#' @param kw2  keyword 2
#' @param verbose      print [httr2::request] string. Default to TRUE.
#' @note For security, credentials should either be stored in a configuration file or entered interactively at runtime.
#' Avoid hardcoding credentials directly in your scripts. See examples for more details.
#' @return a character string
#' @export
#' @examples
#' \dontrun{
#' crd = config::get(config = "druid_api")
#' logstring = ecotopia_login(crd$generic$un, crd$generic$pwd, crd$kw1, crd$kw2)
#' }
#' 
ecotopia_login <- function(un, pwd, kw1, kw2, verbose = TRUE) {
  x <- request("https://www.ecotopiago.com/api/v2/login") |>
    req_body_json(list(
      username = un,
      password = sha256(glue("{un} + {kw1} + {pwd} + {kw2}")) |> as.character()
    ))

  if (verbose) print(x)

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
#' \dontrun{
#' crd = config::get(config = "druid_api")
#' logstring <- ecotopia_login(crd$generic$un, crd$generic$pwd, crd$kw1, crd$kw2)
#' dl = ecotopia_devlist(logstring)
#' }
#' 
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


.ecotopia_data <- function(logstring, id, datetime, what, verbose) {
  type = switch(what,
    gps = "gps",
    odba = "behavior2",
    env = "env",
    sms = "sms"
  )

  x <- request(glue("https://www.ecotopiago.com/api/v2/{type}/device")) |>
    req_headers(
      "X-Druid-Authentication" = logstring,
      "X-result-limit" = 1000, 
      "X-result-sort" = "timestamp"
    ) |>
    req_url_path_append(glue("/{id}/page/")) |>
    req_url_path_append(datetime)

  if (verbose) print(x)

  o <- req_perform(x) |>
    resp_body_json(simplifyVector = TRUE) |>
    as.data.table()

  # remove unnecessary columns
  if (nrow(o) > 0 & type == "gps") {
    o <- o[, .(device_id, uuid, updated_at, timestamp, latitude, longitude, altitude, hdop, vdop)]
  }

  if (nrow(o) > 0 & type == "behavior2") {
    o <- o[, .(device_id, uuid, updated_at, timestamp, odba)]
  }

  if (nrow(o) > 0 & type == "env") {
    o <- o[, .(device_id, uuid, updated_at, timestamp, ambient_light, inner_pressure, battery_power, battery_voltage)]
  }


  o
}

#' Download gps data of a given id after a given datetime
#'
#' @param logstring login string as returned by [apis::ecotopia_login]
#' @param id        device ID (e.g 640dab1c6f2d20ea33538465)
#' @param datetime  datetime string ("2000-01-01T00:00:00Z")
#' @param what      data type: "gps", "odba" , "sms" or "env"
#' @param verbose   print [httr2::request] string. Default to TRUE.
#'
#' @return a data.table
#' @export
#' @examples
#' \dontrun{
#' crd = config::get(config = "druid_api")
#' logstring <- ecotopia_login(crd$generic$un, crd$generic$pwd, crd$kw1, crd$kw2)
#' dl = ecotopia_devlist(logstring)
#' x = ecotopia_data(logstring, dl$id[100], what = "gps")
#' x = ecotopia_data(logstring, dl$id[100], what = "odba")
#' x = ecotopia_data(logstring, dl$id[100], what = "sms")
#' }
#' 
ecotopia_data <- function(logstring, id, datetime = "2000-01-01T00:00:00Z", what = "gps", verbose = TRUE) {

  x = .ecotopia_data(logstring = logstring, id = id, datetime = datetime, what = what, verbose = verbose)

  cli_progress_bar("Querying API (1000 rows):", type = "iterator", clear = FALSE)

  while (nrow(x) > 0) {
    cli_progress_update()
    last_timestamp = ymd_hms(x$timestamp) |> with_tz("UTC")
    last_timestamp = x[which(last_timestamp == max(last_timestamp)), timestamp]

    x_next = .ecotopia_data(logstring = logstring, id = id, datetime = last_timestamp, what = what, verbose = FALSE)

    if (nrow(x_next) == 0) {
      break
    }

    x = rbind(x, x_next)
  }
  
  cli_progress_done()

  x

}
