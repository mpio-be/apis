
# DRUID Ecotopia API

#' ecotopia login token
#'
#' @param un   username
#' @param pwd  password
#'
#' @return a character string
#' @export
#' @examples 
#' crd = config::get(config = "druid_api")
#' logstring = ecotopia_login(crd$generic$un, crd$generic$pwd, crd$kw1,  crd$kw2)
#' 
ecotopia_login <- function(un, pwd, kw1, kw2) {

  auth <- request("https://www.ecotopiago.com/api/v2/login") |>
    req_body_json(list(
      username = un,
      password = sha256(glue("{un} + {kw1} + {pwd} + {kw2}")) |> as.character()
    )) |>
    req_perform()

  auth$headers$`x-druid-authentication`

}


#' List all devices for the given login profile
#'
#' @param logstring   login string as returned by [apis::ecotopia_login]
#' @param cleanup   when FALSE (default) returns a cleaned up data.table removing all unnecessary columns. 
#'
#' @return a data.table
#' @export
#' @examples
#' crd = config::get(config = "druid_api")
#' logstring <- ecotopia_login(crd$generic$un, crd$generic$pwd, crd$kw1, crd$kw2)
#' dl = ecotopia_devlist(logstring)
ecotopia_devlist <- function(logstring, cleanup = TRUE) {

  x <- request("https://www.ecotopiago.com/api/v3/device/page") |>
    req_headers(
      "X-Druid-Authentication" = logstring,
      "X-result-limit" = 1000
    ) |>
    req_perform() |>
    resp_body_json()

  o = lapply(x, data.frame) |>
    rbindlist(fill = TRUE)

  keep = c("id", "uuid", "register_time","status_gps.longitude", "status_gps.latitude", "status_gps.timestamp","model")

  if(cleanup)
  o = o[, keep, with = FALSE]

  o
}


#' Download gps data of a given id after a given datetime
#'
#' @param logstring   login string as returned by [apis::ecotopia_login]
#' @param cleanup   when FALSE (default) returns a cleaned up data.table removing all unnecessary columns. 
#'
#' @return a data.table
#' @export
#' @examples
#' crd = config::get(config = "druid_api")
#' logstring <- ecotopia_login(crd$generic$un, crd$generic$pwd, crd$kw1, crd$kw2)
#' dl = ecotopia_devlist(logstring)
#' 
ecotopia_gps <- function(logstring, id, datetime = "2000-01-01T00:00:00Z", cleanup = TRUE) {

  x = request("https://www.ecotopiago.com/api/v2/gps/device") |>
    req_headers(
      "X-Druid-Authentication" = logstring
    ) |>
    req_url_path_append(glue("/{id}/page/")) |>
    req_url_path_append(datetime) |>
    req_perform() |>
    resp_body_json()

  x




}
