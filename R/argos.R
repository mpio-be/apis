# Argos API

#' Argos login token
#'
#' @param un Username to authenticate with the server.
#' @param pwd Password associated with the username.
#' @param wsdl_server One or more WSDL server addresses. If multiple addresses are provided, they will be tried in the specified order.
#' @note For security, credentials should either be stored in a configuration file or entered interactively at runtime. 
#' Avoid hardcoding credentials directly in your scripts. See examples for more details.
#' @return a character vector.
#' @export
#' @examples 
#' \dontrun{
#' crd = config::get(config = "argos_api")
#' login = argos_login(un = crd$un, pwd = crd$pwd, wsdl_server = crd$wsdl_server)
#' }
argos_login <- function(un, pwd, wsdl_server) {

  if(.isOnline(wsdl_server[1])) {
    wsdl =   wsdl_server[1]
  }  else {
    wsdl =   wsdl_server[2]
  }

  list(un = un, pwd = pwd, wsdl = wsdl)


}


#' @title Argos Device List
#' @description This function retrieves a list of devices from the Argos API.
#' @param login A login object obtained from the `argos_login` function.
#' @return A data.table containing the list of devices.
#' @export
#' @examples 
#' \dontrun{
#' crd = config::get(config = "argos_api")
#' login = argos_login(un = crd$un, pwd = crd$pwd, wsdl_server = crd$wsdl_server)
#' x = argos_devlist(login)
#' }
argos_devlist <- function(login) {

  soap = 
  glue('<soap:Envelope xmlns:soap="http://www.w3.org/2003/05/soap-envelope" xmlns:typ="http://service.dataxmldistribution.argos.cls.fr/types">
    <soap:Header/>
    <soap:Body>
        <typ:platformListRequest>
          <typ:username>{login$un}</typ:username>
          <typ:password>{login$pwd}</typ:password>
        </typ:platformListRequest>
    </soap:Body>
  </soap:Envelope>')

  o = 
    request(login$wsdl) |>
    req_body_raw(soap)  |>
    req_perform()       |>
    resp_body_string()

  
  o = str_split(o, "<return>",  simplify = TRUE)[2]
  o = str_split(o, "</return>", simplify = TRUE)[1]

  o = str_replace_all(o, "&lt;","<" )
  o = str_replace_all(o, "&gt;",">" )

  o  = read_xml(o)

  x = data.table(
    platformId = xml_find_all(o, ".//platformId") |>
      xml_text()
  )
  x  


}


#' Retrieve Argos API Data for a Specific Platform ID
#'
#' This function sends a SOAP request to the Argos API to fetch data associated with a given platform ID.
#' The retrieved data includes platform locations, diagnostic messages, sensor readings, and other relevant information.
#'
#' @param login A list containing authentication credentials and API information, including the username, password, and the WSDL server URL.
#' @param platformId A character string representing the ID of the platform for which data is being requested.
#' @param startDate observation date begin of search and end of search. Only the 10 last days are available. If endDate is not specified, the current date is assumed.  
#'
#' @return A `data.table` containing the retrieved platform data. If an error occurs during the data retrieval process, an empty `data.table` is returned.
#' 
#' @note The function is limited to retrieving data from the last 20 days, as enforced by the Argos system.

#'
#' @export
#' @examples 
#' \dontrun{
#' crd = config::get(config = "argos_api")
#' login = argos_login(un = crd$un, pwd = crd$pwd, wsdl_server = crd$wsdl_server)
#' start_date = (with_tz(Sys.time(), tzone = "UTC") - days(1) ) |> to_timestamp()
#' x = argos_data(login, platformId=266471, startDate =  start_date)
#' }
#' 
# 2025-01-08T14:09:24

argos_data <- function(login, platformId, startDate  ) {


  soap = 
  glue('<soap:Envelope xmlns:soap="http://www.w3.org/2003/05/soap-envelope" xmlns:typ="http://service.dataxmldistribution.argos.cls.fr/types">
    <soap:Header/>
    <soap:Body>
      <typ:csvRequest>
        <typ:username>{login$un}</typ:username>
        <typ:password>{login$pwd}</typ:password>
        <typ:platformId>{platformId}</typ:platformId>
        <typ:period><typ:startDate>{startDate}</typ:startDate></typ:period>
        <typ:referenceDate>MODIFICATION_DATE</typ:referenceDate>
        <typ:displayDiagnostic>true</typ:displayDiagnostic>
        <typ:displayMessage>true</typ:displayMessage>
        <typ:displaySensor>true</typ:displaySensor>
        <typ:showHeader>true</typ:showHeader>
      </typ:csvRequest>
    </soap:Body>
  </soap:Envelope>
  ')

  response = request(login$wsdl) |>
    req_body_raw(soap) |>
    req_perform()

  o =
    response |>
    resp_body_string()

  o = str_split(o, "<return>",  simplify = TRUE)[2]
  o = str_split(o, "</return>", simplify = TRUE)[1]

  if( str_detect(o, "error code") ) {
    x = data.table()

  } else {

    x = fread(o, sep = ";", fill = TRUE)
    x[, !grepl("^V", names(x)), with = FALSE]

  }

  x

}
