
.isOnline <- function(url) {
  tryCatch({
    curl_fetch_memory(url)
    return(TRUE)
  }, error = function(e) {
    message(url, ' is not online.')
    return(FALSE)
  })
}