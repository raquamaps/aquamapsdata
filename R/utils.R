#' Ping a web site
#'@param url a web site to "web ping" to see that it is available
#'@return TRUE if web site is up, FALSE otherwise
#'@importFrom R.utils withTimeout
#'@export
#'@examples
#'http_ping("http://aquamaps.org")
http_ping <- function(url = "http://aquamaps.org", timeout = 10) {
  req <- withTimeout({
    httr::GET(url)
  }, timeout = timeout, onTimeout = "warning")

  if (is.null(req)) return (FALSE)
  return(req$status_code == 200)
}
