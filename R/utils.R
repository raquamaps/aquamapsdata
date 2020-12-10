#' Ping a web site
#'
#'@param url a web site to "web ping" to see that it is available
#'@param timeout default time to wait in seconds before returning FALSE
#'@return TRUE if web site is up, FALSE otherwise
#'@importFrom R.utils withTimeout
#'@export
#'@examples \dontrun{
#'  http_ping("http://aquamaps.org")
#'}
#'
#' @family admin
http_ping <- function(url = "http://aquamaps.org", timeout = 10) {
  req <- withTimeout({
    httr::GET(url)
  }, timeout = timeout, onTimeout = "warning")

  if (is.null(req)) return(FALSE)
  return(req$status_code == 200)
}

#' @noRd
#' @family admin
has_sqlite3 <- function() {
  if (Sys.which("sqlite3") == "") {
    warning("Cannot find sqlite3 database engine. Please install it.")
    return(FALSE)
  }
  return(TRUE)
}
