#' Look up the value of a US dollar in Hungarian forints
#' @param retried number of times the function already failed
#' @return number
#' @export
#' @importFrom jsonlite fromJSON
#' @importFrom logger log_error log_info
#' @importFrom checkmate assert_number
#' @examples
#' get_usd(42)
get_usdhuf <- function(retried = 0) {
  tryCatch({
    ## httr
    usdhuf <- fromJSON('https://api.exchangerate.host/latest?base=USD&symbols=HUF')$rates$HUF
    assert_number(usdhuf, lower = 250, upper = 400)
  }, error = function(e) {
    ## str(e)
    log_error(e$message)
    Sys.sleep(1 + retried ^ 2)
    get_usdhuf(retried = retried + 1)
  })
  log_info('1 USD={usdhuf} HUF')
  usdhuf
}


#' Look up the historical value of a US Dollar in Hungarian Forints
#' @param start_date date
#' @param end_date date
#' @inheritParams get_usdhuf
#' @return \code{data.table} object
#' @export
#' @importFrom logger log_error log_info
#' @importFrom checkmate assert_numeric
#' @importFrom data.table data.table
#' @importFrom httr GET content
get_usdhufs <- function(start_date = Sys.Date() - 30, end_date = Sys.Date(), retried = 0) {
  tryCatch({
    response <- GET(
      'https://api.exchangerate.host/timeseries',
      query = list(
        start_date = start_date,
        end_date = end_date,
        base = 'USD',
        symbols = 'HUF'
      )
    )
    exchange_rates <- content(response)$rates
    usdhufs <- data.table(
      date = as.Date(names(exchange_rates)),
      usdhuf = as.numeric(unlist(exchange_rates)))
    assert_numeric(usdhufs$usdhuf, lower = 250, upper = 400)
  }, error = function(e) {
    log_error(e$message)
    Sys.sleep(1 + retried ^ 2)
    get_usdhufs(start_date = start_date, end_date = end_date, retried = retried + 1)
  })
  usdhufs
}


