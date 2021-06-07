#' Queries historical exchange rates for any 'base' and 'symbol' currency pair. When no params passed check EUR - HUF
#' @param base_arg base currency argument
#' @param symbol_arg to which symbol argument
#' @param start_date date
#' @param end_date date
#' @param retried number of times the function has been retried
#' @return \code{data.table} object
#' @export
#' @importFrom logger log_error log_info
#' @importFrom checkmate assert_numeric
#' @importFrom data.table data.table
#' @importFrom httr GET content
#' @examples
#' get_exchange_rates()
#' get_exchange_rates('EUR', 'USD', start_date = '2020-05-12', end_date = '2020-05-13')

get_exchange_rates <- function(base_arg = 'EUR', symbol_arg = 'HUF', start_date = Sys.Date() - 30, end_date = Sys.Date(), retried = 0){
  tryCatch({
    response <- GET(
      'https://api.exchangerate.host/timeseries',
      query = list(
        start_date = start_date,
        end_date = end_date,
        base = base_arg,
        symbols = symbol_arg
      )
    )
    # stop_for_status(response)
    exchange_rates <- content(response)$rates
    base_to_symbols <- data.table(
      date = as.Date(names(exchange_rates)),
      rate = as.numeric(unlist(exchange_rates)))
    assert_numeric(base_to_symbols$rate)
  }, error = function(e) {
    log_error(e$message)
    Sys.sleep(1 + retried ^ 2)
    get_exchange_rates(base_arg = "EUR", symbol_arg = "HUF", start_date = Sys.Date() - 30, end_date = Sys.Date(), retried = retried + 1)
  })
  base_to_symbols
}
