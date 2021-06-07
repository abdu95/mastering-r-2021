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
#' @examples
#' get_usdhufs()
#' get_usdhufs(start_date = "2021-04-18", end_date = "2021-05-18")
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
    # stop_for_status(response)
    exchange_rates <- content(response)$rates
    usdhufs <- data.table(
      date = as.Date(names(exchange_rates)),
      usdhuf = as.numeric(unlist(exchange_rates)))
    assert_numeric(usdhufs$usdhuf, lower = 250, upper = 400)
  }, error = function(e) {
    log_error(e$message)
    Sys.sleep(1 + retried ^ 2)
    get_usdhufs(start_date = start_date, end_date = end_date, retried = retried)
  })
  usdhufs
}
