#' Gets the price of BTC in USD
#' @return number
#' @export
#' @import data.table
#' @importFrom binancer binance_coins_prices
#' @importFrom scales dollar
#' @examples
#' get_bitcoin_price()
get_bitcoin_price <- function() {
  return(paste("The price of a bitcoin:", binance_coins_prices()[symbol == 'BTC', usd], "USD"))
}
