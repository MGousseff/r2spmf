#' A dataset containing dummy sequences of purchases at given times
#' diamonds.
#'
#' @format A data frame with 10000 rows and 4 variables:
#' \describe{
#'   \item{ID}{The customer Identification Number, allows to build a sequence}
#'   \item{ITEMSETS}{the number of the itemset in which the event occurs, here, a product in a basket}
#'   \item{jour}{the day at which the item was purchased, in days, use "\%d" in timeFormat argument to consider it}
#'   \item{PRODUITSnum}{The name of the product bought for a given day/itemset and customer. It would be smarter to have a name, but at this time, it is a fake number product}
#'   
#' }
#' @source \url{http://www.diamondse.info/}
"seqDF"
