#' Basket creating function
#'
#' This function is not to be used directly, it's called by df2spmfBasket 
#' @param x a dataframe which must contains variable ID, itemset, eventNum, and possibly time
#' @keywords baskets
#' @export
#' @examples ToBasket()


ToBasket<-function(x){x %>%  group_by(ID,itemset) %>% 
    summarise(basket=paste(sort(eventNum),collapse=" ")) %>% ungroup()}
