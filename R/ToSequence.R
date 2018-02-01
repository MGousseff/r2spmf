#' Sequence creating function
#'
#' This function is not to be used directly, it's called by df2spmfSequence
#' @param x a dataframe which must contains variable ID, itemset, eventNum, and possibly time
#' @keywords baskets
#' @export
#' @examples ToBasket()

ToSequence<-function(x){x %>%  group_by(ID,itemset) %>% 
    summarise(basket=paste(as.character(eventNum),collapse=" ")) %>% ungroup() %>% group_by(ID) %>% 
    summarise(sequence=paste(basket,collapse=" -1 ")) %>% mutate(sequence=paste(sequence,"-2"))
}


