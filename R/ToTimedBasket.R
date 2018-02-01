#' Basket creating function, taking care of time
#'
#' This function is not to be used directly, it is called by df2spmfBasket 
#' @param x a dataframe which must contains variable ID, itemset, eventNum and time
#' @keywords baskets
#' @export
#' @examples
#' ToTimedBasket()



ToTimedBasket<-function(x){x %>% group_by(ID) %>% 
    mutate(timeref=min(time),time=time-timeref,itemset=itemset,eventNum=eventNum) %>% 
    select(ID,time,eventNum,itemset) %>% 
    group_by(ID,time) %>%
    mutate(time2=paste("<",toString(unique(time)),">",collapse="")) %>%
    group_by(ID,itemset)  %>% summarise(basket=paste(sort(eventNum),collapse=" "),time=unique(time2))  %>% 
    group_by(ID,itemset) %>% summarise(basket=paste(time,toString(basket),collapse=" ")) %>% ungroup() 
}                                           

                                  
