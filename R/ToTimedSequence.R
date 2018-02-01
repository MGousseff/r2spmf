#' Sequence creating function, taking care of time
#'
#' This function is not to be used directly, it is called by df2spmfBasket 
#' @param x a dataframe which must contains variable ID, itemset, eventNum and time
#' @param timeStep if time is taken into account in the building of the sequence, this parameter allows to perform an integer division. For instance, if your time is in days but you wish to perform a time constrained sequence mining grouping relying on weeks, timeStep will be 7. 
#' @keywords baskets
#' @export
#' @examples
#' ToSequenceBasket()

ToTimedSequence<-function(x,timeStep=timestep){x %>% group_by(ID) %>% 
    mutate(timeref=min(time),time=as.numeric((time-timeref))%/%timeStep,itemset=itemset,eventNum=eventNum) %>% 
    select(ID,time,eventNum,itemset) %>% 
    group_by(ID,time) %>%
    mutate(time2=paste("<",toString(unique(time)),">",collapse="",sep="")) %>%
    group_by(ID,itemset)  %>% summarise(basket=paste(as.character(eventNum), collapse=" "),time=unique(time2)) %>% 
    mutate(basket=paste(time,basket)) %>% 
    group_by(ID) %>% 
    summarise(sequence=paste(basket,collapse=" -1 ")) %>% mutate(sequence=paste(sequence,"-2"))
}

