#' Sequence creating function, taking care of time
#'
#' This function is not to be used directly, it is called by df2spmfBasket 
#' @param x a dataframe which must contains variable ID, itemset, eventNum and time
#' @param timeStep if time is taken into account in the building of the sequence, this parameter allows to perform an integer division. For instance, if your time is in days but you wish to perform a time constrained sequence mining grouping relying on weeks, timeStep will be 7. 
#' @param timeUnit this parameter will be used in future versions
#' @param timeFormat this parameter is inherited from the df2SPMFSequence function
#' @keywords baskets
#' @export
#' @examples
#' ToSequenceBasket()

ToTimedSequence<-function(x,timeStep=timestep,timeUnit=timeUnit,timeFormat=timeFormat){
  
    if(timeFormat!="number"){
      print("Dans ToTimeSequence time format is not a number")
      x$time<-as.POSIXct(x$time)
      x %>% group_by(ID) %>% 
      mutate(timeref=min(time),time=as.numeric(difftime(time,timeref,unit=timeUnit))%/%timeStep,itemset=itemset,eventNum=eventNum) %>% 
      arrange(ID,time) %>% 
      select(ID,time,eventNum,itemset) %>% ungroup() %>% 
      group_by(ID,time) %>%  arrange(ID,time) %>% 
      mutate(time2=paste("<",toString(unique(time)),">",collapse="",sep="")) %>% ungroup() %>% 
      group_by(ID,time)  %>%  summarise(basket=paste(as.character(sort(unique(eventNum))), collapse=" "),time2=unique(time2))  %>%
      mutate(basket=paste(time2,basket)) %>% 
      group_by(ID) %>% 
      summarise(sequence=paste(basket,collapse=" -1 ")) %>% mutate(sequence=paste(sequence,"-2"))
      }
   else{
     x %>% group_by(ID) %>% 
       mutate(timeref=min(time),time=as.numeric(time-timeref)%/%timeStep,itemset=itemset,eventNum=eventNum) %>% 
       arrange(ID,time) %>% 
       select(ID,time,eventNum,itemset) %>% ungroup() %>% 
       group_by(ID,time) %>%  arrange(ID,time) %>% 
       mutate(time2=paste("<",toString(unique(time)),">",collapse="",sep="")) %>% ungroup() %>% 
       group_by(ID,time)  %>%  summarise(basket=paste(as.character(eventNum), collapse=" "),time2=unique(time2))  %>%
       mutate(basket=paste(time2,basket)) %>% 
       group_by(ID) %>% 
       summarise(sequence=paste(basket,collapse=" -1 ")) %>% mutate(sequence=paste(sequence,"-2"))
   }
}



