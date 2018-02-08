#' A function to create sequences ID according to the delay between two events
#' 
#' Input is a data frame and affects a sequence ID to the events related to an ID, built on the time elapsed between an event and thi previous event. 
#' @param df is a dataframe
#' @param ID is the ID, for instance of a customer. Within this ID the function will create one or several sequences, according to time between events. 
#' @param time is the time of the event
#' @param timeFormat is the format in which the time is encoded
#' @param timeMax if the time elapsed between two events is more than timeMax, a new sequence is started
#' @param timeMin if the time elapsed between two events is less than timeMin, these events are considered to happen together : they belong to the same itemset
#' @param parallel if True the function will use the several cores available to split the building of the sequence. It appears to be useful above 1 million of lines, depending, of course, of the specs of the computer. 
#' @return a dataframe with three columns. sequence contains all the frequent sequences. support is the number of times this sequence occurs, and frequence is support divided by the total number of sequences
#' @keywords 
#' @examples 
#'
#' @export 
#' 

TransactionBuilder<-function(df, ID, event, time, timeFormat, timeMax="",timeMin=0,timeUnit="secs",parallel=F ){
   
require(tidyverse)
lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C") # locale time set to a usable format
subVar<-c(ID,event,time)[sapply(c(ID,event,time),nchar)>1]
print(subVar)
x<-df[,subVar] 
names(x)<-c("ID","event","time")
print(names(x))
print(paste("nombre total d'IDs uniques",length(unique(x$ID))))
timeFormat<-timeFormat
timeMax<-timeMax
timeMin<-timeMin
x<-x %>% arrange(ID)

#x$originalID<-x$ID
#x$ID<-as.numeric(as.factor(x$originalID))

if (parallel==T){ 
  library(parallel)
  cl <- detectCores()
  cl
  #création des groupes pour cluster
  
  x$IDnum<-as.numeric(as.factor(x$ID))
  print("premières ID");print(head(x$ID))
  max_ID<-length(unique(x$IDnum))
  nbGroups<-max_ID%/%cl
  group<-pmin(x$IDnum%/%nbGroups+1,cl)
  xG<-cbind(group=group,x)
  
  #création des clusters
  require(multidplyr)
  cluster <- create_cluster(cores = cl)
  #chargement des packages
  xByGroup <- xG%>%partition(group,cluster=cluster)
  xByGroup %>%  cluster_library("dplyr") # assigner le package à chaque cluster
  cluster_copy(xByGroup,TransactionBuilder)  # copie de la fonction elle même, qu'on appellera alors par cluster avec l'argument parallele=F
  cluster_copy(xByGroup,timeFormat)
  cluster_copy(xByGroup,timeMin)
  cluster_copy(xByGroup,timeMax)
  
  y<-xByGroup %>% do(TransactionBuilder(.,ID="ID", event="event", time="time", timeFormat=timeFormat, timeMax=timeMax,timeMin=timeMin,parallel=F)) %>%   
    collect() %>% ungroup() %>% arrange(ID,time)
  
  return(y)
  }
if(parallel==F){
now=Sys.time()
zeroSec<-difftime(now,now,units=timeUnit) 
y<-x %>% select(ID,event,time) %>% group_by(ID) %>%  mutate(time=as.character(time)) %>% 
  mutate(time=as.POSIXct(strptime(time,format=timeFormat))) %>%  
  arrange(time) %>% mutate(IDTimeDiff=c(zeroSec,diff(time))) %>% ungroup()
# Time Unit Management for time diffs within IDs
automTimeUnit<-units(y$IDTimeDiff)
if(automTimeUnit!=timeUnit){units(y$IDTimeDiff)<-timeUnit}

y<-y %>% group_by(ID) %>%  mutate(seqThresh=IDTimeDiff>=timeMax) %>% # computes time between events within an ID
  mutate(seq=cumsum(seqThresh)+1)%>%  mutate(uniqueSeqID=paste(ID,seq)) %>% # creates uniqueSeqIds not interrupted by a time > timeMax
  ungroup() %>% group_by(uniqueSeqID)  %>% arrange(time) %>% 
  mutate(seqTimeDiff=c(zeroSec,diff(time))) # creates time diffs within the new created sequences

# Time Unit Management for time diffs within sequences
automTimeUnit<-units(y$seqTimeDiff)
if(automTimeUnit!=timeUnit){units(y$seqTimeDiff)<-timeUnit}

 y<-y %>% mutate(itemsetThresh=seqTimeDiff>timeMin) %>% mutate(itemset=cumsum(itemsetThresh)+1) %>% 
  ungroup() %>% 
  select(ID,event,time,IDTimeDiff,seqTimeDiff,itemsetThresh,itemset,seqThresh,seq,uniqueSeqID) %>% arrange(ID,time) 
#Sys.setlocale(lct)
#conversion d'unités de temps


return(y)
}

return(y)
}
