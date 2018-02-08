#' Sequence creating function
#'
#' This function is called directly by the user to create sequences from a dataframe, compliant with SPMF formats. It calls the functions ToSequence or ToTimedSequence. It outputs a list with two elements : toSendSPMF contains a variable sequence respecting the input format for SPMF frequent sequence mining, with or without time. The second element, evLev is the matching table to the original item names.
#' @param df a data frame from which to create sequences
#' @param ID the name of the column of IDs, the sequences are built for a given ID
#' @param itemset the name of the column of itemsets, that is of the product bought together. You need to provide at least one of itemset or time parameters
#' @param time the name of the column where the time of an event is stored.You need to provide at least one of itemset or time parameters
#' @param timeFormat the format in which the time column is encoded (example "%d-%m-%Y") If provided df2SPMFBasket will assume you want time to be taken into account. To build the proper format, please refer the man page of strptime (via ?strptime)
#' @param timestep an integer by witch you can divide the time at which an event occurs in a sequence. If your times are expressed in days, setting timestep to 7 will express this delay in weeks, grouping de facto all items of the same week (slideing 7 days from the first item)
#' @param timeUnit the time unit in which time diff will be rendered in timed sequences.
#' @param parallel if TRUE, then the function will use all the cores of your system and parallelize the creation of your baskets. Default is F because the gain depends on the number of cores and the length of the dataframe
#' @return df2SPMFSequence returns a list. The toSendSPMF element contains a tibble/dataframe whose slot sequence contains all the sequences in the proper format to export them to a txt file readable by the spmf java library
#' @keywords
#' @examples seqDF is a dataframe to test the functions. It contains the variables ID, jour, ITEMSETS and PRODUITSnum to be used as an example.
#' @examples test<-df2SPMFSequence(seqDF,ID="ID",time="jour",event="PRODUITSnum",itemset="ITEMSETS",
#' @examples timeFormat="%d",parallel = F)
#' @export



df2SPMFSequence<-function(df,ID,itemset="",event="",time="",timeFormat="",timestep=1,parallel=F,timeUnit="auto"){
  require(tidyverse)
  lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C") # locale time set to a usable format

  print(paste("timestep vaut", timestep))
  if (!is.data.frame(df)|!tibble::is.tibble(df)){stop("Input must be a data frame or a tibble")} # test df2SPMFSequence(c(1,2))
  else {
    #récupération des arguments entrés par l'utilisateur

    if (nchar(ID)<1|nchar(event)<1|(nchar(itemset)<1&nchar(time)<1)){
      print("Arguments ID and event are compulsary, and at least one of time or itemset")} else{
      # sélection des colonnes correspondant aux variables d'intérêt
      subVar<-c(ID,itemset,event,time)[sapply(c(ID,itemset,event,time),nchar)>1]
      x<-df[,subVar]
      names(x)<-c("ID","itemset","event","time")[sapply(c(ID,itemset,event,time),nchar)>1]
      # recodage numérique des produits
      x$eventNum<-as.numeric(x$event)
      evLev<-evLev<-levels(x$event)

      #if (nchar(time)<1&nchar(itemset)>1){x$time<-x$itemset} # reconstruction du temps à partir de l'itemset et réciproque
      if(nchar(itemset)<1&nchar(time)>1){x$itemset<-x$time}
      if (nchar(timeFormat)>1){print("timeformat non vide")
        if(timeFormat=="number"){x$time<-as.numeric(unlist(x$time))}else
        {print("timeformat not number");x$time<-strptime(x$time,format=timeFormat);print(x$time[1:2])}}


      ### Clusterisation pour gros fichiers





      if (parallel==T & length(unique(x$ID))>8){

        library(parallel)
        cl <- detectCores()
        cl
        #création des groupes pour cluster
        x$originalID<-x$ID
        x$ID<-as.numeric(as.factor(x$OriginalID))
        max_ID<-max(x$ID)
        print(max_ID)
        nbGroups<-max_ID%/%cl
        group<-pmin(x$ID%/%nbGroups+1,cl)
        xG<-cbind(group=group,x)

        #création des clusters
        require(multidplyr)
        cluster <- create_cluster(cores = cl)
        #chargement des packages
        xByGroup <- xG%>%partition(group,cluster=cluster)
        xByGroup %>%  cluster_library("dplyr")# assigner le package à chaque cluster
        #cluster_eval(xByGroup,search())
        #chargement des fonctions
        cluster_copy(xByGroup,ToSequence)
        cluster_copy(xByGroup,ToTimedSequence)

        ## Exécuter le code par clusters
        if (nchar(timeFormat)<1){
          print("pas de prise en compte du temps")
          y<-xByGroup %>% do(ToSequence(.)) %>%
            collect() %>% arrange(ID)
        } else{
          print("avec prise en compte du temps")
          y<-xByGroup %>% do(ToTimedSequence(.,timeStep=timestep,timeUnit=timeUnit,timeFormat=timeFormat)) %>%
            collect() %>% arrange(ID)
        }
      } else{  #### SANS PARALLELISATION
        if (nchar(timeFormat)<1){y<-x %>% ToSequence()} else{
          y<-x %>% ToTimedSequence(timeStep=timestep,timeUnit=timeUnit,timeFormat=timeFormat)
        }
      }
    }
  }
  Sys.setlocale("LC_TIME",lct)
  sortie<-list(toSendSPMF=y,evLev=evLev)
}

