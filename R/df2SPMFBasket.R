#' Basket creating function
#'
#' \code{df2SPMFBasket} creates baskets from a dataframe, compliant with SPMF
#' formats.
#'
#' It then calls the functions ToBasket or ToTimedBasket. I outputs a list with
#' two elements : toSendSPMF contains a variable basket respecting the input
#' format for SPMF frequent itemset mining, with or without time. The second
#' element, evLev is the matching table to the original item names.
#'
#' @param df a data frame from which to create baskets
#' @param ID the name of the column of IDs. They allow the grouping by
#'   'customer'.
#' @param itemset the name of the column of itemsets, that is of the product
#'   bought together. You need to provide at least one of itemset or time
#'   parameters.
#' @param time the name of the column where the time of an event is stored. You
#'   need to provide at least one of itemset or time parameters.
#' @param timeFormat the format in which the time column is encoded (example
#'   "\%d-\%m-\%Y") If provided df2SPMFBasket will assume you want time to be taken
#'   into account
#' @param parallel if TRUE, then the function will use all the cores of your
#'   system and parallelize the creation of your baskets. Default is F because
#'   the gain depends on the number of cores and the length of the dataframe.
#' @return df2SPMFBasket returns a list. toSendSPMF contains a dataframe whose
#'   slot basket contains all the basket in the proper format to export them to
#'   a txt file readable by the spmf java library.
#' @keywords baskets
#' @examples seqDF is a dataframe to test the functions. It contains the variables ID, jour, ITEMSETS and PRODUITSnum to be used as an example.
#' test<-df2SPMFBasket(seqDF,ID="ID",time="jour",event="PRODUITSnum",
#' itemset="ITEMSETS",timeFormat="\%d",parallel = F)
#' @export


df2SPMFBasket<-function(df,ID,itemset="",event="",time="",timeFormat="",timestep=1,parallel=F){
  require(tidyverse)
  while(sum(grepl("package:plyr",search()))>0){detach(package:plyr)}
  if (!is.data.frame(df)|!tibble::is.tibble(df)){stop("Input must be a data frame or a tibble")} # test df2SPMFBasket(c(1,2))
  else {
    #récupération des arguments entrés par l'utilisateur

    if (nchar(ID)<1|nchar(event)<1|(nchar(itemset)<1&nchar(time)<1)){
      print("Arguments ID and event are compulsary, and at least one of time or itemset")} else{
        # sélection des colonnes correspondant aux variables d'intérêt
        subVar<-c(ID,itemset,event,time)[sapply(c(ID,itemset,event,time),nchar)>1]
        x<-df[,subVar]
        names(x)<-c("ID","itemset","event","time")[sapply(c(ID,itemset,event,time),nchar)>1]
        # recodage numérique des produits
        x$event<-as.factor(x$event)
        x$eventNum<-as.numeric(x$event)

        evLev<-levels(x$event)
        x

        #if (nchar(time)<1&nchar(itemset)>1){x$time<-x$itemset}
        if(nchar(itemset)<1&nchar(time)>1){x$itemset<-as.numeric(x$time)}
        if (nchar(timeFormat)>1){time<-as.Date(time, timeFormat)}


        ### Clusterisation pour gros fichiers
        if (parallel==T & length(unique(x$ID))>8){

          require(parallel)
          cl <- detectCores()
          cl
          #création des groupes pour cluster

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
          xByGroup
          # assigner le package à chaque cluster
          xByGroup %>%   cluster_library("dplyr")
          #print(cluster_eval(xByGroup,search()))
          #chargement des fonctions
          cluster_assign_value(xByGroup,"ToBasket",ToBasket)
          cluster_assign_value(xByGroup,"ToTimedBasket",ToTimedBasket)


          ## Exécuter le code par clusters
          if (nchar(timeFormat)<1){
            print("pas de prise en compte du temps")
            y<-xByGroup %>% do(ToBasket(.)) %>%       # Pour une raison que j'ignore, le point est nécessaire dans les parenthèses
              collect() %>% arrange(ID)
            #y<- y%>% ungroup() %>%  select(ID,itemset,basket)
          } else{
            print("avec prise en compte du temps")
            y<-xByGroup %>% do(ToTimedBasket(.,timeStep=timestep)) %>%
              collect() %>% arrange(ID)
            #y<- y%>% ungroup() %>%  select(ID,itemset,basket)
          }
        } else{# sans parallélisation
          if (nchar(timeFormat)<1){y<-x %>% ToBasket()} else{
            y<-x %>% ToTimedBasket(timeStep=timestep)
          }
        }
      }
    sortie<-list(toSendSPMF=y,evLev=evLev)}
}






