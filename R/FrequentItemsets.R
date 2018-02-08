#' A function to find frequent itemsets using the wonderful spmf library
#'
#' This function sends a list of baskets to spmf and gets back the most frequent itemsets with their support and frequence.
#' @param x is the output of the fnction df2SPMFBasket function. It must contain a toSendSPMF storing a tibble with a basket column. This basket is sent to spmf java library.
#' @param algo is the name of the algorithm to be used. The possible values are : "Apriori","Apriori_TID","FPGrowth_itemsets","Eclat","HMine","FIN","LCMFreq","AprioriClose","Charm_bitset","dCharm_bitset","LCM","FPMax","Charm_MFI","defME". Please check SPMF documentation at http://www.philippe-fournier-viger.com/spmf/index.php?link=documentation.php
#' @param minsup is the value of the minimum support for an itemset to be seen as frequent. to get all the itemset present in more than 40 percent of the transactions, type either "40\%" or "0.4"
#' @param clean if clean=F the text files sent to spmf will be kept in the working directory.

#' @return a dataframe with three columns. itemset contains all the frequent itemsets, that is a list of products. support is the number of times this itemsets occurs, and frequence is support divided by the total number of transactions
#' @keywords Itemsets
#' @examples
#'
#'
#'
#'
#' @export


FrequentItemsets<-function(x,algo,minsup,clean=T){ # x is the output of df2basketSPMF (better recode this in S4 one day)
  require(tidyverse)
  if((!("toSendSPMF"%in%attributes(x)$names))){stop("The input file is supposed to be the output of a df2SPMFBasket function")}

  freqItemsetAlgos<-c("Apriori","Apriori_TID","FPGrowth_itemsets","Eclat","HMine","FIN","LCMFreq")
  freqClosedItemsetAlgos<-c("AprioriClose","Charm_bitset","dCharm_bitset","LCM")
  freqMaxItemsetAlgos<-c("FPMax","Charm_MFI")
  freqGeneratorItemsetAlgos<-c("defME")

  if(algo%in%freqItemsetAlgos){print(paste("frequent itemsets using",algo,"algorithm"))}else{
    if(algo%in%freqClosedItemsetAlgos){print(paste("frequent closed itemsets using",algo,"algorithm"))}else{
      if(algo%in%freqMaxItemsetAlgos){print(paste("frequent maximal itemsets using",algo,"algorithm"))}else{
        if(algo%in%freqGeneratorItemsetAlgos){print(paste("generator itemsets using",algo,"algorithm"))}else{
          stop("the algorithm you entered is note recognized. Have you checked spelling ? Did you forget the quotes ?")
        }
      }
    }
  }



  df<-x$toSendSPMF$basket
  fileNameIn<-paste(paste(substitute(x),algo,minsup,sep="_"),".txt",sep="")
  fileNameOut<-paste(paste(substitute(x),algo,minsup,"out",sep="_"),".txt",sep="")
  write.table(df,fileNameIn,quote=F,row.names=F,col.names=F)
  instruct<-paste("java -jar spmf.jar run", algo, fileNameIn, fileNameOut,minsup,collapse=" ")
  print(instruct)
  system(instruct)
  print(fileNameOut)
  dfOUT<-gsub(" #SUP: ",";",readLines(fileNameOut)) %>% textConnection() %>% read.table(sep=";") %>% as.data.frame()
  names(dfOUT)<-c("itemset","support")
  dfOUT$freq<-(dfOUT$support)/length(df)


  if(clean==T){
    system(paste("rm",fileNameIn))
    system(paste("rm",fileNameOut))
  }

  if(algo%in%freqClosedItemsetAlgos){comment(dfOUT)<-"Closed Itemsets only"}
  if(algo%in%freqMaxItemsetAlgos){comment(dfOUT)<-"Maximal Itemsets only"}
  if(algo%in%freqGeneratorItemsetAlgos){comment(dfOUT)<-"Generator itemsets only"}

  dfOUT$originalItemset<-sapply(dfOUT$itemset,OriginalItems,evLev=x$evLev)
  dfOUT
}




