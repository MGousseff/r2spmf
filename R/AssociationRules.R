#' A function to find association rules from frequent itemsets (baskets) using the wonderful spmf library
#'
#' This function sends a list of baskets to spmf and gets back Association Rules with their support and confidence.
#' @param x is the output of the function df2SPMFBasket function. It must contain a toSendSPMF storing a tibble with a basket column. This basket is sent to spmf java library.
#' @param algo is the name of the algorithm to be used. The possible values are : Please check SPMF documentation at http://www.philippe-fournier-viger.com/spmf/index.php?link=documentation.php
#' @param minsup is the value of the minimum support for a sequence to be seen as frequent. To get all the sequences present in more than 40 percent of the database sequences, type either "40\%" or "0.4"
#' @param clean if clean=F the text files sent to and received from spmf will be kept in the working directory.
#' @param windowSize the TRuleGrowth searches for rules X==>Y within a given window size, based on the sequences who contains all the items of X before all the items of Y whithin the windowsize.
#' @param Those time parameters are only considered when the input is a timed sequence. The only algorithme to be used are then "Fournier08-Closed+time" and "HirateYamana"
#' @return a dataframe with three columns. sequence contains all the frequent sequences. support is the number of times this sequence occurs, and frequence is support divided by the total number of sequences
#' @keywords Rules
#' @examples
#'
#' @export


AssociationRules<-function(x, algo, minsup=0.5, minconf=0.6, minlift="", clean=F){ # x is the output of df2sequenceSPMF (better recode this in S4 one day)
  require(tidyverse)
  #java -jar spmf.jar run CMRules contextPrefixSpan.txt output.txt 75% 50%
  AssRulesAlgos<-c("FPGrowth_association_rules")
  AssRulesLiftAlgos<-c("FPGrowth_association_rules_with_lift")

  if((!("toSendSPMF"%in%attributes(x)$names))){stop("x is supposed to be the output of a df2SPMFBasket function")}
  if((length(grep("-1",x$toSendSPMF$basket))>0)|(length(grep("-2",x$toSendSPMF$basket))>0)){stop("x is not supposed to be the output of df2SPMFSequence function, but df2SPMFBasket. No sequence taken into account")}

  print("En avant pour programmer AssociationRules()")

  df<-x$toSendSPMF$basket

  if(algo=="FPGrowth_association_rules"){
    fileNameIn<-paste(paste(substitute(x),algo,minsup,minconf,sep="."),".txt",sep="")
    fileNameOut<-paste(paste(substitute(x),algo,minsup,minconf,"out",sep="."),".txt",sep="")
    print(fileNameIn)
    write.table(df,fileNameIn,quote=F,row.names=F,col.names=F)
    instruct<-paste("java -jar spmf.jar run", algo, fileNameIn, fileNameOut, minsup, minconf, collapse=" ")
    print(instruct)
    system(instruct)
    print(fileNameOut)

    dfOUT<-gsub(pattern="(#[[:upper:]]{3}:)|(#[[:upper:]]{4}:)|( ==> )",replacement=";",x=readLines(fileNameOut)) %>%
      textConnection() %>% read.table(sep=";") %>% as.data.frame()
    names(dfOUT)<-c("left","right","sup","conf")

    dfOUT

  }

  if(algo=="FPGrowth_association_rules_with_lift"){
    fileNameIn<-paste(paste(substitute(x),algo,minsup,minconf,minlift,sep="."),".txt",sep="")
    fileNameOut<-paste(paste(substitute(x),algo,minsup,minconf,minlift,"out",sep="."),".txt",sep="")
    print(fileNameIn)
    write.table(df,fileNameIn,quote=F,row.names=F,col.names=F)
    instruct<-paste("java -jar spmf.jar run", algo, fileNameIn, fileNameOut, minsup, minconf, minlift, collapse=" ")
    print(instruct)
    system(instruct)
    print(fileNameOut)

    dfOUT<-gsub(pattern="(#[[:upper:]]{3}:)|(#[[:upper:]]{4}:)|( ==> )",replacement=";",x=readLines(fileNameOut)) %>%
      textConnection() %>% read.table(sep=";") %>% as.data.frame()
    names(dfOUT)<-c("left","right","sup","conf","lift")

    dfOUT

  }


  if(clean==T){
    system(paste("rm",fileNameIn))
    system(paste("rm",fileNameOut))}

  dfOUT
}
