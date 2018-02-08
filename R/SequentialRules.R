#' A function to find frequential rules using the wonderful spmf library
#'
#' This function sends a list of sequences to spmf and gets back Sequential Rules sequences with their support and confidence.
#' @param x is the output of the function df2SPMFSequence function. It must contains a toSendSPMF storing a tibble with a basket column. This basket is sent to spmf java library.
#' @param algo is the name of the algorithm to be used. The possible values are : Please check SPMF documentation at http://www.philippe-fournier-viger.com/spmf/index.php?link=documentation.php
#' @param minsup is the value of the minimum support for a sequence to be seen as frequent. To get all the sequences present in more than 40 percent of the database sequences, type either "40\%" or "0.4"
#' @param clean if clean=F the text files sent to and received from spmf will be kept in the working directory.
#' @param windowSize the TRuleGrowth searches for rules X==>Y within a given window size, based on the sequences who contains all the items of X before all the items of Y whithin the windowsize.
#' @param Those time parameters are only considered when the input is a timed sequence. The only algorithme to be used are then "Fournier08-Closed+time" and "HirateYamana"
#' @return a dataframe with three columns. sequence contains all the frequent sequences. support is the number of times this sequence occurs, and frequence is support divided by the total number of sequences
#' @keywords Rules
#' @examples
#'
#'
#'
#'
#' @export
#'


SequentialRules<-function(x, algo, minsup=0.5, minconf=0.6, clean=F, windowSize=""){ # x is the output of df2sequenceSPMF (better recode this in S4 one day)
  require(tidyverse)
  #java -jar spmf.jar run CMRules contextPrefixSpan.txt output.txt 75% 50%
  seqRulesAlgos<-c("CMRules","CMDeo","RuleGrowth","ERMiner","RuleGen","TRuleGrowth")
  seqRulesTimedAlgos<-"TRuleGrowth"



  if((!("toSendSPMF"%in%attributes(x)$names))){stop("The input file is supposed to be the output of a df2SPMFSequence function")}else{

    df<-x$toSendSPMF$sequence

    if(algo=="TRuleGrowth"){print(length(df))
      fileNameIn<-paste(paste(substitute(x),algo,minsup,minconf,windowSize,sep="."),".txt",sep="")
      fileNameOut<-paste(paste(substitute(x),algo,minsup,minconf,windowSize,"out",sep="."),".txt",sep="")
      print(fileNameIn)
      write.table(df,fileNameIn,quote=F,row.names=F,col.names=F)
      instruct<-paste("java -jar spmf.jar run", algo, fileNameIn, fileNameOut, minsup, minconf, windowSize,collapse=" ")
      print(instruct)
      system(instruct)
      print(fileNameOut)

      dfOUT<-gsub(pattern="(#[[:upper:]]{3}:)|(#[[:upper:]]{4}:)|( ==> )",replacement=";",x=readLines(fileNameOut)) %>%
        textConnection() %>% read.table(sep=";") %>% as.data.frame()
      names(dfOUT)<-c("left","right","sup","conf")
      dfOUT$originalLeft<-sapply(dfOUT$left,OriginalItemsComma,evLev=x$evLev)
      dfOUT$originalRight<-sapply(dfOUT$right,OriginalItemsComma,evLev=x$evLev)

      dfOUT
      }else{

    if(algo!="RuleGen"){
      print(length(df))
      fileNameIn<-paste(paste(substitute(x),algo,minsup,minconf,sep="."),".txt",sep="")
      fileNameOut<-paste(paste(substitute(x),algo,minsup,minconf, "out",sep="."),".txt",sep="")
      print(fileNameIn)
      write.table(df,fileNameIn,quote=F,row.names=F,col.names=F)
      instruct<-paste("java -jar spmf.jar run", algo, fileNameIn, fileNameOut, minsup, minconf,collapse=" ")

      print(instruct)
      system(instruct)
      print(fileNameOut)

      dfOUT<-gsub(pattern="(#[[:upper:]]{3}:)|(#[[:upper:]]{4}:)|( ==> )",replacement=";",x=readLines(fileNameOut)) %>%
        textConnection() %>% read.table(sep=";") %>% as.data.frame()

      if(algo=="CMRules"|algo=="RuleGrowth"|algo=="ERMiner"){
        names(dfOUT)<-c("leftHand","rightHand","support","confidence")
      }else{if(algo=="CMDeo")
        {names(dfOUT)<-c("leftHand","rightHand","support","confidence","lift")
        dfOUT$originalLeft<-sapply(dfOUT$left,OriginalItemsComma,evLev=x$evLev)
        dfOUT$originalRight<-sapply(dfOUT$right,OriginalItemsComma,evLev=x$evLev)
      }}


      if(clean==T){
      system(paste("rm",fileNameIn))
      system(paste("rm",fileNameOut))
      }

    dfOUT
    }

    else{ # Taking into account inconsistency problem within spmf behaviour
      if((minsup>0&minsup<1))
      {minsupInst<-round(minsup*length(df)); minsupInst; print("via perc nrow de df vaut"); print(length(df))}else{
      if((minsup>0&minsup<100)){minsupInst<-round((minsup/100*length(df))); minsupInst;
      print("minsupInst from prob vaut"); print(minsupInst)}else
        {stop("for RuleGen algorithm, minsup muste be a percentage, either between 0 and 1 or between 0 and 100")}
      }

      if((as.numeric(minconf)>0&as.numeric(minconf)<1)){minconfInst<-as.numeric(minconf)*100}else{
        if(((as.numeric(minconf)>0&as.numeric(minconf)<100))){minconfInst<-as.numeric(minconf)}else{
          stop("for RuleGen algorithm, minconf must be a percentage, either 0 and 1 or between 0 and 100")}}

      fileNameIn<-paste(paste(substitute(x),algo,minsup,minconf,sep="."),".txt",sep="")
      fileNameOut<-paste(paste(substitute(x),algo,minsup,minconf, "out",sep="."),".txt",sep="")
      print(fileNameIn)
      write.table(df,fileNameIn,quote=F,row.names=F,col.names=F)
      instruct<-paste("java -jar spmf.jar run", algo, fileNameIn, fileNameOut, minsupInst, paste(minconfInst,"%",sep=""),sep=" ",collapse=" ")

      print(instruct)
      system(instruct)
      print(fileNameOut)

      dfOUT<-gsub(pattern="(#[[:upper:]]{3}:)|(#[[:upper:]]{4}:)|( ==> )",replacement=";",x=readLines(fileNameOut)) %>% textConnection() %>%
      read.table(sep=";") %>% as.data.frame()
      names(dfOUT)<-c("left","right","sup","conf")
      dfOUT$originalLeft<-sapply(dfOUT$left,OriginalItemsBrace,evLev=x$evLev)
      dfOUT$originalRight<-sapply(dfOUT$right,OriginalItemsBrace,evLev=x$evLev)

      dfOUT
      dfOUT
    }
  }
  }
}
