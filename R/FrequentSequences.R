#' A function to find frequent sequences using the wonderful spmf library
#'
#' This function sends a list of sequences to spmf and gets back the most frequent sequences with their support and frequence.
#' @param x is the output of the function df2SPMFSequence function. It must contains a toSendSPMF storing a tibble with a basket column. This basket is sent to spmf java library.
#' @param algo is the name of the algorithm to be used. The possible values are : Please check SPMF documentation at http://www.philippe-fournier-viger.com/spmf/index.php?link=documentation.php
#' @param minsup is the value of the minimum support for a sequence to be seen as frequent. To get all the sequences present in more than 40 percent of the database sequences, type either "40\%" or "0.4"
#' @param clean if clean=F the text files sent to and received from spmf will be kept in the working directory.
#' @param minTime is the minimum time interval for 2 items to be considered as belonging to the same sequence
#' @param maxTime is the maximum time interval for 2 items to be considered as belonging to the same sequence
#' @param minWhole is the minimum length of time for a whole sequence to be counted
#' @param maxWhole is the maximum length of time for a whole sequence to be counted
#' @param Those time parameters are only considered when the input is a timed sequence. The only algorithme to be used are then "Fournier08-Closed+time" and "HirateYamana"
#' @return a dataframe with three columns. sequence contains all the frequent sequences. support is the number of times this sequence occurs, and frequence is support divided by the total number of sequences
#' @keywords sequences
#' @examples
#'
#'
#'
#'
#' @export

FrequentSequences<-function(x,algo,minsup=0.5,nbEventMax="",showID="",clean=T,
                            minTime="",maxTime="",minWhole="",maxWhole="",k=NULL, maxSeq=NULL,maxGap=NULL){ # x is the output of df2sequenceSPMF (better recode this in S4 one day)
  require(tidyverse)
  freqSeqAlgos<-c("PrefixSpan","PrefixSpan_AGP","GSP","SPADE","CM-SPADE","SPAM","LAPIN","ClaSP")
  freqClosedSeqAlgos<-c("ClaSP","CM-ClaSP","CloSpan","BIDE+")
  freqMaxSeqAlgos<-c("MaxSP","VMSP")
  freqGeneratorSeqAlgos<-c("FEAT","FSGP","VGEN")
  CompressingSeq<-"GoKrimp"
  freqTimeConstraintAlgos<-c("Fournier08-Closed+time","HirateYamana")
  topK<-"TKS"

  #### Encore à inclure :
  # freqTopKSeqAlgos<-c("TKS","TSP")      # ATTENTION AUX PARAMETRES SUPPLEMENTAIRES
  # freq


 ## Attention : the code can be drastically improved. For now, the instructions are duplicated for readbility purpose


  if(algo=="GoKrimp"|algo=="TKS"){minsup<-""}

  if((!("toSendSPMF"%in%attributes(x)$names))){stop("The input file is supposed to be the output of a df2SPMFSequence function")}
  else{
    df<-x$toSendSPMF$sequence

    if(algo%in%freqTimeConstraintAlgos & !grepl("<[[:digit:]]+>",df[1]))
      {stop("Hirate Yamana algorithm can only be use on time extended sequences")}else{

      if (algo=="PrefixSpan_AGP"){
        if(showID==T | showID=="true"){showID<-"true"}else{showID=""}
        fileNameIn<-paste(paste(substitute(x),algo,minsup,showID,sep="."),".txt",sep="")
        fileNameOut<-paste(paste(substitute(x),algo,minsup,showID, "out",sep="."),".txt",sep="")
        print(fileNameIn)
        write.table(df,fileNameIn,quote=F,row.names=F,col.names=F)
        instruct<-paste("java -jar spmf.jar run", algo,
                        fileNameIn, fileNameOut, minsup, nbEventMax, showID, collapse=" ")}

      if(algo%in%freqSeqAlgos & (algo!="PrefixSpan_AGP")){
        fileNameIn<-paste(paste(substitute(x),algo,minsup,showID,sep="."),".txt",sep="")
        fileNameOut<-paste(paste(substitute(x),algo,minsup,showID, "out",sep="."),".txt",sep="")
        print(fileNameIn)
        write.table(df,fileNameIn,quote=F,row.names=F,col.names=F)
        instruct<-paste("java -jar spmf.jar run", algo, fileNameIn, fileNameOut, minsup)
      }

      if(algo=="GoKrimp"){            # Special case of GoKrimp algorithm
        fileNameIn<-paste(paste(substitute(x),algo,sep="."),".txt",sep="")
        fileNameLab<-paste(paste(substitute(x),algo,sep="."),".lab",sep="")
        write.table(df,fileNameIn,quote=F,row.names=F,col.names=F)
        write.table(x$evLev,fileNameLab,quote=F,row.names=F,col.names=F)
        instruct<-paste("java -jar spmf.jar run", algo, fileNameIn,
                        fileNameOut,fileNameLab, collapse=" ")
      }

      if(algo%in%freqTimeConstraintAlgos){            # Special case of Time constrained algorithm
        fileNameIn<-paste(paste(substitute(x),algo, minsup, minTime, maxTime, minWhole, maxWhole, sep="."), ".txt",sep="")
        fileNameOut<-paste(paste(substitute(x),algo,minTime, maxTime, minWhole, maxWhole,"out", sep="."), ".txt",sep="")

        instruct<-paste("java -jar spmf.jar run", algo, fileNameIn, fileNameOut, minsup,minTime, maxTime, minWhole,maxWhole, collapse=" ")
      }

        if(algo=="TKS"){
          print(paste(algo,k))
          fileNameIn<-paste(paste(substitute(x),algo,k,sep="."),".txt",sep="")
          fileNameOut<-paste(paste(substitute(x),algo,k,"out", sep="."), ".txt",sep="")
          write.table(df,fileNameIn,quote=F,row.names=F,col.names=F)
          instruct<-paste("java -jar spmf.jar run", algo, fileNameIn, fileNameOut,k, collapse=" ")
        }

        if(algo=="VMSP"){
          print(paste(algo,k))
          if (showID==TRUE|showID=="true"|showID=="TRUE"){showID<-"true"}else(showID<-"")
          fileNameIn<-paste(paste(substitute(x),algo,minsup,maxSeq,maxGap,showID,sep="."),".txt",sep="")
          fileNameOut<-paste(paste(substitute(x),algo,minsup,maxSeq,maxGap,showID,"out", sep="."), ".txt",sep="")
          write.table(df,fileNameIn,quote=F,row.names=F,col.names=F)
          instruct<-paste("java -jar spmf.jar run", algo, fileNameIn, fileNameOut,minsup,maxSeq,maxGap,showID, collapse=" ")
        }



      print(instruct)
      system(instruct)
      print(fileNameOut)

      dfOUT<-gsub(pattern="(#[[:upper:]]{3}:)",replacement=";",x=readLines(fileNameOut)) %>% textConnection() %>%
        read.table(sep=";") %>% as.data.frame()   # the SUP: or #IDS: seperators in SPMF are simply changed in ";" in order to read the output file of SPMF

      if ((algo=="PrefixSpan_AGP" |algo=="VMSP") & (showID==T|showID=="true")){ names(dfOUT)<-c("sequence","support","IDs")}else{
        names(dfOUT)<-c("sequence","support")}

      if(clean==T){
        system(paste("rm",fileNameIn))
        system(paste("rm",fileNameOut))
      }

      #if(algo%in%freqClosedItemsetAlgos){comment(dfOUT)<-"Closed Itemsets only"}
      #if(algo%in%freqMaxItemsetAlgos){comment(dfOUT)<-"Maximal Itemsets only"}
      #if(algo%in%freqGeneratorItemsetAlgos){comment(dfOUT)<-"Generator itemsets only"}

      if(algo!="GoKrimp"){

        if(algo%in%freqTimeConstraintAlgos){
          dfOUT$freq<-(dfOUT$support)/length(df)
          dfOUT$originalSequences<-sapply(dfOUT$sequence,OriginalItems2,evLev=x$evLev)
        }else{
          if (algo=="PrefixSpan_AGP"& (showID==T | showID=="true")){
            dfOUT$originalSequences<-sapply(dfOUT$sequence,OriginalItems,evLev=x$evLev)
            dfOUT$originalIDs<-sapply(dfOUT$IDs,OriginalIDs,evLev=x$toSendSPMF$ID)
          } else{dfOUT$originalSequences<-sapply(dfOUT$sequence,OriginalItems,evLev=x$evLev)}}

      }
      dfOUT
    }
  }
}
