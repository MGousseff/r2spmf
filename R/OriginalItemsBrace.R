#' A function to match original items form their numeric value (when recoded in the spmf format)
#'
#' This function is not to be used by the user but to be called by FrequentItemsets or more likely by FrequentSequences or SequentialRules
#' @param x is a basket or a sequence in the sense of the SPMF format. It is an element from the basket slot of the output of a df2SPMFBasket function call. I
#' @param evLev the original factor levels to recode to
#' @keywords Items
#' @examples
#'
#'
#'
#'
#' @export

OriginalItemsBrace<-function(x,evLev){
  if(x[1]=="<"){stop("A problem occured, we shloud not be dealing with timed sequence with this algorithm")}else{
    chaine<-x %>%as.character()
    chaine2<-gsub("\\}\\{","-1 ",chaine)
    index<-gsub("\\{|\\}","",chaine2) %>%  strsplit(split=" ") %>% unlist() %>% as.numeric() %>% na.omit() #récupération du panier comme un numéric
    index[index==-1]<-NA
    z<-evLev[index]
    z[is.na(z)]<-"|"
    z<-paste(z,collapse=" ",sep="")
  }
  #z<-gsub(",","",z)
  #print(z)

  #}
}




