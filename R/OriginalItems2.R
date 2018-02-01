#' A function to match original items form their numeric value (when recoded in the spmf format)
#'
#' This function is not to be used by the user but to be called by FrequentSequences
#' @param x is a Timed basket or a timed sequence in the sense of the SPMF format. It is an element from the basket slot of the output of a df2SPMFBasket function call. I
#' @param evLev the original factor levels to recode to
#' @keywords
#' @examples
#'
#'
#'
#'
#' @export

OriginalItems2<-function(x,evLev){
  # if(x[1]!="<"){ stop("A problem occured : only timed sequences should be passed to this function")}else{
  z<-x%>%as.character() %>%  strsplit(split=" ") %>% unlist() %>% head(-1)
  indexTime<-which(grepl(pattern="(<[[:digit:]]+>)",x=z),arr.ind=T)
  indexSep<-which(grepl(pattern="-1",x=z),arr.ind=T)
  indexNum<-which((grepl(pattern="(<[[:digit:]]+>)",x=z)+grepl(pattern="-1",x=z))==0,arr.in=T)

  z[indexNum]<-evLev[na.omit(as.numeric(as.character(z[indexNum])))]
  z[indexSep]<-"|"
  z<-paste(z,collapse=" ",sep="")
  print(z)
  z
  #}
}
