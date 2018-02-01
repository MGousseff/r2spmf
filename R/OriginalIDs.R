#' A function to match original items form their numeric value (when recoded in the spmf format)
#'
#' This function is not to be used by the user but to be called by FrequentSequences.
#' @param x is a set of IDs supporting a given sequence
#' @param evLev the original IDs list
#' @keywords
#' @examples
#'
#'
#'
#'
#' @export

OriginalIDs<-function(x,evLev){
  index<-x %>%as.character() %>%  strsplit(split=" ") %>% unlist() %>% as.numeric() %>% na.omit() %>% +1 #récupération du panier comme un numéric
  z<-evLev[index]
  z<-paste(z,collapse=" ",sep="")
}
