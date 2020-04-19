#'This is a get total when columns are in symbors
#'
#'@param... numeric
#'@return a dataframe
#'@examples
#'
#'@export
gettotalsym<-function(df,...){
  Vars1<-rlang::ensyms(...)
  Df1<-df%>%
    select(!!!Vars1)%>%
    summarise_each(sum)
  Df1
}

