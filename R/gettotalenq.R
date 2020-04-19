#'This is a get total when columns are in quotes
#'
#'@param... numeric
#'@return a dataframe
#'@examples
#'
#'@export
gettotalenq<-function(df,...){
  Vars1<-rlang::enquos(...)
  Df1<-df%>%
    select(!!!Vars1)%>%
    summarise_each(sum)
  Df1
}
