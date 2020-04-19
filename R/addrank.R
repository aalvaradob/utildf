#'Add a "MyRank" column with the rank of a variable
#'
#'@details
#'
#' This function needs the following imputs:
#' \enumerate{
#'  \item \strong{df}  = dataframe.
#'  \item \strong{Var} = variables for which the rank will be added.  e.g. the df has 2 categories in a column that we will need to add individual ranks, we include the variables quoted of in that column that will be ranked.
#'}
#'
#'@param... numeric
#'
#'@return a dataframe
#'
#'@examples
#'
#'@export
addrank<-function(df,Var){
  Var1<-rlang::sym(Var)
  df2<-df%>%
    mutate(MyRank=rank(-!!Var1,ties.method = "first"))%>%
    arrange(MyRank)
  df2
}
