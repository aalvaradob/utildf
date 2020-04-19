
#' This function calculates create a weighted variable
#'
#'@param... numeric
#'@return a dataframe
#'@examples
#'
#'@export
addw<-function(df, Var,Weight, FilterVarCero=FALSE, Group=FALSE){
  # This function can be used to add weighted average metrics
  # This function requires the Weight variable to be provided, which can be raw data or weight (1%-99%), in both cases the sum of the weight*Var will be devided by sum(weights)
  # the VarCero exclude from the weights the Var with cero value, meaning that only consider the ones with values other than cero!!!

  df2<-df
  Weight1<-rlang::sym(Weight)
  Var1<-rlang::sym(Var)
  if(Group==FALSE){}else{GroupBy1<-rlang::syms(Group)}

  if(FilterVarCero==TRUE){
    if(Group==FALSE){
      df2<-df2%>%
        mutate(!!Var1:=sum(!!Weight1*!!Var1)*1/sum(case_when(!!Var1==0~0,!!Var1!=0~!!Weight1)))
    }else{
      df2<-df2%>%
        group_by(!!!GroupBy1)%>%
        mutate(!!Var1:=sum(!!Weight1*!!Var1)*1/sum(case_when(!!Var1==0~0,!!Var1!=0~!!Weight1)))
    }
  }else{
    if(Group==FALSE){
      df2<-df %>%mutate(!!Var1:=sum(!!Weight1*!!Var1)*1/sum(!!Weight1))
    }else{
      df2<-df %>%
        group_by(!!!GroupBy1)%>%
        mutate(!!Var1:=sum(!!Weight1*!!Var1)*1/sum(!!Weight1))
    }
  }
  df2
}

