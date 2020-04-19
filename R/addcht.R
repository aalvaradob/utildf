#' Add a new factor with a change between the values of two factors in a tidy table
#'
#'This function is used to calculate changes between values of categories (dates, ages, countries, etc).
#'
#'The data may come as TIDY, meaning that all values are in 1 column and other columns show information related to the value, meaning categories.
#'
#' All inputs must be in quotes ("").
#'
#'\strong{Inputs:}
#'\enumerate{
#'  \item  df   =  is the database containing a column with Factors.
#'  \item  Var1 =  ARE the "VariableS" valueS that will be used to calculate the differences vs Var0.
#'  \item  Var0 =  IS the Variable value  anchor for all changes calculation.
#'}
#'Var1, Var0 and Var must be factors with leverls or characters NOT NUMBERS
#'
#'
#'@param... numeric
#'
#'@return a dataframe
#'
#'@examples
#'
#'@export
#'
addcht<-function(df,VarA,VarB,Var,Value){
  {
    # === aux variables
    Var1<-rlang::sym(Var)
    Value1<-rlang::sym(Value)
    VarTable<<-c(as.character(unlist(df%>%select(!!Var1)%>%unique())))

    # === Spread the data to calculate changes
    df1<-df
    df2<-spread(df1,key=!!Var1,value=!!Value1)
    df3<-df2

  } # spread data to calc changes
  {# === create change columns
    for (i in 1:length(VarB)){
      VarA11<-rlang::sym(VarA[1])
      VarB11<-rlang::sym(VarB[i])
      Chng<-rlang::sym(paste0("Chng_",VarA,"_",VarB[i]))
      df3<-df3 %>%
        dplyr::mutate(!!Chng:=!!VarA11-!!VarB11)
    }
  } # calculate changes
  { # === gather the data to original layout
    df3<-df3%>%gather(key=!!enquo(Var),value=Value,VarTable,contains("Chng_"))
  } # gather the data to original layout
  df3
}

