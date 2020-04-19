
#'This is a get total and subtotal by rank addtasbr
#'
#'@param... numeric
#'@return a dataframe
#'@examples
#'
#'@export
addtasbr<-function(df,Rank,RankT,...){
  MyRank<-rlang::enquo(Rank)
  Vars1<-rlang::enquos(...)

  df1<-df%>%filter(!!MyRank<=RankT)

  df2<-df%>%filter(!!MyRank>RankT)%>%
    gettotalenq(...)

  df3<-df%>%gettotalenq(...)

  df3<-bind_rows(df1,df2,df3)

  nr<-df3%>%nrow()

  df3[nr-1,1]<- "Sub-Total"
  df3[nr,1]<- "Total"

  df3
}
