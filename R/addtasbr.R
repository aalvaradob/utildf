
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
  #get the ones within the rank
  df1<-df%>%filter(!!MyRank<=RankT)

  #get totals of top rank
  df1T<-df1%>%
    gettotalenq(...)

  #get the other over the rank
  df2<-df%>%filter(!!MyRank>RankT)%>%
    gettotalenq(...)

  #get the totals
  df3<-df%>%gettotalenq(...)

  #add all tables
  df3<-bind_rows(df1,df1T,df2,df3)

  nr<-df3%>%nrow()

  df3[nr-2,1]<- "Sub-Total"
  df3[nr-1,1]<- "Others"
  df3[nr,1]<- "Total"

  df3
}
