d= file.choose()
data=read.csv(d,stringsAsFactors = F)
attach(data)
data_quality=function(df=Null){
  if(is.null(df)) print("enter a data")
  summ_table=do.call(data.frame,
                     list(
                       min=sapply(df, function(x) min(x,na.rm = T)),
                       max=sapply(df, function(x) max(x,na.rm = T)),
                       mean=sapply(df, function(x) mean(x,na.rm = T)),
                       sd=sapply(df, function(x) sd(x,na.rm = T)),
                       total=apply(df, 2,length),
                       nulls=sapply(df, function(x) sum(is.na(x))),
                       uniq=sapply(df, function(x) length(unique(x))),
                       dtype=sapply(df, class)
                     )
                     )
  nums=vapply(summ_table, is.numeric, FUN.VALUE = logical(1))
  summ_table[,nums]=round(summ_table[,nums], digits = 3)
  return(summ_table)
  
}
qual=data_quality(data)
qual=cbind(Columns=rownames(qual),
           data.frame(qual,row.names=NULL)
           )
write.csv(qual,paste0("quality_report", format(Sys.time(),"%d=%m-%Y-%H%M%S","csv"),row.names = F))
