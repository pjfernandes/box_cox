setwd("/home/pedro/Área de Trabalho/arquivos/humberto/box_cox/arquivos/")
library(moments)
pastas<-dir(pattern="folder")

for (i in 1:length(pastas)) {
  setwd(pastas[[i]])
  arquivos<-dir()
  
  resultados<-matrix(ncol=3)
  list_caret<-list()
  list_fpp<-list()
  
  for (j in 1:length(arquivos)) {
    df<-read.csv(arquivos[j],h=T,sep=",")
    list_caret<-append(list_caret,data.frame(df[,2]))
    list_fpp<-append(list_fpp,data.frame(df[,2]))
    v<-as.vector(df[,2])
    st<-shapiro.test(v)
    #at<-agostino.test(v)
    resultado<-c(arquivos[j],
                 as.numeric(st$statistic),
                 as.numeric(st$p.value))
    
    resultados<-rbind(resultados, resultado)
  }
  
  df_caret<-rbind.fill(
    data.frame(list_caret[[1]]),
    data.frame(list_caret[[2]]),
    data.frame(list_caret[[3]]),
    data.frame(list_caret[[4]]),
    data.frame(list_caret[[5]]),
    data.frame(list_caret[[6]]),
    data.frame(list_caret[[7]]),
    data.frame(list_caret[[8]]),
    data.frame(list_caret[[9]]),
    data.frame(list_caret[[10]]),
    data.frame(list_caret[[11]]),
    data.frame(list_caret[[12]])
  )
  
  df_fpp<-rbind.fill(
    data.frame(list_fpp[[1]]),
    data.frame(list_fpp[[2]]),
    data.frame(list_fpp[[3]]),
    data.frame(list_fpp[[4]]),
    data.frame(list_fpp[[5]]),
    data.frame(list_fpp[[6]]),
    data.frame(list_fpp[[7]]),
    data.frame(list_fpp[[8]]),
    data.frame(list_fpp[[9]]),
    data.frame(list_fpp[[10]]),
    data.frame(list_fpp[[11]]),
    data.frame(list_fpp[[12]])
  )
  

  resultados<-as.data.frame(resultados)
  names(resultados)<-c("CLASSE","SHAPIRO","PVALUE")
  write.csv(resultados,"RESULTADO_TESTE.csv",row.names = FALSE)
  write.csv(df_caret,"df_caret.csv",row.names = FALSE)
  write.csv(df_fpp,"df_fpp.csv",row.names = FALSE)
  
  setwd("..")
}