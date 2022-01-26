setwd("/home/pedro/Área de Trabalho/arquivos/humberto/box_cox/arquivos/")
library(moments)

pastas<-dir(,pattern="folder")

for (i in 1:length(pastas)) {
  setwd(pastas[[i]])
  arquivos<-dir()
  
  resultados<-matrix(ncol=3)
  for (j in 1:length(arquivos)) {
    df<-read.csv(arquivos[j],h=T,sep=",")
    v<-as.vector(df[,2])
    st<-shapiro.test(v)
    #at<-agostino.test(v)
    
    resultado<-c(arquivos[j],
                 as.numeric(st$statistic),
                 as.numeric(st$p.value))
    
    resultados<-rbind(resultados, resultado)
    resultados<-as.data.frame(resultados)
    names(resultados)<-c("CLASSE","SHAPIRO","PVALUE")
    write.csv(resultados,"RESULTADO_TESTE.csv",row.names = FALSE)
  }
  
  setwd("..")
}