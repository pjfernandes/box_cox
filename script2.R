setwd("/home/pedro/Área de Trabalho/arquivos/humberto/box_cox/arquivos")
arquivos<-dir()

for (arquivo in arquivos){
    
    df<-read.csv(arquivo, sep=";", h=T)
    #df[,"Acum_CT"]<-as.numeric(gsub(",", ".", gsub("\\.", "", df$Acum_CT)))
    head(df)
    
    #########################
    library(caret)
    bc<-BoxCoxTrans(df[,2])
    df[,'BOXCOX_CARET']<-predict(bc, df[,2])
    
    #########################
    library(fpp)
    lambda <- BoxCox.lambda(df[,2], method="loglik", lower=-2, upper=2)
    df[,'BOXCOX_FPP_FORECAST'] <- BoxCox(df[,2], lambda)
    
    df[,1]<-as.factor(df[,1])
    classes<-levels(df[,1])
    
    #########################
    list_sem_transform<-list()
    list_caret<-list()
    list_fpp<-list()
    
    for (i in 1:length(classes)) {
      classe<-classes[i]
      list_sem_transform[[classe]]<-as.vector(df[df[,1]==classe,2])
      list_caret[[classe]]<-as.vector(df[df[,1]==classe,3])
      list_fpp[[classe]]<-as.vector(df[df[,1]==classe,4])
    }

    dir.create(paste0(getwd(),"/",arquivo,"_folder"))
    
    for (i in 1:length(list_caret)) {
      df<-data.frame(list_caret[[i]])
      classe<-names(list_caret)[i]
      names(df)<-classe
      write.csv(df,paste0(arquivo,"_folder","/",classe,"_caret.csv"))
    }
    
    for (i in 1:length(list_fpp)) {
      df<-data.frame(list_fpp[[i]])
      classe<-names(list_fpp)[i]
      names(df)<-classe
      write.csv(df,paste0(arquivo,"_folder","/",classe,"_fpp.csv"))
    }

}
#########################