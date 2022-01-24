# Package names
packages <- c("onewaytests", "AID", "GFD","MASS","caret","fpp")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

##################CARREGANDO TABELA
df<-read.csv("CT_teste_ONE_WAY.csv", sep=";", h=T)
df[,"Acum_CT"]<-as.numeric(gsub(",", ".", gsub("\\.", "", df$Acum_CT)))
head(df)

###################TESTE COM PACOTE AID
library(AID)
library(onewaytests)

describe(Acum_CT ~ Lagoas, data = df)
#nor.test(Acum_CT ~ Lagoas, data = df)
homog.test(Acum_CT ~ Lagoas, data = df, method = "Bartlett")

out <- boxcoxfr(df$Acum_CT, as.factor(df$Lagoas), lambda = seq(-10, 10, 1/10), tau=0.001)

##########################################################################TESTE COM MASS
library(MASS)
out2<-boxcox(Acum_CT~Lagoas,data=df,lambda = seq(-2, 2, length = 100))
out2

##################################################################TESTE COM PACOTE caret
library(caret)
bc<-BoxCoxTrans(df$Acum_CT)

shapiro.test(predict(bc, df$Acum_CT))

anova(aov(predict(bc, df$Acum_CT)~df$Lagoas))

GFD(predict(bc, df$Acum_CT)~df$Lagoas)
##################################################################TESTE COM PACOTE FPP/FORECAST
library(fpp)

lambda <- BoxCox.lambda(df$Acum_CT, method="loglik", lower=-5, upper=5)
boxcox_trans <- BoxCox(df$Acum_CT, lambda)
shapiro.test((df$Acum_CT))
shapiro.test(boxcox_trans)

anova(aov(predict(bc, df$Acum_CT)~df$Lagoas))

GFD(predict(bc, df$Acum_CT)~df$Lagoas)
#####################################################TESTE DE COMPARAÇÃO DE RESULTADOS DE TRANSFORMAÇÃO
data(AADT)
out<-boxcoxfr(AADT$aadt, AADT$class, lambda = seq(-3, 3, 0.01))
out$tf.data

lambda <- BoxCox.lambda(AADT$aadt, method="loglik", lower=-3, upper=3)
boxcox_trans <- BoxCox(AADT$aadt, lambda)
boxcox_trans

plot(out$tf.data,as.vector(boxcox_trans),xlab="Transformação pelo pacote fpp", ylab="Transformação pelo pacote AADT")
abline(lm(as.vector(boxcox_trans)~out$tf.data),col="red")
