# Package names
packages <- c("onewaytests", "AID", "GFD","MASS","caret","fpp")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

df<-read.csv("CT_teste_ONE_WAY.csv", sep=";", h=T)
df[,"Acum_CT"]<-as.numeric(gsub(",", ".", gsub("\\.", "", df$Acum_CT)))
head(df)

library(AID)
library(onewaytests)

describe(Acum_CT ~ Lagoas, data = df)
nor.test(Acum_CT ~ Lagoas, data = df)
homog.test(Acum_CT ~ Lagoas, data = df, method = "Bartlett")

out <- boxcoxfr(df$Acum_CT, as.factor(df$Lagoas), lambda = seq(-10, 10, 1/10), tau=0.001)

##########################################################################
library(MASS)
out2<-boxcox(Acum_CT~Lagoas,data=df,lambda = seq(-2, 2, length = 100))
out2

########################################################################
library(caret)
bc<-BoxCoxTrans(df$Acum_CT)

shapiro.test(predict(bc, df$Acum_CT))

anova(aov(predict(bc, df$Acum_CT)~df$Lagoas))

GFD(predict(bc, df$Acum_CT)~df$Lagoas)
#####################################################################
library(fpp)

lambda <- BoxCox.lambda(df$Acum_CT, method="loglik", lower=-5, upper=5)
boxcox_trans <- BoxCox(df$Acum_CT, lambda)
shapiro.test((df$Acum_CT))
shapiro.test(boxcox_trans)

anova(aov(predict(bc, df$Acum_CT)~df$Lagoas))

GFD(predict(bc, df$Acum_CT)~df$Lagoas)


