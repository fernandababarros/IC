#01-organizando os dados

#carregando os pacotes
library(mice)
library(VIM)
## install.packages("CASdatasets", repos = "http://cas.uqam.ca/pub/R/", type="source")
library(CASdatasets)
library(tidyverse)
library(gridExtra)
library(ggplot2)
## library(pryr)
library(car)
library(knitr)
library(boot)


#carregando o banco de dados
data(ustermlife)
head(ustermlife)
dados_originais = subset(ustermlife, select = c(Gender, Age, MarStat,
                                                Education, Ethnicity, Income))
## organizando as variáveis categóricas
dados_originais$Gender <- ifelse(dados_originais$Gender==0, "Feminino", "Masculino")
dados_originais$MarStat <- ifelse(dados_originais$MarStat==0,"Outros",ifelse(dados_originais$MarStat==1,"Casado","Morando Juntos"))
dados_originais$Ethnicity <- ifelse(dados_originais$Ethnicity==1, "Branco", ifelse(dados_originais$Ethnicity==2, "Negro", ifelse(dados_originais$Ethnicity==3, "Hispânico", "Outros")))
## Recodificação da Variável Anos de Escolaridade
## Separando em tipo de Ensino
dados_originais$Education2 <- Recode(dados_originais$Education, "2:10='Ensino Fundamental'; 11:14='Ensino Médio'; 15:17='Ensino Superior'")


## BANCO DE DADOS PARA IMPUTAÇÃO MCAR
#gerando NA's na variável Income
#e criando um novo banco de dados com os valores faltantes
set.seed(0)
dados_MCAR = dados_originais
random_MCAR = rbinom(length(dados_MCAR$Income), size = 1, prob=0.2)
#valores_ext_MCAR = dados_originais[random_MCAR,6]
dados_MCAR[,6] = ifelse(random_MCAR, NA, dados_MCAR$Income)


## BANCO DE DADOS PARA IMPUTAÇÃO MAR
#gênero feminino
set.seed(0)
dados_MAR = dados_originais
dados_MAR_fem = dados_MAR[dados_originais$Gender=='Feminino',]
random_MAR_fem = rbinom(length(dados_MAR_fem$Income), size = 1, prob=0.1)
#valores_ext_MAR_fem = dados_originais[random_MAR_fem,6]
dados_MAR_fem[,6] = ifelse(random_MAR_fem, NA, dados_MAR_fem$Income)

#gênero masculino
set.seed(0)
dados_MAR_masc = dados_MAR[dados_originais$Gender=='Masculino',]
random_MAR_masc = rbinom(length(dados_MAR_masc$Income), size = 1, prob=0.3)
#valores_ext_MAR_masc = dados_originais[random_MAR_masc,6]
dados_MAR_masc[,6] = ifelse(random_MAR_masc, NA, dados_MAR_masc$Income)

#ensino fundamental
set.seed(0)
dados_MAR_ef = dados_MAR[dados_originais$Education2=='Ensino Fundamental',]
random_MAR_ef = rbinom(length(dados_MAR_ef$Income), size = 1, prob=0.05)
#valores_ext_MAR_ef = dados_originais[random_MAR_ef,6]
dados_MAR_ef[,6] = ifelse(random_MAR_ef, NA, dados_MAR_ef$Income)

#ensino médio
set.seed(0)
dados_MAR_em = dados_MAR[dados_originais$Education2=='Ensino Médio',]
random_MAR_em = rbinom(length(dados_MAR_em$Income), size = 1, prob=0.2)
#valores_ext_MAR_em = dados_originais[random_MAR_em,6]
dados_MAR_em[,6] = ifelse(random_MAR_em, NA, dados_MAR_em$Income)

#ensino superior
set.seed(0)
dados_MAR_es = dados_MAR[dados_originais$Education2=='Ensino Superior',]
random_MAR_es = rbinom(length(dados_MAR_es$Income), size = 1, prob=0.4)
#valores_ext_MAR_es = dados_originais[random_MAR_es,6]
dados_MAR_es[,6] = ifelse(random_MAR_es, NA, dados_MAR_es$Income)

#banco completo com os missings na renda pelas probabilidades de feminino
#e de masculino
dados_MAR_genero = rbind(dados_MAR_fem,dados_MAR_masc)

#banco completo com os missings na renda pelas probabilidades de ensino fundamental,
#ensino médio e ensino superior
dados_MAR_ensino = rbind(dados_MAR_ef,dados_MAR_em,dados_MAR_es)


## BANCO DE DADOS PARA IMPUTAÇÃO MNAR
set.seed(0)
dados_MNAR = dados_originais
minimo = min(log(dados_MNAR$Income))
maximo = max(log(dados_MNAR$Income))
media = mean(log(dados_MNAR$Income))
mediana = median(log(dados_MNAR$Income))
quantile(dados_MNAR$Income,0.75)
quantil3 = 106000
#####
#CENÁRIO 8
#máximo sem outlier=0,9
#media sem outlier=0,3
maximo = max(log(dados_MNAR[-111,]$Income)) #16.1181
media = mean(log(dados_MNAR[-111,]$Income)) #10.9101
beta1 = (log(0.9/0.1)-log(0.3/0.7)) / (maximo-media) #0.5845858
beta0 = log(0.9/0.1) - (beta1*maximo) #-7.225185
PIi = inv.logit(beta0 + beta1*log(dados_MNAR$Income))
random_MNAR = rbinom(length(dados_MNAR$Income), size = 1, prob=PIi)
dados_MNAR[,6] = ifelse(random_MNAR, NA, dados_MNAR$Income)
sum(is.na(dados_MNAR$Income))
#quantidade de na=158

#####
#CENÁRIO 1
#máximo=0,7
#mínimo=0,1
#beta1 = (log(0.7/0.3)-log(0.1/0.9)) / (maximo-minimo) #0.2421608
#beta0 = log(0.7/0.3) - (beta1*maximo) #-3.543804
#PIi = inv.logit(beta0 + beta1*log(dados_MNAR$Income))
#random_MNAR = rbinom(length(dados_MNAR$Income), size = 1, prob=PIi)
#dados_MNAR[,6] = ifelse(random_MNAR, NA, dados_MNAR$Income)
#sum(is.na(dados_MNAR$Income))
#quantidade de na=144
#
#####
#CENÁRIO 2
#máximo=0,3
#mínimo=0,1
#beta1 = (log(0.3/0.7)-log(0.1/0.9)) / (maximo-minimo) #0.1073729
#beta0 = log(0.3/0.7) - (beta1*maximo) #-2.794291
#PIi = inv.logit(beta0 + beta1*log(dados_MNAR$Income))
#random_MNAR = rbinom(length(dados_MNAR$Income), size = 1, prob=PIi)
#dados_MNAR[,6] = ifelse(random_MNAR, NA, dados_MNAR$Income)
#sum(is.na(dados_MNAR$Income))
#quantidade de na=84
#
#####
#CENÁRIO 3
#máximo=0,9
#3ºquantil=0.7
#beta1 = (log(0.9/0.1)-log(0.7/0.3)) / (maximo-quantil3) #-1.273734e-05
#beta0 = log(0.9/0.1) - (beta1*maximo) #2.197456
#PIi = inv.logit(beta0 + beta1*log(dados_MNAR$Income))
#random_MNAR = rbinom(length(dados_MNAR$Income), size = 1, prob=PIi)
#dados_MNAR[,6] = ifelse(random_MNAR, NA, dados_MNAR$Income)
#sum(is.na(dados_MNAR$Income))
#quantidade de na=448
#
#####
#CENÁRIO 4
#media=0,3
#minimo=0,1
#beta1 = (log(0.3/0.7)-log(0.1/0.9)) / (media-minimo) #0.2516708
#beta0 = log(0.3/0.7) - (beta1*media) #-3.596686
#PIi = inv.logit(beta0 + beta1*log(dados_MNAR$Income))
#random_MNAR = rbinom(length(dados_MNAR$Income), size = 1, prob=PIi)
#dados_MNAR[,6] = ifelse(random_MNAR, NA, dados_MNAR$Income)
#sum(is.na(dados_MNAR$Income))
#quantidade de na=150
#
#####
#CENÁRIO 5
#3ºquantil=0,7
#media=0,3
#beta1 = (log(0.7/0.3)-log(0.3/0.7)) / (quantil3-media) #1.59884e-05
#beta0 = log(0.7/0.3) - (beta1*quantil3) #-0.8474725
#PIi = inv.logit(beta0 + beta1*log(dados_MNAR$Income))
#random_MNAR = rbinom(length(dados_MNAR$Income), size = 1, prob=PIi)
#dados_MNAR[,6] = ifelse(random_MNAR, NA, dados_MNAR$Income)
#sum(is.na(dados_MNAR$Income))
#quantidade de na=146
#
#####
#CENÁRIO 6
#3ºquantil=0,6
#minimo=0,4
#beta1 = (log(0.6/0.4)-log(0.4/0.6)) / (quantil3-minimo) #1.59884e-05
#beta0 = log(0.6/0.4) - (beta1*quantil3) #-0.8474725
#PIi = inv.logit(beta0 + beta1*log(dados_MNAR$Income))
#random_MNAR = rbinom(length(dados_MNAR$Income), size = 1, prob=PIi)
#dados_MNAR[,6] = ifelse(random_MNAR, NA, dados_MNAR$Income)
#sum(is.na(dados_MNAR$Income))
#quantidade de na=189
#
#####
#CENÁRIO 7
#3ºquantil=0,9
#media=0,3
#beta1 = (log(0.9/0.1)-log(0.3/0.7)) / (quantil3-media) #2.872487e-05
#beta0 = log(0.9/0.1) - (beta1*quantil3) #-0.8476117
#PIi = inv.logit(beta0 + beta1*log(dados_MNAR$Income))
#random_MNAR = rbinom(length(dados_MNAR$Income), size = 1, prob=PIi)
#dados_MNAR[,6] = ifelse(random_MNAR, NA, dados_MNAR$Income)
#sum(is.na(dados_MNAR$Income))
#quantidade de na=146
#
#método determinístico dos quantis
#quantile(dados_MNAR$Income,0.00)
#quantil0_minimo = 260
#quantile(dados_MNAR$Income,0.25)
#quantil1 = 28000
#quantile(dados_MNAR$Income,0.50)
#quantil2_mediana = 54000
#quantile(dados_MNAR$Income,0.75)
#quantil3 = 106000
#quantile(dados_MNAR$Income,1.00)
#quantil4_maximo = 75000000
#
#set.seed(0)
#dados_MNAR_01 = dados_MNAR[dados_MNAR$Income >= 260 & dados_MNAR$Income <= 28000,]
#random_MNAR_01 = rbinom(length(dados_MNAR_01$Income), size = 1, prob=0.10)
#dados_MNAR_01[,6] = ifelse(random_MNAR_01, NA, dados_MNAR_01$Income)
#
#set.seed(0)
#dados_MNAR_02 = dados_MNAR[dados_MNAR$Income > 28000 & dados_MNAR$Income <= 54000,]
#random_MNAR_02 = rbinom(length(dados_MNAR_02$Income), size = 1, prob=0.25)
#dados_MNAR_02[,6] = ifelse(random_MNAR_02, NA, dados_MNAR_02$Income)
#
#set.seed(0)
#dados_MNAR_03 = dados_MNAR[dados_MNAR$Income > 54000 & dados_MNAR$Income <= 106000,]
#random_MNAR_03 = rbinom(length(dados_MNAR_03$Income), size = 1, prob=0.35)
#dados_MNAR_03[,6] = ifelse(random_MNAR_03, NA, dados_MNAR_03$Income)
#
#set.seed(0)
#dados_MNAR_04 = dados_MNAR[dados_MNAR$Income > 106000 & dados_MNAR$Income <= 75000000,]
#random_MNAR_04 = rbinom(length(dados_MNAR_04$Income), size = 1, prob=0.70)
#dados_MNAR_04[,6] = ifelse(random_MNAR_04, NA, dados_MNAR_04$Income)
#
#dados_MNAR = rbind(dados_MNAR_01,dados_MNAR_02,dados_MNAR_03,dados_MNAR_04)
#