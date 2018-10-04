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
dados_MNAR$Minimo = ifelse(dados_MNAR$Income==min(dados_MNAR$Income),NA,1)
dados_MNAR$Maximo = ifelse(dados_MNAR$Income==max(dados_MNAR$Income),NA,1)
dados_MNAR$Mediana = ifelse(dados_MNAR$Income==median(dados_MNAR$Income),NA,1)


#Mínimo
##set.seed(0)
##dados_MNAR = dados_originais
##dados_MNAR$Minimo = ifelse(dados_MNAR$Income==min(dados_MNAR$Income),NA,1)
#random_MNAR_min = rbinom(length(dados_MNAR$Income), size = 1, prob=0.1)
#dados_MNAR_min[,6] = ifelse(random_MNAR_min, NA, dados_MNAR_min$Income)
#minimo da renda possui 2 observações no banco de dados

#Máximo
##set.seed(0)
##dados_MNAR$Maximo = ifelse(dados_MNAR$Income==max(dados_MNAR$Income),NA,1)
#random_MNAR_max = rbinom(length(dados_MNAR$Income), size = 1, prob=0.7)
#dados_MNAR[,6] = ifelse(random_MNAR, NA, dados_MNAR$Income)
#máximo da renda possui 1 observação no banco de dados

##set.seed(0)
##dados_MNAR$Mediana = ifelse(dados_MNAR$Income==median(dados_MNAR$Income),NA,1)
#random_MNAR_mediana = rbinom(length(dados_MNAR$Income), size = 1, prob=0.2)
#dados_MNAR[,6] = ifelse(random_MNAR, NA, dados_MNAR$Income)
#mediana da renda possui 4 observações no banco de dados

set.seed(0)
dados_MNAR = dados_originais
head(dados_MNAR)
summary(dados_MNAR)
sapply(dados_MNAR, sd)

PIi <- glm(Income ~ Gender + Age + MarStat + Education + Ethnicity + Education2,
               data=dados_MNAR, family="binomial")




