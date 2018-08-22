#01-organizando os dados

#carregando os pacotes
library(mice)
library(VIM)
library(CASdatasets)
library(tidyverse)
library(gridExtra)
library(ggplot2)
## library(pryr)
library(car)

#carregando o banco de dados
data(ustermlife)
head(ustermlife)
dados_originais = subset(ustermlife, select = c(Gender, Age, MarStat,
                                                Education, Ethnicity, Income))

## BANCO DE DADOS PARA IMPUTAÇÃO MCAR
#gerando NA's na variável Income
#e criando um novo banco de dados com os valores faltantes
set.seed(0)
dados_MCAR = subset(ustermlife, select = c(Gender, Age, MarStat, Education,
                                      Ethnicity, Income))
random_MCAR = rbinom(length(dados_MCAR$Income), size = 1, prob=0.2)
dados_MCAR[,6] = ifelse(random_MCAR, NA, dados_MCAR$Income)


source(file="02-analise_descritiva.R", encoding="UTF-8")
## BANCO DE DADOS PARA IMPUTAÇÃO MAR
#gênero feminino
set.seed(0)
dados_MAR = dados_originais
dados_MAR_fem = dados_MAR[dados_originais$Gender=='Feminino',]
random_MAR_fem = rbinom(length(dados_MAR_fem$Income), size = 1, prob=0.1)
dados_MAR_fem[,7] = ifelse(random_MAR_fem, NA, dados_MAR_fem$Income)

#gênero masculino
dados_MAR_masc = dados_MAR[dados_originais$Gender=='Masculino',]
random_MAR_masc = rbinom(length(dados_MAR_masc$Income), size = 1, prob=0.3)
dados_MAR_masc[,7] = ifelse(random_MAR_masc, NA, dados_MAR_masc$Income)

#ensino fundamental
dados_MAR_ef = dados_MAR[dados_originais$Education2=='Ensino Fundamental',]
random_MAR_ef = rbinom(length(dados_MAR_ef$Income), size = 1, prob=0.05)
dados_MAR_ef[,7] = ifelse(random_MAR_ef, NA, dados_MAR_ef$Income)

#ensino médio
dados_MAR_em = dados_MAR[dados_originais$Education2=='Ensino Médio',]
random_MAR_em = rbinom(length(dados_MAR_em$Income), size = 1, prob=0.2)
dados_MAR_em[,7] = ifelse(random_MAR_em, NA, dados_MAR_em$Income)

#ensino superior
dados_MAR_es = dados_MAR[dados_originais$Education2=='Ensino Superior',]
random_MAR_es = rbinom(length(dados_MAR_es$Income), size = 1, prob=0.4)
dados_MAR_es[,7] = ifelse(random_MAR_es, NA, dados_MAR_es$Income)

#banco completo com os missings na renda pelas probabilidades de feminino
#e de masculino
dados_MAR_genero = rbind(dados_MAR_fem,dados_MAR_masc)

#banco completo com os missings na renda pelas probabilidades de ensino fundamental,
#ensino médio e ensino superior
dados_MAR_ensino = rbind(dados_MAR_ef,dados_MAR_em,dados_MAR_es)

