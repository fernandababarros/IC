#01-organizando os dados

#carregando os pacotes
library(mice)
library(VIM)
library(CASdatasets)

#carregando o banco de dados
data(ustermlife)
head(ustermlife)
dados_originais = subset(ustermlife, select = c(Gender, Age, MarStat,
                                                Education, Ethnicity, Income))

#gerando NA's na variável Income
#e criando um novo banco de dados com os valores faltantes
set.seed(0)
dados = subset(ustermlife, select = c(Gender, Age, MarStat, Education,
                                      Ethnicity, Income))
random = rbinom(length(dados$Income), size = 1, prob=0.2)
dados[,6] = ifelse(random, NA , dados$Income)

