#03-imputação

#carregando os pacotes
library(mice)
library(VIM)
library(CASdatasets)
library(tidyverse)
library(gridExtra)
library(ggplot2)
## library(pryr)
library(car)
library(knitr)

source(file="01-organizando_os_dados.R", encoding="UTF-8")

## IMPUTAÇÃO POR MISSING COMPLETE AT RANDOM - MCAR (PERDA COMPLETAMENTE ALEATÓRIA)
#a probabilidade de perda não depende das variáveis presentes no estudo, sendo
#portanto, constante para todos os indivíduos.
md.pattern(dados_MCAR) #quantidade de missing
#md.pairs(novo_dados) #quantidade de missing em pares de variáveis

#gerando as m=5 imputações pelo método default pmm
imp_MCAR <- mice(dados_MCAR, seed=2018)
#imp_MCAR

#diagnóstico = verificar se os valores imputados são plausíveis
valores_imp = imp_MCAR$imp$Income
#valores_imp possui somente os valores imputados

com_MCAR <- mice::complete(imp_MCAR, "long", include=TRUE) #complete extrai o banco de dados 
#original com os 5 bancos de dados de imputações, gerando uma matriz com 6*500=3000
com_MCAR$Imputed <- factor(com_MCAR$.imp >0,labels = c("Observado","Imputado"))
com_MCAR$Missing <- factor(is.na(com_MCAR$Income), labels=c("Observado","Ausente"))
vetor = com_MCAR[1:500,11]
com_MCAR$Missing = rep(vetor,6)



## IMPUTAÇÃO POR MISSING AT RANDOM - MAR (PERDA ALEATÓRIA)
#a probabilidade de perda está relacionada com outras variáveis do estudo, mas
#não com a variável de interesse, ou seja, a probabilidade de perda está
#relacionada com um subconjunto conhecido dos dados.
#imputação pelo missing relacionado ao gênero
md.pattern(dados_MAR_genero)
#gerando as m=5 imputações pelo método default pmm
imp_MAR_genero <- mice(dados_MAR_genero, seed=2018)
valores_imp_genero = imp_MAR_genero$imp$Income
com_MAR_genero <- mice::complete(imp_MAR_genero, "long", include=T)


#imputação pelo missing relacionado ao tipo de ensino
md.pattern(dados_MAR_ensino)
#gerando as m=5 imputações pelo método default pmm
imp_MAR_ensino <- mice(dados_MAR_ensino, seed=2018)
valores_imp_ensino = imp_MAR_ensino$imp$Income
com_MAR_ensino <- mice::complete(imp_MAR_ensino, "long", include=T)


## IMPUTAÇÃO POR NOT MISSING AT RANDOM - NMAR (PERDA NÃO ALEATÓRIA)
#ocorre quando a probabilidade de perda está relacionada com os valores da
#própria variável de interesse, que não foram observados.

