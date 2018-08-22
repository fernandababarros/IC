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

source(file="01-organizando_os_dados.R", encoding="UTF-8")

## IMPUTAÇÃO POR MISSING COMPLETE AT RANDOM - MCAR (PERDA COMPLETAMENTE ALEATÓRIA)
#a probabilidade de perda não depende das variáveis presentes no estudo, sendo
#portanto, constante para todos os indivíduos.
md.pattern(dados) #quantidade de missing
#md.pairs(novo_dados) #quantidade de missing em pares de variáveis

#gerando as m=5 imputações pelo método default pmm
imp <- mice(dados)
imp

#diagnóstico = verificar se os valores imputados são plausíveis
diag = imp$imp$Income

com <- mice::complete(imp, "all", include=T) #complete extrai o banco de dados 
#original com os 5 bancos de dados de imputações, gerando uma matriz com 6*500=3000


## IMPUTAÇÃO POR MISSING AT RANDOM - MAR (PERDA ALEATÓRIA)
#a probabilidade de perda está relacionada com outras variáveis do estudo, mas
#não com a variável de interesse, ou seja, a probabilidade de perda está
#relacionada com um subconjunto conhecido dos dados.




## IMPUTAÇÃO POR NOT MISSING AT RANDOM - NMAR (PERDA NÃO ALEATÓRIA)
#ocorre quando a probabilidade de perda está relacionada com os valores da
#própria variável de interesse, que não foram observados.
