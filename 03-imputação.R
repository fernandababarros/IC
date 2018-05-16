#03-imputação

#carregando os pacotes
library(mice)
library(VIM)
library(CASdatasets)

getwd()
setwd("C:/Users/ferna/Desktop/IC - MICE/Códigos organizados")
load("01-organizando os dados.RData")

md.pattern(dados) #quantidade de missing
#md.pairs(novo_dados) #quantidade de missing em pares de variáveis

#gerando as m=5 imputações pelo método default pmm
imp <- mice(dados)
imp

#diagnóstico = verificar se os valores imputados são plausíveis
diag = imp$imp$Income

com <- complete(imp, "long", inc=T) #complete extrai o banco de dados 
#original com os 5 bancos de dados de imputações, gerando uma matriz com 6*500=3000

save.image(file = "03-imputação.RData")