#05-regressão

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
library(boot)

source(file="04-graficos_imputacao.R", encoding="UTF-8")

#####REGRESSÃO MCAR
modelo1 = lm(log(Income) ~ Gender+Age+MarStat+Education+Ethnicity+Education2, data=dados_originais2)
#usar o banco de dados impcom_MCAR de 



#####REGRESSÃO MAR
#####GÊNERO
modelo2 = lm(log(Income) ~ Gender+Age+MarStat+Education+Ethnicity+Education2, data=dados_originais3)


#####ENSINO
modelo3 = lm(log(Income) ~ Gender+Age+MarStat+Education+Ethnicity+Education2, data=dados_originais4)

#####REGRESSÃO MNAR
modelo4 = lm(log(Income) ~ Gender+Age+MarStat+Education+Ethnicity+Education2, data=dados_originais5)
