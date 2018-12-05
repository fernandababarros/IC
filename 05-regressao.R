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
fit_orig_MCAR = lm(log(Income) ~ Gender+Age+MarStat+Education+Ethnicity+Education2, data=dados_originais2)
#usar o banco de dados impcom_MCAR que contém o banco de dados original e 
#o banco de dados com as 5 imputações

fit_MCAR <- with(imp_MCAR, lm(log(Income)~Gender+Age+MarStat+Education+Ethnicity+Education2))
resumo_MCAR <- summary(fit_MCAR)
est_MCAR <- pool(fit_MCAR)




#####REGRESSÃO MAR
#####GÊNERO
fit_orig_MAR_genero = lm(log(Income) ~ Gender+Age+MarStat+Education+Ethnicity+Education2, data=dados_originais3)

fit_MAR_genero <- with(imp_MAR_genero, lm(log(Income)~Gender+Age+MarStat+Education+Ethnicity+Education2))
resumo_MAR_genero <- summary(fit_MAR_genero)
est_MAR_genero <- pool(fit_MAR_genero)


#####ENSINO
fit_orig_MAR_ensino = lm(log(Income) ~ Gender+Age+MarStat+Education+Ethnicity+Education2, data=dados_originais4)

fit_MAR_ensino <- with(imp_MAR_ensino, lm(log(Income)~Gender+Age+MarStat+Education+Ethnicity+Education2))
resumo_MAR_ensino <- summary(fit_MAR_ensino)
est_MAR_ensino <- pool(fit_MAR_ensino)




#####REGRESSÃO MNAR
fit_orig_MNAR = lm(log(Income) ~ Gender+Age+MarStat+Education+Ethnicity+Education2, data=dados_originais5)

fit_MNAR <- with(imp_MNAR, lm(log(Income)~Gender+Age+MarStat+Education+Ethnicity+Education2))
resumo_MNAR <- summary(fit_MNAR)
est_MNAR <- pool(fit_MNAR)
