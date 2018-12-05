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
fit_orig <- lm(log(Income) ~ Gender+Age+MarStat+Education+Ethnicity+Education2, data=dados_originais)
resumo_orig <- summary(fit_orig)

#usar o banco de dados impcom_MCAR que contém o banco de dados original e 
#o banco de dados com as 5 imputações para fazer o caso com loop

fit_MCAR <- with(imp_MCAR, lm(log(Income)~Gender+Age+MarStat+Education+Ethnicity+Education2))
resumo_MCAR <- summary(fit_MCAR)
est_MCAR <- pool(fit_MCAR)
reg_MCAR <- summary(pool(fit_MCAR))

#####REGRESSÃO MAR
#####GÊNERO

fit_MAR_genero <- with(imp_MAR_genero, lm(log(Income)~Gender+Age+MarStat+Education+Ethnicity+Education2))
resumo_MAR_genero <- summary(fit_MAR_genero)
est_MAR_genero <- pool(fit_MAR_genero)
reg_MAR_genero <- summary(pool(fit_MAR_genero))

#####ENSINO

fit_MAR_ensino <- with(imp_MAR_ensino, lm(log(Income)~Gender+Age+MarStat+Education+Ethnicity+Education2))
resumo_MAR_ensino <- summary(fit_MAR_ensino)
est_MAR_ensino <- pool(fit_MAR_ensino)
reg_MAR_ensino <- summary(pool(fit_MAR_ensino))

#####REGRESSÃO MNAR

fit_MNAR <- with(imp_MNAR, lm(log(Income)~Gender+Age+MarStat+Education+Ethnicity+Education2))
resumo_MNAR <- summary(fit_MNAR)
est_MNAR <- pool(fit_MNAR)
reg_MNAR <- summary(pool(fit_MNAR))

##### comparação dos ajustes dos mecanismos de imputação
#compara_reg <- pool.compare(fit_MCAR, fit_MNAR)

#install.packages("plyr")
library(plyr)
lista <- ldply(list(pool(fit_MNAR),pool(fit_MCAR)), summary)

#list(summary(pool(fit_MNAR)), summary(pool(fit_MCAR)))
#plot(list(summary(pool(fit_MNAR)), summary(pool(fit_MCAR))))
