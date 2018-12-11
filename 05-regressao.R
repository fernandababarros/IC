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
## install.packages("plyr")
library(plyr)


source(file="04-graficos_imputacao.R", encoding="UTF-8")

##### REGRESSÃO BANCO DE DADOS ORIGINAL
fit_orig <- lm(log(Income) ~ Gender+Age+MarStat+Education+Ethnicity+Education2, data=dados_originais)
resumo_orig <- summary(fit_orig)
ic_fit_orig <- confint(fit_orig, level=0.95)
coef_fit_orig <- fit_orig$coefficients
#list(coef_fit_orig,ic_fit_orig)
coef_fit_orig <- as.matrix(coef_fit_orig)
ic_fit_orig <- as.matrix(ic_fit_orig)
ic_completo_orig <- cbind(coef_fit_orig,ic_fit_orig)
range_orig <- round(range(ic_completo_orig),digits=2)

#usar o banco de dados impcom_MCAR que contém o banco de dados original e 
#o banco de dados com as 5 imputações para fazer o caso com loop

#####REGRESSÃO MCAR
fit_MCAR <- with(imp_MCAR, lm(log(Income)~Gender+Age+MarStat+Education+Ethnicity+Education2))
## resumo_MCAR <- summary(fit_MCAR)
est_MCAR <- pool(fit_MCAR)
reg_MCAR <- summary(pool(fit_MCAR))
coef_fit_MCAR <- as.matrix(subset(reg_MCAR, select=estimate))
ic_MCAR <- summary(est_MCAR, type=c("tests", "all"), conf.int=TRUE, 
                   conf.level=0.95, exponentiate=FALSE)
ic_completo_MCAR <- as.matrix(subset(ic_MCAR, select=c('estimate','2.5 %','97.5 %')))
range_MCAR <- round(range(ic_completo_MCAR),digits=2)

#####REGRESSÃO MAR

#####GÊNERO
fit_MAR_genero <- with(imp_MAR_genero, lm(log(Income)~Gender+Age+MarStat+Education+Ethnicity+Education2))
## resumo_MAR_genero <- summary(fit_MAR_genero)
est_MAR_genero <- pool(fit_MAR_genero)
reg_MAR_genero <- summary(pool(fit_MAR_genero))
coef_fit_MAR_genero <- as.matrix(subset(reg_MAR_genero, select=estimate))
ic_MAR_genero <- summary(est_MAR_genero, type=c("tests", "all"), conf.int=TRUE,
                         conf.level=0.95, exponentiate=FALSE)
ic_completo_MAR_genero <- as.matrix(subset(ic_MAR_genero,select=c('estimate','2.5 %','97.5 %')))
range_MAR_genero <- round(range(ic_completo_MAR_genero),digits=2)

#####ENSINO
fit_MAR_ensino <- with(imp_MAR_ensino, lm(log(Income)~Gender+Age+MarStat+Education+Ethnicity+Education2))
## resumo_MAR_ensino <- summary(fit_MAR_ensino)
est_MAR_ensino <- pool(fit_MAR_ensino)
reg_MAR_ensino <- summary(pool(fit_MAR_ensino))
coef_fit_MAR_ensino <- as.matrix(subset(reg_MAR_ensino, select=estimate))
ic_MAR_ensino <- summary(est_MAR_ensino, type=c("tests", "all"), conf.int=TRUE,
                         conf.level=0.95, exponentiate=FALSE)
ic_completo_MAR_ensino <- as.matrix(subset(ic_MAR_ensino, select=c('estimate','2.5 %','97.5 %')))
range_MAR_ensino <- round(range(ic_completo_MAR_ensino),digits=2)

#####REGRESSÃO MNAR
fit_MNAR <- with(imp_MNAR, lm(log(Income)~Gender+Age+MarStat+Education+Ethnicity+Education2))
## resumo_MNAR <- summary(fit_MNAR)
est_MNAR <- pool(fit_MNAR)
reg_MNAR <- summary(pool(fit_MNAR))
coef_fit_MNAR <- as.matrix(subset(reg_MNAR, select=estimate))
ic_MNAR <- summary(est_MNAR, type=c("tests", "all"), conf.int=TRUE, 
                   conf.level=0.95, exponentiate=FALSE)
ic_completo_MNAR <- as.matrix(subset(ic_MNAR, select=c('estimate','2.5 %','97.5 %')))
range_MNAR <- round(range(ic_completo_MNAR),digits=2)


##### comparação dos ajustes dos mecanismos de imputação
#compara_reg <- pool.compare(fit_MCAR, fit_MNAR)
#lista <- ldply(list(pool(fit_MNAR),pool(fit_MCAR)), summary)
#list(summary(pool(fit_MNAR)), summary(pool(fit_MCAR)))
#plot(list(summary(pool(fit_MNAR)), summary(pool(fit_MCAR))))


###### GRÁFICO
plot(1, type="n", xlab="", ylab="", xlim=range_orig, ylim=range_orig)
points(x=coef_fit_orig, y=coef_fit_orig, col="black", pch=20, cex=1.5)
segments(x0=ic_completo_orig[,1], x1=ic_completo_orig[,1], y0=ic_completo_orig[,2],
         y1=ic_completo_orig[,3], col="black", lwd=3, lty=1, pch=20)
points(x=coef_fit_MCAR, y=coef_fit_MCAR, col="gray", pch=4, cex=1.5)
segments(x0=ic_completo_MCAR[,1], x1=ic_completo_MCAR[,1], y0=ic_completo_MCAR[,2],
         y1=ic_completo_MCAR[,3], col="gray", lwd=3, lty=1, pch=4)
points(x=coef_fit_MAR_genero, y=coef_fit_MAR_genero, col="gray", pch=17, cex=1.5)
segments(x0=ic_completo_MAR_genero[,1], x1=ic_completo_MAR_genero[,1],
         y0=ic_completo_MAR_genero[,2], y1=ic_completo_MAR_genero[,3], col="gray",
         lwd=3, lty=1, pch=17)
points(x=coef_fit_MAR_ensino, y=coef_fit_MAR_ensino, col="gray", pch=15, cex=1.5)
segments(x0=ic_completo_MAR_ensino[,1], x1=ic_completo_MAR_ensino[,1],
         y0=ic_completo_MAR_ensino[,2], y1=ic_completo_MAR_ensino[,3], col="gray",
         lwd=3, lty=1, pch=15)
points(x=coef_fit_MNAR, y=coef_fit_MNAR, col="gray", pch='-', cex=1.5)
segments(x0=ic_completo_MNAR[,1], x1=ic_completo_MNAR[,1], y0=ic_completo_MNAR[,2],
         y1=ic_completo_MNAR[,3], col="gray", lwd=3, lty=1, pch='-')
