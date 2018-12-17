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
p=11
par(mar=c(5,1,3,1))
plot(1, type="n", xlab="",
     ylab="", xlim=c(0,11), ylim=c(-1.60,11.10),
     xaxt="n", yaxt="n",
     main="Intervalos de Confiança dos Coeficientes da Regressão")

points(x=0:(p-1), y=coef_fit_orig, col="black", pch=20, cex=1.5)
segments(x0=0, x1=0, y0=ic_completo_orig[1,2], y1=ic_completo_orig[1,3],
         col="black", lwd=3, lty=1, pch=20)
segments(x0=1, x1=1, y0=ic_completo_orig[2,2], y1=ic_completo_orig[2,3],
         col="black", lwd=3, lty=1, pch=20)
segments(x0=2, x1=2, y0=ic_completo_orig[3,2], y1=ic_completo_orig[3,3],
         col="black", lwd=3, lty=1, pch=20)
segments(x0=3, x1=3, y0=ic_completo_orig[4,2], y1=ic_completo_orig[4,3],
         col="black", lwd=3, lty=1, pch=20)
segments(x0=4, x1=4, y0=ic_completo_orig[5,2], y1=ic_completo_orig[5,3],
         col="black", lwd=3, lty=1, pch=20)
segments(x0=5, x1=5, y0=ic_completo_orig[6,2], y1=ic_completo_orig[6,3],
         col="black", lwd=3, lty=1, pch=20)
segments(x0=6, x1=6, y0=ic_completo_orig[7,2], y1=ic_completo_orig[7,3],
         col="black", lwd=3, lty=1, pch=20)
segments(x0=7, x1=7, y0=ic_completo_orig[8,2], y1=ic_completo_orig[8,3],
         col="black", lwd=3, lty=1, pch=20)
segments(x0=8, x1=8, y0=ic_completo_orig[9,2], y1=ic_completo_orig[9,3],
         col="black", lwd=3, lty=1, pch=20)
segments(x0=9, x1=9, y0=ic_completo_orig[10,2], y1=ic_completo_orig[10,3],
         col="black", lwd=3, lty=1, pch=20)
segments(x0=10, x1=10, y0=ic_completo_orig[11,2], y1=ic_completo_orig[11,3],
         col="black", lwd=3, lty=1, pch=20)

points(x=0:(p-1) + 0.2, y=coef_fit_MCAR, col="gray50", pch='*', cex=1.5)
segments(x0=0.2, x1=0.2, y0=ic_completo_MCAR[1,2], y1=ic_completo_MCAR[1,3],
         col="gray50", lwd=3, lty=1, pch='*')
segments(x0=1.2, x1=1.2, y0=ic_completo_MCAR[2,2], y1=ic_completo_MCAR[2,3],
         col="gray50", lwd=3, lty=1, pch='*')
segments(x0=2.2, x1=2.2, y0=ic_completo_MCAR[3,2], y1=ic_completo_MCAR[3,3],
         col="gray50", lwd=3, lty=1, pch='*')
segments(x0=3.2, x1=3.2, y0=ic_completo_MCAR[4,2], y1=ic_completo_MCAR[4,3],
         col="gray50", lwd=3, lty=1, pch='*')
segments(x0=4.2, x1=4.2, y0=ic_completo_MCAR[5,2], y1=ic_completo_MCAR[5,3],
         col="gray50", lwd=3, lty=1, pch='*')
segments(x0=5.2, x1=5.2, y0=ic_completo_MCAR[6,2], y1=ic_completo_MCAR[6,3],
         col="gray50", lwd=3, lty=1, pch='*')
segments(x0=6.2, x1=6.2, y0=ic_completo_MCAR[7,2], y1=ic_completo_MCAR[7,3],
         col="gray50", lwd=3, lty=1, pch='*')
segments(x0=7.2, x1=7.2, y0=ic_completo_MCAR[8,2], y1=ic_completo_MCAR[8,3],
         col="gray50", lwd=3, lty=1, pch='*')
segments(x0=8.2, x1=8.2, y0=ic_completo_MCAR[9,2], y1=ic_completo_MCAR[9,3],
         col="gray50", lwd=3, lty=1, pch='*')
segments(x0=9.2, x1=9.2, y0=ic_completo_MCAR[10,2], y1=ic_completo_MCAR[10,3],
         col="gray50", lwd=3, lty=1, pch='*')
segments(x0=10.2, x1=10.2, y0=ic_completo_MCAR[11,2], y1=ic_completo_MCAR[11,3],
         col="gray50", lwd=3, lty=1, pch='*')

points(x=0:(p-1) + 0.4, y=coef_fit_MAR_genero, col="gray50", pch=4, cex=1.5)
segments(x0=0.4, x1=0.4, y0=ic_completo_MAR_genero[1,2], y1=ic_completo_MAR_genero[1,3],
         col="gray50", lwd=3, lty=1, pch=4)
segments(x0=1.4, x1=1.4, y0=ic_completo_MAR_genero[2,2], y1=ic_completo_MAR_genero[2,3],
         col="gray50", lwd=3, lty=1, pch=4)
segments(x0=2.4, x1=2.4, y0=ic_completo_MAR_genero[3,2], y1=ic_completo_MAR_genero[3,3],
         col="gray50", lwd=3, lty=1, pch=4)
segments(x0=3.4, x1=3.4, y0=ic_completo_MAR_genero[4,2], y1=ic_completo_MAR_genero[4,3],
         col="gray50", lwd=3, lty=1, pch=4)
segments(x0=4.4, x1=4.4, y0=ic_completo_MAR_genero[5,2], y1=ic_completo_MAR_genero[5,3],
         col="gray50", lwd=3, lty=1, pch=4)
segments(x0=5.4, x1=5.4, y0=ic_completo_MAR_genero[6,2], y1=ic_completo_MAR_genero[6,3],
         col="gray50", lwd=3, lty=1, pch=4)
segments(x0=6.4, x1=6.4, y0=ic_completo_MAR_genero[7,2], y1=ic_completo_MAR_genero[7,3],
         col="gray50", lwd=3, lty=1, pch=4)
segments(x0=7.4, x1=7.4, y0=ic_completo_MAR_genero[8,2], y1=ic_completo_MAR_genero[8,3],
         col="gray50", lwd=3, lty=1, pch=4)
segments(x0=8.4, x1=8.4, y0=ic_completo_MAR_genero[9,2], y1=ic_completo_MAR_genero[9,3],
         col="gray50", lwd=3, lty=1, pch=4)
segments(x0=9.4, x1=9.4, y0=ic_completo_MAR_genero[10,2], y1=ic_completo_MAR_genero[10,3],
         col="gray50", lwd=3, lty=1, pch=4)
segments(x0=10.4, x1=10.4, y0=ic_completo_MAR_genero[11,2], y1=ic_completo_MAR_genero[11,3],
         col="gray50", lwd=3, lty=1, pch=4)

points(x=0:(p-1) + 0.6, y=coef_fit_MAR_ensino, col="gray50", pch=18, cex=1.5)
segments(x0=0.6, x1=0.6, y0=ic_completo_MAR_ensino[1,2], y1=ic_completo_MAR_ensino[1,3],
         col="gray50", lwd=3, lty=1, pch=18)
segments(x0=1.6, x1=1.6, y0=ic_completo_MAR_ensino[2,2], y1=ic_completo_MAR_ensino[2,3],
         col="gray50", lwd=3, lty=1, pch=18)
segments(x0=2.6, x1=2.6, y0=ic_completo_MAR_ensino[3,2], y1=ic_completo_MAR_ensino[3,3],
         col="gray50", lwd=3, lty=1, pch=18)
segments(x0=3.6, x1=3.6, y0=ic_completo_MAR_ensino[4,2], y1=ic_completo_MAR_ensino[4,3],
         col="gray50", lwd=3, lty=1, pch=18)
segments(x0=4.6, x1=4.6, y0=ic_completo_MAR_ensino[5,2], y1=ic_completo_MAR_ensino[5,3],
         col="gray50", lwd=3, lty=1, pch=18)
segments(x0=5.6, x1=5.6, y0=ic_completo_MAR_ensino[6,2], y1=ic_completo_MAR_ensino[6,3],
         col="gray50", lwd=3, lty=1, pch=18)
segments(x0=6.6, x1=6.6, y0=ic_completo_MAR_ensino[7,2], y1=ic_completo_MAR_ensino[7,3],
         col="gray50", lwd=3, lty=1, pch=18)
segments(x0=7.6, x1=7.6, y0=ic_completo_MAR_ensino[8,2], y1=ic_completo_MAR_ensino[8,3],
         col="gray50", lwd=3, lty=1, pch=18)
segments(x0=8.6, x1=8.6, y0=ic_completo_MAR_ensino[9,2], y1=ic_completo_MAR_ensino[9,3],
         col="gray50", lwd=3, lty=1, pch=18)
segments(x0=9.6, x1=9.6, y0=ic_completo_MAR_ensino[10,2], y1=ic_completo_MAR_ensino[10,3],
         col="gray50", lwd=3, lty=1, pch=18)
segments(x0=10.6, x1=10.6, y0=ic_completo_MAR_ensino[11,2], y1=ic_completo_MAR_ensino[11,3],
         col="gray50", lwd=3, lty=1, pch=18)

points(x=0:(p-1) + 0.8, y=coef_fit_MNAR, col="gray50", pch='-', cex=1.5)
segments(x0=0.8, x1=0.8, y0=ic_completo_MNAR[1,2], y1=ic_completo_MNAR[1,3],
         col="gray50", lwd=3, lty=1, pch='-')
segments(x0=1.8, x1=1.8, y0=ic_completo_MNAR[2,2], y1=ic_completo_MNAR[2,3],
         col="gray50", lwd=3, lty=1, pch='-')
segments(x0=2.8, x1=2.8, y0=ic_completo_MNAR[3,2], y1=ic_completo_MNAR[3,3],
         col="gray50", lwd=3, lty=1, pch='-')
segments(x0=3.8, x1=3.8, y0=ic_completo_MNAR[4,2], y1=ic_completo_MNAR[4,3],
         col="gray50", lwd=3, lty=1, pch='-')
segments(x0=4.8, x1=4.8, y0=ic_completo_MNAR[5,2], y1=ic_completo_MNAR[5,3],
         col="gray50", lwd=3, lty=1, pch='-')
segments(x0=5.8, x1=5.8, y0=ic_completo_MNAR[6,2], y1=ic_completo_MNAR[6,3],
         col="gray50", lwd=3, lty=1, pch='-')
segments(x0=6.8, x1=6.8, y0=ic_completo_MNAR[7,2], y1=ic_completo_MNAR[7,3],
         col="gray50", lwd=3, lty=1, pch='-')
segments(x0=7.8, x1=7.8, y0=ic_completo_MNAR[8,2], y1=ic_completo_MNAR[8,3],
         col="gray50", lwd=3, lty=1, pch='-')
segments(x0=8.8, x1=8.8, y0=ic_completo_MNAR[9,2], y1=ic_completo_MNAR[9,3],
         col="gray50", lwd=3, lty=1, pch='-')
segments(x0=9.8, x1=9.8, y0=ic_completo_MNAR[10,2], y1=ic_completo_MNAR[10,3],
         col="gray50", lwd=3, lty=1, pch='-')
segments(x0=10.8, x1=10.8, y0=ic_completo_MNAR[11,2], y1=ic_completo_MNAR[11,3],
         col="gray50", lwd=3, lty=1, pch='-')

text(0.4, -4, "Intercepto", xpd=NA, srt=90, cex=0.8)
text(1.4, -4, "Gênero\nMasculino", xpd=NA, srt=90, cex=0.8)
text(2.4, -3.2, "Idade", xpd=NA, srt=90, cex=0.8)
text(3.4, -5, "Estado Civil\nMorando Juntos", xpd=NA, srt=90, cex=0.8)
text(4.4, -4.5, "Estado Civil\nOutros", xpd=NA, srt=90, cex=0.8)
text(5.4, -4.5, "Anos de\nEscolaridade", xpd=NA, srt=90, cex=0.8)
text(6.4, -4, "Etnia\nHispânico", xpd=NA, srt=90, cex=0.8)
text(7.4, -3.3, "Etnia\nNegro", xpd=NA, srt=90, cex=0.8)
text(8.4, -3.4, "Etnia\nOutros", xpd=NA, srt=90, cex=0.8)
text(9.4, -4.6, "Ensino Médio", xpd=NA, srt=90, cex=0.8)
text(10.4, -5.1, "Ensino Superior", xpd=NA, srt=90, cex=0.8)
