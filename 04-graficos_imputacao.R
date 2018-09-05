#04-gráficos imputação

#carregando os pacotes
library(mice)
library(VIM)
library(CASdatasets)
library(tidyverse)
library(gridExtra)
library(ggplot2)
## library(pryr)
library(car)

source(file="03-imputacao.R", encoding="UTF-8")

## IMPUTAÇÃO MCAR
cores = rgb(t(col2rgb(c("grey","black")))/255,alpha=0.5)

col <- rep(cores[1+as.numeric(is.na(imp_MCAR$data$Income))],7)
#separa os valores observados=cinza e os imputados=preto para a 
#variável Income_missing

#stripplot(Income~.imp, data=com, jit=TRUE, fac=0.8,
#          col=col, pch=20,
#          cex=1.4, xlab="Imputation number")
pdf("p40-graf.pdf", width=8, height=8, pointsize=24)
stripplot(log(Income)~.imp, data=com_MCAR, jitter.data=TRUE, factor=0.8,
          col=col, pch=20,
          cex=1.4, xlab="Número de Imputações", ylab="Log(Renda)",
          main="Gráfico com as distribuições dos valores imputados")
dev.off()


#plotando os valores dos observados com os imputados percebemos que está próximo

#boxplots da variável Income com os NA e as 5 imputações
pdf("p41-graf.pdf", width=12, height=12, pointsize=24)
boxplot(log(Income) ~ .imp, data=com_MCAR, xlab="Imputações", ylab="log(Renda)",
        col=c("grey","#d7191c","#fdae61","#e78ac3","#abdda4","#2b83ba"),
        main="Box-plots da variável Renda com os dados ausentes \n e as imputações")
dev.off()

#criando o banco de dados com as 500 linhas dos dados originais e 
#as 2500 linhas com as 5 imputações
impcom_MCAR = com_MCAR[-(1:500),-2]
dados_originais[".imp"] = "0-original"
dados_originais2 = dados_originais[c(".imp","Gender","Age","MarStat",
                                     "Education","Ethnicity","Income","Education2")]
origcomimp_MCAR = rbind(dados_originais, impcom_MCAR)

#boxplots da variável Income dos dados originais e as 5 imputações
pdf("p42-graf.pdf", width=10, height=10, pointsize=24)
boxplot(log(Income) ~ .imp, data=origcomimp_MCAR, xlab="Imputações", ylab="Renda",
        col=c("white","#d7191c","#fdae61","#e78ac3","#abdda4","#2b83ba"),
        main="Box-plots dos dados originais e das imputações")
dev.off()

#QQ-plot
imp0com_MCAR = com_MCAR[1:500,]
imp1com_MCAR = com_MCAR[501:1000,]
imp2com_MCAR = com_MCAR[1001:1500,]
imp3com_MCAR = com_MCAR[1501:2000,]
imp4com_MCAR = com_MCAR[2001:2500,]
imp5com_MCAR = com_MCAR[2501:3000,]

#qqplot(log(dados_originais$Income), log(imp0com$Income), pch=0)
#qqplot(log(dados_originais$Income), log(imp1com$Income), pch=1)
#qqplot(log(dados_originais$Income), log(imp2com$Income), pch=2)
#qqplot(log(dados_originais$Income), log(imp3com$Income), pch=3)
#qqplot(log(dados_originais$Income), log(imp4com$Income), pch=4)
#qqplot(log(dados_originais$Income), log(imp5com$Income), pch=5)
#qqplot(log(dados_originais2$Income), log(com$Income), pch=0:6)
pdf("p43-graf.pdf", width=12, height=12, pointsize=24)
qqplot(log(dados_originais$Income), log(imp0com_MCAR$Income), pch=20,
       main="QQ-plot das imputações", xlab="Quantil original da Renda",
       ylab="Quantil imputado da Renda")
imp1 = qqplot(log(dados_originais$Income), log(imp1com_MCAR$Income), plot.it=FALSE)
imp2 = qqplot(log(dados_originais$Income), log(imp2com_MCAR$Income), plot.it=FALSE)
imp3 = qqplot(log(dados_originais$Income), log(imp3com_MCAR$Income), plot.it=FALSE)
imp4 = qqplot(log(dados_originais$Income), log(imp4com_MCAR$Income), plot.it=FALSE)
imp5 = qqplot(log(dados_originais$Income), log(imp5com_MCAR$Income), plot.it=FALSE)
points(c(imp1, imp2, imp3, imp4, imp5),
       col=c("black", "#d7191c", "#fdae61","#e78ac3","#abdda4","#2b83ba"),
       pch=20)
legend(x=16.5,y=14.5,c("imp0", "imp1", "imp2", "imp3", "imp4", "imp5"),
       col=c("black", "#d7191c", "#fdae61","#e78ac3","#abdda4","#2b83ba"),
       pch=20)
abline(0,1)
dev.off()

#fazer box-plot das 96 observações retiradas do banco original e as imputadas




## IMPUTAÇÃO MAR
col <- rep(cores[1+as.numeric(is.na(imp_MAR_genero$data$Income))],7)
#separa os valores observados=cinza e os imputados=preto para a 
#variável Income_missing

pdf("p45-graf.pdf", width=8, height=8, pointsize=24)
stripplot(log(Income)~.imp, data=com_MAR_genero, jitter.data=TRUE, factor=0.8,
          col=col, pch=20,
          cex=1.4, xlab="Número de Imputações", ylab="Log(Renda)",
          main="Gráfico com as distribuições dos valores imputados")
dev.off()


#plotando os valores dos observados com os imputados percebemos que está próximo
#boxplots da variável Income com os NA e as 5 imputações
pdf("p46-graf.pdf", width=10, height=10, pointsize=24)
boxplot(log(Income) ~ .imp, data=com_MAR_genero, xlab="Imputações",
        ylab="log(Renda)",
        col=c("grey","#d7191c","#fdae61","#e78ac3","#abdda4","#2b83ba"),
        main="Box-plots da variável Renda com os dados ausentes \n e as imputações")
dev.off()

#criando o banco de dados com as 500 linhas dos dados originais e 
#as 2500 linhas com as 5 imputações
impcom_MAR_genero = com_MAR_genero[-(1:500),-2]
dados_originais[".imp"] = "0-original"
dados_originais2 = dados_originais[c(".imp","Gender","Age","MarStat",
                                     "Education","Ethnicity","Income","Education2")]
origcomimp_MAR_genero = rbind(dados_originais, impcom_MAR_genero)

#boxplots da variável Income dos dados originais e as 5 imputações
pdf("p47-graf.pdf", width=10, height=10, pointsize=24)
boxplot(log(Income) ~ .imp, data=origcomimp_MAR_genero, xlab="Imputações",
        ylab="Renda",
        col=c("white","#d7191c","#fdae61","#e78ac3","#abdda4","#2b83ba"),
        main="Box-plots dos dados originais e das imputações")
dev.off()

#QQ-plot
imp0com_MAR_genero = com_MAR_genero[1:500,]
imp1com_MAR_genero = com_MAR_genero[501:1000,]
imp2com_MAR_genero = com_MAR_genero[1001:1500,]
imp3com_MAR_genero = com_MAR_genero[1501:2000,]
imp4com_MAR_genero = com_MAR_genero[2001:2500,]
imp5com_MAR_genero = com_MAR_genero[2501:3000,]

#qqplot(log(dados_originais$Income), log(imp0com$Income), pch=0)
#qqplot(log(dados_originais$Income), log(imp1com$Income), pch=1)
#qqplot(log(dados_originais$Income), log(imp2com$Income), pch=2)
#qqplot(log(dados_originais$Income), log(imp3com$Income), pch=3)
#qqplot(log(dados_originais$Income), log(imp4com$Income), pch=4)
#qqplot(log(dados_originais$Income), log(imp5com$Income), pch=5)
#qqplot(log(dados_originais2$Income), log(com$Income), pch=0:6)
pdf("p48-graf.pdf", width=12, height=12, pointsize=24)
qqplot(log(dados_originais$Income), log(imp0com_MAR_genero$Income), pch=20,
       main="QQ-plot das imputações", xlab="Quantil original da Renda",
       ylab="Quantil imputado da Renda")
imp1 = qqplot(log(dados_originais$Income), log(imp1com_MAR_genero$Income),
              plot.it=FALSE)
imp2 = qqplot(log(dados_originais$Income), log(imp2com_MAR_genero$Income),
              plot.it=FALSE)
imp3 = qqplot(log(dados_originais$Income), log(imp3com_MAR_genero$Income),
              plot.it=FALSE)
imp4 = qqplot(log(dados_originais$Income), log(imp4com_MAR_genero$Income),
              plot.it=FALSE)
imp5 = qqplot(log(dados_originais$Income), log(imp5com_MAR_genero$Income),
              plot.it=FALSE)
points(c(imp1, imp2, imp3, imp4, imp5),
       col=c("black", "#d7191c", "#fdae61","#e78ac3","#abdda4","#2b83ba"),
       pch=20)
legend(x=16.5,y=14.5,c("imp0", "imp1", "imp2", "imp3", "imp4", "imp5"),
       col=c("black", "#d7191c", "#fdae61","#e78ac3","#abdda4","#2b83ba"),
       pch=20)
abline(0,1)
dev.off()

#fazer box-plot das 96 observações retiradas do banco original e as imputadas


## IMPUTAÇÃO MAR
col <- rep(cores[1+as.numeric(is.na(imp_MAR_ensino$data$Income))],7)
#separa os valores observados=cinza e os imputados=preto para a 
#variável Income_missing

pdf("p50-graf.pdf", width=8, height=8, pointsize=24)
stripplot(log(Income)~.imp, data=com_MAR_ensino, jitter.data=TRUE, factor=0.8,
          col=col, pch=20,
          cex=1.4, xlab="Número de Imputações", ylab="Log(Renda)",
          main="Gráfico com as distribuições dos valores imputados")
dev.off()


#plotando os valores dos observados com os imputados percebemos que está próximo
#boxplots da variável Income com os NA e as 5 imputações
pdf("p51-graf.pdf", width=10, height=10, pointsize=24)
boxplot(log(Income) ~ .imp, data=com_MAR_ensino, xlab="Imputações",
        ylab="log(Renda)",
        col=c("grey","#d7191c","#fdae61","#e78ac3","#abdda4","#2b83ba"),
        main="Box-plots da variável Renda com os dados ausentes \n e as imputações")
dev.off()

#criando o banco de dados com as 500 linhas dos dados originais e 
#as 2500 linhas com as 5 imputações
impcom_MAR_ensino = com_MAR_ensino[-(1:500),-2]
dados_originais[".imp"] = "0-original"
dados_originais2 = dados_originais[c(".imp","Gender","Age","MarStat",
                                     "Education","Ethnicity","Income","Education2")]
origcomimp_MAR_ensino = rbind(dados_originais, impcom_MAR_ensino)

#boxplots da variável Income dos dados originais e as 5 imputações
pdf("p52-graf.pdf", width=10, height=10, pointsize=24)
boxplot(log(Income) ~ .imp, data=origcomimp_MAR_ensino, xlab="Imputações",
        ylab="Renda",
        col=c("white","#d7191c","#fdae61","#e78ac3","#abdda4","#2b83ba"),
        main="Box-plots dos dados originais e das imputações")
dev.off()

#QQ-plot
imp0com_MAR_ensino = com_MAR_ensino[1:500,]
imp1com_MAR_ensino = com_MAR_ensino[501:1000,]
imp2com_MAR_ensino = com_MAR_ensino[1001:1500,]
imp3com_MAR_ensino = com_MAR_ensino[1501:2000,]
imp4com_MAR_ensino = com_MAR_ensino[2001:2500,]
imp5com_MAR_ensino = com_MAR_ensino[2501:3000,]

pdf("p53-graf.pdf", width=12, height=12, pointsize=24)
qqplot(log(dados_originais$Income), log(imp0com_MAR_ensino$Income), pch=20,
       main="QQ-plot das imputações", xlab="Quantil original da Renda",
       ylab="Quantil imputado da Renda")
imp1 = qqplot(log(dados_originais$Income), log(imp1com_MAR_ensino$Income),
              plot.it=FALSE)
imp2 = qqplot(log(dados_originais$Income), log(imp2com_MAR_ensino$Income),
              plot.it=FALSE)
imp3 = qqplot(log(dados_originais$Income), log(imp3com_MAR_ensino$Income),
              plot.it=FALSE)
imp4 = qqplot(log(dados_originais$Income), log(imp4com_MAR_ensino$Income),
              plot.it=FALSE)
imp5 = qqplot(log(dados_originais$Income), log(imp5com_MAR_ensino$Income),
              plot.it=FALSE)
points(c(imp1, imp2, imp3, imp4, imp5),
       col=c("black", "#d7191c", "#fdae61","#e78ac3","#abdda4","#2b83ba"),
       pch=20)
legend(x=16.5,y=14.5,c("imp0", "imp1", "imp2", "imp3", "imp4", "imp5"),
       col=c("black", "#d7191c", "#fdae61","#e78ac3","#abdda4","#2b83ba"),
       pch=20)
abline(0,1)
dev.off()

#fazer box-plot das 96 observações retiradas do banco original e as imputadas









## IMPUTAÇÃO NMAR