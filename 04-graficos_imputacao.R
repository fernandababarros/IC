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

col <- rep(cores[1+as.numeric(is.na(imp_MCAR$data$Income))],6)
#separa os valores observados=cinza e os imputados=preto para a 
#variável Income_missing

#stripplot(Income~.imp, data=com, jit=TRUE, fac=0.8,
#          col=col, pch=20,
#          cex=1.4, xlab="Imputation number")
pdf(NULL)
dev.control(displaylist="enable")
stripplot(log(Income)~.imp, data=com_MCAR, jitter.data=TRUE, factor=0.8,
          col=col, pch=20,
          cex=1.4, xlab="Número de Imputações", ylab="Log(Renda)",
          main="Gráfico com as distribuições dos valores imputados")
p40.graf <- recordPlot()
invisible(dev.off())


#plotando os valores dos observados com os imputados percebemos que está próximo

#boxplots da variável Income com os NA e as 5 imputações
pdf(NULL)
dev.control(displaylist="enable")
boxplot(log(Income) ~ .imp, data=com_MCAR, xlab="Imputações", ylab="log(Renda)",
        col=c("grey","#d7191c","#fdae61","#e78ac3","#abdda4","#2b83ba"),
        main="Box-plots da variável Renda com os dados ausentes \n e as imputações")
p41.graf <- recordPlot()
invisible(dev.off())

#criando o banco de dados com as 500 linhas dos dados originais e 
#as 2500 linhas com as 5 imputações
impcom = com_MCAR[-(1:500),-2]
dados_originais[".imp"] = "0-original"
dados_originais2 = dados_originais[c(".imp", "Gender", "Age", "MarStat",
                                     "Education", "Ethnicity", "Income")]
origcomimp = rbind(dados_originais, impcom)

#boxplots da variável Income dos dados originais e as 5 imputações
pdf(NULL)
dev.control(displaylist="enable")
boxplot(log(Income) ~ .imp, data=origcomimp, xlab="Imputações", ylab="Renda",
        col=c("white","#d7191c","#fdae61","#e78ac3","#abdda4","#2b83ba"),
        main="Box-plots dos dados originais e das imputações")
p42.graf <- recordPlot()
invisible(dev.off())

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
pdf(NULL)
dev.control(displaylist="enable")
qqplot(log(dados_originais$Income), log(imp0com$Income), pch=20,
       main="QQ-plot das imputações", xlab="Quantil original da Renda",
       ylab="Quantil imputado da Renda")
imp1 = qqplot(log(dados_originais$Income), log(imp1com$Income), plot.it=FALSE)
imp2 = qqplot(log(dados_originais$Income), log(imp2com$Income), plot.it=FALSE)
imp3 = qqplot(log(dados_originais$Income), log(imp3com$Income), plot.it=FALSE)
imp4 = qqplot(log(dados_originais$Income), log(imp1com$Income), plot.it=FALSE)
imp5 = qqplot(log(dados_originais$Income), log(imp1com$Income), plot.it=FALSE)
points(c(imp1, imp2, imp3, imp4, imp5),
       col=c("black", "#d7191c", "#fdae61","#e78ac3","#abdda4","#2b83ba"),
       pch=20)
legend(x=16.5,y=14.5,c("imp0", "imp1", "imp2", "imp3", "imp4", "imp5"),
       col=c("black", "#d7191c", "#fdae61","#e78ac3","#abdda4","#2b83ba"),
       pch=20)
abline(0,1)
p43.graf <- recordPlot()
invisible(dev.off())

#fazer box-plot das 96 observações retiradas do banco original e as imputadas
#valores_imputados = impcom[rep(random,6),]
#View(impcom)



## IMPUTAÇÃO MAR







## IMPUTAÇÃO NMAR