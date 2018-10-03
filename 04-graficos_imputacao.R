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
library(knitr)

source(file="03-imputacao.R", encoding="UTF-8")

## IMPUTAÇÃO MCAR
cores = rgb(t(col2rgb(c("grey","black")))/255,alpha=0.5)

col <- rep(cores[1+as.numeric(is.na(imp_MCAR$data$Income))],7)
#separa os valores observados=cinza e os imputados=preto para a 
#variável Income_missing


gg21.graf = ggplot(com_MCAR, aes(x=log(Income), colour=Imputed)) +
              geom_density()
gg22.graf = ggplot(com_MCAR, aes(x=log(Income), colour=Imputed, group=.imp)) +
              geom_density()
gg23.graf = ggplot(data=com_MCAR, aes(x=.imp, y=log(Income), color=Imputed)) +
              geom_point()
#plotando os valores dos observados com os 96 valores ausentes
#e os observados mais os ausentes imputados
gg24.graf = ggplot(com_MCAR, aes(x=log(Income), colour=Missing)) +
  geom_density()
#gg25.graf = ggplot(com_MCAR, aes(x=log(Income), colour=Missing, group=.imp)) +
#  geom_density()
gg26.graf = ggplot(data=com_MCAR, aes(x=.imp, y=log(Income), color=Missing)) +
  geom_point(position=position_dodge(0.3))
gg27.graf = ggplot(data=com_MCAR, aes(x=.imp, y=log(Income), color=Missing)) +
  geom_point() +
  geom_jitter(width = 0.15, height = 0.3) +
  scale_color_manual( values=c("gray45","black"))

#boxplots da variável Income com os NA e as 5 imputações
pdf("p41-graf.pdf", width=12, height=12, pointsize=24)
boxplot(log(Income) ~ .imp, data=com_MCAR, xlab="Imputações", ylab="log(Renda)",
        col=c("grey","#d7191c","#fdae61","#e78ac3","#abdda4","#2b83ba"),
        main="Box-plots da variável Renda com os dados ausentes \n e as imputações")
dev.off()

#criando o banco de dados com as 500 linhas dos dados originais e 
#as 2500 linhas com as 5 imputações
impcom_MCAR = com_MCAR[-(1:500),c(-2,-10,-11)]
dados_originais[".imp"] = "0-original"
dados_originais2 = dados_originais[c(".imp","Gender","Age","MarStat",
                                     "Education","Ethnicity","Income","Education2")]

origcomimp_MCAR = rbind(dados_originais2, impcom_MCAR)

#boxplots da variável Income dos dados originais e as 5 imputações
pdf("p42-graf.pdf", width=10, height=10, pointsize=24)
boxplot(log(Income) ~ .imp, data=origcomimp_MCAR, xlab="Imputações",
        ylab="Log(Renda)",
        col=c("white","#d7191c","#fdae61","#e78ac3","#abdda4","#2b83ba"),
        main="Box-plots dos dados originais e das imputações")
dev.off()

#stripplot do banco original com os valores observados e o banco com os valores
#imputados
origcomimp_MCAR = cbind(origcomimp_MCAR,com_MCAR$Missing)

colnames(origcomimp_MCAR) <- c(".imp","Gender","Age","MarStat","Education",
                               "Ethnicity","Income","Education2","Indication")

gg28.graf = ggplot(origcomimp_MCAR, aes(x=log(Income), colour=Indication)) +
              geom_density()
#gg29.graf = ggplot(origcomimp_MCAR, aes(x=log(Income), colour=Indication, group=.imp)) +
#              geom_density()
gg30.graf = ggplot(data=origcomimp_MCAR, aes(x=.imp, y=log(Income), color=Indication)) +
              geom_point()
#plotando os valores dos observados com os 96 valores ausentes
#e os observados mais os ausentes imputados
gg31.graf = ggplot(origcomimp_MCAR, aes(x=log(Income), colour=Indication)) +
              geom_density()
#gg32.graf = ggplot(origcomimp_MCAR, aes(x=log(Income), colour=Indication, group=.imp)) +
#              geom_density()
gg33.graf = ggplot(data=origcomimp_MCAR, aes(x=.imp, y=log(Income), color=Indication)) +
              geom_point(position=position_dodge(0.3))
gg34.graf = ggplot(data=origcomimp_MCAR, aes(x=.imp, y=log(Income), color=Indication)) +
              geom_point() +
              geom_jitter(width = 0.15, height = 0.3) +
              scale_color_manual( values=c("gray45","black"))

#QQ-plot
imp0com_MCAR = com_MCAR[1:500,]
imp1com_MCAR = com_MCAR[501:1000,]
imp2com_MCAR = com_MCAR[1001:1500,]
imp3com_MCAR = com_MCAR[1501:2000,]
imp4com_MCAR = com_MCAR[2001:2500,]
imp5com_MCAR = com_MCAR[2501:3000,]

pdf("p43-graf.pdf", width=12, height=12, pointsize=24)
qqplot(log(dados_originais$Income), log(imp0com_MCAR$Income), pch=20,
       main="QQ-plot das imputações", xlab="Quantil original da Log(Renda)",
       ylab="Quantil imputado da Log(Renda)")
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

#box-plot das 96 observações retiradas do banco original e as imputadas
somente_valorigeimp_MCAR <- subset(origcomimp_MCAR, Indication=='Ausente')

pdf("p44-graf.pdf", width=10, height=10, pointsize=24)
boxplot(log(Income) ~ .imp, data=somente_valorigeimp_MCAR, xlab="Imputações",
        ylab="Renda",
        col=c("white","#d7191c","#fdae61","#e78ac3","#abdda4","#2b83ba"),
        main="Box-plots das imputações")
dev.off()

gg35.graf = ggplot(data=somente_valorigeimp_MCAR, aes(x=.imp, y=log(Income))) +
              geom_point()
gg36.graf = ggplot(data=somente_valorigeimp_MCAR, aes(x=.imp, y=log(Income))) +
              geom_point() +
              geom_jitter(width = 0.05, height = 0.3)


## IMPUTAÇÃO MAR - gênero
col <- rep(cores[1+as.numeric(is.na(imp_MAR_genero$data$Income))],7)
#separa os valores observados=cinza e os imputados=preto para a 
#variável Income_missing

gg40.graf = ggplot(com_MAR_genero, aes(x=log(Income), colour=Imputed)) +
  geom_density()
gg41.graf = ggplot(com_MAR_genero, aes(x=log(Income), colour=Imputed, group=.imp)) +
  geom_density()
gg42.graf = ggplot(data=com_MAR_genero, aes(x=.imp, y=log(Income), color=Imputed)) +
  geom_point()
#plotando os valores dos observados com os 96 valores ausentes
#e os observados mais os ausentes imputados
gg43.graf = ggplot(com_MAR_genero, aes(x=log(Income), colour=Missing)) +
  geom_density()
gg44.graf = ggplot(data=com_MAR_genero, aes(x=.imp, y=log(Income), color=Missing)) +
  geom_point(position=position_dodge(0.3))
gg45.graf = ggplot(data=com_MAR_genero, aes(x=.imp, y=log(Income), color=Missing)) +
  geom_point() +
  geom_jitter(width = 0.15, height = 0.3) +
  scale_color_manual( values=c("gray45","black"))

#boxplots da variável Income com os NA e as 5 imputações
pdf("p46-graf.pdf", width=10, height=10, pointsize=24)
boxplot(log(Income) ~ .imp, data=com_MAR_genero, xlab="Imputações",
        ylab="log(Renda)",
        col=c("grey","#d7191c","#fdae61","#e78ac3","#abdda4","#2b83ba"),
        main="Box-plots da variável Renda com os dados ausentes \n e as imputações")
dev.off()

#criando o banco de dados com as 500 linhas dos dados originais e 
#as 2500 linhas com as 5 imputações
impcom_MAR_genero = com_MAR_genero[-(1:500),c(-2,-10,-11)]
dados_originais[".imp"] = "0-original"
dados_originais3 = dados_originais[c(".imp","Gender","Age","MarStat","Education",
                                     "Ethnicity","Income","Education2")]

origcomimp_MAR_genero = rbind(dados_originais3, impcom_MAR_genero)

#boxplots da variável Income dos dados originais e as 5 imputações
pdf("p47-graf.pdf", width=10, height=10, pointsize=24)
boxplot(log(Income) ~ .imp, data=origcomimp_MAR_genero, xlab="Imputações",
        ylab="Renda",
        col=c("white","#d7191c","#fdae61","#e78ac3","#abdda4","#2b83ba"),
        main="Box-plots dos dados originais e das imputações")
dev.off()

#stripplot do banco original com os valores observados e o banco com os valores
#imputados
origcomimp_MAR_genero = cbind(origcomimp_MAR_genero,com_MAR_genero$Missing)

colnames(origcomimp_MAR_genero) <- c(".imp","Gender","Age","MarStat","Education",
                                    "Ethnicity","Income","Education2","Indication")

gg46.graf = ggplot(origcomimp_MAR_genero, aes(x=log(Income), colour=Indication)) +
  geom_density()
gg47.graf = ggplot(data=origcomimp_MAR_genero, aes(x=.imp, y=log(Income), color=Indication)) +
  geom_point()
#plotando os valores dos observados com os 96 valores ausentes
#e os observados mais os ausentes imputados
gg48.graf = ggplot(origcomimp_MAR_genero, aes(x=log(Income), colour=Indication)) +
  geom_density()
gg49.graf = ggplot(data=origcomimp_MAR_genero, aes(x=.imp, y=log(Income), color=Indication)) +
  geom_point(position=position_dodge(0.3))
gg50.graf = ggplot(data=origcomimp_MAR_genero, aes(x=.imp, y=log(Income), color=Indication)) +
  geom_point() +
  geom_jitter(width = 0.15, height = 0.3) +
  scale_color_manual( values=c("gray45","black"))


#QQ-plot
imp0com_MAR_genero = com_MAR_genero[1:500,]
imp1com_MAR_genero = com_MAR_genero[501:1000,]
imp2com_MAR_genero = com_MAR_genero[1001:1500,]
imp3com_MAR_genero = com_MAR_genero[1501:2000,]
imp4com_MAR_genero = com_MAR_genero[2001:2500,]
imp5com_MAR_genero = com_MAR_genero[2501:3000,]

pdf("p48-graf.pdf", width=12, height=12, pointsize=24)
qqplot(log(dados_originais$Income), log(imp0com_MAR_genero$Income), pch=20,
       main="QQ-plot das imputações", xlab="Quantil original da Log(Renda)",
       ylab="Quantil imputado da Log(Renda)")
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

#box-plot das 96 observações retiradas do banco original e as imputadas
somente_valorigeimp_MARgen <- subset(origcomimp_MAR_genero, Indication=='Ausente')

pdf("p49-graf.pdf", width=10, height=10, pointsize=24)
boxplot(log(Income) ~ .imp, data=somente_valorigeimp_MARgen, xlab="Imputações",
        ylab="Renda",
        col=c("white","#d7191c","#fdae61","#e78ac3","#abdda4","#2b83ba"),
        main="Box-plots das imputações")
dev.off()

gg51.graf = ggplot(data=somente_valorigeimp_MARgen, aes(x=.imp, y=log(Income))) +
  geom_point()
gg52.graf = ggplot(data=somente_valorigeimp_MARgen, aes(x=.imp, y=log(Income))) +
  geom_point() +
  geom_jitter(width = 0.05, height = 0.3)


## IMPUTAÇÃO MAR - tipo de ensino
col <- rep(cores[1+as.numeric(is.na(imp_MAR_ensino$data$Income))],7)
#separa os valores observados=cinza e os imputados=preto para a 
#variável Income_missing

gg55.graf = ggplot(com_MAR_ensino, aes(x=log(Income), colour=Imputed)) +
  geom_density()
gg56.graf = ggplot(com_MAR_ensino, aes(x=log(Income), colour=Imputed, group=.imp)) +
  geom_density()
gg57.graf = ggplot(data=com_MAR_ensino, aes(x=.imp, y=log(Income), color=Imputed)) +
  geom_point()
#plotando os valores dos observados com os 96 valores ausentes
#e os observados mais os ausentes imputados
gg58.graf = ggplot(com_MAR_ensino, aes(x=log(Income), colour=Missing)) +
  geom_density()
gg59.graf = ggplot(data=com_MAR_ensino, aes(x=.imp, y=log(Income), color=Missing)) +
  geom_point(position=position_dodge(0.3))
gg60.graf = ggplot(data=com_MAR_ensino, aes(x=.imp, y=log(Income), color=Missing)) +
  geom_point() +
  geom_jitter(width = 0.15, height = 0.3) +
  scale_color_manual( values=c("gray45","black"))


#boxplots da variável Income com os NA e as 5 imputações
pdf("p51-graf.pdf", width=10, height=10, pointsize=24)
boxplot(log(Income) ~ .imp, data=com_MAR_ensino, xlab="Imputações",
        ylab="log(Renda)",
        col=c("grey","#d7191c","#fdae61","#e78ac3","#abdda4","#2b83ba"),
        main="Box-plots da variável Renda com os dados ausentes \n e as imputações")
dev.off()

#criando o banco de dados com as 500 linhas dos dados originais e 
#as 2500 linhas com as 5 imputações
impcom_MAR_ensino = com_MAR_ensino[-(1:500),c(-2,-10,-11)]
dados_originais[".imp"] = "0-original"
dados_originais4 = dados_originais[c(".imp","Gender","Age","MarStat","Education",
                                     "Ethnicity","Income","Education2")]

origcomimp_MAR_ensino = rbind(dados_originais4, impcom_MAR_ensino)

#boxplots da variável Income dos dados originais e as 5 imputações
pdf("p52-graf.pdf", width=10, height=10, pointsize=24)
boxplot(log(Income) ~ .imp, data=origcomimp_MAR_ensino, xlab="Imputações",
        ylab="log(Renda)",
        col=c("white","#d7191c","#fdae61","#e78ac3","#abdda4","#2b83ba"),
        main="Box-plots dos dados originais e das imputações")
dev.off()

#stripplot do banco original com os valores observados e o banco com os valores
#imputados
origcomimp_MAR_ensino = cbind(origcomimp_MAR_ensino,com_MAR_ensino$Missing)

colnames(origcomimp_MAR_ensino) <- c(".imp","Gender","Age","MarStat","Education",
                                     "Ethnicity","Income","Education2","Indication")

gg61.graf = ggplot(origcomimp_MAR_ensino, aes(x=log(Income), colour=Indication)) +
  geom_density()
gg62.graf = ggplot(data=origcomimp_MAR_ensino, aes(x=.imp, y=log(Income), color=Indication)) +
  geom_point()
#plotando os valores dos observados com os 96 valores ausentes
#e os observados mais os ausentes imputados
gg63.graf = ggplot(origcomimp_MAR_ensino, aes(x=log(Income), colour=Indication)) +
  geom_density()
gg64.graf = ggplot(data=origcomimp_MAR_ensino, aes(x=.imp, y=log(Income), color=Indication)) +
  geom_point(position=position_dodge(0.3))
gg65.graf = ggplot(data=origcomimp_MAR_ensino, aes(x=.imp, y=log(Income), color=Indication)) +
  geom_point() +
  geom_jitter(width = 0.15, height = 0.3) +
  scale_color_manual( values=c("gray45","black"))


#QQ-plot
imp0com_MAR_ensino = com_MAR_ensino[1:500,]
imp1com_MAR_ensino = com_MAR_ensino[501:1000,]
imp2com_MAR_ensino = com_MAR_ensino[1001:1500,]
imp3com_MAR_ensino = com_MAR_ensino[1501:2000,]
imp4com_MAR_ensino = com_MAR_ensino[2001:2500,]
imp5com_MAR_ensino = com_MAR_ensino[2501:3000,]

pdf("p53-graf.pdf", width=12, height=12, pointsize=24)
qqplot(log(dados_originais$Income), log(imp0com_MAR_ensino$Income), pch=20,
       main="QQ-plot das imputações", xlab="Quantil original da Log(Renda)",
       ylab="Quantil imputado da Log(Renda)")
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

#box-plot das 96 observações retiradas do banco original e as imputadas
somente_valorigeimp_MARens <- subset(origcomimp_MAR_ensino, Indication=='Ausente')

pdf("p54-graf.pdf", width=10, height=10, pointsize=24)
boxplot(log(Income) ~ .imp, data=somente_valorigeimp_MARens, xlab="Imputações",
        ylab="log(Renda)",
        col=c("white","#d7191c","#fdae61","#e78ac3","#abdda4","#2b83ba"),
        main="Box-plots das imputações")
dev.off()

gg66.graf = ggplot(data=somente_valorigeimp_MARens, aes(x=.imp, y=log(Income))) +
  geom_point()
gg67.graf = ggplot(data=somente_valorigeimp_MARens, aes(x=.imp, y=log(Income))) +
  geom_point() +
  geom_jitter(width = 0.05, height = 0.3)


## IMPUTAÇÃO MNAR

