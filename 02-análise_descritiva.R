#02-análise descritiva

#carregando os pacotes
library(mice)
library(VIM)
library(CASdatasets)
library(tidyverse)
library(gridExtra)
library(pryr)

source("01-organizando_os_dados.R")

#attach(dados_originais)

#interesse em analisar as correlações entre as covariáveis e a variável resposta

#visão geral das variáveis
summary(dados_originais)

pdf(NULL)
dev.control(displaylist="enable")
hist(log(dados_originais$Income), col="grey", probability=TRUE,
     xlab="Log(Renda)", main="Histograma da Log(Renda)", ylim=c(0,0.4))
lines(density(log(dados_originais$Income)), col="blue", lwd=2)
p1.hist <- recordPlot()
invisible(dev.off())

#pdf(file= "hist_renda.pdf", width=12, height=10)
#pdf(NULL)
#dev.control(displaylist="enable")
p1.pryr %<a-% {hist(log(dados_originais$Income), col="grey", probability=TRUE,
                    xlab="Log(Renda)", main="Histograma da Log(Renda)", ylim=c(0,0.4))
lines(density(log(dados_originais$Income)), col="blue", lwd=2)
}
#p1.hist <- recordPlot()
#invisible(dev.off())
#dev.off()

#grid::grid.newpage()
#p1.hist

#p1.pryr

# densityplot(log(Income), col="black", xlab="Log(Renda)")

#diferença entre os valores de Renda para homens e mulheres
#hipótese homens ganham mais do que mulheres?
#table(Gender)
#boxplot(log(Income) ~ Gender, col="grey",
#        xlab="Gênero", main="Boxplot da Log(Renda) e Gênero")
#0=mulher, 1=homem

#quanto maior os anos de escolaridade maior a renda?
#boxplot(log(Income) ~ Education, col="grey",
#      main="Boxplot da Log(Renda) e Anos de \n Escolaridade",
#      xlab="Anos de Escolaridade")
#abline(h=mean(log(Income)), lty=2, col="red")

#avaliar se o estado civil influencia na renda dos entrevistados
#table(MarStat)
#boxplot(log(Income) ~ MarStat, col="grey",
#        main="Boxplot da Log(Renda) e o Estado \n Civil",
#        xlab="Estado Civil")
#0=outros, 1=casado(a), 2=morando com o parceiro(a)

#avaliar a etnia com a renda
#table(Ethnicity)
#boxplot(log(Income) ~ Ethnicity, col="grey",
#        main="Boxplot da Log(Renda) e a Etnia",
#        xlab="Etnia")
#1=white, non-hispanic,
#2=black, non-hispanic,
#3=hispanic,
#7=others

#gráfico de dispersão da renda pela idade
#plot(Age, log(Income), ylab="Log(Renda)", xlab="Idade",
#     main="Gráfico da Log(Renda) e Idade", pch=20)
#mod= lm(log(Income)~Age)
#abline(mod, lwd=2)


#gráfico de dispersão da renda pela idade com o gênero
#plot(Age, log(Income), ylab="Log(Renda)", xlab="Idade",
#     main="Gráfico da Log(Renda) e Idade", col=ifelse(Gender==0,2,1), pch=20)
#mod1 = lm(log(Income)~Age, data=dados_originais[Gender==0,])
#mod2 = lm(log(Income)~Age, data=dados_originais[Gender==1,])
#abline(mod1, col=2, lwd=2)
#abline(mod2, col=1, lwd=2)
#legend("topright", leg=c("Masc.", "Fem."), pch=20, col=c(1,2))



#detach(dados_originais)


