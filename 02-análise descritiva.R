#02-análise descritiva

#carregando os pacotes
library(mice)
library(VIM)
library(CASdatasets)

load("01-organizando os dados.RData")

attach(dados_originais)

#interesse em analisar as correlações entre as covariáveis e a variável resposta

#visão geral das variáveis
summary(dados_originais)

hist(log(Income), col="grey", probability=TRUE, xlab="Renda",
     main="Histograma da Renda")
lines(density(log(Income)), col="blue", lwd=2)

# densityplot(log(Income), col="black", xlab="Renda")

#diferença entre os valores de Renda para homens e mulheres
#hipótese homens ganham mais do que mulheres?
table(Gender)
boxplot(log(Income) ~ Gender, col="grey",
        xlab="Gênero", main="Boxplot da variável Renda e Gênero")

#quanto maior os anos de escolaridade maior a renda?
boxplot(log(Income) ~ Education, col="#636363",
      main="Boxplot da variável Renda e Anos de Escolaridade",
      xlab="Anos de Escolaridade")
abline(h=mean(log(Income)), lty=2)

#avaliar se o estado civil influencia na renda dos entrevistados
table(MarStat)
boxplot(log(Income) ~ MarStat, col="#636363",
        main="Boxplot da variável Renda e o Estado civil",
        xlab="Estado Civil")
#0=outros, 1=casado(a), 2=morando com o parceiro(a)

detach(dados_originais)

save.image(file = "02-análise descritiva.RData")