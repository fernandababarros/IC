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

hist(log(Income), col="grey", probability=TRUE, xlab="Log(Renda)",
     main="Histograma da Log(Renda)")
lines(density(log(Income)), col="blue", lwd=2)

# densityplot(log(Income), col="black", xlab="Log(Renda)")

#diferença entre os valores de Renda para homens e mulheres
#hipótese homens ganham mais do que mulheres?
table(Gender)
boxplot(log(Income) ~ Gender, col="grey",
        xlab="Gênero", main="Boxplot da Log(Renda) e Gênero")
#0=mulher, 1=homem

#quanto maior os anos de escolaridade maior a renda?
boxplot(log(Income) ~ Education, col="grey",
      main="Boxplot da Log(Renda) e Anos de \n Escolaridade",
      xlab="Anos de Escolaridade")
abline(h=mean(log(Income)), lty=2, col="red")

#avaliar se o estado civil influencia na renda dos entrevistados
table(MarStat)
boxplot(log(Income) ~ MarStat, col="grey",
        main="Boxplot da Log(Renda) e o Estado \n Civil",
        xlab="Estado Civil")
#0=outros, 1=casado(a), 2=morando com o parceiro(a)

#avaliar a etnia com a renda
table(Ethnicity)
boxplot(log(Income) ~ Ethnicity, col="grey",
        main="Boxplot da Log(Renda) e a Etnia",
        xlab="Etnia")
#1=white, non-hispanic,
#2=black, non-hispanic,
#3=hispanic,
#7=others

#gráfico de dispersão da renda pela idade com o gênero
plot(log(Income), Age, xlab="Log(Renda)", ylab="Idade",
     main="Gráfico da Log(Renda) e Idade")
abline(lm(Age ~ Gender), col="red")
text(13,80, "Gênero", col = 2, adj = c(-.1, -.1))


detach(dados_originais)

save.image(file = "02-análise descritiva.RData")
