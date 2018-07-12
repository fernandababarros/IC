## 02-análise descritiva


## carregando os pacotes
library(mice)
library(VIM)
library(CASdatasets)
library(tidyverse)
library(gridExtra)
library(ggplot2)
## library(pryr)


source("01-organizando_os_dados.R")
copy_dados_originais <- dados_originais


## interesse em analisar as correlações entre as covariáveis e a variável resposta

## visão geral das variáveis
summary(dados_originais)

## organizando as variáveis categóricas
dados_originais$Gender <- ifelse(dados_originais$Gender==0, "Mulher", "Homem")
dados_originais$MarStat <- ifelse(dados_originais$MarStat==0,"Outros",ifelse(dados_originais$MarStat==1,"Casado","Morando Juntos"))
dados_originais$Ethnicity<- ifelse(dados_originais$Ethnicity==1, "Branco", ifelse(dados_originais$Ethnicity==2, "Negro", ifelse(dados_originais$Ethnicity==3, "Hispânico", "Outros")))

## DISTRIBUIÇÕES MARGINAIS
## tabelas de frequência para variáveis categóricas
## histogramas para variáveis contínuas

## Gênero
## tabela
tabela1 <- addmargins(table(dados_originais$Gender), FUN=sum)
## barplot
cont_gen <- table(dados_originais$Gender)
pdf(NULL)
dev.control(displaylist="enable")
barplot(cont_gen, main="Distribuição do Gênero", col="grey", density=60,
        axis.lty=20, axes=TRUE)
text(0.7,390,"82,6%")
text(1.90,50,"17,4%")
p1.graf <- recordPlot()
invisible(dev.off())

## Estado Civil
## tabela
tabela2 <- addmargins(table(dados_originais$MarStat), FUN=sum)
## barplot
cont_ecivil <- table(dados_originais$MarStat)
pdf(NULL)
dev.control(displaylist="enable")
barplot(cont_ecivil, main="Distribuição do Estado Civil", col="grey", density=60,
        axis.lty=20, axes=TRUE)
text(0.7,300,"66,6%")
text(1.95,50,"6,2%")
text(3.1,150,"27,2%")
p2.graf <- recordPlot()
invisible(dev.off())

## Etnia
## tabela da Etnia
tabela3 <- addmargins(table(dados_originais$Ethnicity), FUN=sum)
## barplot
cont_etnia <- table(dados_originais$Ethnicity)
pdf(NULL)
dev.control(displaylist="enable")
barplot(cont_etnia, main="Distribuição da Etnia", col="grey", density=60,
        axis.lty=20, axes=TRUE)
text(0.7,340,"73%")
text(1.95,57,"8%")
text(3.1,95,"14%")
text(4.3,50,"5%")
p3.graf <- recordPlot()
invisible(dev.off())

## Idade
## histograma
pdf(NULL)
dev.control(displaylist="enable")
hist(dados_originais$Age, col="grey", probability=TRUE,
     xlab="Idade", main="Histograma da Idade")
p4.graf <- recordPlot()
invisible(dev.off())

## Anos de Escolaridade
## tabela
tabela4 <- addmargins(table(dados_originais$Education), FUN=sum)
## histograma
pdf(NULL)
dev.control(displaylist="enable")
hist(dados_originais$Education, col="grey", probability=TRUE,
     xlab="Anos de Escolaridade", main="Histograma dos Anos de Escolaridade")
p5.graf <- recordPlot()
invisible(dev.off())
## barplot
cont_anosesc <- table(dados_originais$Education)
pdf(NULL)
dev.control(displaylist="enable")
barplot(cont_anosesc, main="Distribuição dos Anos de Escolaridade",
        col="grey", density=60, axis.lty=20, axes=TRUE, space=0)
abline(v=9)
abline(v=13)
text(4,120,"Escola Elementar")
text(11,120,"Ensino\nMédio")
text(14.5,120,"Ensino\nSuperior")
p6.graf <- recordPlot()
invisible(dev.off())

## Renda
## summary
tabela5 <- summary(dados_originais$Income)
## histograma
pdf(NULL)
dev.control(displaylist="enable")
hist(log(dados_originais$Income), col="grey", probability=TRUE,
     xlab="Log(Renda)", main="Histograma da Log(Renda)", ylim=c(0,0.4))
lines(density(log(dados_originais$Income)), col="blue", lwd=2)
p7.graf <- recordPlot()
invisible(dev.off())
## grid::grid.newpage()
## p1.graf


## DISTRIBUIÇÕES CONJUNTAS
## Gênero e Renda
## diferença entre os valores de Renda para homens e mulheres
## hipótese homens ganham mais do que mulheres?
## boxplot
pdf(NULL)
dev.control(displaylist="enable")
boxplot(log(dados_originais$Income) ~ dados_originais$Gender,
        col="grey", xlab="Gênero", ylab="Log(Renda)",
        main="Boxplot do Gênero e da Log(Renda)")
p8.graf <- recordPlot()
invisible(dev.off())
## 0=mulher, 1=homem

## Gênero e Estado Civil
##tabela
tabela6 <- addmargins(table(dados_originais$MarStat, dados_originais$Gender), FUN=sum)

## Gênero e Anos de Escolaridade
tabela7 <- addmargins(table(dados_originais$Education, dados_originais$Gender), FUN=sum)

##Gênero e Etnia
tabela8 <- addmargins(table(dados_originais$Ethnicity, dados_originais$Gender), FUN=sum)

## Anos de Escolaridade e Renda
## quanto maior os anos de escolaridade maior a renda?
pdf(NULL)
dev.control(displaylist="enable")
boxplot(log(dados_originais$Income) ~ dados_originais$Education,
        col="grey", xlab="Anos de Escolaridade", ylab="Log(Renda)",
        main="Boxplot dos Anos de Escolaridade e a Log(Renda)")
abline(h=mean(log(dados_originais$Income)), lty=2, col="red")
p9.graf <- recordPlot()
invisible(dev.off())

## Estado Civil e Renda
## avaliar se o estado civil influencia na renda dos entrevistados
pdf(NULL)
dev.control(displaylist="enable")
boxplot(log(dados_originais$Income) ~ dados_originais$MarStat,
        col="grey", xlab="Estado Civil", ylab="Log(Renda)",
        main="Boxplot do Estado Civil e a Log(Renda)")
p10.graf <- recordPlot()
invisible(dev.off())
## 0=outros, 1=casado, 2=morando juntos

## Etnia e Renda
## a etnia influencia na renda dos entrevistados?
pdf(NULL)
dev.control(displaylist="enable")
boxplot(log(dados_originais$Income) ~ dados_originais$Ethnicity,
        col="grey", xlab="Etnia", ylab="Log(Renda)",
        main="Boxplot da Etnia e a Log(Renda)")
p11.graf <- recordPlot()
invisible(dev.off())
## 1=white, non-hispanic,
## 2=black, non-hispanic,
## 3=hispanic,
## 7=others

## Etnia e Estado Civil
tabela9 <- addmargins(table(dados_originais$Ethnicity, dados_originais$MarStat), FUN=sum)

## Etnia e Anos de Escolaridade
tabela10 <- addmargins(table(dados_originais$Ethnicity, dados_originais$Education), FUN=sum)

## Idade e Renda
pdf(NULL)
dev.control(displaylist="enable")
plot(dados_originais$Age, log(dados_originais$Income), ylab="Log(Renda)",
     xlab="Idade", main="Gráfico da Idade e a Log(Renda)", pch=20)
## abline(h=quantile(log(dados_originais$Income)), lty=2, col="purple")
mod0 = lm(log(dados_originais$Income)~dados_originais$Age)
abline(mod0, col="red", lwd=2)
p12.graf <- recordPlot()
invisible(dev.off())

## Gênero e Idade
pdf(NULL)
dev.control(displaylist="enable")
boxplot(dados_originais$Age ~ dados_originais$Gender, col="grey",
        main="Boxplot do Gênero e a Idade",
        xlab="Gênero", ylab="Idade")
p13.graf <- recordPlot()
invisible(dev.off())

## Estado Civil e Idade
pdf(NULL)
dev.control(displaylist="enable")
boxplot(dados_originais$Age ~ dados_originais$MarStat, col="grey",
        main="Boxplot do Estado Civil e a Idade",
        xlab="Estado Civil", ylab="Idade")
p14.graf <- recordPlot()
invisible(dev.off())

## Anos de Escolaridade e a Idade
pdf(NULL)
dev.control(displaylist="enable")
boxplot(dados_originais$Age ~ dados_originais$Education, col="grey",
        main="Boxplot dos Anos de Escolaridade e a Idade",
        xlab="Anos de Escolaridade", ylab="Idade")
p15.graf <- recordPlot()
invisible(dev.off())

## Etnia e a Idade
pdf(NULL)
dev.control(displaylist="enable")
boxplot(dados_originais$Age ~ dados_originais$Ethnicity, col="grey",
        main="Boxplot da Etnia e a Idade", xlab="Etnia", ylab="Idade")
p16.graf <- recordPlot()
invisible(dev.off())

## GGPLOT GRÁFICOS
gg1.graf = ggplot(data = dados_originais) + 
  geom_point(mapping = aes(x = Education, y = log(Income)))

gg2.graf = ggplot(data = dados_originais) + 
  geom_point(mapping = aes(x = Gender, y = log(Income)))

gg3.graf = ggplot(data = dados_originais) + 
  geom_point(mapping = aes(x = MarStat, y = log(Income)))

gg4.graf = ggplot(data = dados_originais) + 
  geom_point(mapping = aes(x = Ethnicity, y = log(Income)))

gg5.graf = ggplot(data = dados_originais) + 
  geom_point(mapping = aes(x = Age, y = log(Income)))

gg6.graf = ggplot(data = dados_originais, aes(x = Age, y = log(Income), color = Gender)) +
  geom_point()

gg7.graf = ggplot(data = dados_originais, aes(x = Age, y = log(Income), color = Gender)) +
  facet_grid(. ~ Gender) +
  geom_point()

gg8.graf = ggplot(data = dados_originais, aes(x = Age, y = log(Income), color = MarStat)) +
  geom_point()

gg9.graf = ggplot(data = dados_originais, aes(x = Age, y = log(Income), color = MarStat)) +
  facet_grid(. ~ MarStat) +
  geom_point()

gg10.graf = ggplot(data = dados_originais, aes(x = Age, y = log(Income), color = Ethnicity)) +
  geom_point()

gg11.graf = ggplot(data = dados_originais, aes(x = Age, y = log(Income), color = Ethnicity)) +
  facet_grid(. ~ Ethnicity) +
  geom_point()

gg12.graf = ggplot(data = dados_originais, aes(x = Age, y = log(Income), color = Education)) +
  geom_point()

## Gênero, Idade e Renda
pdf(NULL)
dev.control(displaylist="enable")
plot(dados_originais$Age, log(dados_originais$Income),
     ylab="Log(Renda)", xlab="Idade",
     main="Gráfico da Idade e a Log(Renda)",
     col=ifelse(dados_originais$Gender=='Mulher',2,1), pch=20)
homem_dados = dados_originais[dados_originais$Gender=='Homem',]
mulher_dados = dados_originais[dados_originais$Gender=='Mulher',]
mod1 <- lm(log(mulher_dados$Income)~mulher_dados$Age, data=mulher_dados)
mod2 <- lm(log(homem_dados$Income)~homem_dados$Age, data=homem_dados)
abline(mod1, col=2, lwd=2)
abline(mod2, col=1, lwd=2)
legend("topright", leg=c("Masculino", "Feminino"), pch=20, col=c(1,2), cex=0.8)
p17.graf <- recordPlot()
invisible(dev.off())

## Anos de Escolaridade
## dados_originais$Education <- recode(dados_originais$Education, "c('2','3','4','5','6','7','8','9','10') = 'Escola Elementar'; c('11','12','13','14') = 'Ensino Médio'; c('15','16','17') = 'Ensino Superior'")

## Estado Civil, Idade e Renda
pdf(NULL)
dev.control(displaylist="enable")
plot(dados_originais$Age, log(dados_originais$Income),
     ylab="Log(Renda)", xlab="Idade",
     main="Gráfico da Idade e a Log(Renda)",
     col=ifelse(dados_originais$MarStat=='Outros',3,ifelse(dados_originais$MarStat=='Casado',2,1)),
     pch=20)
ecoutros_dados = dados_originais[dados_originais$MarStat=='Outros',]
eccasado_dados = dados_originais[dados_originais$MarStat=='Casado',]
ecmorjuntos_dados = dados_originais[dados_originais$MarStat=='Morando Juntos',]
mod3 <- lm(log(ecoutros_dados$Income)~ecoutros_dados$Age, data=ecoutros_dados)
mod4 <- lm(log(eccasado_dados$Income)~eccasado_dados$Age, data=eccasado_dados)
mod5 <- lm(log(ecmorjuntos_dados$Income)~ecmorjuntos_dados$Age, data=ecmorjuntos_dados)
abline(mod3, col=3, lwd=2)
abline(mod4, col=2, lwd=2)
abline(mod5, col=1, lwd=2)
legend("topright", leg=c("Outros", "Casado", "Morando Juntos"),
       pch=20, col=c(3,2,1), cex=0.8)
p18.graf <- recordPlot()
invisible(dev.off())

## Etnia, Idade e Renda
pdf(NULL)
dev.control(displaylist="enable")
plot(dados_originais$Age, log(dados_originais$Income),
     ylab="Log(Renda)", xlab="Idade",
     main="Gráfico da Idade e a Log(Renda)",
     col=ifelse(dados_originais$Ethnicity=='Branco',1,ifelse(dados_originais$Ethnicity=='Negro',2,ifelse(dados_originais$Ethnicity=='Hispânico',4,6))),
     pch=20)
etniabranco_dados = dados_originais[dados_originais$Ethnicity=='Branco',]
etnianegro_dados = dados_originais[dados_originais$Ethnicity=='Negro',]
etniahisp_dados = dados_originais[dados_originais$Ethnicity=='Hispânico',]
etniaoutros_dados = dados_originais[dados_originais$Ethnicity=='Outros',]
mod6 <- lm(log(etniabranco_dados$Income)~etniabranco_dados$Age, data=etniabranco_dados)
mod7 <- lm(log(etnianegro_dados$Income)~etnianegro_dados$Age, data=etnianegro_dados)
mod8 <- lm(log(etniahisp_dados$Income)~etniahisp_dados$Age, data=etniahisp_dados)
mod9 <- lm(log(etniaoutros_dados$Income)~etniaoutros_dados$Age, data=etniaoutros_dados)
abline(mod6, col=1, lwd=2)
abline(mod7, col=2, lwd=2)
abline(mod8, col=4, lwd=2)
abline(mod9, col=6, lwd=2)
legend("topright", leg=c("Branco", "Negro", "Hispânico", "Outros"),
       pch=20, col=c(1,2,4,6), ncol=2, cex=0.75)
p19.graf <- recordPlot()
invisible(dev.off())


