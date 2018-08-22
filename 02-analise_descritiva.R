## 02-análise descritiva


## carregando os pacotes
library(mice)
library(VIM)
library(CASdatasets)
library(tidyverse)
library(gridExtra)
library(ggplot2)
## library(pryr)
library(car)


source(file="01-organizando_os_dados.R", encoding="UTF-8")
copy_dados_originais <- dados_originais


## interesse em analisar as correlações entre as covariáveis e a variável resposta

## visão geral das variáveis
summary(dados_originais)

## organizando as variáveis categóricas
dados_originais$Gender <- ifelse(dados_originais$Gender==0, "Feminino", "Masculino")
dados_originais$MarStat <- ifelse(dados_originais$MarStat==0,"Outros",ifelse(dados_originais$MarStat==1,"Casado","Morando Juntos"))
dados_originais$Ethnicity <- ifelse(dados_originais$Ethnicity==1, "Branco", ifelse(dados_originais$Ethnicity==2, "Negro", ifelse(dados_originais$Ethnicity==3, "Hispânico", "Outros")))

## DISTRIBUIÇÕES MARGINAIS
## tabelas de frequência para variáveis categóricas
## histogramas para variáveis contínuas

## Gênero
## tabela
## tabela1 <- addmargins(table(dados_originais$Gender), FUN=sum)
## barplot
cont_gen <- table(dados_originais$Gender)
pdf(NULL)
dev.control(displaylist="enable")
barplot(cont_gen, main="Distribuição do Gênero", col="grey", density=60,
        axis.lty=20, axes=TRUE)
text(0.7,110,"17,4%")
text(1.90,350,"82,6%")
p1.graf <- recordPlot()
invisible(dev.off())

## Estado Civil
## tabela
## tabela2 <- addmargins(table(dados_originais$MarStat), FUN=sum)
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
## tabela3 <- addmargins(table(dados_originais$Ethnicity), FUN=sum)
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

########## Gráfico no Relatório
## p1.graf
## p2.graf
## p3.graf
pdf(NULL)
dev.control(displaylist="enable")
par(mfrow=c(2,2))
barplot(cont_gen, main="Distribuição do Gênero", col="grey", density=60,
        axis.lty=20, axes=TRUE)
text(0.7,110,"17,4%")
text(1.90,350,"82,6%")
barplot(cont_ecivil, main="Distribuição do Estado Civil", col="grey", density=60,
        axis.lty=20, axes=TRUE)
text(0.7,300,"66,6%")
text(1.95,50,"6,2%")
text(3.1,150,"27,2%")
barplot(cont_etnia, main="Distribuição da Etnia", col="grey", density=60,
        axis.lty=20, axes=TRUE)
text(0.7,340,"73%")
text(1.95,57,"8%")
text(3.1,95,"14%")
text(4.3,50,"5%")
p100.graf <- recordPlot()
invisible(dev.off())
##########

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
## tabela4 <- addmargins(table(dados_originais$Education), FUN=sum)
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
text(4,120,"Ensino Fundamental")
text(11,120,"Ensino Médio")
text(14.5,120,"Ensino Superior")
p6.graf <- recordPlot()
invisible(dev.off())

## Renda
## summary
## tabela5 <- summary(dados_originais$Income)
## histograma
pdf(NULL)
dev.control(displaylist="enable")
hist(log(dados_originais$Income), col="grey", probability=TRUE,
     xlab="Log(Renda)", main="Histograma da Log(Renda)", ylim=c(0,0.4))
lines(density(log(dados_originais$Income)), col="black", lwd=2)
p7.graf <- recordPlot()
invisible(dev.off())
## grid::grid.newpage()
## p1.graf

########## Gráfico no Relatório
## p4.graf
## p5.graf
## p7.graf
pdf(NULL)
dev.control(displaylist="enable")
par(mfrow=c(2,2))
hist(dados_originais$Age, col="grey", probability=TRUE,
     xlab="Idade", main="Histograma da Idade")
hist(dados_originais$Education, col="grey", probability=TRUE,
     xlab="Anos de Escolaridade", main="Histograma dos Anos de\n Escolaridade")
hist(log(dados_originais$Income), col="grey", probability=TRUE,
     xlab="Log(Renda)", main="Histograma da Log(Renda)", ylim=c(0,0.4))
lines(density(log(dados_originais$Income)), col="black", lwd=2)
p101.graf <- recordPlot()
invisible(dev.off())
##########

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
## tabela6 <- addmargins(table(dados_originais$MarStat, dados_originais$Gender), FUN=sum)

## Gênero e Anos de Escolaridade
## tabela7 <- addmargins(table(dados_originais$Education, dados_originais$Gender), FUN=sum)

##Gênero e Etnia
## tabela8 <- addmargins(table(dados_originais$Ethnicity, dados_originais$Gender), FUN=sum)

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


########## Gráfico no Relatório
## p8.graf
## p10.graf
## p11.graf
pdf(NULL)
dev.control(displaylist="enable")
par(mfrow=c(1,3))
boxplot(log(dados_originais$Income) ~ dados_originais$Gender,
        col="grey", xlab="Gênero", ylab="Log(Renda)",
        main="Boxplot do Gênero\ne da Log(Renda)")
boxplot(log(dados_originais$Income) ~ dados_originais$MarStat,
        col="grey", xlab="Estado Civil", ylab="Log(Renda)",
        main="Boxplot do Estado Civil\ne a Log(Renda)")
boxplot(log(dados_originais$Income) ~ dados_originais$Ethnicity,
        col="grey", xlab="Etnia", ylab="Log(Renda)",
        main="Boxplot da Etnia\ne a Log(Renda)")
p103.graf <- recordPlot()
invisible(dev.off())
##########


## Etnia e Estado Civil
## tabela9 <- addmargins(table(dados_originais$Ethnicity, dados_originais$MarStat), FUN=sum)

## Etnia e Anos de Escolaridade
## tabela10 <- addmargins(table(dados_originais$Ethnicity, dados_originais$Education), FUN=sum)

## Idade e Renda
pdf(NULL)
dev.control(displaylist="enable")
plot(dados_originais$Age, log(dados_originais$Income), ylab="Log(Renda)",
     xlab="Idade", main="Gráfico da Idade e a Log(Renda)", pch=20)
## abline(h=quantile(log(dados_originais$Income)), lty=2, col="purple")
mod0 = lm(log(dados_originais$Income)~dados_originais$Age)
abline(mod0, col="grey55", lwd=2)
p12.graf <- recordPlot()
invisible(dev.off())

pdf(NULL)
dev.control(displaylist="enable")
plot(dados_originais$Education, log(dados_originais$Income), ylab="Log(Renda)",
     xlab="Anos de Escolaridade", pch=20,
     main="Gráfico dos Anos de Escolaridade e a Log(Renda)")
mod22 = lm(log(dados_originais$Income)~dados_originais$Education)
abline(mod22, col="grey55", lwd=2)
p26.graf <- recordPlot()
invisible(dev.off())

pdf(NULL)
dev.control(displaylist="enable")
plot(dados_originais$Education, dados_originais$Age, ylab="Idade",
     xlab="Anos de Escolaridade", pch=20,
     main="Gráfico dos Anos de Escolaridade e a Idade")
mod23 = lm(dados_originais$Age~dados_originais$Education)
abline(mod23, col="grey55", lwd=2)
p27.graf <- recordPlot()
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

## Gênero, Idade e Renda
pdf(NULL)
dev.control(displaylist="enable")
plot(dados_originais$Age, log(dados_originais$Income),
     ylab="Log(Renda)", xlab="Idade",
     main="Gráfico da Idade e a Log(Renda)",
     col=ifelse(dados_originais$Gender=='Feminino','gray55','black'), pch=20)
homem_dados1 = dados_originais[dados_originais$Gender=='Masculino',]
mulher_dados1 = dados_originais[dados_originais$Gender=='Feminino',]
mod1 <- lm(log(mulher_dados1$Income)~mulher_dados1$Age, data=mulher_dados1)
mod2 <- lm(log(homem_dados1$Income)~homem_dados1$Age, data=homem_dados1)
abline(mod1, col='gray55', lwd=2)
abline(mod2, col='black', lwd=2)
legend("topright", leg=c("Masculino", "Feminino"), pch=20, col=c('black','gray55'),
       horiz=FALSE)
p17.graf <- recordPlot()
invisible(dev.off())

## Gênero, Anos de Escolaridade, Renda
pdf(NULL)
dev.control(displaylist="enable")
plot(dados_originais$Education, log(dados_originais$Income),
     ylab="Log(Renda)", xlab="Anos de Escolaridade",
     main="Gráfico dos Anos de Escolaridade e a Log(Renda)",
     col=ifelse(dados_originais$Gender=='Feminino',2,1), pch=20)
homem_dados2 = dados_originais[dados_originais$Gender=='Masculino',]
mulher_dados2 = dados_originais[dados_originais$Gender=='Feminino',]
mod13 <- lm(log(mulher_dados2$Income)~mulher_dados2$Education, data=mulher_dados2)
mod14 <- lm(log(homem_dados2$Income)~homem_dados2$Education, data=homem_dados2)
abline(mod13, col=2, lwd=2)
abline(mod14, col=1, lwd=2)
legend("topleft", leg=c("Masculino", "Feminino"), pch=20, col=c(1,2), cex=1)
p23.graf <- recordPlot()
invisible(dev.off())

## Estado Civil, Anos de Escolaridade, Renda
pdf(NULL)
dev.control(displaylist="enable")
plot(dados_originais$Education, log(dados_originais$Income),
     ylab="Log(Renda)", xlab="Anos de Escolaridade",
     main="Gráfico dos Anos de Escolaridade e a Log(Renda)",
     col=ifelse(dados_originais$MarStat=='Outros',3,ifelse(dados_originais$MarStat=='Casado',2,1)),
     pch=20)
ecoutros_dados2 = dados_originais[dados_originais$MarStat=='Outros',]
eccasado_dados2 = dados_originais[dados_originais$MarStat=='Casado',]
ecmorjuntos_dados2 = dados_originais[dados_originais$MarStat=='Morando Juntos',]
mod15 <- lm(log(ecoutros_dados2$Income)~ecoutros_dados2$Education, data=ecoutros_dados2)
mod16 <- lm(log(eccasado_dados2$Income)~eccasado_dados2$Education, data=eccasado_dados2)
mod17 <- lm(log(ecmorjuntos_dados2$Income)~ecmorjuntos_dados2$Education, data=ecmorjuntos_dados2)
abline(mod15, col=3, lwd=2)
abline(mod16, col=2, lwd=2)
abline(mod17, col=1, lwd=2)
legend("topleft", leg=c("Outros", "Casado", "Morando Juntos"),
       pch=20, col=c(3,2,1), cex=1)
p24.graf <- recordPlot()
invisible(dev.off())

## Etnia, Anos de Escolaridade e Renda
pdf(NULL)
dev.control(displaylist="enable")
plot(dados_originais$Education, log(dados_originais$Income),
     ylab="Log(Renda)", xlab="Anos de Escolaridade",
     main="Gráfico dos Anos de Escolaridade e a Log(Renda)",
     col=ifelse(dados_originais$Ethnicity=='Branco',1,ifelse(dados_originais$Ethnicity=='Negro',2,ifelse(dados_originais$Ethnicity=='Hispânico',4,6))),
     pch=20)
etniabranco_dados2 = dados_originais[dados_originais$Ethnicity=='Branco',]
etnianegro_dados2 = dados_originais[dados_originais$Ethnicity=='Negro',]
etniahisp_dados2 = dados_originais[dados_originais$Ethnicity=='Hispânico',]
etniaoutros_dados2 = dados_originais[dados_originais$Ethnicity=='Outros',]
mod18 <- lm(log(etniabranco_dados2$Income)~etniabranco_dados2$Age, data=etniabranco_dados2)
mod19 <- lm(log(etnianegro_dados2$Income)~etnianegro_dados2$Age, data=etnianegro_dados2)
mod20 <- lm(log(etniahisp_dados2$Income)~etniahisp_dados2$Age, data=etniahisp_dados2)
mod21 <- lm(log(etniaoutros_dados2$Income)~etniaoutros_dados2$Age, data=etniaoutros_dados2)
abline(mod18, col=1, lwd=2)
abline(mod19, col=2, lwd=2)
abline(mod20, col=4, lwd=2)
abline(mod21, col=6, lwd=2)
legend("topleft", leg=c("Branco", "Negro", "Hispânico", "Outros"),
       pch=20, col=c(1,2,4,6), ncol=2, cex=0.75)
p25.graf <- recordPlot()
invisible(dev.off())

## Estado Civil, Idade e Renda
pdf(NULL)
dev.control(displaylist="enable")
plot(dados_originais$Age, log(dados_originais$Income),
     ylab="Log(Renda)", xlab="Idade",
     main="Gráfico da Idade e a Log(Renda)",
     col=ifelse(dados_originais$MarStat=='Outros','gray55',ifelse(dados_originais$MarStat=='Casado','black','black')),
     pch=ifelse(dados_originais$MarStat=='Outros',20,ifelse(dados_originais$MarStat=='Casado',20,14)))
ecoutros_dados1 = dados_originais[dados_originais$MarStat=='Outros',]
eccasado_dados1 = dados_originais[dados_originais$MarStat=='Casado',]
ecmorjuntos_dados1 = dados_originais[dados_originais$MarStat=='Morando Juntos',]
mod3 <- lm(log(ecoutros_dados1$Income)~ecoutros_dados1$Age, data=ecoutros_dados1)
mod4 <- lm(log(eccasado_dados1$Income)~eccasado_dados1$Age, data=eccasado_dados1)
mod5 <- lm(log(ecmorjuntos_dados1$Income)~ecmorjuntos_dados1$Age, data=ecmorjuntos_dados1)
abline(mod3, col='gray55', lwd=2, lty=19)
abline(mod4, col='black', lwd=2, lty=19)
abline(mod5, col='black', lwd=2, lty=14)
legend("topright", leg=c("Outros", "Casado", "Morando Juntos"),
       pch=c(20,20,14), col=c('gray55','black','black'), cex=0.8)
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
etniabranco_dados1 = dados_originais[dados_originais$Ethnicity=='Branco',]
etnianegro_dados1 = dados_originais[dados_originais$Ethnicity=='Negro',]
etniahisp_dados1 = dados_originais[dados_originais$Ethnicity=='Hispânico',]
etniaoutros_dados1 = dados_originais[dados_originais$Ethnicity=='Outros',]
mod6 <- lm(log(etniabranco_dados1$Income)~etniabranco_dados1$Age, data=etniabranco_dados1)
mod7 <- lm(log(etnianegro_dados1$Income)~etnianegro_dados1$Age, data=etnianegro_dados1)
mod8 <- lm(log(etniahisp_dados1$Income)~etniahisp_dados1$Age, data=etniahisp_dados1)
mod9 <- lm(log(etniaoutros_dados1$Income)~etniaoutros_dados1$Age, data=etniaoutros_dados1)
abline(mod6, col=1, lwd=2)
abline(mod7, col=2, lwd=2)
abline(mod8, col=4, lwd=2)
abline(mod9, col=6, lwd=2)
legend("topright", leg=c("Branco", "Negro", "Hispânico", "Outros"),
       pch=20, col=c(1,2,4,6), ncol=2, cex=0.75)
p19.graf <- recordPlot()
invisible(dev.off())

## Etnia Branco, Idade e Renda
pdf(NULL)
dev.control(displaylist="enable")
plot(etniabranco_dados1$Age, log(etniabranco_dados1$Income),
     ylab="Log(Renda)", xlab="Idade",
     main="Gráfico da Idade e a Log(Renda)",
     col='black', pch=20, ylim=c(0,20))
abline(mod6, col='black', lwd=2)
legend("topright", leg="Branco", pch=20, col='black', ncol=2, cex=0.75)
p28.graf <- recordPlot()
invisible(dev.off())
## Etnia Hispânico, Idade e Renda
pdf(NULL)
dev.control(displaylist="enable")
plot(etniahisp_dados1$Age, log(etniahisp_dados1$Income),
     ylab="Log(Renda)", xlab="Idade",
     main="Gráfico da Idade e a Log(Renda)",
     col='grey60', pch=20, ylim=c(0,20))
abline(mod6, col='grey60', lwd=2)
legend("topright", leg="Hispânico", pch=20, col='grey60', ncol=2, cex=0.75)
p29.graf <- recordPlot()
invisible(dev.off())
## Etnia Negro, Idade e Renda
pdf(NULL)
dev.control(displaylist="enable")
plot(etnianegro_dados1$Age, log(etnianegro_dados1$Income),
     ylab="Log(Renda)", xlab="Idade",
     main="Gráfico da Idade e a Log(Renda)",
     col='black', pch=14, ylim=c(0,20))
abline(mod6, col='black', lwd=2, lty=14)
legend("topright", leg="Negro", pch=14, col='black', ncol=2, cex=0.75)
p30.graf <- recordPlot()
invisible(dev.off())
## Etnia Outros, Idade e Renda
pdf(NULL)
dev.control(displaylist="enable")
plot(etniaoutros_dados1$Age, log(etniaoutros_dados1$Income),
     ylab="Log(Renda)", xlab="Idade",
     main="Gráfico da Idade e a Log(Renda)",
     col='black', pch=13, ylim=c(0,20))
abline(mod6, col='black', lwd=2, lty=6)
legend("topright", leg="Outros", pch=13, col='black', ncol=2, cex=0.75)
p31.graf <- recordPlot()
invisible(dev.off())

## Recodificação da Variável Anos de Escolaridade
## Separando em tipo de Ensino
dados_originais$Education2 <- Recode(dados_originais$Education, "2:10='Ensino Fundamental'; 11:14='Ensino Médio'; 15:17='Ensino Superior'")

## barplot
cont_anosesc_rec <- table(dados_originais$Education2)
pdf(NULL)
dev.control(displaylist="enable")
barplot(cont_anosesc_rec, main="Distribuição dos Tipos de Ensino",
        col="grey", density=60, axis.lty=20, axes=TRUE, space=0, ylim=c(0,250))
text(0.5,20,"7,4%")
text(1.5,190,"43,0%")
text(2.5,230,"49,6%")
p20.graf <- recordPlot()
invisible(dev.off())

## boxplot Ensino e Renda
pdf(NULL)
dev.control(displaylist="enable")
boxplot(log(dados_originais$Income) ~ dados_originais$Education2,
        col="grey", xlab="Tipos de Ensino", ylab="Log(Renda)",
        main="Boxplot dos Tipos de Ensino e a Log(Renda)")
p21.graf <- recordPlot()
invisible(dev.off())

########## Gráfico no Relatório
## p6.graf
## p20.graf
pdf(NULL)
dev.control(displaylist="enable")
par(mfrow=c(2,1))
barplot(cont_anosesc, main="Distribuição dos Anos de Escolaridade",
        col="grey", density=60, axis.lty=20, axes=TRUE, space=0)
abline(v=9)
abline(v=13)
text(4,120,"Ensino Fundamental")
text(11,120,"Ensino Médio")
text(14.5,120,"Ensino Superior")
barplot(cont_anosesc_rec, main="Distribuição dos Tipos de Ensino",
        col="grey", density=60, axis.lty=20, axes=TRUE, space=0, ylim=c(0,250))
text(0.5,20,"7,4%")
text(1.5,190,"43,0%")
text(2.5,230,"49,6%")
p102.graf <- recordPlot()
invisible(dev.off())
##########

## Ensino, Idade e Renda
pdf(NULL)
dev.control(displaylist="enable")
plot(dados_originais$Age, log(dados_originais$Income),
     ylab="Log(Renda)", xlab="Idade",
     main="Gráfico da Idade e a Log(Renda)",
     col=ifelse(dados_originais$Education2=='Ensino Fundamental','black',ifelse(dados_originais$Education2=='Ensino Médio','grey60','black')),
     pch=ifelse(dados_originais$Education2=='Ensino Fundamental',14,ifelse(dados_originais$Education2=='Ensino Médio',20,20)))
ed.ef_dados = dados_originais[dados_originais$Education2=='Ensino Fundamental',]
ed.em_dados = dados_originais[dados_originais$Education2=='Ensino Médio',]
ed.es_dados = dados_originais[dados_originais$Education2=='Ensino Superior',]
mod10 <- lm(log(ed.ef_dados$Income)~ed.ef_dados$Age, data=ed.ef_dados)
mod11 <- lm(log(ed.em_dados$Income)~ed.em_dados$Age, data=ed.em_dados)
mod12 <- lm(log(ed.es_dados$Income)~ed.es_dados$Age, data=ed.es_dados)
abline(mod10, col='black', lwd=2, lty=14)
abline(mod11, col='grey60', lwd=2, lty=19)
abline(mod12, col='black', lwd=2, lty=19)
legend("topright", leg=c("Ensino Fundamental", "Ensino Médio", "Ensino Superior"),
       pch=c(14,20,20), col=c('black','grey60','black'), cex=0.8)
## possui uma observação do ensino superior com a legenda por cima!!! conferir no relatório
p22.graf <- recordPlot()
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

## gráfico do ggplot2 entre a idade e a log(renda) separado por cores para a 
## recodificação dos tipos de ensino
gg13.graf = ggplot(data = dados_originais, aes(x = Age, y = log(Income), color = Education2)) +
  geom_point()

gg14.graf = ggplot(data = dados_originais, aes(x = Age, y = log(Income), color = Education2)) +
  facet_grid(. ~ Education2) +
  geom_point()

gg15.graf = ggplot(data = dados_originais, aes(x = Education, y = log(Income), color = Gender)) +
  geom_point()

gg16.graf = ggplot(data = dados_originais, aes(x = Education, y = log(Income), color = Gender)) +
  facet_grid(. ~ Gender) +
  geom_point()

gg17.graf = ggplot(data = dados_originais, aes(x = Education, y = log(Income), color = MarStat)) +
  geom_point()

gg18.graf = ggplot(data = dados_originais, aes(x = Education, y = log(Income), color = MarStat)) +
  facet_grid(. ~ MarStat) +
  geom_point()

gg19.graf = ggplot(data = dados_originais, aes(x = Education, y = log(Income), color = Ethnicity)) +
  geom_point()

gg20.graf = ggplot(data = dados_originais, aes(x = Education, y = log(Income), color = Ethnicity)) +
  facet_grid(. ~ Ethnicity) +
  geom_point()

