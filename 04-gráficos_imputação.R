#04-gráficos imputação

#carregando os pacotes
library(mice)
library(VIM)
library(CASdatasets)

source("03-imputação.R")

cores = rgb(t(col2rgb(c("grey","black")))/255,alpha=0.5)

col <- rep(cores[1+as.numeric(is.na(imp$data$Income))],6)
#separa os valores observados=cinza e os imputados=preto para a 
#variável Income_missing

#stripplot(Income~.imp, data=com, jit=TRUE, fac=0.8,
#          col=col, pch=20,
#          cex=1.4, xlab="Imputation number")
p40.pryr %<a-% {stripplot(log(Income)~.imp, data=com, jit=TRUE, fac=0.8,
          col=col, pch=20,
          cex=1.4, xlab="Número de Imputações", ylab="Renda",
          main="Gráfico com as distribuições dos valores imputados")
}

#plotando os valores dos observados com os imputados percebemos que está próximo

#boxplots da variável Income com os NA e as 5 imputações
#boxplot(Income ~ .imp, data=com, xlab="Imputações", ylab="Renda",
#        col=c("grey","white","white","white","white","white"),
#        main="Box-plots da variável Income com os missings e as imputações")
p41.pryr %<a-% {boxplot(log(Income) ~ .imp, data=com, xlab="Imputações", ylab="Renda",
        col=c("grey","#d7191c","#fdae61","#e78ac3","#abdda4","#2b83ba"),
        main="Box-plots da variável Renda com os dados ausentes \n e as imputações")
}

#criando o banco de dados com as 500 linhas dos dados originais e 
#as 2500 linhas com as 5 imputações
impcom = com[-(1:500),-2]
dados_originais[".imp"] = "0-original"
dados_originais2 = dados_originais[c(".imp", "Gender", "Age", "MarStat",
                                     "Education", "Ethnicity", "Income")]
origcomimp = rbind(dados_originais, impcom)

#boxplots da variável Income dos dados originais e as 5 imputações
#boxplot(Income ~ .imp, data=origcomimp, xlab="Imputações", ylab="Renda",
#        col=c("grey","white","white","white","white","white"),
#        main="Box-plots dos dados originais e das imputações")
p42.pryr %<a-% {boxplot(log(Income) ~ .imp, data=origcomimp, xlab="Imputações", ylab="Renda",
        col=c("white","#d7191c","#fdae61","#e78ac3","#abdda4","#2b83ba"),
        main="Box-plots dos dados originais e das imputações")
}

#QQ-plot
imp0com = com[1:500,]
imp1com = com[501:1000,]
imp2com = com[1001:1500,]
imp3com = com[1501:2000,]
imp4com = com[2001:2500,]
imp5com = com[2501:3000,]

#qqplot(log(dados_originais$Income), log(imp0com$Income), pch=0)
#qqplot(log(dados_originais$Income), log(imp1com$Income), pch=1)
#qqplot(log(dados_originais$Income), log(imp2com$Income), pch=2)
#qqplot(log(dados_originais$Income), log(imp3com$Income), pch=3)
#qqplot(log(dados_originais$Income), log(imp4com$Income), pch=4)
#qqplot(log(dados_originais$Income), log(imp5com$Income), pch=5)
#qqplot(log(dados_originais2$Income), log(com$Income), pch=0:6)

p43.pryr %<a-% {qqplot(log(dados_originais$Income), log(imp0com$Income), pch=20,
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
}

#fazer box-plot das 96 observações retiradas do banco original e as imputadas
#valores_imputados = impcom[rep(random,6),]
#View(impcom)



