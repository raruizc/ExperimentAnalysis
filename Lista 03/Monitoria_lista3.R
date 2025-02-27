#Lista 3



#Questao 2
braquiaria <- read.csv("q_2.csv",sep=';')
str(braquiaria)

braquiaria<- transform(braquiaria, Rep = factor (Rep), Tratamento = factor(Tratamento))


aov.braquiaria<- aov(viabilidade ~ Tratamento, data= braquiaria)
anova(aov.braquiaria)



#Questao 3

altura<- read.csv("q_3.csv",sep=';')
str(altura)

altura <- transform(altura, rep= factor(rep), hibrido = factor (hibrido))

aov.altura<- aov(altura ~ hibrido + rep, data = altura)
anova(aov.altura)

library(agricolae)
tk <- HSD.test(aov.altura, 'Tratamento',
               group=TRUE, alpha=0.05)

bar.group(tk$groups,
          ylim=c(0, 250),
          col= c('red',"gray","gray","gray","green"),
          main = "Tukey 5%",
          xlab =" Tratamentos",
          ylab = "Média")

library(lsmeans)
library(multcomp)

marginal = lsmeans(aov.altura, ~ hibrido)
(cld_v <- cld(marginal,
              alpha=0.05,
              Letters=letters,
              adjust="Dunnet"))
plot(cld_v, ylab="Híbridos",xlab="Média")
?cld

#Teste de Dunnet
install.packages("Tratamentos.ad")
library(Tratamentos.ad)

#Help na funcao para ver como funciona
?dbc.ad


#Criando uma coluna para identificar as testemunhas
#altura$Testemunha <- ifelse(altura$hibrido %in% c('HT01','HT02'), c(1,2),NA)

#Organizando na ordem que o pacote exige
altura<-altura[,c("Testemunha","hibrido","rep","altura")]

#Testes de media
dun<-dbc.ad(altura,alfa=0.05,quali = TRUE,verbose=TRUE)

#Teste de Dunnet
dun$Dunnett

library(DescTools)
?DescTools
ScheffeTest(aov.altura)
print()
?ScheffeTest
PostHocTest(aov.altura, which = NULL,
            method = "scheffe",
            conf.level = 0.95, ordered = F)

numerador = 5*(172+165+139+149-(2*158)-(2*154))
denominador = 1^2+ 1^2+ 1^2+ 1^2+(-2)^2+(-2)^2
SQY1 = numerador/denominador
SQY1
