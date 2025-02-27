#Monitoria Lista 6
#Questao 1
q1<-read.csv("q_1.csv",sep=';')
str(q1)

q1<-transform(q1,rep=factor(rep), bloco = factor(bloco), trat = factor(trat))

#Intrablocos
aov.intra<- aov(vol ~ rep + rep%in%bloco+ trat, data=q1)

library(car)
Anova(aov.intra, type = "II") #Car (correto)
anova(aov.intra) #stats --> Padrao (Errado)

#Interbloco
library(lme4)

lmer.inter<- lmer(vol ~ rep + (1|rep:bloco)+ trat, data=q1)
anova(lmer.inter)

pf(1.6615,df1 = 24, df2= 36, lower.tail = F)

####################################################
################ Com easyanova
library(easyanova)
library(ggplot2)

#######################
## exemplo do pacote ##
data(data15)
data15

######################

#Organizando
q1<-q1[c("trat","rep","bloco","vol")]

#Intrabloco
intra.easy<-ea1(q1, design = 10)
intra.easy

intra.easy$`Analysis of variance`


#Interbloco
inter.easy<-ea1(q1, design = 11)
inter.easy

inter.easy$`Analysis of variance (marginal anova = type III SS)`

#letra g

mean.inter<-inter.easy$`Adjusted means`[c("treatment","adjusted.mean")]
mean.intra<-intra.easy$`Adjusted means`[c("treatment","adjusted.mean")]

mean.inter<-mean.inter[order(mean.inter$treatment),]
mean.intra<-mean.intra[order(mean.intra$treatment),]

cor(mean.inter$adjusted.mean,mean.intra$adjusted.mean, method = 'spearman')

#grafico
df.grafico<- rbind(mean.inter[c("treatment","adjusted.mean")],mean.intra[c("treatment","adjusted.mean")])
df.grafico$Ajuste<- rep(c("Inter", "Intra"), c(25, 25))
names(df.grafico)[1:2]<-c("Progenie","Media")

ggplot(df.grafico, aes(x= Ajuste, y = Media, group= Progenie))+
  geom_line(aes(color= Progenie), size=2)+
  geom_point()+
  labs(title="Comparação de ranking",size=2)+
  scale_x_discrete(limits=c("Intra","Inter"))

#letra h

#EFICIENCIA INTRABLOCOS
aov.dbc<-aov(vol ~ rep + trat, data=q1)
anova.dbc<-anova(aov.dbc)

#Variancia DBC
653.2247*2/3
(var.dbc<-2/3*anova.dbc[3,3])

#Variancia DBI
anova.intra<-intra.easy$`Analysis of variance`
anova.intra[4,3]
var.intra<-( ( 1+( 3/ ( (3-1)*(5+1) ) ) )* anova.intra[4,3])

#Eficiencia
var.dbc*100/(var.intra*2/3)
intra.easy$`Efficiency of the design (%)` #33% mais eficiente que o DBC no controle do erro

#EFICIENCIA INTERBLOCOS
var.inter<-inter.easy$`Effective variance`

var.dbc*100/(var.inter*2/3)
inter.easy$`Efficiency of the design (%)` #40% mais eficiente que o DBC no controle do erro


##############
### Quest?o 2

#letra b
q2<-read.csv("q_2.csv",sep=';')
View(q2)
q2<- transform(q2, trat = factor(trat), rep= factor(rep), bloco = factor(bloco))
str(q2)

###Organizando a ordem para o easyanova
### 1? Tratamento\ 2? Repeticao\ 3? Bloquinho \4? Variavel resposta
q2<-q2[c("trat","rep","bloco","prod")]

library(easyanova)
?ea1

inter.q2<-ea1(q2, design = 11)
inter.q2

#Analise de Variancia
inter.q2$`Analysis of variance (marginal anova = type III SS)`

#Eficiencia

var.eff<-2/3*inter.q2$`Effective variance`

aov.dbc2<-aov(prod ~ rep + trat, data= q2)
Anova.dbc2<-car::Anova(aov.dbc2)
anova.dbc2<-anova(aov.dbc2)
(var.dbc2<-2/3*(Anova.dbc2[3,1]/Anova.dbc2[3,2]))

#Eficiencia Relativa
inter.q2$`Efficiency of the design (%)`#Resultado easyanova
var.dbc2*100/var.eff #resultado manual

#O Delineamento alfa latic .... eh 24% mais eficiente no controle do erro que o DBC


#letra c
inter.q2$`Adjusted means`[c('treatment','adjusted.mean','scott_knott')]


###############################################################
#Comparacao media ordinaria e media ajustada
df.ord<-data.frame(tapply(q2$prod,q2$trat,mean))
df.ord$treatment<- rownames(df.ord)
names(df.ord)[1]<-'media ordinaria'

df.aj<-inter.q2$`Adjusted means`[c('treatment','adjusted.mean')]

merge(df.aj,df.ord)
#############################################################


#Questao 3
dados<-read.csv('q_3.csv',sep=';')
str(dados)
dados <- transform(dados, local = factor(local), bloco=factor(bloco), trat=factor(trat))
aov(prod ~  bloco + prog, data = dados) #Individual
aov(prod ~ local + bloco + prog) # Conjunta

dados.loc1 <- subset(dados,local==1) # O arquivo conter??? apenas os dados referentes ao local 1
dados.loc2 <- subset(dados,local==2) # O arquivo conter??? apenas os dados referentes ao local 2
dados.loc3 <- subset(dados,local==3) # O arquivo conter??? apenas os dados referentes ao local 3
str(dados.loc1)

print(anova_1 <- anova(aov(prod ~  bloco + trat,data =dados.loc1 )))
print(anova_2 <- anova(aov(prod ~  bloco + trat,data =dados.loc2  )))
print(anova_3 <- anova(aov(prod ~  bloco + trat,data =dados.loc3  )))
aov(prod ~  bloco + trat,data =dados.loc2 ) #Individual
aov(prod ~  bloco + trat,data =dados.loc3 ) #Individual

aov(prod ~ local + bloco + trat,data = dados)# Conjunta
aov.conj <- aov(prod ~ local + bloco:local + trat,data = dados)# Conjunta

anova.q3_mean <-anova(aov.conj)

library(agricolae)
install.packages("tidyverse")
library(dplyr)
tk <- HSD.test(aov.conj, "trat",
               group=TRUE, alpha=0.05)
var(tk$means)
print(arrange(tk$means))
tk$means[1]

library(emmeans)
med.aj<-emmeans(aov.conj, ~ trat)

