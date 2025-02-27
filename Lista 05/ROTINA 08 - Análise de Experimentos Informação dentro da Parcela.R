rm(list = ls(all=T)) #Remover todos os objetos

setwd("D:/UFLA/Disciplinas/Pos-Graduacao/Analise de Experimentos em Genetica e Melhoramento de Plantas/Semestre 2020.2/Rotinas R")

############################ AN�LISE COM INFORMA��O DENTRO DE PARCELA #############################
eucalipto <- read.table("Rotina8.eucalipto.txt",header=T)
head(eucalipto)
eucalipto<-transform(eucalipto, rep=factor(rep), familia=factor(familia))
str(eucalipto)

#### Single-step analysis - Em n�vel de planta
m1 <- lm(volume~rep+familia+rep:familia, data=eucalipto) #Aten��o aos testes F incorretos
anova(m1)

m2 <- aov(volume~rep+familia+Error(rep:familia), data=eucalipto) #Aten��o aos testes F incorretos
summary(m2)

library(EMSaov)
euca.result <- EMSanova(volume ~ rep + familia, data=eucalipto,
                        type=c("R","R"))
euca.result                        

#Estima��o dos componentes da vari�ncia
library(sommer)
euca_mmer <- mmer(volume ~ rep, random= ~ familia + rep:familia, data= eucalipto)
(VC_euca <- summary(euca_mmer)$varcomp)

library(lme4)
euca2_mmer <- lmer(volume ~ 1 + rep + (1|familia) + (1|rep:familia), data= eucalipto)
summary(euca2_mmer)
plot(euca2_mmer)


