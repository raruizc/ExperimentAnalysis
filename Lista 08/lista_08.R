library(readxl)
lista_8 <- read_excel("lista_8.xlsx")
View(lista_8)

str(lista_8)
dados_conjuntos <-transform(lista_8, Local = as.factor(Local), Especie = as.factor(Especie),
                        Bloco = as.factor(Bloco))

library(dplyr)
str(lista_8)

dados_arara <-  filter(lista_8, Local == "Araras")
dados_mogi <-  filter(lista_8, Local == "Mogi-Guaçu")
dados_simao <-filter(lista_8, Local =="Sao Simao")

##Local 1
str(dados_arara)
dados_arara <-transform(dados_arara, Local = as.factor(Local), Especie = as.factor(Especie),
                        Bloco = as.factor(Bloco))
aov.arara <- aov(Altura~ Bloco + Especie, data = dados_arara)

(anova.arara <- anova(aov.arara))

##Local 2
str(dados_mogi)
dados_mogi <-transform(dados_mogi, Local = as.factor(Local), Especie = as.factor(Especie),
                        Bloco = as.factor(Bloco))
aov.mogi <- aov(Altura~ Bloco + Especie, data = dados_mogi)

(anova.mogi <- anova(aov.mogi))


###Local 3
str(dados_simao)
dados_simao <-transform(dados_simao, Local = as.factor(Local), Especie = as.factor(Especie),
                       Bloco = as.factor(Bloco))
aov.simao <- aov(Altura~ Bloco + Especie, data = dados_simao)

(anova.simao <- anova(aov.simao))

### Barlett test
bartlett.test(dados_conjuntos$Altura, dados_conjuntos$Local)

### Coeficientes de Variacao e Acurácia Seletiva

##letra c

#CV
(cv_araras<-sqrt(anova.arara[3,3])*100/mean(dados_arara$Altura))
(cv_mogi<-sqrt(anova.mogi[3,3])*100/mean(dados_mogi$Altura))
(cv_sao<-sqrt(anova.simao[3,3])*100/mean(dados_simao$Altura))

##Acuracia
(rgg_araras<-sqrt(1-1/anova.arara[2,4]))
(rgg_mogi<-sqrt(1-1/anova.mogi[2,4]))
(rgg_sao<-sqrt(1-1/anova.simao[2,4]))

##letra d
#Y = u + b%in%l + l + s + l:s + e
str(dados_conjuntos)
aov_conj<- aov(Altura ~ Bloco%in%Local + Local + Especie + Local:Especie, data=dados_conjuntos)
(anova_conj<-anova(aov_conj))

#letra e
library(dplyr)
library(ggplot2)
dados.conj.mean<-dados_conjuntos %>% group_by(Local,Especie) %>% summarise(Altura = mean(Altura))

ggplot(dados.conj.mean, aes(x = Local, y= Altura, group = Especie))+
  geom_line(aes(color = Especie),size=1)

##letra f
QMG_araras<-anova.arara[2,3]
QMG_mogi<-anova.mogi[2,3]
QMG_sao<-anova.simao[2,3]


#Parte simples
QMG_medio<- mean(c(QMG_araras,QMG_mogi,QMG_sao))
(ps<-(1/(3-1)) * (  (sqrt(QMG_araras) - sqrt(QMG_medio))^2 +
                      (sqrt(QMG_mogi) - sqrt(QMG_medio))^2 +
                      (sqrt(QMG_sao) - sqrt(QMG_medio))^2 ))

#Parte Composta

var.gen<-(anova_conj[2,3]-anova_conj[4,3])/(4*3)

(r12<- (var.gen)/( (sqrt ( (QMG_araras)/4*(QMG_mogi)/4)   )) )
(r23<- (var.gen)/( (sqrt ( (QMG_mogi)/4*(QMG_sao)/4)   )) )
(r13<- (var.gen)/( (sqrt ( (QMG_araras)/4*(QMG_sao)/4)   )) )

(pc<-  (2/(3*(3-1))) *( (1-r12) * sqrt(QMG_araras * QMG_mogi)+
                          (1-r23) * sqrt(QMG_mogi * QMG_sao)+
                          (1-r13) * sqrt(QMG_araras * QMG_sao)))

ps+pc #Precisa ser igual ou proximo ao QM da interação da conjunta
anova_conj

#% das partes
ps*100/(ps+pc) #Parte simples
pc*100/(ps+pc) #Partes complexa

?qf
qf(0.05, 17, 16, lower.tail=F)
