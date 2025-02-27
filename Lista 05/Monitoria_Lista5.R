Q1<- read.csv("q_3.csv",sep=';')
str(Q1)

Q1<- transform(Q1, prog = factor(prog), bloco = factor(bloco), planta = factor(planta))



#Questão 1 b

########################################
######### Nível de planta de individual
#####


aov.q1<- aov(alt ~ bloco + prog + prog:bloco ,data=Q1)
anova(aov.q1)

########################################
######### Nível de média
#####
library(dplyr)
?dplyr

# %>% Usa o resultado do que vier antes como argumento pra proxima funcao
aov(alt ~ bloco + prog + prog:bloco ,data=Q1) %>% anova()

#Tirando as medias dentro do bloco
q1_mean<- Q1 %>% group_by(prog,bloco) %>% summarise( alt = mean(alt))

#Analise utilizando medias
aov.q1_mean<- aov(alt ~ bloco + prog  ,data=q1_mean)
anova.q1_mean <-anova(aov.q1_mean)
tukey <- TukeyHSD(aov.q1_mean, "prog", ordered = TRUE)
library(agricolae)
tk <- HSD.test(aov.q1_mean, "prog",
             group=TRUE, alpha=0.05)
var(tk(means))
var(tk$means$alt)
########################################
######### Nível de Totais
#####

#Tirando os totais dentro do bloco
q1_tot<- Q1 %>% group_by(prog,bloco) %>% summarise( alt = sum(alt))

#Analise utilizando medias
aov.q1_tot<- aov(alt ~ bloco + prog  ,data=q1_tot)
anova(aov.q1_tot)

########################################
######### Equivalencia
####


anova.indv<-anova(aov.q1)
anova.tot<-anova(aov.q1_tot)
anova.med<-anova(aov.q1_mean)

anova.indv['Mean Sq']
anova.tot['Mean Sq']/5
anova.med['Mean Sq']*5

#letra c
library(EMSaov)

#forma correta --> Observar o testador correto no teste F (Slide 10 Aula Informação Dentro da parcela)
(EMS.Q1<-EMSanova(alt ~  bloco + prog , data = Q1,type = c('R',"R")))

EMS.Q1[c("MS","EMS")]


#letra f

#Com base nas medias
q1_meanprog<- Q1 %>% group_by(prog) %>% summarise(alt = mean(alt))
var(q1_meanprog$alt)

#Com base no QMT
anova.indv[2,3]/(2*5)
anova.med[2,3]/2
#Com base na soma das variancias

#Progenie+ erro dentro / (rep * nplantas) + erro entre / (rep)
0.019977 + 0.02083/(2*5)+ 0.0073636/(2)

# e 



(EMS.Q1<-EMSanova(alt ~  bloco + prog , data = Q1,type = c('R',"R")))
View(EMS.Q1)

aov.q1<- aov(alt ~ bloco + prog + prog:bloco ,data=Q1)
anova.q1 <- anova(aov.q1)

anova.q1_mean <-anova(aov.q1_mean)

QME<-anova.q1_mean[3,3]
QMP<-anova.q1_mean[2,3]
QMB<-anova.q1_mean[1,3]

QME<-EMS.Q1[3,3]
QMP<-EMS.Q1[2,3]
QMB<-EMS.Q1[1,3]

(v<- ( (QMP - QME)^2 )/( ((QMP^2)/13) + ((QME^2)/13) ))

chiq_2.5<-qchisq(0.025,df = v)
chiq_97.5<-qchisq(0.475,df = v)

#intervalo qui quadrado
var.progenie = (QMP-QME)/10
(v*var.progenie)/chiq_2.5
(v*var.progenie)/chiq_97.5



#intervalo com distribuição t

t.milho<- abs(qt(0.025,v))

var.var<- (2/2^2) *(  (QMP^2)/((14-1)+2) + (QME^2)/(  14*(2-1)+2  ) )    

var.progenie + t.milho*sqrt(var.var)
var.progenie - t.milho*sqrt(var.var)

# ponto f

library(agricolae)
tk <- HSD.test(aov.q1_mean, "prog",
               group=TRUE, alpha=0.05)
var(tk(means))
var(tk$means$alt)

## Metodo dos QM progenies
variancia_fenotipica = (qmprogenies <- QMP/10)

(0.020830/10)+(0.00736/2)+(0.01997)

### 


herd.milho <- var.progenie/variancia_fenotipica

(f.milho_0.975<-qf(0.975, df1 = 13, df2= 13))
(f.milho_0.025<-qf(0.025, df1 = 13, df2= 13))

1 - ( (QMP/QME)*f.milho_0.025 )^-1
1 - ( (QMP/QME)*f.milho_0.975 )^-1

### Questão 2


Q2<- read.csv("q_3.1.csv", sep=';',stringsAsFactors = T)
str(Q2)
View(Q2)
aov.q2<- aov(kg ~ bloco + clone + bloco +ano + clone:bloco + clone:ano + ano:bloco + clone:bloco:ano, data=Q2)
anova(aov.q2)


#Clone aleatorio
#Bloco e ano fixo

EMSanova(kg ~ clone + bloco +ano, data= Q2, type =c("R","F","F"))
lmer.prod <- lmer(kg ~ rep + (1|rep:bloco) + trat,data= Q2)
