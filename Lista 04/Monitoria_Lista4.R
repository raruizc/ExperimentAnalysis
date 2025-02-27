#Monitoria Lista 4

#Questão 1
milho<- read.csv("q_4.csv", sep=';')
str(milho)

milho<-transform(milho, prog = factor(prog))

aov.milho<-aov(altura ~ bloco + prog, data=milho)
anova.milho<-anova(aov.milho)

QME<-anova.milho[3,3]
QMP<-anova.milho[2,3]
QMB<-anova.milho[1,3]


(var.bloco = (QMB- QME)/14)
(var.progenie = (QMP - QME)/6)


############################ EXTRA ###########################

#Conferindo as variancias com abordagem de modelos mistos
library(lme4)
lmer.milho<- lmer(altura ~ (1|bloco) + (1|prog), data=milho)
summary(lmer.milho)
QME

##############################################################

#letra e

#calculo do grau de liberdade

(v<- ( (QMP - QME)^2 )/( ((QMP^2)/13) + ((QME^2)/65) ))

chiq_2.5<-qchisq(0.025,df = v)
chiq_97.5<-qchisq(0.975,df = v)

#intervalo qui quadrado

(v*var.progenie)/chiq_2.5
(v*var.progenie)/chiq_97.5

#var(progenie) = 0.02507326
#IC[var(progenie)] = (0.01203611; 0.08052915)


#intervalo com distribuição t

t.milho<- abs(qt(0.025,v))

var.var<- (2/6^2) *(  (QMP^2)/((14-1)+2) + (QME^2)/(  14*(6-1)+2  ) )    

var.progenie + t.milho*sqrt(var.var)
var.progenie - t.milho*sqrt(var.var)


#IC[var(progenie)] = (0.0009470476; 0.04919947)


#letra f

herd.milho<-var.progenie/(var.progenie+ QME/6)

(f.milho_0.975<-qf(0.975, df1 = 65, df2= 13))
(f.milho_0.025<-qf(0.025, df1 = 65, df2= 13))

1 - ( (QMP/QME)*f.milho_0.025 )^-1
1 - ( (QMP/QME)*f.milho_0.975 )^-1


#IC[herd(altura)] = [0.6922733;0.9462207]

