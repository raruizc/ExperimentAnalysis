#Monitoria Lista 8

#### Questao 1

q1<- read.csv("q_1.csv",sep=';')
str(q1)

q1<-transform(q1, local = factor(local), bloco = factor(bloco), esp = factor(esp))

## letra b

#Separando os locais
q1_araras<-q1[q1$local == 'araras',] #ou subset(q1, local == 'araras')
q1_mogi<- q1[q1$local == 'mogi',] #ou subset(q1, local == 'mogi')
q1_sao<- q1[q1$local == 'sãosimao',] #ou subset(q1, local == 'sãosimao')

#Ajustando os modelos
aov_araras<- aov(H ~ bloco + esp , data=q1_araras)
aov_mogi<- aov(H ~ bloco + esp , data=q1_mogi)
aov_sao<- aov(H ~ bloco + esp , data=q1_sao)

#Tabela de ANOVA
(anova_araras <-anova(aov_araras))
(anova_mogi <-anova(aov_mogi))
(anova_sao <-anova(aov_sao))


#Teste de Bartlett
bartlett.test(q1$H,q1$local) #Homogeneo


##letra c

#CV
(cv_araras<-sqrt(anova_araras[3,3])*100/mean(q1_araras$H))
(cv_mogi<-sqrt(anova_mogi[3,3])*100/mean(q1_mogi$H))
(cv_sao<-sqrt(anova_sao[3,3])*100/mean(q1_sao$H))

##Acuracia
(rgg_araras<-sqrt(1-1/anova_araras[2,4]))
(rgg_mogi<-sqrt(1-1/anova_mogi[2,4]))
(rgg_sao<-sqrt(1-1/anova_sao[2,4]))

##letra d
#Y = u + b%in%l + l + s + l:s + e
str(q1)
aov_conj<- aov(H ~ bloco%in%local + local + esp + local:esp, data=q1)
(anova_conj<-anova(aov_conj))

#letra e
library(dplyr)
library(ggplot2)
q1_mean<-q1 %>% group_by(local,esp) %>% summarise(H = mean(H))

ggplot(q1_mean, aes(x = local, y= H, group = esp))+
  geom_line(aes(color = esp),size=2)

##letra f
QMG_araras<-anova_araras[2,3]
QMG_mogi<-anova_mogi[2,3]
QMG_sao<-anova_sao[2,3]


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


##Quest?o 2

q2<- read.csv("q_2.csv",sep=';',stringsAsFactors = T)
str(q2)
r<-4 #numero de repeti?oes

##letra a
QMEs<-c(0.03642,0.03525,0.01808,0.02058)

#Teste de Hartley
variance <- QMEs #variancia residual de cada ambiente
(Fmax <- max(variance)/min(variance))
#rep <- r #numero de repeticoes
(Fmaxtab <- SuppDists::qmaxFratio(p = 0.05, df = 12, k=length(variance), lower.tail=F, log.p=FALSE))
(Pvalor <- SuppDists::pmaxFratio(q = Fmax, df = 12, k=length(variance), lower.tail=F, log.p=FALSE))
if (Pvalor < 0.05) print("Vari???ncias residuais heterog???neas - Modelo heteroced???stico ??? indicado") else print("Vari???ncias residuais homog???neas")

##letra b

#Y = u + l +b%in%l + c + c:l + e

##letra c

#Y = u + l + c + l:c
mean(QMEs)
str(q2)
aov.q2<-aov(prod ~ local + cult, data=q2)
anova(aov.q2)


##letra d

##################################
#####  Ecovalency function  ######
##################################
##         Integrantes         ##
##       Bruna L Carvalho      ##
##     Cinthia S Rodrigues     ##
##     Scheila R Guilherme     ##
##################################

ecovalency <- function (Y , MSE, r, t, k, int.effect="FIX",eco.env=FALSE) 
{
  Mean.treat <- rowMeans(Y)
  Mean.env <- colMeans(Y)
  L <- matrix(rowMeans(Y),nc=1)%*%matrix(1,1,k)
  C <- matrix(1,9,1)%*%matrix(colMeans(Y),nc=k)
  M <- mean(L)*matrix(1,t,k)
  W <- Y - L - C + M
  W2.treat <- rowSums(W^2)*r
  W2.env <- colSums(W^2)*r
  W2.total <- sum(W^2)*r
  RC.treat <- W2.treat/W2.total*100
  RC.env <- W2.env/W2.total*100
  
  if (int.effect=="FIX") 
  {
    Fi.treat <- ((t*W2.treat/(t-1))/(k-1))/MSE
    p.value.treat <- 1 - pf(Fi.treat,(k-1), (r-1)*(t-1)*k)  
    Fj.env <- ((k*W2.env/(k-1))/(t-1))/MSE
    p.value.env <- 1 - pf(Fj.env,(t-1), (r-1)*(t-1)*k) 
    
  } else {
    
    Fi.treat <- (t*(t-2)*W2.treat)/((t-1)*W2.total-t*W2.treat)
    p.value.treat <- 1 - pf(Fi.treat,(k-1), (t-2)*(k-1))
    Fj.env <- (k*(k-2)*W2.env)/((k-1)*W2.total-k*W2.env)
    p.value.env <- 1 - pf(Fj.env,(t-1), (k-2)*(t-1))    
  }
  
  if (eco.env==FALSE) 
  {
    x <- cbind(1:t,round(Mean.treat,1), round(W2.treat,2), round(p.value.treat,4), round(RC.treat,4))
    colnames(x) <- c("Genotypes","Mean","Ecovalency","p-value","RC (%)")
    x <- as.data.frame(x)
    total<-c("Total",round(mean(Mean.treat),digits=1),round(W2.total,digits=2),"","")
    x<-rbind(x,total)
    
    
  } else {
    
    x <- cbind(1:k,round(Mean.env,1), round(W2.env,2), round(p.value.env,4), round(RC.env,4))
    colnames(x) <- c("Environments","Mean","Ecovalency","p-value","RC (%)")
    x <- as.data.frame(x)
    total<-c("Total",round(mean(Mean.env),digits=1),round(W2.total,digits=2),"","")
    x<-rbind(x,total)
  }
  
  return(x) 
}  

# Y: matrix of means (Genotypes in rows, environment in columns)
# MSE: Mean Square of Error
# r: number of repetitions
# t: number of treatments
# k: number of environments
# int.effet:  "FIX" if genotype/environment is considered of fix effect. 
#             "RANDOM" if genotype/environment is considered of random effect
#eco.enc: FALSE if genotype ecovalency is required
#         TRUE if environment ecovalency is required


##### Exemplo #####

#Especificar o diretorio de trabalho
x <- read.csv("q_2eco.csv",sep=';',dec=',')
Y <- x[,-1]
rownames(Y)<-x[,1]

MSE <- mean(QMEs)
k=4
t=5
r=4

ecovalency (Y , MSE, r, t, k, int.effect="FIX",eco.env=F) 
#ecovalency (Y , MSE, r, t, k, int.effect="FIX",eco.env=T)

##letra e
library(agricolae)
tk <- HSD.test(aov.q2, 'cult',
               group=TRUE, alpha=0.05)

bar.group(tk$groups,ylim=c(0,15))

###Questao 8

q4<-read.csv("q_4.csv",sep=';')
q4<- transform(q4, trat = factor(trat), rep= factor (rep), bloco = factor(bloco))

s1<- q4[q4$safra == "I",]
s2<-  q4[q4$safra == "II",]

##letra b

##### Easyanova para obter as medias ajustadas

library(easyanova)
s1<-s1[c("trat","rep","bloco","prod")]
s2<-s2[c("trat","rep","bloco","prod")]
str(s1)

#ANOVA intrablocos
#SAFRA
aov.intrasafra<-aov(massa~blocao+trat+bloquinho%in%blocao, data=safra)
library(car)
Anova(aov.intrasafra, type = "II") #Car (correto)

#SAFRINHA
aov.intrasafrinha<-aov(massa~blocao+trat+bloquinho%in%blocao, data=safrinha)
Anova(aov.intrasafrinha, type = "II")

intra.easy<-ea1(s1, design = 10)
intra.easy

intra.easy$`Analysis of variance`

intra.easy2 <- ea1(s2, design = 10)

intra.easy2$`Analysis of variance`

s1_ea<- ea1(s1, 11)
s2_ea<- ea1(s2, 11)
summary(s1_ea)

s1_ea$`Efficiency of the design (%)`
s2_ea$`Efficiency of the design (%)`

anova.s1<-s1_ea$`Analysis of variance (marginal anova = type III SS)`
anova.s2<-s1_ea$`Analysis of variance (marginal anova = type III SS)`

s1_mean<-s1_ea$`Adjusted means`[c("treatment", "adjusted.mean")]; s1_mean$safra <-"I"
s2_mean<-s2_ea$`Adjusted means`[c("treatment", "adjusted.mean")]; s2_mean$safra <-"II"

library(lme4) 
# an�lise via modelos mistos: efeito aleat�rio deve ser representado por (1|efeito)
str(s1)
lmer.prod <- lmer(prod ~ rep + (1|rep:bloco) + trat,data = s1)
summary(lmer.prod)
VarCorr(lmer.prod)

anova(lmer.prod) # Tem como sa�da, a an�lise dos efeitso fixos, sem o P.valor
lmerTest::ranova(lmer.prod) # Teste dos componentes de vari�ncia

(aa <- car::Anova(lmer.prod,type = 'II', test.statistic=c("F")))
car::Anova(lmer.prod, test.statistic="F")

##letra d

s_mean<- rbind(s1_mean,s2_mean)
s_mean<- transform(s_mean, treatment = factor(treatment), safra = factor(safra))

#Analise conjunta com as m?dias
str(s_mean)
aov_conj<- aov(adjusted.mean ~ treatment + safra, data= s_mean)
anova.conj<-car::Anova(aov_conj,type ='II')

q4<-read.csv("q_4.csv",sep=';')
q4<- transform(q4, trat = factor(trat), rep= factor (rep), bloco = factor(bloco), safra=factor(safra))
str(q4)
library(lme4)
aov.conjunta<-lmer(prod ~ safra + bloco%in%safra+ (1|trat), data=q4)
library(car)
Anova(aov.conjunta, type = "III")

#Pegando QMEs e Graus de liberdade
anova.s1<-anova(aov(prod ~ rep + bloco:rep + trat , data=s1)) 
anova.s2<-anova(aov(prod ~ rep + bloco:rep + trat , data=s2))
QME.conj<-(1149633+1716047)/2


#letra e

#Variancias geneticas indvs
QME.s1<-1716047
QME.s2<-1149633

QMG.s1<-anova.s1[2,3]
QMG.s2<-anova.s2[2,3]

(var.s1<-(QMG.s1-QME.s1)/4)
(var.s2<-(QMG.s2-QME.s2)/4)

mean(c(var.s1,var.s2))

#Variancias conjs

#Para  bater a media das variancias geneticas ? preciso ajustar o modelo novamente:
anova.fix<-anova(aov(prod ~ safra + safra:rep + safra:rep:bloco + trat + trat:safra, data=q4)) 

#Com os valores do modelo completo agora eh possivel estimar as variancias geneticas e da interacao
(varg.conj<- (anova.fix[2,3] - anova.fix[4,3] )/ (4*2) )
(varint.conj<-   (anova.fix[4,3]  - QME.conj )/ (4) )

#verificando equivalencia 
(varg.conj + varint.conj) == mean(c(var.s1,var.s2))
