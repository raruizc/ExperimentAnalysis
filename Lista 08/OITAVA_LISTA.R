###OITAVA LISTA

#IMPORTANDO BASE DE DADOS DE UMA FORMA MAIS FACIL
q1<- read.csv("q_1.csv",sep=';')

#VISUALIZANDO COMO ESTA A BASE
str(q1)

#TRANSFORMANDO OS FATORES (O QUE JÁ ESTIVER CERTO NÃO PRECISA)
q1<-transform(q1, local=factor(local),bloco=factor(bloco),esp=factor(esp))
View(q1)

###########1.B)###########
#SEPARANDO POR LOCAIS
q1_araras<-subset(q1, local=='araras')
q1_mogi<- subset(q1, local=='mogi')
q1_sao<-subset(q1, local=='sao')

#AJUSTANDO OS MODELOS
aov_araras<- aov(H ~ bloco + esp , data=q1_araras)
aov_mogi<- aov(H ~ bloco + esp , data=q1_mogi)
aov_sao<- aov(H ~ bloco + esp , data=q1_sao)

#TABELA DE ANOVA
(anova_araras <-anova(aov_araras))
(anova_mogi <-anova(aov_mogi))
(anova_sao <-anova(aov_sao))


#Teste de Bartlett para testar a homigeneidade 
#CONJUNTA
bartlett.test(q1$H,q1$local)

#LOCAL 1 ARARAS
bartlett.test(q1_araras$H,q1_araras$esp)

#LOCAL 2 MOGI
bartlett.test(q1_mogi$H,q1_mogi$esp)

#LOCAL 3 SAO
bartlett.test(q1_sao$H,q1_sao$esp)

###########1.C)###########

#CV
(cv_araras<-sqrt(anova_araras[3,3])*100/mean(q1_araras$H))
(cv_mogi<-sqrt(anova_mogi[3,3])*100/mean(q1_mogi$H))
(cv_sao<-sqrt(anova_sao[3,3])*100/mean(q1_sao$H))

##Acuracia
(rgg_araras<-sqrt(1-1/anova_araras[2,4]))
(rgg_mogi<-sqrt(1-1/anova_mogi[2,4]))
(rgg_sao<-sqrt(1-1/anova_sao[2,4]))

###########1.D)###########
#Y = u + b%in%l + l + s + l:s + e
str(q1)
aov_conj<- aov(H ~ bloco%in%local + local + esp + local:esp, data=q1)
(anova_conj<-anova(aov_conj))

###########1.E)###########
library(dplyr)
library(ggplot2)
q1_mean<-q1 %>% group_by(local,esp) %>% summarise(H = mean(H))

ggplot(q1_mean, aes(x = local, y= H, group = esp))+
  geom_line(aes(color = esp),size=2)

###########1.F)###########
QMG_araras<-anova_araras[2,3]
QMG_mogi<-anova_mogi[2,3]
QMG_sao<-anova_sao[2,3]


#Parte simples
QMG_medio<- mean(c(QMG_araras,QMG_mogi,QMG_sao))
(ps<-(1/(3-1)) * (  (sqrt(QMG_araras) - sqrt(QMG_medio))^2 +
                      (sqrt(QMG_mogi) - sqrt(QMG_medio))^2 +
                      (sqrt(QMG_sao) - sqrt(QMG_medio))^2 ))

#Parte Complexa

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


############2.A)###########

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

###########2.b)###########

#Y = u + l +b%in%l + c + c:l + e

##letra c

#Y = u + l + c + l:c
mean(QMEs)
str(q2)
aov.q2<-aov(prod ~ local + cult, data=q2)
anova(aov.q2)


###########1.D)###########


#####  Ecovalency function 


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


##### Exemplo

#Especificar o diretorio de trabalho
x <- read.csv("q_2eco.csv",sep=';',dec='.')
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


######################3################

#CARREGANDO BASE DE DADOS
q3<-read.csv("q_3.csv", sep = ";")
#str(q3)
q3<-transform(q3, epoca=factor(epoca), blocao=factor(blocao),bloquinho=factor(bloquinho),trat=factor(trat))

safra <- subset(q3,epoca=="SAFRA")
safrinha <- subset(q3,epoca=="SAFRINHA")


######################3.B)################
#ANOVA intrablocos
#SAFRA
aov.intrasafra<-aov(massa~blocao+trat+bloquinho%in%blocao, data=safra)
library(car)
Anova(aov.intrasafra, type = "II") #Car (correto)

#SAFRINHA
aov.intrasafrinha<-aov(massa~blocao+trat+bloquinho%in%blocao, data=safrinha)
Anova(aov.intrasafrinha, type = "II")

########OU USANDO O PACOTE easyanova
library(easyanova)

#SAFRA
#Organizando
safraorg<-safra[c("trat","blocao","bloquinho","massa")]

#Intrabloco 
intra.easy_safra<-ea1(safraorg, design = 10)
intra.easy_safra

intra.easy_safra$`Analysis of variance`
intra.easy_safra$`Efficiency of the design (%)`


#SAFRINHA
#Organizando
safrinhaorg<-safrinha[c("trat","blocao","bloquinho","massa")]

#Intrabloco 
intra.easy_safrinha<-ea1(safrinhaorg, design = 10)
intra.easy_safrinha

intra.easy_safrinha$`Analysis of variance`
intra.easy_safrinha$`Efficiency of the design (%)`

### OU EFICIENCIA INTRABLOCOS (MANUAL)
aov.dbc<-aov(massa~blocao+trat, data=safrinha)
anova.dbc<-anova(aov.dbc)

#Variancia DBC
(var.dbc<-2/3*anova.dbc[3,3])

#Variancia DBI
anova.intrasafrinha<-intra.easy$`Analysis of variance`
anova.intrasafrinha[4,3]
var.intra<-( ( 1+( 3/ ( (3-1)*(5+1) ) ) )* anova.intrasafrinha[4,3])

#Eficiencia
var.dbc*100/(var.intra*2/3)
intra.easy$`Efficiency of the design (%)`

######################3.C)################

conjunta<-q3
#usar o lme4 quando tiver fator aleatório e este não vai sair na analise
#de variancia vito que só aparecem os que sao fixo 
library(lme4)
aov.conjunta<-lmer(massa ~ epoca + blocao%in%epoca+ (1|trat), data=conjunta)
Anova(aov.conjunta, type = "III")

#####################3.D)################

#MEDIA AJUSTADA SAFRA
mean.intra_safra<-intra.easy_safra$`Adjusted means`[c("treatment","adjusted.mean")]
(mean.intra_safra<-mean.intra_safra[order(mean.intra_safra$treatment),])

#MEDIA AJUSTADA SAFRINHA
mean.intra_safrinha<-intra.easy_safrinha$`Adjusted means`[c("treatment","adjusted.mean")]
(mean.intra_safrinha<-mean.intra_safrinha[order(mean.intra_safrinha$treatment),])

#IMPORTANDO ARQUVIDO COM MÉDIAS AJUSTADAS
q_3d<-read.csv('q_3d.csv', sep=';')
q_3d<-transform(q_3d, epoca=factor(epoca), trat=factor(trat))
str(q_3d)


aov.q_3d<-aov(med_ajus ~ epoca + trat, data=q_3d)
anova(aov.q_3d)
Anova(aov.q_3d, type = "III")

#####################3.E)################
#VARIANCIA DE HIBRIDO SAFRA
I<-4
K<-4

(vhsafra<-(2565965-1716047)/I*K)

(vhsafrinha<-(4224541-1149633)/I*K)

(vhconjunta<-(1132107-1081117)/I)
