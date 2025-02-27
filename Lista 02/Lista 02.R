#--------------------------------------------------------------------------------
# AN?LISE DE EXPERIMENTO 2022.2
# PRIMEIRA LISTA
# RICARDO ANTONIO RUIZ CARDOZO
#--------------------------------------------------------------------------------

rm(list = ls())
# ANAVA, delineamentos básicos, pressupostos e medidas da qualidade de experimentos
library(openxlsx)
library(fBasics)
## Primeira Questao DIC


dados_01 <- read.csv("01-dados.csv", header = T, sep=",")
dados_01 <- read.xlsx(xlsxFile = "dados_lista02.xlsx", sheet = "01-dados")
str(dados_01)
#Cambiar nombre de columna
colnames(dados_01)[1] <- "Progenie" 

# a) Modelo Estatístico Delineamento inteiramente Casualizado
##
# yij = u + pi + Eij

# b) Formule as hipóteses H0 e H1

# H0: as progênies não diferem estatisticamente entre si.
# H1: ao menos uma progênie difere das demais.

# c) verificar pressupostos
## Primeiramente convertir os dados progenies e repeticoes em fatores

dados_01 <- transform(dados_01, Progenie = as.factor(Progenie), Rep =as.factor(Rep))
str(dados_01)

## Primeiramente Realizar Anova 

anova.dap <- aov(DAP ~ Progenie, data=dados_01)
anova(anova.dap) #solta o quadro da an???lise de vari???ncia

res.dap <- resid(anova.dap) #captura os erros

# Q-Q Plot

#teste de normalidade dos erros - Shapiro-Wilk
shapiro.test(res.dap)

qqnorm(res.dap)
fBasics::qqnormPlot(res.dap)

# Q-Q Plot
qqnorm(res.dap)
require("fBasics") # Carrega o pacote e suas funcoes
qqnormPlot(res.dap)
res <- qqnormPlot(res.dap) # Atribui o qqplot a um objeto denominado de res
cor(res$x,res$y) # Correlação entre os quantis observados e esperados

#Shapiro-Wilk
shapiro.test(res.dap)
#Homocedasticidade
#Teste de Hartley

variance <- tapply(dados_01$DAP, INDEX = dados_01$Progenie, FUN = var)
#variância dentro de cada 
(Fmax <- max(variance)/min(variance))
## Instalar pacote SuppDists pelo menu
rep <- tapply(dados_01$DAP, INDEX = dados_01$Progenie, FUN = length)
#variância dentro de cada progenie
(Fmaxtab <- SuppDists::qmaxFratio(p = 0.05, df = mean(rep)-1,
                                  k=length(variance), lower.tail=F, log.p=FALSE))
(Pvalor <- SuppDists::pmaxFratio(q = Fmax, df = mean(rep)-1,
                                 k=length(variance), lower.tail=F, log.p=FALSE))
if (Pvalor < 0.05) print("Variancias residuais heterogeneas - Modelo
heterocedastico ??? indicado") else print("Variancias residuais homogeneas")

#Teste de Bartlett
bartlett.test(dados_01$DAP,dados_01$Progenie)

# Letra d
write.table(anova(anova.dap),file ='anova1d.txt')

##Letra E
var<- vector()
k=1
j=4
for(i in 1:10){
  vari<-print(var(dados_01$DAP[k:j]))
  k=k+4
  j=j+4
  var= c(var,vari)
} #Obtendo as 10 variancias
var
mean(var) #Médias das variâncias

#INDEPENDENCIA

#testes de independencia dos erros por Durbin-Watson
install.packages("car")
library(car)
car::durbinWatsonTest(anova.dap)


##LETRA F
anova.dap <- anova(anova.dap) #Capturando o analise de variância
(QME <- anova.dap[2,3]) #captura o QM do Erro a partir do objeto anava.DAP
(QMC <- anova.dap[1,3]) #captura o QM de cultivares a partir do objeto
anova.dap
(Fcal <- anova.dap$`F value`[1]) #captura o Fc associado ao efeito de cultivares
# a partir do objeto anava.prod
(media <- mean(dados_01$DAP))
#CVe coeficinte de variacao experimental
CVe <- (sqrt(QME)/media)*100
CVe
#IVe Indice de variacao experimental
r <- nlevels(dados_01$Rep)
IVe <- CVe/sqrt(r) #nlevels retorna o n???mero de n???veis do objeto, no caso
IVe
#r2 Coeficiente de determinacao genotipico
r2 <- (QMC-QME)/QMC
r2
#CVg Coeficiente de variação genotipico
CVg <- (sqrt((QMC-QME)/r)/media)*100
CVg
#Cvr Coeficiente de variação relativo

CVr <- CVg/CVe
CVr
#rgg Acuracia seletiva
(rgg1 <- sqrt(r2)*100)

##### Coeficiente de Varia��o Genot�pico (CVg)
CVg <- (sqrt((QMC-QME)/r)/media)*100
CVg


####### QUESTAO 2) DBCC
# Um experimento de avaliação de seis cultivares
# de feijão foi instalado no delineamento de blocos completos casualizados(DBCC). 

# C)	Verifique os pressupostos básicos a 5% de probabilidade
# para fins de realização da ANAVA (normalidade dos erros: Shapiro Wilk e Q-Q plot;
# aditividade dos efeitos: teste de Tukey; homocedasticidade: 
# teste de Anscombe e Tukey (1963) e gráfico resíduos vs preditos; 
# independência: Durbin-Watson). Interprete os resultados.


dados_02 <- read.xlsx(xlsxFile = "dados_lista02.xlsx", sheet = "02-dados")
View(dados_02)
str(dados_02)
dados_02 <- transform(dados_02, Cultivar = as.factor(Cultivar), Bloco =as.factor(Bloco))

# Q-Q Plot
aov.prod <- aov(Prod ~ Bloco + Cultivar, data=dados_02)
(anova.prod <- anova(aov.prod))
anava.prod <- anova.prod
res.dados <- aov.prod$residuals
#str(aov.prod)
#names(aov.prod)
#Q-Q Plot
qqnorm(res.dados)
require("fBasics") # Carrega o pacote e suas fun??????es
qqnormPlot(res.dados)
res <- qqnormPlot(res.dados) # Atribui o qqplot a um objeto denominado de res
cor(res$x,res$y) # Correla??????o entre os quantis observados e esperados

#Shapiro-Wilk
shapiro.test(res.dados)
#Teste de Tukey
library(asbio)
attach(dados_02) # fixa o objeto dados, nao sendo mais necessario ficar utilizando 
tukey.add.test(Prod,Cultivar, Bloco)
detach(dados_02)
#Teste de Anscombe e Tukey
#Funcao: AT.HOVtest
#Argumentos: ## nivel.conf => nivel de confianca => 1-alfa
## variavel => variável - resposta
## tratamento => fator de tratamento
## bloco => fator de blocagem
AT.HOVtest<-function(nivel.conf,variavel, tratamento, bloco)
{
  aov.plan <- aov(variavel ~ bloco + tratamento); aov.plan
  anova.plan <- anova(aov.plan);anova.plan
  #numerador
  matrix.obs.m<-matrix(fitted(aov.plan)-mean(variavel),ncol=1);matrix.obs.m
  matrix.erro2<-matrix(residuals(aov.plan)^2);matrix.erro2
  soma.2<-(sum(matrix.obs.m*matrix.erro2))^2;soma.2 #numerador
  numerador<-(soma.2)*(nlevels(tratamento)*nlevels(bloco));numerador
  #denominador, foi dividido em duas parte para ficar melhor de entender
  denom.1<-((2*(anova.plan["Residuals","Df"])*(anova.plan["Residuals","Mean Sq"]^2)))/((anova.plan["Residuals","Df"])+2);denom.1
  denom.2<-(nlevels(tratamento)-2)*(nlevels(bloco)-1)*(anova.plan["tratamento","Sum Sq"])+(nlevels(tratamento)-1)*(nlevels(bloco)-2)*(anova.plan["bloco","Sum Sq"]);denom.2
  denom<-denom.1*denom.2
  Fc<-(numerador/denom)
  Ft<-qf(nivel.conf,1,(anova.plan["Residuals","Df"]))
  resultado<-rbind(Fc,Ft)
  #rownames(resultado)<-c("F","Ft5%")
  
  print(resultado) 
}
AT.HOVtest(nivel.conf = 0.95,variavel = dados_02$Prod, tratamento =
             factor(dados_02$Cultivar), bloco = factor(dados_02$Bloco))


#Gráfico residuos x preditos
opar <- par(mfrow = c(2,2))
plot(aov.prod)
opar <- par(mfrow = c(1,1))

#testes de independencia dos erros por Durbin-Watson
install.packages("car")
library(car)
durbinWatsonTest(aov.prod)

### LETRA D
write.table(anova(aov.prod),file ='anova2d.txt')
anova(aov.prod)
##LETRA E
(QME <- anova.prod [3,3]) #captura o QM do Erro a partir do objeto anava.prod
#numero entre colchetes é a ordem do fator na tabela anava
(QMC <- anova.prod [2,3]) #captura o QM de cultivares a partir do objeto
anava.prod
(Fcal <- anava.prod$`F value`[1]) #captura o Fc associado ao efeito de cultivares a partir do objeto anava.prod
#CVe Coeficiente de variação experimental
(media <- mean(dados_02$Prod))
CVe <- (sqrt(QME)/media)*100
CVe
#Rgg Acurácia seletiva
r2 <- (QMC-QME)/QMC
(rgg1 <- sqrt(r2)*100)

####### QUESTAO 3
##### CUADRADO LATINO

dados_03 <- read.xlsx(xlsxFile = "dados_lista02.xlsx", sheet = "03-dados")
dados_03 <- transform(dados_03,Variedade = as.factor(Variedade), 
                      Linea = as.factor(Linea), Coluna = as.factor(Coluna))

str(dados_03)

aov.cana <- lm(Prod ~ Linea + Coluna + Variedade, data =dados_03)
anova(aov.cana)
anava.cana <- anova(aov.cana)

#### Pressupostos
#Q-Q PLOT
res.cana <- aov.cana$residuals
require("fBasics") # Carrega o pacote e suas fun??????es
qqnormPlot(res.cana)
res <- qqnormPlot(res.cana) # Atribui o qqplot a um objeto denominado de res
cor(res$x,res$y) # Correla??????o entre os quantis observados e esperados
# Shapiro-Wilk
shapiro.test(res.cana)
# Gráfico Resíduos x Preditos
opar <- par(mfrow = c(2,2))
plot(aov.cana)
opar <- par(mfrow = c(1,1))
### LETRA c
write.table(anova(aov.cana),file ='anova3d.txt')
##LETRA D
(QME <- anava.cana [4,3]) #captura o QM do Erro a partir do objeto anava.prod
#numero entre colchetes é a ordem do fator na tabela anava
(QMC <- anava.cana [3,3]) #captura o QM de cultivares a partir do objeto ou do 
# tratamento
(Fcal <- anava.cana$`F value`[1]) #captura o Fc associado ao efeito de cultivares
#a partir do objeto anava.prod
#CVe
(media <- mean(dados_03$Prod))
CVe <- (sqrt(QME)/media)*100
CVe
#Rgg
r2 <- (QMC-QME)/QMC
(rgg1 <- sqrt(r2)*100)

####### QUESTAO 4
##### DIC

dados_04 <- read.xlsx(xlsxFile = "dados_lista02.xlsx", sheet = "04-dados")
View(dados_04)
str(dados_04)
dados_04 <- transform(dados_04,Variedade = as.factor(Variedade), 
                      Rep = as.factor(Rep))

aov.manga <- aov(NF ~ Variedade, data= dados_04) 
anova(aov.manga) #
anava.manga <- anova(aov.manga)

var<- vector()
k=1
j=6
for(i in 1:5){
  vari<-print(var(dados_04$NF[k:j]))
  k=k+6
  j=j+6
  var= c(var,vari)
} #Obtendo as 10 variancias
var
mean(var) #M?dias das vari?ncias

var_variedade1 <- var(dados_04$NF[1:4]) 

var_variedade3 <- var(dados_04$NF[13:17])

Media_var <- ((var_variedade1*3)+(var[2]*5)+(var_variedade3*4)+(var[4]*5)+(var[5]*5))
Media_var
QME_dentro_progenies <- Media_var/anava.manga[2,1]
