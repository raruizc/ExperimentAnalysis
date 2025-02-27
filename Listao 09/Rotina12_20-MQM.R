####################################################################################
########################## M�TODO DOS QUADRADOS M�NIMOS ############################
####################################################################################

############################### Comandos b�sicos ###################################
#Criando uma matriz com elementos pr�-determinados
(M<-matrix(c(1,2,5,7),2,2)) 

is.matrix(M)
str(M)
dim(M) # Dimens�es da matriz (L x C)
M[1,2] # capturar o elemento na 1� linha e 2� coluna na matriz A
M[1,] # capturar todos os elementos 2� linha da matriz A
M[,2] # capturar todos os elementos 4� coluna da matriz A

#Matriz de 1s 5 x 5
(J<-matrix(1,5,5))

#Matriz identidade
(I <- diag(5))

#matriz com n�meros inteiros sequenciais de 1 a 25 preenchidos por coluna
(A<-matrix(c(1:25),5,5, byrow = F))

#matriz com n�meros inteiros sequenciais de 1 a 25 preenchidos por linha
(B<-matrix(c(1:25),5,5, byrow = T))


M
t(M) # Transposta da matriz 
diag(M) # Captura a diagonal principal da matriz
fBasics::rk(M) # Rank ou posto da matriz
det(M) # Determinante da matriz
(Mi <- solve(M)) # Inversa cl�ssica da matriz de posto completo

Mi%*%M
M%*%Mi
M%*%Mi%*%M

A
dim(A)
fBasics::rk(A) # Rank ou posto da matriz
det(A) # Determinante da matriz
solve(A)
(G <- MASS::ginv(A)) # Inversa generalizada de Moore-Penrose da matriz A

A%*%G%*%A

solve(M)
MASS::ginv(M) # Inversa generalizada da matriz y = inversa classica de y com posto completo


A+B # Soma de matrizes
A*B # Element-wise product - a[i,j] x b[i,j]
A%*%B # Multiplica��o de matrizes
A%*%M

########################### Exemplo - Modelo ANAVA #############################
setwd("D:/UFLA/Disciplinas/Pos-Graduacao/Analise de Experimentos em Genetica e Melhoramento de Plantas/Semestre 2020.2/Rotinas R")
dados <- read.table("Rotina12_dados.txt",header=TRUE)
head(dados)
dados$t <-factor(dados$t)
str(dados)

intercept <- matrix(1,nrow(dados),1)
(mod.t <- model.matrix(~dados$t-1))
X <- cbind(intercept,mod.t)
Y <- as.matrix(dados$y)
is.matrix(Y)
dim(Y)

#Sistema de Equa��es Normais (X'X)B = X'Y
(XlX <- t(X)%*%X)
(XlY <- t(X)%*%Y)

#XlX n�o tem posto completo
(posto <- fBasics::rk(XlX))
#XlX � singular - Sistema com infinitas solu��es
det(XlX)

####Resolvendo o SEN com restri��es

# Restri��o padr�o do R - Zerando o primeiro efeito
#Xr1 <- model.matrix(~dados$t)
(Xr1 <- model.matrix(~t, contrasts.arg = list(t = "contr.treatment"), data=dados))

(postoR1 <- fBasics::rk(t(Xr1)%*%Xr1))

(B1 <- solve(t(Xr1)%*%Xr1)%*%(t(Xr1)%*%Y))

(medias <- tapply(dados$y, dados$t, mean))
(int1 <- medias[1])
(t2 <- medias[2]-medias[1])
(t3 <- medias[3]-medias[1])
(t4 <- medias[4]-medias[1])
(t5 <- medias[5]-medias[1])

# Restri��o da Soma dos efeitos de trat igual a zero
Xr2 <- model.matrix(~t, contrasts.arg = list(t = "contr.sum"), data=dados)

(postoR <- fBasics::rk(t(Xr2)%*%Xr2))

(B2 <- solve(t(Xr2)%*%Xr2)%*%(t(Xr2)%*%Y))

(mediaG <- mean(dados$y))
medias <- tapply(dados$y, dados$t, mean)
medias[1]-mediaG
medias[2]-mediaG
medias[3]-mediaG
medias[4]-mediaG
medias[5]-mediaG


#Invari�ncia

(Yhat1 <- Xr1%*%B1)
(Yhat2 <- Xr2%*%B2)
cbind(Yhat1,Yhat2)
plot(Yhat1,Yhat2)
cor(Yhat2,Yhat2)

####ANAVA matricialmente

#Corre��o
J1 <- matrix(1,20,20)
(Co <- (t(Y)%*%J1%*%Y)/nrow(Y))

#SQ Total
(SQT <- t(Y)%*%Y-Co)

#SQ Tratamentos
(SQTrat <- t(B1)%*%t(Xr1)%*%Y-Co)

#SQ Erro
(SQE <- SQT-SQTrat)

#Constrasts
#Restri��o padr�o do R
#contr.treatment(levels(dados$t))
(lm.R1 <- lm(y ~ t, data=dados,contrasts = list(t = "contr.treatment")))

aov.R1 <- aov(y ~ t, data=dados)
aov.R1 <- aov(y ~ t, data=dados, 
              contrasts = list(t = "contr.treatment"))
model.matrix(aov.R1)
coef(aov.R1)
emmeans::emmeans(aov.R1,"t")

summary(aov.R1)

#Restri��o soma zero
aov.R2 <- aov(y ~ t, data=dados, 
              contrasts = list(t = "contr.sum"))
model.matrix(aov.R2)
coef(aov.R2)
emmeans::emmeans(aov.R2,"t")



### Estima��o do ganho gen�tico com sucessivos ciclos seletivos

### y = XB + e
### yi = bo + b1xi + ei

intercepto<-matrix(1,4,1) #matriz de 1's com 4 linhas e uma coluna
ciclos<-matrix(c(0:3),4,1) #matriz de 0 a 3, com 4 linhas e uma coluna
x<-cbind(intercepto,ciclos)# matriz x do MQM. Comando para unir as colunas das duas matrizes
y<-matrix(c(5986.5,6487.8,6559.4,6713.6),4,1)
nrep <-20 #numero de repeti��es

xx<-t(x)%*%x #Obtendo a matriz x'x
dim(xx)
fBasics::rk(xx) # Posto completo
invxx<-solve(xx) # Inversa classica de xx
B<-invxx%*%(t(x)%*%y) #estimativas dos par�metros

#intervalo de confian�a do ganho:

QME<-559383.13
s<-sqrt(diag(invxx)*(QME/nrep)) #DEVE DIVIDIR O QME PELO N�MERO DE REPETI�OES, VISTO QUE, 
# A MATRIZ (X'X)-1 FOI CALCULADA COM BASE EM M�DIAS!
sGS<-s[2]

ganho<-B[2,1]
LS<-ganho+(qt(0.025, 12, lower.tail=FALSE)*sqrt(sGS));LS
LI<-ganho-(qt(0.025, 12, lower.tail=FALSE)*sqrt(sGS));LI        

#teste t H0:b1=0
tc <- ganho/sGS
ttab <- abs(qt(0.05/2,54))

#Adequacidade do modelo via teste F 
correcao<-(1/nrow(y)*((t(y)%*%intercepto%*%t(intercepto)%*%y)))
SQY<-((t(y)%*%y)-correcao)*nrep
SQModelo<-(t(B)%*%t(x)%*%y-correcao)*nrep
SQdesv<-SQY-SQModelo

Qmdesvio<-SQdesv/2
Fdesvio<-Qmdesvio/QME
TesteFmodelo<-SQModelo/QME # Modelo tem 1 GL


#Grafico da regress�o
yhat <- x%*%B
dadosg <- data.frame(cbind(x[,2],y,yhat))
colnames(dadosg) <- c("Ciclos", "Medias.Obs", "Medias.Est")

library(tidyverse)
library(ggplot2)
ggplot(data = dadosg,
       mapping = aes(x = Ciclos, y = Medias.Obs)) +
  geom_point() +
  geom_line(data = dadosg,
            mapping = aes(x = Ciclos,
                          y = Medias.Est),
            color = "red") +
  xlab(label = "Ciclos Seletivos") +
  ylab(label = "Produ��o (kg/ha)")


######Usando a fun��o lm - Aten��o aos valores n�o corretos dos testes F e t e erros-padroes 
dadosreg <- data.frame(cbind(x,y))
colnames(dadosreg) <- c("intercepto","ciclos","y")
lm.y <- lm(y ~ ciclos)
summary(lm.y)
anova(lm.y)
plot(y ~ ciclos)