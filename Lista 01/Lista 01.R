#--------------------------------------------------------------------------------
# ANÁLISE DE EXPERIMENTO 2022.2
# PRIMEIRA LISTA
# RICARDO ANTONIO RUIZ CARDOZO
#--------------------------------------------------------------------------------

# QUESTÂO 01.	Seja Z uma variável normal padronizada [Z ~ N(0,1)]. 
#Determine as seguintes probabilidades e as represente em gráficos:
#--------------------------------------------------------------------------------
#ITEM 1.a)	P(Z ≥ 1,8)
#--------------------

###Calcular a seguinte probabilidade:
#P(Z>a)
a= 1.8

pnorm(q = a, mean = 0, sd = 1, lower.tail = F, log.p = FALSE)


#Graph
z <- seq(-5, 5, length=100)
mean=0; sd=1
lb=a; ub=5*sd + mean

#lb: limite inferior
#ub: limite superior 

dnorm()
hz <- dnorm(z,mean,sd)
?plot
plot(z, hz, type="n", xlab="Z", ylab="",
     main="Distribuição Normal Padronizada", axes= T)



i <- z >= lb & z <= ub
lines(z, hz)
polygon(c(lb,z[i],ub), c(0,hz[i],0), col="red")

#area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
area <- pnorm(lb, mean, sd, lower.tail = F)

# result <- paste("P(",lb,"< Z <",ub,") =",
#                 signif(area, digits=3))
result <- paste("P(Z >=",lb,") =",
                signif(area, digits=3))

mtext(result,3)
#axis(1, at=seq(-5*sd + mean, 5*sd + mean, 1), pos=0) 

#--------------------------------------------------------------------------------  
#ITEM 1.b)	P(-1,4 ≤ Z ≤ -0,45)
#-----------------------------

###Calcular a seguinte probabilidade:
#P(a<Z<b)

a=-1.4;b=-0.45
pnorm(q = a, mean = 0, sd = 1, lower.tail = F, log.p = FALSE)-
  pnorm(q = b, mean = 0, sd = 1, lower.tail = F, log.p = FALSE)

#Graph
z <- seq(-4, 4, length=100)
mean=0; sd=1
lb=a; ub=b

hz <- dnorm(z,mean,sd)

plot(z, hz, type="n", xlab="Z", ylab="",
     main="Distribuição Normal Padronizada", axes=T)

i <- z >= lb & z <= ub
lines(z, hz)
polygon(c(lb,z[i],ub), c(0,hz[i],0), col="red")

area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
result <- paste("P(",lb,"<= Z <=",ub,") =",
                signif(area, digits=3))

mtext(result,3)
axis(1, at=seq(-4*sd + mean, 4*sd + mean, 1), pos=0) 

#--------------------------------------------------------------------------------
#ITEM 1.c)	P(Z ≤ 0,45)
#---------------------

###Calcula a probabilidade
#P(Z≤a)

a=0.45

pnorm(q = a, mean = 0, sd = 1, lower.tail = T, log.p = F)

#Graph
z <- seq(-5, 5, length=100)
mean=0; sd=1
#lb=a; ub=6*sd + mean
#lb=a; ub=((5*sd) - mean)
lb=-(5*sd); ub=a

hz <- dnorm(z,mean,sd)

plot(z, hz, type="n", xlab="Z", ylab="",
     main="Distribuição Normal Padronizada", axes=T)

i <- z >= lb & z <= ub
lines(z, hz)
polygon(c(lb,z[i],ub), c(0,hz[i],0), col="red")

#area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
area <- pnorm(a, mean, sd, lower.tail = T)

# result <- paste("P(",lb,"< Z <",ub,") =",
#                 signif(area, digits=3))
result <- paste("P(Z <=",ub,") =",
                signif(area, digits=3))

mtext(result,3)
axis(1, at=seq(-5*sd + mean, 5*sd + mean, 1), pos=NA)
#--------------------------------------------------------------------------------
#ITEM 1.d)	P(Z ≤ -0,3)
#---------------------

### ###Calcula a probabilidade
#P(Z≤a)

a=-0.3

pnorm(q = a, mean = 0, sd = 1, lower.tail = T, log.p = F)

#Graph
z <- seq(-5, 5, length=100)
mean=0; sd=1
#lb=a; ub=6*sd + mean
#lb=a; ub=((5*sd) - mean)
lb=-(5*sd); ub=a

hz <- dnorm(z,mean,sd)

plot(z, hz, type="n", xlab="Z", ylab="",
     main="Distribuição Normal Padronizada", axes=T)

i <- z >= lb & z <= ub
lines(z, hz)
polygon(c(lb,z[i],ub), c(0,hz[i],0), col="red")

#area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
area <- pnorm(a, mean, sd, lower.tail = T)

# result <- paste("P(",lb,"< Z <",ub,") =",
#                 signif(area, digits=3))
result <- paste("P(Z <=",ub,") =",
                signif(area, digits=3))

mtext(result,3)

#--------------------------------------------------------------------------------
#ITEM 1.e)	P(-0,7 ≤ Z ≤ 0,6)
#---------------------

### ###Calcula a probabilidade
#P(a≤Z≤b)

a=-0.7; b= 0.6

pnorm(q = a, mean = 0, sd = 1, lower.tail = F, log.p = F)-
  pnorm(q = b, mean = 0, sd = 1, lower.tail = F, log.p = FALSE)

#Graph
z <- seq(-5, 5, length=100)
mean=0; sd=1
#lb=a; ub=6*sd + mean
#lb=a; ub=((5*sd) - mean)
lb=a; ub=b

hz <- dnorm(z,mean,sd)

plot(z, hz, type="n", xlab="Z", ylab="",
     main="Distribuição Normal Padronizada", axes=T)

i <- z >= lb & z <= ub
lines(z, hz)
polygon(c(lb,z[i],ub), c(0,hz[i],0), col="red")

#area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)

# result <- paste("P(",lb,"< Z <",ub,") =",
#                 signif(area, digits=3))
result <- paste("P(",lb,"<= Z <=",ub,") =",
                signif(area, digits=3))

mtext(result,3)

#--------------------------------------------------------------------------------
#ITEM 2.a) 2)	Encontre os valores de z da distribuição N(0,1), tais que:
#a)	P(Z > z) = 0,9798   
#---------------------
### 2.a
qnorm(p = 0.9798, mean = 0, sd = 1, lower.tail = F, log.p = FALSE)

### 2.b 
## b)	P(Z < z) = 0,063 
qnorm(p = 0.063, mean = 0, sd = 1, lower.tail = T, log.p = FALSE)

## c)	P(1< Z < z) = 0,10 
qnorm(p = 0.0, mean = 0, sd = 1, lower.tail = T, log.p = FALSE)
pnorm(1,lower.tail = T)


c<-pnorm(1,lower.tail = T)+.1

#qnorm(p = 0.9413, mean = 0, sd = 1, lower.tail = T)

qnorm(c)

##d)	P(-1,5 < Z < z)= 0,30
pnorm(-1.5, lower.tail = T)

c<- pnorm(-1.5, lower.tail = T)+0.3
qnorm(c)

#--------------------------------------------------------------------------------
#3)	Foi realizado um estudo sobre a altura de plantas de trigo em Latossolo Vermelho 
#do Cerrado com adubação fosfatada. Observou-se que este caráter se distribui normalmente
#com média 1,70 m e variância de 400 cm2. Calcule a probabilidade da altura (X) 
#de uma planta 

#a)	X > 1,90 m;  
#---------------------
a=1.90
pnorm(q = a, mean = 1.70, sd = 0.2, lower.tail = F, log.p = F)

# b)	1,90 < X < 2,10 m; 
a=1.90; b= 2.10

# Cálculo de la probabilidad en intervalo

pnorm(q = a, mean = 1.70, sd = 0.2, lower.tail = F, log.p = F)-
  pnorm(q = b, mean = 1.70, sd = 0.2, lower.tail = F, log.p = F)

# c)	X < 1,50 m;
a = 1.50

pnorm(q = a, mean = 1.70, sd = 0.2, lower.tail = T, log.p = F)

# d)	A partir de que altura (m) se encontra 30% das plantas mais altas?

qnorm(p = 0.30, mean = 1.70, sd = 0.2, lower.tail = F, log.p = FALSE)

# e)	A partir de que altura (m) se encontra 40% das plantas mais baixas?
# P (Z < z) = 0.4

qnorm(p = 0.40, mean = 1.70, sd = 0.2, lower.tail = T, log.p = FALSE)

# f)	Suponha que as plantas de trigo apresentem a seguinte classificação:
# Classe E – 10% mais baixas - P (Z < z) = 0.1
qnorm(p = 0.10, mean = 1.70, sd = 0.2, lower.tail = T, log.p = FALSE)

# Classe D – 30% seguintes à classe E - P (Z < z) = 0.4
qnorm(p = 0.40, mean = 1.70, sd = 0.2, lower.tail = T, log.p = FALSE)

# Classe C – 15% seguintes à classe D - P (Z < z) = 0.55
qnorm(p = 0.55, mean = 1.70, sd = 0.2, lower.tail = T, log.p = FALSE)

# Classe B – 25% seguintes à classe C - P (Z < z) = 0.8
qnorm(p = 0.8, mean = 1.70, sd = 0.2, lower.tail = T, log.p = FALSE)

# Classe A – 20% mais altas - P (Z < z) = 1
qnorm(p = 0.2, mean = 1.70, sd = 0.2, lower.tail = F, log.p = FALSE)

#--------------------------------------------------------------------------------
# 4. μ=9,8 (kg/parcela) e desvio padrão σ=3,5 (kg/parcela).
# Num programa de melhoramento, entre outras características, 
# uma cultivar deve satisfazer a condição 7,0 < x < 15,0 kg/parcela.
# Nessas condições, tendo-se 169 linhagens de feijão, pergunta-se: 
#--------------------------------------------------------------------------------
# a)	Qual a proporção de linhagens que deverá ser aceita?

## P(7 < Y < 15)
pnorm(q = 7, mean = 9.8, sd = 3.5, lower.tail = F, log.p = FALSE) -
  pnorm(q = 15, mean = 9.8, sd = 3.5, lower.tail = F, log.p = FALSE)

mean=9.8; sd=3.5
lb=7; ub=15

x <- seq(-4,4,length=169)*sd + mean
hx <- dnorm(x,mean,sd)

plot(x, hx, type="n", xlab="Produção (kg/parcela)", ylab="",
     main="Normal Distribution", axes=F)

i <- x >= lb & x <= ub
lines(x, hx)
polygon(c(lb,x[i],ub), c(0,hx[i],0), col="red")

area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
result <- paste("P(",lb," kg/parcela < Y <",ub," kg/parcela) =",
                signif(area, digits=3))
mtext(result,3)
axis(1, at=seq(-4*sd + mean, 4*sd + mean, 1.2), pos=0)

# P (y > 14.0 kg/parcela)
pnorm(q = 14, mean = 9.8, sd = 3.5, lower.tail = F, log.p = FALSE)

# 5)	Suponha que as medidas dos grãos de pólen de Euterpe oleracea (açaizeiro) 
# em vista equatorial, em µm, seja uma variável normalmente distribuída 
# (Dados adaptados da dissertação de Oliveira, 2011). 
# Assume-se que o comprimento do colpo tenha média 96,60 µm e desvio padrão de 12,00 µm, 
# e que a largura do colpo tenha média 1,23 µm e desvio padrão de 0,30 µm. 
# Qual a probabilidade de sortear um grão de pólen com

# a)	comprimento do colpo maior que 97,20 µm e largura do colpo menor que 1,19 µm?

# Comprimento
# P (comp > 97.20)
compr <- pnorm(q = 97.20, mean = 96.60, sd = 12, lower.tail = F, log.p = FALSE) 
# Largura
# P (larg < 97.20)
larg <- pnorm(q = 1.19, mean = 1.23, sd = 0.30, lower.tail = T, log.p = FALSE)

# producto de probabilidades

compr*larg

# b)	comprimento do colpo menor que 95,60 µm ou largura do colpo maior que 1,26 µm?
# P (comp < 95.60)
#Comprimento
compr <- pnorm(q = 95.60, mean = 96.60, sd = 12, lower.tail = T, log.p = FALSE) 

# Largura
larg <- pnorm(q = 1.26, mean = 1.23, sd = 0.30, lower.tail = F, log.p = FALSE)

compr+larg

compr
larg

#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
#QUESTÃO 6
#--------------------------------------------------------------------------------


productividade<- c(1.38,	4.14,	6.23,	12.13,	17.12, 3.65,	4.54,	6.79,	12.56,	19.68,
             3.72,	5.64,	8.21,	13.19,	21.26, 3.87,	5.67,	9.79,	15.6,	24.57)

## Sumatorio 

sum(produção)

#ITEM 6.b) somatório ao quadrado, isto é, cada valor de produção elevado ao quadrado e
# somados, ou simplesmente mutiplicar o vetor produção por ele mesmo e depois 
# somá-lo

sum_cuadrados <- productividade*productividade

sumatoria_cuadrados <- sum(sum_cuadrados)

## Média amostral
media <- mean(productividade)

# Soma dos desvios em relacao a média
dados <- data.frame(productividade, media)
desvios <- dados$productividade - dados$media

dados <- cbind(dados, desvios) #cbind para colocar una nueva columna en un dataframe
head(dados)
suma_desvios <- sum(dados$desvios)

format(round(suma_desvios,4))

# e)	Calcule a soma de quadrados de desvios em relação à média
qua_desv <- (dados$productividade - dados$media)^2
dados <- cbind(dados, qua_desv)

sum_qua_desv <- sum(dados$qua_desv)
sum(sum_qua_desv)

#f ) Variancia amostral S2
#S^2=  1/(n-1)  [∑_(i=1)^n▒X_i^2 - (∑▒X_i )^2/n]

sumatoria_dados <- sum(dados$productividade)

sumatoria_dados
variancia_amostral <- (1/(20-1)) * (sumatoria_cuadrados - ((sumatoria_dados^2)/20))
variancia_amostral

### desvio padrao 
sd(dados$productividade)
desvest <- sqrt(variancia_amostral)
desvest
coefic_varia <- desvest/media 
coefic_varia
##### h) sumar constante 10 achar de novo a média, variancia amostral, o desvio e coeficiente

head(dados)

suma_product <- dados$productividade + 10
dados <- cbind(dados, suma_product) ### Agregando la suma de la constante en la tabla

media_sum_prod <- mean(dados$suma_product)
media_sum_prod
var(dados$productividade)
var(dados$suma_product)
sd(dados$suma_product)
cv = sd(dados$suma_product)/media_sum_prod
cv

mean= mean(dados$productividade); sd=sd(dados$productividade)

x <- seq(-3,3,length=20)*sd + mean
hx <- dnorm(x,mean,sd)
?seq
plot(x, hx, type="n", xlab="Produção (g/planta)", ylab="",
     main="A) Normal Distribution Original", axes=T)

lines(x, hx)
abline(v=mean, col="red", lty="dashed", lwd="2")

mean= mean(dados$suma_product); sd=sd(dados$suma_product)

x <- seq(-3,3,length=20)*sd + mean
hx <- dnorm(x,mean,sd)
?seq
plot(x, hx, type="n", xlab="Produção (g/planta)", ylab="",
     main="B) Normal Distribution Somatório", axes=T)

lines(x, hx)
abline(v=mean, col="red", lty="dashed", lwd="2")



# i) multiplica constante 10 achar de novo a média, variancia amostral, o desvio e coeficiente

head(dados)

mult_product <- dados$productividade * 10
dados <- cbind(dados, mult_product) ### Agregando la suma de la constante en la tabla

media_mult_prod <- mean(dados$mult_product)
media_mult_prod
var(dados$productividade)
var(dados$mult_product)
sd(dados$mult_product)
cv = sd(dados$mult_product)/media_mult_prod
cv


mean= mean(dados$mult_product); sd=sd(dados$mult_product)

x <- seq(-3,3,length=20)*sd + mean
hx <- dnorm(x,mean,sd)
?seq
plot(x, hx, type="n", xlab="Produção (g/planta)", ylab="",
     main="B) Normal Distribution Multiplo (k=10)", axes=T)

lines(x, hx)

abline(v=mean, col="red", lty="dashed", lwd="2")

#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
#QUESTÃO 7
#--------------------------------------------------------------------------------

altura<-c(20,9.2,12,14.4,12.5,5.8,10.8,15.7,21.5,13.2,12.8,14.8,15.8,22.2,11.2,
         8.7,12,20.6,17.5,8.8,19,15.4,18.5,13.3,13.8,19.3,17.6,23.2,8.2,11.4)

#Questao 7 
cap <-
  c(20,
    5.8,
    12.8,
    8.7,
    19,
    19.30,
    9.2,
    10.8,
    14.8,
    12,
    15.40,
    17.60,
    12,
    15.70,
    15.80,
    20.60,
    18.50,
    23.20,
    14.40,
    21.50,
    22.20,
    17.50,
    13.30,
    8.20,
    12.5,
    13.2,
    11.2,
    8.8,
    13.8,
    11.4
  )
cap

library(fBasics)
basicStats(cap)


#letra b

qchisq(0.05/2,29) #Superior
qchisq(1- 0.05/2,29) #Inferior

(LS = ( (30 -1)*20.675586)/qchisq(0.05/2,29) )
(LI = ( (30 -1)*20.675586)/qchisq(1- 0.05/2 ,29) )



#letra 
hist(cap,main="Circunferência à altura do peito", xlab="Circunfência (cm)", ylab="Frequência", col="darkred", border="black")

#letra e
#QQ PLOT
qq<-qqnormPlot(cap)

##Q-Q Plot da Distribui��o Normal
qqnorm(dados$t.ha)


cor(qq$x,qq$y) # Correla��o entre os quantis observados e esperados


cor(qq$x,qq$y) #Correlaçao para comparacao na tabela Aula 2 Slide 19

#letra f
shapiro.test(cap)

#-------------------------------------------------------------------------------
#QUESTÃO 8
#-------------------------------------------------------------------------------


batata <-
  c(9.2,21.1,22.6,15.4,12.7,20,23.1,18,13.4,27,29.9,11.9,18,21.1,24.2,24.6,11,26.4
    ,24.2,10.1,18.2,20,26.4,24,9.2,25.7,25.1,12.3,17.1,28)
batata
#8 A) QQ PLOT
qqnorm(batata)
require("fBasics") # Carrega o pacote e suas funções
qqnormPlot(batata) # Função do fBasics para realizar o Q-Q plot da Distribuição normal

res <- qqnormPlot(batata) # Atribui o qqplot a um objeto denominado de res
cor(res$x,res$y) # Correlação entre os quantis observados e esperados
## Teste de Normalidade de Shapiro-Wilk
shapiro.test(batata)

##8 B) intervalo de confiança da media
ic.media <- t.test(batata, conf.level=0.99)
ic.media

basicStats(x = batata, ci = 0.99)

####C) intervalo de confiança da variancia de uma população normal
install.packages("asbio")
library(asbio)
ci.sigma(batata, conf = 0.99)

##D) intervalo de confiança da media
ic.media <- t.test(batata, conf.level=0.95)
ic.media
basicStats(x = batata, ci = 0.95)
## intervalo de confiança da variancia de uma população normal
ci.sigma(batata, conf = 0.95)
