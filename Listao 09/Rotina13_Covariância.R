####################################################################################
############################## EMPREGO DA COVARI�NCIA ##############################
####################################################################################

setwd("D:/UFLA/Disciplinas/Pos-Graduacao/Analise de Experimentos em Genetica e Melhoramento de Plantas/Semestre 2021.2/Rotinas R")
dados<-read.table("rotina13.milho.txt", h=T)
str(dados)
dados<-transform(dados, Bloco=factor(Bloco), Tratamento=factor(Tratamento))

#### ANCOVA NA EXPERIMENTA��O AGR�COLA

### PASSOS DA ANCOVA

### PASSO1: An�lise da vari�ncia adicional - n�mero de plantas ou estande final (X)
# Verificar se podemos utiliz�-la como covari�vel para a produ��o
#### Modelo: xij = u + rj + ti + eij

lm.estande <-lm(Estande ~ Bloco + Tratamento, data=dados)
(aov.estande <- anova(lm.estande))

str(aov.estande)

(SQTrat.estande <- aov.estande["Tratamento","Sum Sq"])
(SQE.estande <- aov.estande["Residuals","Sum Sq"])

res.estande <- resid(lm.estande) # armazena os residuos da anava, para posterior uso
plot(res.estande)

### PASSO2: An�lise de vari�ncia da vari�vel de interesse produ��o (Y)
#### Modelo: yij = u + rj + ti + eij
lm.producao <- lm(Producao ~ Bloco + Tratamento, data=dados)
(aov.prod <- anova(lm.producao))

(QME.prod <- aov.prod["Residuals","Mean Sq"]) #Captura o QME da anava para o efeito principal
aov.prod[3,3]

res.prod <- resid(lm.producao) # armazena os residuos da anava, para posterior uso
plot(res.prod)

### PASSO3: An�lise de covari�ncia (Y/X)
#### Modelo: yij = u + rj + ti + b(xij - Xm) + eij'

require(car)
str(dados) # Aten��o no n�mero de plantas (n�o � fator!)
#dados$Bloco <- as.numeric(dados$Bloco)

lm.ancova <- lm(Producao ~ Estande + Bloco + Tratamento, data=dados)
anova(lm.ancova)
summary(lm.ancova) #Teste t --- Ho: b=0

lm.ancova <- lm(terms(Producao ~ Bloco + Tratamento + Estande, keep.order=TRUE), data=dados)
anova(lm.ancova) #Aten��o ao ajuste da covari�vel

(ancova <- Anova(lm.ancova, type= "II"))

str(ancova)

names(ancova)
QME.prod2 <- ancova$`Sum Sq`[4]/ancova$Df[4]

#### Usando a fun��o MANOVA - An�lise de Vari�ncia Multivariada
### Modelo: (x,y) = u + rj + ti + eij
# MANOVA
manova.Prod.Est <- manova(cbind(Producao, Estande) ~ Bloco + Tratamento, data=dados)
summary.aov(manova.Prod.Est)
(manova <- summary(manova.Prod.Est, test ="Wilks"))
#test = c("Pillai", "Wilks", "Hotelling-Lawley", "Roy")
SSSP <- manova[["SS"]]
SSSP_Error <- SSSP[["Residuals"]]

SSE_ProdAj <- SSSP_Error[1,1] - SSSP_Error[2,1]**2/SSSP_Error[2,2]


########## Efici�ncia
QME.prod.Efetivo <- QME.prod2*(1 + (SQTrat.estande/((12-1)*SQE.estande)))
(Efic <- QME.prod/QME.prod.Efetivo*100)

########## Estimando o coeficiente da regress�o "b"
# Op��o 01
plot(res.prod ~ res.estande)
reg.res <- lm(res.prod ~ res.estande) # Regress�o dos erros
summary(reg.res)
(b <- reg.res$coefficients[2]) # valor do coefiente angular

# Op��o 02
coef(lm.ancova)
summary(lm.ancova)

# Op��o 03 - MANOVA
(b_est <- SSSP_Error[2,1]/SSSP_Error[2,2])


########## Obtendo as m�dias ajustadas
# Op��o 1
## yi(m) = y -b(xi(m) - xmg)
library(doBy)
med.prod <- LSmeans(lm.producao, "Tratamento")
med.prod1 <- med.prod[["coef"]][["estimate"]]

med.estande <- LSmeans(lm.estande, "Tratamento")
med.estande1 <- med.estande[["coef"]][["estimate"]]

medaj.prod <- med.prod1 - b*(med.estande1 - mean(med.estande1))

# Op��o 2
require(doBy)
medaj.prod2 <- LSmeans(lm.ancova, "Tratamento")   


#### ANCOVA NA GEN�TICA E MELHORAMENTO DE PLANTAS
dadosm <- read.table("rotina13.mand.txt", h=T)
str(dadosm)
dadosm <- transform(dadosm, BLOCO=factor(BLOCO), CLONE=factor(CLONE))

# Estimando a vari�ncia gen�tica usando a covari�ncia
## Op��o 01
library(sommer)
mmer.raiz <- mmer(RAIZES ~ BLOCO, random= ~ CLONE, tolpar = 1e-08, data=dadosm)
(VC_raiz <- summary(mmer.raiz)$varcomp)
pred_raiz <- predict.mmer(mmer.raiz, classify = "CLONE")
pred_raiz$pvals

write.csv(pred_raiz$pvals, file = "Med.BLUPS.Raiz.csv")

mmer.rama <- mmer(RAMAS ~ BLOCO, random= ~ CLONE, tolpar = 1e-08, data=dadosm)
(VC_rama <- summary(mmer.rama)$varcomp)
pred_rama <- predict.mmer(mmer.rama, classify = "CLONE")
write.csv(pred_rama$pvals, file = "Med.BLUPS.Rama.csv")

library(lme4)
lmer.raiz <- lmer(RAIZES ~ BLOCO + (1|CLONE), data=dadosm)
summary(lmer.raiz)
data.frame(VarCorr(lmer.raiz))

## Op��o 02
library(tidyr)
dados.r <- dadosm[,-4]
table_raiz <- spread(dados.r, BLOCO, RAIZES)

cov(table_raiz$`1`,table_raiz$`2`)

# Estimando as correla��es
## Op��o 1 - Usando a soma das vari�veis
dadosm$soma <- dadosm$RAIZES + dadosm$RAMAS
mmer.soma <- mmer(soma ~ BLOCO, random= ~ CLONE, tolpar = 1e-08, data=dadosm)
(VC_soma <- summary(mmer.soma)$varcomp)

(cov.g <- 1/2*(VC_soma[1,1] - VC_raiz[1,1] - VC_rama[1,1]))
(cov.e <- 1/2*(VC_soma[2,1] - VC_raiz[2,1] - VC_rama[2,1]))
(cov.f <- cov.g + cov.e/2)

(rg <- cov.g/sqrt(VC_raiz[1,1]*VC_rama[1,1]))
(re <- cov.e/sqrt(VC_raiz[2,1]*VC_rama[2,1]))
(rf <- cov.f/sqrt((VC_raiz[1,1] + VC_raiz[2,1]/2)*(VC_rama[1,1] + VC_rama[2,1]/2)))

## Op��o 2

library(sommer)
mmer.mult <- mmer(cbind(RAIZES,RAMAS) ~ BLOCO, random= ~ CLONE, tolpar = 1e-08, data=dadosm)
(VC <- summary(mmer.mult)$varcomp)

# Correla��o gen�tica
vpredict(mmer.mult, rg ~ V2 / sqrt( V1 * V3 ) ) #Erro-padr�o da estimativa da correla��o

# Correla��o ambiental
vpredict(mmer.mult, re ~ V5 / sqrt( V4 * V6 ) )

# Correla��o fenot�pica
vpredict(mmer.mult, rf ~ ((V2 + V5/2) / sqrt((V1 + V4/2) * (V3 + V6/2))))

aa <- predict.mmer(mmer.mult, classify = "CLONE")

write.csv(aa$pvals, file = "Med.BLUPS.Mult.csv")