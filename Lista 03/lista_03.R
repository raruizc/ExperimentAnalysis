#--------------------------------------------------------------------------------
# AN?LISE DE EXPERIMENTO 2022.2
# TERCEIRA LISTA
# RICARDO ANTONIO RUIZ CARDOZO
#--------------------------------------------------------------------------------


rm(list = ls())
# ANAVA, delineamentos básicos, pressupostos e medidas da qualidade de experimentos
library(openxlsx)
library(fBasics)
## Primeira Questao DIC

dados_01 <- read.xlsx(xlsxFile = "lista_03_dados.xlsx", sheet = "dados_1")
str(dados_01)

dados_01 <- transform(dados_01, Linhagen = as.factor(Linhagen), Rep =as.factor(Rep))

# A) Analise de Variancia 
anova.prod <- aov(Prod ~ Linhagen + Rep, data=dados_01)
anova(anova.prod) #solta o quadro da análise de variância

# B) TESTE TUKEY E SCOTT-KNOTT
#Opção 01
TukeyHSD(anova.prod, "Linhagen", ordered = TRUE)
plot(TukeyHSD(anova.prod, "Linhagen", ordered = TRUE))


#Opção 02
library(agricolae)
require(agricolae)
tk <- HSD.test(anova.prod, 'Linhagen',
               group=TRUE, alpha=0.05)

tk$groups

bar.group(tk$groups, ylim=c(0, 100),density=4, border="blue") #cOLOCAR OS LIMITES "ylim" de acordo com os seus dados

#Opção 03 
library(lsmeans)
library(multcomp)

marginal = lsmeans(anova.prod, ~ Linhagen)
(cld_v <- cld(marginal,
              alpha=0.05,
              Letters=letters,
              adjust="tukey"))
plot(cld_v, xlab = "Médias")

#Opção 04
library(ExpDes)
attach(dados_01)
rbd(treat = Linhagen, block = Rep, resp = Prod, quali = TRUE, mcomp = "tukey", sigT = 0.05, sigF = 0.05)
detach(dados_01)

##Teste de agrupamento de Scott-Knott

# Opção 01

library(ExpDes)
attach(dados_01)
rbd(treat = Linhagen, block = Rep, resp = Prod, quali = TRUE, mcomp = "sk", sigT = 0.05, sigF = 0.05)
detach(dados_01)

# Opção 02


install.packages("ScottKnott")
library(ScottKnott)
sk <- SK(x=anova.prod, which='Linhagen', sig.level = 0.05)
summary(sk)
plot(sk)
 # title='Linhagens de Soja')

#Opção 03

##Least Square Means - Scott-Knott ####

library(emmeans)
medg.i <- summary(emmeans(anova.prod, ~ Linhagen))
x.i <- cbind(medg.i[,1],medg.i[,2])
nobs <- nlevels(dados_01$Rep)
mult <- matrix(1,nobs,1)
xk.i <- kronecker(x.i,mult)
#colnames(xk.i) <- c("Genotipos","Medias")

GLe.i <- mean(medg.i$df,na.rm = T)
ve.i <- mean(medg.i$SE^2, na.rm = T)*nobs
SSe.i <- ve.i*GLe.i

#Using a function SKT - Scott-Knott at the end of this file
result <- SKT(xk.i[,2], xk.i[,1], round(GLe.i,digits=0), SSe.i, alpha = 0.05, debug=TRUE)
result$Treatments <- as.character(result$Treatments)
result$Treatments <- as.numeric(result$Treatments)
result <- result[order(result$Treatments),]

medg.i$SK <- result$Groups

#Plotting a catterpillar with grouping of SK

library(ggplot2)
ggplot(medg.i, aes(y=reorder(Linhagen,emmean), x=emmean, color = SK)) + #sort the varieties by the values of the means
  labs (x = "Produção de grãos", y = "Linhagens de Soja") +
  geom_point() +
  geom_errorbarh(aes(xmin=lower.CL,
                     xmax=upper.CL), height=0.5) +
  theme(axis.text.x = element_text(color="black"),
        axis.text.y = element_text(color="black"))
#theme(legend.position = "none") #hide the legend


#Função SKT
SKT <- function (y, trt, DFerror, SSerror, alpha = 0.05, group = TRUE, 
                 main = NULL, debug=FALSE) 
{
  sk <- function(medias, s2, dfr, prob) {
    bo <- 0
    si2 <- s2
    defr <- dfr
    parou <- 1
    np <- length(medias) - 1
    for (i in 1:np) {
      g1 <- medias[1:i]
      g2 <- medias[(i + 1):length(medias)]
      B0 <- sum(g1)^2/length(g1) + sum(g2)^2/length(g2) - 
        (sum(g1) + sum(g2))^2/length(c(g1, g2))
      if (B0 > bo) {
        bo <- B0
        parou <- i
      }
    }
    g1 <- medias[1:parou]
    g2 <- medias[(parou + 1):length(medias)]
    teste <- c(g1, g2)
    sigm2 <- (sum(teste^2) - sum(teste)^2/length(teste) + 
                defr * si2)/(length(teste) + defr)
    lamb <- pi * bo/(2 * sigm2 * (pi - 2))
    v0 <- length(teste)/(pi - 2)
    p <- pchisq(lamb, v0, lower.tail = FALSE)
    if (p < prob) {
      for (i in 1:length(g1)) {
        cat(names(g1[i]), "\n", file = "skresult", 
            append = TRUE)
      }
      cat("*", "\n", file = "skresult", 
          append = TRUE)
    }
    if (length(g1) > 1) {
      sk(g1, s2, dfr, prob)
    }
    if (length(g2) > 1) {
      sk(g2, s2, dfr, prob)
    }
  }
  
  medias <- sort(tapply(y, trt, mean), decreasing = TRUE)
  dfr <- DFerror
  rep <- tapply(y, trt, length)
  s0 <- MSerror <- SSerror/DFerror
  s2 <- s0/rep[1]
  prob <- alpha
  sk(medias, s2, dfr, prob)
  f <- names(medias)
  names(medias) <- 1:length(medias)
  resultado <- data.frame(r = 0, f = f, m = medias)
  if (file.exists("skresult") == FALSE) {
    stop
  }
  else {
    xx <- read.table("skresult")
    file.remove("skresult")
    x <- xx[[1]]
    x <- as.vector(x)
    z <- 1
    for (j in 1:length(x)) {
      if (x[j] == "*") {
        z <- z + 1
      }
      for (i in 1:length(resultado$f)) {
        if (resultado$f[i] == x[j]) {
          resultado$r[i] <- z
        }
      }
    }
  }
  letras <- letters
  if (length(resultado$r) > 26) {
    l <- floor(length(resultado$r)/26)
    for (i in 1:l) letras <- c(letras, paste(letters, i, 
                                             sep = ""))
  }
  res <- 1
  for (i in 1:(length(resultado$r) - 1)) {
    if (resultado$r[i] != resultado$r[i + 1]) {
      resultado$r[i] <- letters[res]
      res <- res + 1
      if (i == (length(resultado$r) - 1)) {
        resultado$r[i + 1] <- letters[res]
      }
    }
    else {
      resultado$r[i] <- letters[res]
      if (i == (length(resultado$r) - 1)) {
        resultado$r[i + 1] <- letters[res]
      }
    }
  }
  names(resultado) <- c("Groups", "Treatments", 
                        "Means")
  
  if (debug==TRUE) {return(resultado)}
}

####################### QUESTÃO 2 #############################

#Importing the data
dados_02 <- read.xlsx(xlsxFile = "lista_03_dados.xlsx", sheet = "dados_2")
str(dados_02)

dados_02 <- transform(dados_02, Acesso = as.factor(Acesso), Rep =as.factor(Rep))

# A) Analise de Variancia 
anova.polen <- aov(Viabilidade ~ Acesso, data=dados_02)
anova(anova.polen) #solta o quadro da análise de variância


# B) TESTE TUKEY 
#Opção 01
TukeyHSD(anova.polen, "Acesso", ordered = TRUE)
plot(TukeyHSD(anova.polen, "Acesso", ordered = TRUE))


#Opção 02
library(agricolae)
require(agricolae)


tk <- HSD.test(anova.polen, 'Acesso',
               group=TRUE, alpha=0.05)

tk$groups

bar.group(tk$groups, ylim=c(0, 250),density=4, border="pink") #cOLOCAR OS LIMITES "ylim" de acordo com os seus dados

#Opção 03 
library(lsmeans)
library(multcomp)

marginal = lsmeans(anova.polen, ~ Acesso)
(cld_v <- cld(marginal,
              alpha=0.05,
              Letters=letters,
              adjust="Tukey"))
plot(cld_v, xlab="Média")

#Opção 04
library(ExpDes)
attach(dados_02)
crd(Acesso, Viabilidade, quali = TRUE, mcomp = "tukey", sigT = 0.05, sigF = 0.05)
detach(dados_02)

####################### QUESTÃO 3 #############################

#Importing the data
dados_03 <- read.xlsx(xlsxFile = "lista_03_dados.xlsx", sheet = "dados_3")
str(dados_03)


dados_03 <- transform(dados_03, Hibrido = as.factor(Hibrido), Rep =as.factor(Rep))

# A) Analise de Variancia 
anova.alt <- aov(Alt ~ Hibrido + Rep, data=dados_03)
anova(anova.alt) #solta o quadro da análise de variância

aov.altura<- aov(altura ~ hibrido + rep, data = altura)
anova(aov.altura)

#Teste de Dunnet
library(Tratamentos.ad)

#Help na funcao para ver como funciona
?dbc.ad


#Criando uma coluna para identificar as testemunhas
#altura$Testemunha <- ifelse(altura$hibrido %in% c('HT01','HT02'), c(1,2),NA)

#Organizando na ordem que o pacote exige
altura<-altura[,c("Testemunha","hibrido","rep","altura")]

#Testes de media
dun<-dbc.ad(altura,alfa=0.05,quali = TRUE,verbose=TRUE)

#Teste de Dunnet
dun$Dunnett

#### 
install.packages("DescTools")
library(DescTools)
ScheffeTest(aov.altura)
