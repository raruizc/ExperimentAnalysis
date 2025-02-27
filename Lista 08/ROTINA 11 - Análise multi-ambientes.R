rm(list = ls(all=T)) #Remover todos os objetos

setwd("D:/UFLA/Disciplinas/Pos-Graduacao/Analise de Experimentos em Genetica e Melhoramento de Plantas/Semestre 2021.2/Rotinas R")

#### ENSAIOS MULTI-AMBIENTES ####

dados <- read.table("Rotina11.feijao.txt",header=T)
head(dados)
str(dados)
dados <- transform(dados,local = factor(local), bloco = factor(bloco),linhagem = factor(linhagem))
str(dados)

####An�lises individuais (por local)####

####Op��o 01####
dados.loc1 <- subset(dados,local==1) # O arquivo conter� apenas os dados referentes ao local 1
dados.loc2 <- subset(dados,local==2) # O arquivo conter� apenas os dados referentes ao local 2

anova(aov(producao ~ bloco + linhagem, data = dados.loc1))
anova(aov(producao ~ bloco + linhagem, data = dados.loc2))

####Op��o 02####
anova(aov(producao ~ bloco + linhagem, data = dados[dados$local==1,]))
anova(aov(producao ~ bloco + linhagem, data = dados[dados$local==2,]))

####Op��o 03####
#Usando a fun��o by
teste <- with (dados,
               by(dados,local, function(x) anova(aov(producao ~ bloco + linhagem, data = x))))

#Teste is a list
teste

names(teste)
teste[['1']]

names(teste$"1")
QME1 <- teste$"1"["Residuals","Mean Sq"];QME1
QME2 <- teste$"2"["Residuals","Mean Sq"];QME2

GLE <- teste$"1"["Residuals","Df"];GLE

QME <- c(QME1,QME2);QME

(Fmax <- max(QME)/min(QME))
(Fmaxtab <- SuppDists::qmaxFratio(p = 0.05, df = GLE, k=nlevels(dados$local), lower.tail=FALSE, log.p=FALSE))


####Op��o 04 - mais geral####

dados$local <- factor(dados$local, levels = c(1,2), labels = c("Lavras", "Sete Lagoas"))
levels(dados$local)

head(dados)
tail(dados)

#i=4
for (i in 4:ncol(dados))
{
  cat("------------------------------------------------------------------", "\n")
  cat("Anovas Individuais (Amb) - Vari�vel:", colnames(dados)[i], "\n")
  cat("------------------------------------------------------------------", "\n")
  dados.i <- dados[,c(1:3,i)]
  dados.i <- na.omit(dados.i)
  #head(dados.i)
  nomamb <- levels(droplevels(dados.i)$local)
  statsamb <- matrix(0, nlevels(droplevels(dados.i)$local),6)
  rownames(statsamb) <- levels(droplevels(dados.i)$local)
  colnames(statsamb) <- c("QME", "CVe", "Acuracia", "Fc Genot", "Pr > Fc", "Shapiro_P-value")
  
  #j=1
  for (j in 1:nlevels(droplevels(dados.i)$local))
  {
    cat("Anova - Vari�vel:", nomamb[j], colnames(dados)[i], "\n")
    lm.i <- lm(dados.i[dados.i$local==nomamb[j],ncol(dados.i)] ~ bloco + linhagem, data=dados.i[dados.i$local==nomamb[j],])
    anova.i <- anova(lm.i)
    GLe.i <- anova.i$Df[3]
    
    statsamb[nomamb[j],1] <- anova.i$'Mean Sq'[3]
    statsamb[nomamb[j],2] <- sqrt(statsamb[nomamb[j],1])/mean(dados.i[dados.i$local==nomamb[j],ncol(dados.i)])*100
    statsamb[nomamb[j],3] <- sqrt(1-1/anova.i$'F value'[2])*100
    statsamb[nomamb[j],4] <- anova.i$'F value'[2]
    statsamb[nomamb[j],5] <- anova.i$'Pr(>F)'[2]
    
    resid <- residuals(lm.i)
    statsamb[nomamb[j],6] <- shapiro.test(resid)$ p.value
    
    print(qqnorm(resid, main = paste("Normal Q-Q Plot of" , nomamb[j], colnames(dados)[i])))
    
    #identify(qqnorm(resid, main = paste("Normal Q-Q Plot of" , nomamb[j], colnames(dados)[i])))
    #dados.ij <- dados.i[dados.i$local==nomamb[j],]
    #dados.ij[c(70),]
  }
  
  print(statsamb)
  cat("Teste de Hartley - Vari�vel:", colnames(dados)[i], "\n")
  print("Fmax"); print(Fmax <- max(statsamb[,1])/min(statsamb[,1]))
  print("P > Fmax"); print(Pvalor <- SuppDists::pmaxFratio(q = Fmax, df = GLe.i, k=nlevels(droplevels(dados.i)$local), lower.tail=F, log.p=FALSE))
  if (Pvalor < 0.05) print("Vari�ncias residuais heterog�neas - Modelo heteroced�stico � indicado") else print("Vari�ncias residuais homog�neas")
  
  statsamb <- data.frame(statsamb)
  statsamb$local <- factor(rownames(statsamb))
  statsamb$lPvalue <- -log(statsamb$Pr...Fc)
  #Calculo do fator de equival�ncia das escalas para o segundo eixo y
  fc <- max(statsamb$Acuracia)/max(statsamb$lPvalue) #usando -log natural do P-value
  #str(statsamb)
  
  require(ggplot2) 
  
  #usando -log natural do P-value
  #  tiff(filename = paste("AOV_Amb_",colnames(dados)[i],".tiff",sep = ""), width = 5, height = 7, units = "in", res = 300, compression = "lzw")
  
  print(ggplot(statsamb, aes(x = local)) +
          geom_col(aes(y = Acuracia), color="lightblue", fill="lightblue") +
          geom_col(aes(y = CVe), color="green", fill="green") +
          geom_line(aes(y = lPvalue*fc), group=1, stat="identity", color="darkblue") +
          geom_point(aes(y = lPvalue*fc),color="darkblue", size=2) +
          scale_y_continuous( # Features of the first axis
            name = "%",
            # Add a second axis and specify its features
            sec.axis = sec_axis(~./fc, name = "-log(P-value)")) +
          geom_hline(yintercept = fc*3, linetype="dashed", 
                     color = "darkblue", size=0.5) +
          theme_minimal() +
          theme(axis.text.y.right = element_text(color = "darkblue"), 
                axis.title.y.right = element_text(color = "darkblue"),
                legend.position="bottom"))
  
  #  dev.off()
  
}



####An�lise Multi-ambientes: An�lise Conjunta####

####Op��o 01####
aov.conj <- aov(producao ~ local/bloco + linhagem + local:linhagem, data = dados)
# A sintaxe "local/bloco" significa local + bloco dentro de local e a "local:linhagem" representa a intera��o
# entre esses fatores
anova(aov.conj)

aov2.conj <- aov(terms(producao ~ local + bloco%in%local + linhagem + local:linhagem, keep.order=T), data=dados)
# A fun��o "terms" � utilizada para que o R realize o ajuste da SQ de acordo com a sequ�ncia apresentada no modelo
anova(aov2.conj)

aov3.conj <- aov(producao ~ local*linhagem + bloco%in%local, data = dados)
anova(aov3.conj)
#local*linhagem significa local + linhagem + local:linhagem

###Op��o 02####
library(emmeans)
library(multcomp)
library(ggplot2)
library(tidyr)
library(tidyverse)

#i=4
nom_traits <- c("producao (kg/ha)")
for (i in 4:ncol(dados))
{
  cat("------------------------------------------------------------------", "\n")
  cat("Anovas multiambientes - Vari�vel:", colnames(dados)[i], "\n")
  cat("------------------------------------------------------------------", "\n")
  dados.i <- dados[,c(1:3,i)]
  colnames(dados.i)[ncol(dados.i)] <- "y"
  dados.i <- na.omit(dados.i)
  
  gls.y <- lm(y ~ local + bloco:local + linhagem + linhagem:local, 
              data=dados.i)
  
  print(anova.i <- data.frame(anova(gls.y)))
  
  ##Least Square Means - Scott-Knott ####
  ### Main effect of Lines ####
  medg.i <- summary(emmeans(gls.y, ~linhagem))
  x.i <- cbind(medg.i[,1],medg.i[,2])
  nobs <- nlevels(droplevels(dados.i)$local)*nlevels(droplevels(dados.i)$bloco)
  mult <- matrix(1,nobs,1)
  xk.i <- kronecker(x.i,mult)
  #colnames(xk.i) <- c("Genotipos","Medias")
  
  GLe.i <- mean(medg.i$df,na.rm = T)
  ve.i <- mean(medg.i$SE^2, na.rm = T)*nobs
  SSe.i <- ve.i*GLe.i
  
  #Using a function SK - Scott-Knott at the end of this file
  result <- SK(xk.i[,2], xk.i[,1], round(GLe.i,digits=0), SSe.i, alpha = 0.05, debug=TRUE)
  result$Treatments <- as.character(result$Treatments)
  result$Treatments <- as.numeric(result$Treatments)
  result <- result[order(result$Treatments),]
  
  medg.i$SK <- result$Groups
  
  print(ggplot(medg.i, aes(y=reorder(linhagem,emmean), x=emmean, color = SK)) + #sort the hybrids by the values of the BLUPs
          labs (x = nom_traits[i-3], y = "Linhagens") +
          geom_point() +
          geom_vline(xintercept = mean(dados.i$y, na.rm = T), col = "red") +
          geom_errorbarh(aes(xmin=lower.CL,
                             xmax=upper.CL), height=0.5) +
          theme(axis.text.x = element_text(color="black"),
                axis.text.y = element_text(color="black")) +
          theme(legend.position = "none")) #hide the legend
  #dev.off()
  
  ### G x E means - Linhagens x Locais ####
  medge.i <- summary(emmeans(gls.y, ~ linhagem:local))
  medge.i$SK <- NA
  
  nAmb <- unique(medge.i$local)
  
  #iAmb="2"
  
  for (iAmb in nAmb)
  {
    medge.ii <- medge.i[medge.i$local==iAmb,]
    x.i <- cbind(medge.ii[,1],medge.ii[,3])
    nrep <- nlevels(droplevels(dados.i)$bloco)
    mult <- matrix(1,nrep,1)
    xk.i <- kronecker(x.i,mult)
    #colnames(xk.i) <- c("Genotipos","Medias")
    
    GLe.i <- mean(medge.ii$df,na.rm = T)
    ve.i <- mean(medge.ii$SE^2, na.rm = T)*nrep
    SSe.i <- ve.i*GLe.i
    
    #Using a function SK - Scott-Knott at the end of this file
    result <- SK(xk.i[,2], xk.i[,1], round(GLe.i,digits=0), SSe.i, alpha = 0.05, debug=TRUE)
    result$Treatments <- as.character(result$Treatments)
    result$Treatments <- as.numeric(result$Treatments)
    result <- result[order(result$Treatments),]
    
    medge.ii$SK <- result$Groups
    medge.i[medge.i$local==iAmb,] <- medge.ii
  }
  
  #    tiff(filename = paste(colnames(dados)[i],iVCU,"GE",".tiff", sep = "_"), width = 5, height = 10, units = "in", res = 300, compression = "lzw")
  
  #Plotting a catterpillar with grouping of SK - ordered for the first label of the facets
  nl <- nlevels(medge.i$linhagem)
  medge.test <- medge.i %>% 
    ungroup() %>% # As a precaution / handle in a separate .grouped_df method
    arrange(local, emmean)
  medge.test <- medge.test %>%
    mutate(linhagem=factor(linhagem, levels=medge.test$linhagem[1:nl]))
  
  #Plotting a vertical line by Amb within facets
  mean_y <- data.frame(tapply(medge.test$emmean, medge.test$local, mean))
  mean_y$local <- rownames(mean_y)
  colnames(mean_y) <- c("emmean","local")
  
  print(ggplot(medge.test, aes(y=linhagem,x=emmean, color = SK)) +
          labs (x = nom_traits[i-12], y = "linhagens") +
          geom_point() +
          geom_errorbarh(aes(y=linhagem, xmin=lower.CL,
                             xmax=upper.CL), height=0.5) +
          geom_vline(aes(xintercept = emmean), col = "red", mean_y) +
          theme(axis.text.x = element_text(color="black", size = 7),
                axis.text.y = element_text(color="black", size = 7)) +
          #scale_color_manual(values=SK_colors) +
          facet_wrap(~local, scales = "free_y" ,nrow = 5, ncol = 1, strip.position = "right") +
          theme(legend.position = "none")) #hide the legend
  #dev.off()
}

####Op��o 03 - Pacote metan####

library(metan)
citation("metan")
#Genotype-environment analysis by mixed-effect models

#random = "all"
#Modelo aleat�rio

model <- gamem_met(.data = dados,
                   env = local,
                   gen = linhagem,
                   rep = bloco,
                   resp = producao,
                   random = "all"
)

#Ajustando o modelo aleat�rio no pacote lme4
library(lme4)
fit_prod <- lmer(producao ~ (1|local) + (1|bloco:local) + (1|linhagem) + (1|linhagem:local), 
                 data=dados)
data.frame(VarCorr(fit_prod))

####Ecoval�ncia de Wricke ####
ecovalence(dados, env = local, gen = linhagem, rep = bloco, resp = producao, verbose = TRUE)


# dados <- as_tibble(dados)
# dados1 <- dados
# colnames(dados1) <- c("ENV", "REP", "GEN", "producao")
# head(dados1)
# 
# library(metan)
# model <- gamem_met(dados1,
#                    env = ENV,
#                    gen = GEN,
#                    rep = REP,
#                    resp = producao,
#                    random="env")
# get_model_data(model)
# str(data_ge)


#Fun��o customizada Scott-Knott do pacote ExpDes
SK <- function (y, trt, DFerror, SSerror, alpha = 0.05, group = TRUE, 
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



#### An�lise conjunta de esp�cies perenes - Exerc�cio 03 - Lista 07
library(openxlsx)
setwd("D:\\UFLA\\Disciplinas\\Pos-Graduacao\\Analise de Experimentos em Genetica e Melhoramento de Plantas\\Semestre 2020.2\\Listas de Exercicios")
met <- read.xlsx(xlsxFile = "DADOS-LISTA-7.xlsx", sheet = "DADOS3")
met <- transform(met,ANO = factor(ANO),BLOCO = factor(BLOCO), CLONE = factor(CLONE))
str(met)

####OP��O 1 - Via ANAVA - Modelo Fixo
aov.met <- aov(Total ~ CLONE + BLOCO + CLONE:BLOCO + ANO + BLOCO:ANO + CLONE:ANO,data = met)
summary(aov.met)
#Obs:Alguns testes F n�o est�o corretos.
#Pela E(QM) � poss�vel fazer os testes F corretos.

####OP��O 2 - Via Abordagem de modelos mistos
lmer.met <- lmer(Total ~ CLONE + BLOCO + (1|CLONE:BLOCO) + ANO + (1|BLOCO:ANO) + CLONE:ANO,data = met)
summary(lmer.met)

#ANAVA dos efeitos fixos
anova(lmer.met) # N�o mostra as signific�ncias
car::Anova(lmer.met,type = 'II', test.statistic=c("F"))

#Teste das componentes de vari�ncias
lmerTest::ranova(lmer.met)

#Exerc�cio 04 - Lista 07
library(lme4)
library(emmeans)

met2 <- read.xlsx(xlsxFile = "DADOS-LISTA-7.xlsx", sheet = "DADOS4")
met2 <- transform(met2,Safras = factor(Safras),Rep = factor(Rep), Bloco = factor(Bloco), Trat = factor(Trat))
str(met2)

#An�lises individuais
means.aj <- matrix(0,nlevels(met2$Trat),nlevels(met2$Safras)+1)
colnames(means.aj) <- c("Trat", levels(droplevels(met2)$Safras))
means.aj[,'Trat'] <- 1:16

QME.ef <- matrix(0,1,nlevels(met2$Safras))
colnames(QME.ef) <- levels(droplevels(met2)$Safras)
#j=1
for (j in 1:nlevels(droplevels(met2)$Safras))
{
  cat("Anova Individual - Vari�vel:", nomamb[j], colnames(met2)[ncol(met2)], "\n")
  lmer.i <- lmer(met2[met2$Safras==nomamb[j],ncol(met2)] ~ Rep + (1|Rep:Bloco) + Trat, data=met2[met2$Safras==nomamb[j],])
  anova.i <- car::Anova(lmer.i,type = 'II', test.statistic=c("F"))
  summary(lmer.i)
  print(QME.Intra <- sigma(lmer.i)^2)
  med.i <- emmeans(lmer.i, "Trat")
  means.aj[,j+1] <- summary(med.i)[,2]
  
  SE1 <- summary(pairs(med.i))$SE[1]
  SE2 <- summary(pairs(med.i))$SE[7]
  Average <- (12*SE1^2 + 3*SE2^2)/(12+3)
  QME.ef[,j] <- nlevels(met2$Rep)*Average/2
}

#An�lise conjunta das m�dias
means.aj <- data.frame(means.aj)
library(tidyr)
my_data2 <- gather(means.aj,
                   key = "Safras",
                   value = "Peso",
                   -Trat)

str(my_data2)
my_data2 <- transform(my_data2, Trat = factor(Trat), Safras = factor(Safras))

anova <- anova(aov(Peso ~ Safras + Trat, data = my_data2))

#An�lise combinada
str(anova)
anova <- dplyr::add_row(anova, `Mean Sq` = mean(QME.ef))
rownames(anova) <- c("Safras","Trat","S x T", "Residuals")
anova["Safras",'F value'] <- anova["Safras",'Mean Sq']/anova["S x T",'Mean Sq']
anova["Trat",'F value'] <- anova["Trat",'Mean Sq']/anova["S x T",'Mean Sq']
anova["S x T",'F value'] <- anova["S x T",'Mean Sq']/anova["Residuals",'Mean Sq']

#Para continuar...
anova["Safras",'Pr(>F)'] <- anova["Safras",'Mean Sq']/anova["S x T",'Mean Sq']
anova["Trat",'Pr(>F)'] <- anova["Trat",'Mean Sq']/anova["S x T",'Mean Sq']
anova["S x T",'Pr(>F)'] <- anova["S x T",'Mean Sq']/anova["Residuals",'Mean Sq']