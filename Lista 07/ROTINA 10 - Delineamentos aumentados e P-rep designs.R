################ DBA - Delineamento em blocos aumentados ###################

################ DBA - Sorteio #############

# 50 treatments (10 replicated r=2 + 40 unreplicated) == 60 plots
## blocks of size k=30 (10 + 20)

library(agricolae)
T1<-1:10
T2<-11:50
outdesign.dau <-design.dau(T1,T2, r=2,serie=2)

# field book
book.dau <- outdesign.dau$book
by(book.dau,book.dau[2],function(x) paste(x[,1],"-",as.character(x[,3])))
install.packages("DiGGer")
library(DiGGer)
# Augmented Block Design
########################################
#Partially replicated designs, prDiGGer#
########################################
#unreplicated design with 40 unreplicated treatments and 10 replicated twice. 
#The block sequence has been chosen to match the replication level in the design.
#Layout: #plots = 60 -- 5rows by 12columns
#Each level of replication will contain 30 plots
#First blocking: The  [5x6] blocks result in 2 blocks spread across the design
#and should allow for each block to contain a #set of the 10 replicated treat.could be
#used. 
prep50k30 <- prDiGGer(numberOfTreatments = 50, 
                      rowsInDesign = 6,
                      columnsInDesign = 10,
                      blockSequence = list(c(6,5)),
                      treatRepPerRep = rep(c(1,2), c(40, 10)),
                      treatGroup = rep(c(1, 2), c(40, 10)),
                      runSearch = TRUE)
prep50k30list <- prep50k30$dlist
head(prep50k30list)
m50k30 <- getDesign(prep50k30)
desPlot(m50k30,seq(40),col=8,new=TRUE,label=FALSE)
desPlot(m50k30,seq(10)+40,col=2,new=FALSE,label=TRUE,
        bdef=cbind(6,5),bcol=4,bwd=4)

# Prep
########################################
#Partially replicated designs, prDiGGer#
########################################
#unreplicated design with 40 unreplicated treatments and 10 replicated twice. 
#The block sequence has been chosen to match the replication level in the design.
#Layout: #plots = 60 -- 6rows by 10columns
#Each level of replication will contain 30 plots
#First blocking: The  [6x5] blocks result in 2 blocks spread across the design
#and should allow for each block to contain a #set of the 10 replicated treat.could be
#used. 
#Second blocking: The sub-blocks of  [1x5] will encourage replicated
#entries to be evenly placed within columns of the design.

prep50k5 <- prDiGGer(numberOfTreatments = 50, 
                     rowsInDesign = 6,
                     columnsInDesign = 10,
                     blockSequence = list(c(6,5),c(1,5)),
                     treatRepPerRep = rep(c(1,2), c(40, 10)),
                     treatGroup = rep(c(1, 2), c(40, 10)),
                     runSearch = TRUE)
prep50k5list <- prep50k5$dlist
head(prep50k5list)
prep50k5list <- prep50k5list[c(-2,-3,-6)]
prep50k5list$BLOCK <- c(rep(1:6,times=5),rep(7:12,times=5))

#prep50k5list$y <- rnorm(60,16,4)

m50k5 <- getDesign(prep50k5)
desPlot(m50k5,seq(40),col=8,new=TRUE,label=FALSE)
desPlot(m50k5,seq(10)+40,col=2,new=FALSE,label=TRUE,
        bdef=cbind(1,5),bcol=4,bwd=4)



library(agricolae)
rm(list = ls(all=T))
DBA <- read.table("Rotina10.dba.txt",header=T)
DBA <- transform(DBA,bloco = factor(bloco), trat = factor(trat))

head(DBA)
str(DBA)

bloco <- factor(DBA$bloco)
trat <- factor(DBA$trat)
yield <- DBA$prod 

##An�lise intrablocos via DAU.test

library(agricolae)
library(MASS)
model <- DAU.test(bloco,trat,yield, method = "tukey", console=TRUE)
model$means

#Analise intrablocos via lm ou aov
lm.DBA <- lm(prod ~ bloco + trat, data=DBA)
anova(lm.DBA)
lsmeans::lsmeans(lm.DBA, ~trat)

#Analise intrablocos das testemunhas
lmt.DBA <- lm(prod ~ bloco + trat, data=DBA[DBA$tipo=="teste",])
anova(lmt.DBA)

#Adaptado da Tese do Jo�o Batista Duarte (Prof. UFG)
#An�lise intrablocos com a decomposi��o de trat = TR + teste + TR vs teste
#vari�veis auxiliares
DBA$C <- factor(ifelse(DBA$tipo == "TR",0,DBA$trat))
DBA$X <- factor(ifelse(DBA$tipo == "teste",0,DBA$trat))
head(DBA)

lm1.dba <- lm(terms(prod ~ bloco + C + X%in%C, keep.order = TRUE), # modelo
              data = DBA, contrasts = list(C = contr.sum, X = contr.sum, bloco = contr.sum))
anova(lm1.dba)

#refere-se a saida da fun��o DAU.test - ANOVA2
lm2.dba <- lm(terms(prod ~ C + X%in%C + bloco , keep.order = TRUE), # modelo
              data = DBA, contrasts = list(C = contr.sum, X = contr.sum, bloco = contr.sum))
anova(lm2.dba)


library(lme4)
#Ambos tr e tc aleatorios
lmer.dba <- lmer(prod ~ bloco + (1|trat) , data=DBA)
summary(lmer.dba)

#Help Julio Bueno - SEP of BLUPs via lme4
blups <- ranef(lmer.dba, condVar = TRUE, drop=TRUE)
str(blups)
blups.trat <- blups$trat
blups.PEV <- attr(blups.trat,"postVar")
blups.SEP <- sqrt(blups.PEV)
t.blups <- blups.trat/blups.SEP

cbind(blups.trat,blups.SEP,t.blups)

#To produce a (list of) "caterpillar plots" of the random effects
blups <- ranef(lmer.dba, condVar = TRUE) #usando o argumento drop n�o plotou
lattice::dotplot(blups)

# tr aleatorios e tc fixos
DBA$X1 <- factor(ifelse(DBA$tipo == "TR",1,0))
head(DBA)

lmer.dba1 <- lmer(prod ~ bloco + C + (1|trat:X1) , data=DBA)
summary(lmer.dba1)
ranef(lmer.dba1)

#Prep
rm(list = ls(all=T))
prep <- read.table("Rotina10.prepG.txt",header=T)
head(prep)
prep <- transform(prep,trt = factor(trt), block = factor(block))

#Analise intrablocos via lm ou aov

lm.prep <- lm(y ~ block + trt, data=prep)
anova(lm.prep)
lsmeans::lsmeans(lm.prep, ~trt)