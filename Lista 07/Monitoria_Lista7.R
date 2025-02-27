#Monitoria lista 7


library(FielDHub)

??FieldDHub
run_app()

set.seed(2022.11)

#Com agricolae
library(agricolae)
T1<-c("A","B","C","D", "E")
T2<-1:100
outdesign.dau <-design.dau(T1,T2, r=10,serie=2)

# field book
book.dau <- outdesign.dau$book
by(book.dau,book.dau[2],function(x) paste(x[,1],"-",as.character(x[,3])))
library(dplyr)
arrange(book.dau)
print(book.dau %>% group_by(trt))
##### 3ª Opcao
library('dae')
croqui<- function(book, linha, coluna, bloco, disp){
  
  mat<-array(book$trt,dim=c(linha,coluna,bloco)) #Array com i linhas j colunas em k dimensões (k blocos)
  par(mfrow=disp,mai=c(0.005,0.005,0.005,0.005)) 
  for (i in 1:bloco){
    designPlot(array(sample(mat[,,i],length(mat[,,i])), dim=c(linha,coluna)))
  }       
}

croqui(book.dau,linha = 3,coluna =5, bloco = 10, disp = c(1,10))


#Questao 2
library(car)

trigo<- read.csv("q_2.csv",sep=';')
str(trigo)

trigo<-transform(trigo, bloco = factor(bloco), trat = factor(trat))
anova.trigo<- Anova(aov(prod ~ bloco + trat,data= trigo), Type="II")

#Adaptado da Tese do Jo�o Batista Duarte (Prof. UFG)
#An�lise intrablocos com a decomposi��o de trat = TR + teste + TR vs teste
#vari�veis auxiliares
trigo$C <- factor(ifelse(trigo$tipo == "test",0,as.character(trigo$trat)))
trigo$X <- factor(ifelse(trigo$tipo == "trat",0,as.character(trigo$trat)))
trigo$X1 <- factor(ifelse(trigo$tipo == "trat",0,1))
head(trigo)

#gl testemunha --> 3-1 =2
#gl tratamento --> 30-1 =29
#gl comparacao (contraste) --> 2-1 = 1

aov.cult.decomp<- aov(prod ~ bloco + X1 + C + X:C, data=trigo)
anova.decomp<-anova(aov.cult.decomp)

#Soma de quadrados dos tratamentos juntos
anova.trigo$`Sum Sq`[2]

#Soma de quadrados separados
sum(anova.decomp$`Sum Sq`[2:4])

###Comparacao
med.aj<-data.frame(emmeans(aov(prod ~ bloco + trat,data= trigo), ~ trat))
med.aj

med.ord<-data.frame(tapply(trigo$prod, trigo$trat, mean))
med.ord$trat <-rownames(med.ord)
names(med.ord)[1]<- 'media ord'

print(med.aj %>% group_by(trat))

med.comp<-merge(med.aj,med.ord)


df.comp<- data.frame(Tratamento = rep(med.comp$trat,2),
                     Media = c(med.comp$emmean,med.comp$`media ord`),
                     Tipo = rep(c("Media Ajustada","Media Ordinaria"),c(33,33))
                     )

ggplot(df.comp, aes( x=Tipo, y = Media, group = Tratamento))+
  geom_line(aes(color= Tratamento),size=1.5)



set.seed(2022.12)

#Com agricolae
library(agricolae)
T1<-1:60
T2<-61:180
outdesign.dau <-design.dau(T1,T2, r=3,serie=2)

# field book
book.dau <- outdesign.dau$book
by(book.dau,book.dau[2],function(x) paste(x[,1],"-",as.character(x[,3])))
library(dplyr)
arrange(book.dau)
print(book.dau %>% group_by(trt))
##### 3ª Opcao
library('dae')
croqui<- function(book, linha, coluna, bloco, disp){
  
  mat<-array(book$trt,dim=c(linha,coluna,bloco)) #Array com i linhas j colunas em k dimensões (k blocos)
  par(mfrow=disp,mai=c(0.005,0.005,0.005,0.005)) 
  for (i in 1:bloco){
    designPlot(array(sample(mat[,,i],length(mat[,,i])), dim=c(linha,coluna)))
  }       
}
??croqui
croqui(book.dau,linha = 30,coluna =4, bloco = 3, disp = c(1,3))



