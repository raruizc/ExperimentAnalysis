##################################
#####  Ecovalency function  ######
##################################
##         Integrantes         ##
##       Bruna L Carvalho      ##
##     Cinthia S Rodrigues     ##
##     Scheila R Guilherme     ##
##################################

# Y: matrix of means (Genotypes in rows, environment in columns)
# MSE: Mean Square of Error
# r: number of repetitions
# t: number of treatments
# k: number of environments
# int.effet:  "FIX" if genotype/environment is considered of fix effect. 
#             "RANDOM" if genotype/environment is considered of random effect
#eco.enc: FALSE if genotype ecovalency is required
#         TRUE if environment ecovalency is required

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

##### Exemplo #####

#Especificar o diretorio de trabalho
setwd("D:/UFLA/Disciplinas/Pos-Graduacao/Emprego de softwares Gen�tica/Fun��es R")
x <- read.table("ex_dados.txt",header=T)
Y <- x[,-1]

MSE <- 181416
k=10
t=9
r=6

ecovalency (Y , MSE, r, t, k, int.effect="FIX",eco.env=F) 
#ecovalency (Y , MSE, r, t, k, int.effect="FIX",eco.env=T)