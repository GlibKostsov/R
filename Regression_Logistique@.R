
#Descent du gradient 


install.packages("MASS")
library(MASS)

#Echantillone d'apprentisage 50 crabs

set.seed(111)

#Nomre d'obsevations
n<-nrow(crabs)
indexAprentCrabs<-sample(n,50)

dataCrabs<-crabs[indexAprentCrabs,c("sex","RW","BD")]


SEX<- as.matrix((dataCrabs$sex =="M")*2-1)
RW<-as.matrix(dataCrabs$RW)
BD<-as.matrix(dataCrabs$BD)
X<-as.matrix(cbind(rep(1,nrow(SEX)),RW,BD))


#Fonction a minimiser
Jfun<-function(X,Y,Theta){
  J<-1/n*(log(1+exp(t(SEX)%*%(X%*%Theta))))
  return(J)
}

pas<-c( min(eigen(t(X)%*%X)$values)/n,
        0.00001,
        0.001)

#pas<-min(eigen(t(X)%*%X)$values)/n
#pas<-0.001
 #Descent pour les pas differents pas[1] - optimal
for(i in pas)
  
  Theta<-as.matrix(rep(0,3))
  Err<-1
  compteur<-0
  while(Err>10^-4){
    compteur<-compteur+1
    Jder<- -1/n*as.matrix(c( (X[,1]%*%SEX)/(1+exp(t(SEX)%*%(X%*%Theta))) ,
                             (X[,2]%*%SEX)/(1+exp(t(SEX)%*%(X%*%Theta))) ,
                             (X[,3]%*%SEX)/(1+exp(t(SEX)%*%(X%*%Theta)))
                                                                          ))
    tempTheta<-Theta-i*Jder
    Err<-sqrt(t(Jder)%*%Jder)
    
    
    Theta<-tempTheta
   
    #Test imprime les valeurs de fonction a minimiser et erreur 
   if(compteur %% 100000 == 0) {print(c(Err,Jfun(X,SEX,Theta)))}
  }
  
  print(c(counter,Err,Jfun(X,SEX,Theta,i)))
  
}

