

#Logistic regresion    


install.packages("MASS")
library("MASS")

set.seed(100)
sampleIndex<-sample(200,195)
head(crabs)




learnCrabs<-crabs[sampleIndex,]
predictionCrabs<-crabs[-sampleIndex,]

logisticRegression <- glm(sex~RW+BD,data = learnCrabs,family=binomial)

#Coefficientd deregression
regCoefs<-logisticRegression$coefficients

#Prediction manuelle
1/(1+exp(-(regCoefs[]+predictionCrabs$RW*(-4.424783) + predictionCrabs$BD*3.261905)))*100

#Prediction avec function predict
pred<- predict(logisticRegression,predictionCrabs)
exp(pred)/(1+exp(pred))*100



##Probleme convergence!!!!!




##Methode de gredient

#Fonction a minimiser

J<-function(Y,X,theta){
  
  I<-matrix(rep(1,length(Y)),ncol=1)
  
  return(-1/length(Y)*(log(1/(1+exp(-t(theta)%*%X)))%*%Y +
                         log(t(I)-1/(1+exp(-t(theta)%*%X)))%*%(I-as.matrix(Y))))
  
}

#Initialisation des variables
SEX<-(crabs$sex[sampleIndex]=="M")*1
RW<-crabs$RW[sampleIndex]
BD<-crabs$BD[sampleIndex]

Xcol<-matrix(c(rep(1,length(sampleIndex)),crabs$RW[sampleIndex],crabs$BD[sampleIndex]),ncol =3)
X<-t(Xcol)
X0<-matrix(rep(1,length(sampleIndex)),ncol = 1)
X1<-matrix(crabs$RW[sampleIndex],ncol= 1)
X2<-matrix(crabs$BD[sampleIndex],ncol = 1)

theta<-matrix(rep(0,3),ncol=1)
pas<- 0.0001

erreur<-1
Jcal<-c()
i<-0

while(erreur>0.00001){
  i=i+1
  
  derivJ <- matrix(c((1/length(sampleIndex))*((1/(1+exp(-t(theta)%*%X))-SEX)%*%X0),
                     (1/length(sampleIndex))*(1/(1+exp(-t(theta)%*%X))-SEX)%*%X1,
                     (1/length(sampleIndex))*(1/(1+exp(-t(theta)%*%X))-SEX)%*%X2), nrow=1 ,ncol = 3)
  
  theta1<- t(theta)-pas*derivJ
  
  theta<-t(theta1)
  
  erreur<-sqrt(derivJ%*%t(derivJ))  
  J(SEX,X,theta)
}


#Prediction manuelle
1/(1+exp(-(theta[1]+predictionCrabs$RW*theta[2] + predictionCrabs$BD*theta[3])))*100

#Plot J/nombre iteratioins
plot(c(Jcal[1:1000]))
