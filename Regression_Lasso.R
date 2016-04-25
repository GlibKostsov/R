#####REGRESSION LASSO

install.packages("MASS")
library(MASS)



#Echantillon d'apprentisage
set.seed(111)
indexTress<-sample(31,25)

#Traitement des donnees
dataTrees<-trees[indexTrees,]
X<-as.matrix(cbind(rep(1,n),dataTrees$Girth, dataTrees$Height))
Volume<-as.matrix(dataTrees$Volume)
Theta<-as.matrix(rep(0,3))

#Fonction a minimiser

J<-function(X,Y,Theta,lambda){
  I<-as.matrix(rep(1,length(Theta)))
  n<-ifelse(is.null(nrow(Y)),1,nrow(Y))
  J<- 1/(2*n)*(t(X%*%Theta-Y)%*%(X%*%Theta-Y))+t(lambda*I)%*%abs(Theta)
  return(J)
}

#Verification J dimension 1
J(1,2,0,0.5)


#dimension 1 Slp fonction (argmin J/Theta)

Slam<-function(Y,lambda){
  
  ThetaEtoile<-sign(Y)*max(abs(Y)-labbda,0)
  return(ThetaEtoile)
}



##Test1 - concergence
X<-1
Y<-11
lambda<-3
Theta<-0

Thetas<-seq(0,10,by = 0.01)
Counter<-0
Js<-c()
for(i in Thetas){
  Counter<-Counter+1
  Js[Counter]<-J(X,Y,i,lambda)
}

plot(Thetas,Js,type = 'l')
points(Thetas[which(Js==min(Js))],min(Js),, col = 'red',pch=10)

##Test2 - non convergence
X<-1
Y<-2
lambda<-3
Theta<-0

Thetas<-seq(-10,10,by = 0.01)
Counter<-0
Js<-c()
for(i in Thetas){
  Counter<-Counter+1
  Js[Counter]<-J(X,Y,i,lambda)
}

plot(Thetas,Js,type = 'l')
points(Thetas[which(Js==min(Js))],min(Js),, col = 'red',pch=10)

##Test gradient dimension 1 avec fonction Slam
#Initialisation des variables 

Compteur<-0
pas<-0.001
Erreur<- 1

while(Erreur>0.001){
  Compteur<-Compteur+1
  Jderiv<-Theta-Y+lambda*sign(Theta)
  tempTheta<-Theta-pas*Jderiv
  Erreur<-Jderiv^2

  Theta<-tempTheta
}



###############################################

###Gradient dimensions >1 avec la fonction Slam



#Fonction a minimiser
Jfunc<-function(X,Y,Theta,lambda){
  
  J<-(1/2*n)*(t(X%*%Theta-Volume)%*%(X%*%Theta-Volume))+sum(lambda/2*(abs(Theta)))
  return(J)
}



Slam<-function(z,lambda,L){
  d<-length(z)
  res<-matrix(0,d,1)
  for (i in 1:d){
    res[i] <-sign(z[i])*max(abs(z[i])-lambda/L,0)
  }
  return(res)
  
}

z<-as.matrix(c(2,4,-10))

Slam<-function(z,lambda,L){
    res <-(abs(z)-lambda/L)
    res<-replace(res,which(res<0),0)
  return(sign(z)*res)
}
?replace()

L<-max(eigen(t(X)%*%X)$values)


pas<-1/L

Theta<-as.matrix(c(0,0,0)

lambda<-0.5

Erreur<-1

counter<-0
while(Erreur>0.0001){
  
  
  counter<-counter+1
  Jderiv<- (t(X)%*%X%*%Theta-t(X)%*%Volume)
  tempTheta<-Theta-pas*Jderiv
  Theta<- Slam(tempTheta,pas*lambda,L)
  
  Erreur<-t(Jderiv)%*%Jderiv

  if (counter %% 50000==0) {print(c(Jfunc(X,Volume,Theta,lambda),Erreur,Theta)) }
  
}



#Verification avec glmnet
reg<-glmnet(t(X)%*%X,t(X)%*%Volume,family = "gaussian",alpha =1,intercept = TRUE, lambda = lambda, standardize = FALSE)


#Coeficiets de regression glmnet
reg$beta

#Coeficients de regression par descent du gradient
Theta
