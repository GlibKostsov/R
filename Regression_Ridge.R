#####REGRESSION RIDGE

install.packages("MASS")
ibrary(MASS)


head(trees)


##Traitement des donnees 

#Extraction du sample a etudier
set.seed(111)
indexTrees<-sample(31,25)
dataTrees<-trees[indexTrees,]
n<-nrow(dataTrees)
lambda<-0.5

X<-as.matrix(cbind(rep(1,n),dataTrees$Girth,dataTrees$Height))
Volume<-as.matrix(dataTrees$Volume)

A<-t(X)%*%X+n*lambda*diag(rep(1,3))
b<-t(X)%*%Volume

###Resolution exact d'une systeme lineaaire

solve(A,b)


##Fonction a minimiser
Y<-Volume
J(X,Volume)
J<-function(X,Y,Theta){
  n<-nrow(Y)
  J<-1/(2*n)*t(X%*%Theta-Y)%*%(X%*%Theta-Y)+(lambda/2)*t(Theta)%*%Theta
  return(J)
}


###Resolution Descente de gradient 
I<-as.matrix(rep(1,length(Theta)))
pas<-0.00001
Theta<-as.matrix(c(rep(0,3)))

#Stat<-as.data.frame(matrix(vector(), 0, 3, dimnames=list(c(), c("J","Err","Comp"))))
Erreur<-1
Compteur<-0
Erreurs<-c()
Js<-c()

while(Erreur>0.0001){
  Compteur<-Compteur+1
  Jderiv<- (t(X)%*%X%*%Theta -t(X)%*%Volume)+ n*lambda*Theta
  tempTheta<-Theta - pas*Jderiv
  
  Erreur<-t(Jderiv)%*%Jderiv
  Erreurs[Compteur]<-Erreur
  Js[Compteur]<-J(X,Volume,Theta)
  Theta<-tempTheta
  
}


#J en fonction de nomre d'iterations 
plot(1:length(Js),Js,type = 'l')

###Resolution package GLMNET (descente de gradient conjugue)

install.packages("glmnet")
library(glmnet)



