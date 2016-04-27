

#Regressio Lasso+Validation Croisee


dataTrees<-trees

ValidationErreur<-function(lambda){
  T<-0
  for(v in 1:nrow(dataTrees)){
    print(v)
    dataTreesExclusion<-dataTrees[-v,]
    X<-as.matrix(cbind(rep(1,n-1),dataTreesExclusion$Girth,dataTreesExclusion$Height))
    Volume<-as.matrix(dataTreesExclusion$Volume)
    b<-t(X)%*%Volume
    A<-t(X)%*%X+(n-1)*lambda*diag(rep(1,3))
    
    Theta<-solve(A,b)
    
    dataTreesValidation<-dataTrees[v,]
    x<-as.matrix(cbind(1,dataTreesValidation$Girth,dataTreesValidation$Height))
    T<-T+(dataTreesValidation$Volume-x%*%Theta)^2
  }
  return(T)
}

lambda<-optimize(ValidationErreur,lower = 5*10^-5,upper = 4*10^-4)

X<-as.matrix(cbind(rep(1,n),dataTrees$Girth,dataTrees$Height))
Volume<-as.matrix(dataTrees$Volume)
Girth<-as.matrix(dataTrees$Girth)
Height<-as.matrix(dataTrees$Height)


Theta<-as.matrix(rep(0,3))
Err<-1
compteur<-0
pas<-0.00001

while(Err>10^-3){
  compteur<-compteur+1
  Jder<- t(X)%*%X%*%Theta-t(X)%*%Volume+n*lambda$minimum*Theta
  Err<-t(Jder)%*%Jder
  Theta<-Theta-Jder*pas
  if(compteur %% 10000 == 0 ){print(Err)}
}


reg<-lm(Volume~Girth+Height)

#Comparaison des resultats avec fonction lm

t(Theta)

reg$coefficients





