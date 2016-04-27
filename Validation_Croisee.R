
######VALIDATION CROISEE
##Regression Ridge

head(dataTrees)<-trees

n<-nrow(dataTrees)


#Evaluation d'erreur en fonction de lambda

EvaluationErreur<-function(lambda){
  Stat<-0
  for(v in 1:n){
    
    dataTrees<-trees[-v,]
    X<-as.matrix(cbind(rep(1,n-1),dataTrees$Girth, dataTrees$Height))
    Y<-dataTrees$Volume
    A<-t(X)%*%X+(n-1)*lambda*diag(c(1,1,1))
    B<-t(X)%*%Y
    
    theta<-solve(A,B)
    test<-trees[v,]
    
    x<-as.matrix(cbind(1,test$Girth,test$Height))
    Stat<-Stat+(test$Volume-x%*%theta)^2
  }
  return(Stat)
}

#Verifivation de la fonction

EvaluationErreur(0.00005)
EvaluationErreur(0.00006)
EvaluationErreur(0.00007)


#Plot - Erreurs en fonction de lambda

lambda<-seq(0.00005,0.0004,by = 0.00001)

T<-lapply(lambda,EvaluationErreur)

plot(lambda,T)

#Minimum de la fonction sur l'intervalle [0,1]

MinimomLambda<-optimize(EvaluationErreur,lower= 0, upper = 1)$minimum
MinimomEvaluationErreur<-optimize(EvaluationErreur,lower= 0, upper = 1)$objective

#Ajoute la point mininom sur le plot
points(MinimomLambda,MinimomEvaluationErreur, col = 'red',pch=20)


