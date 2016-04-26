
data(trees)
n=31
v<-1
lambda<-0.5
erreurlambda=function(lambda){
  T=0
  for(v in 1:n){
    donnees<-trees[-v,]
    x<-as.matrix(cbind(rep(1,(n-1)),donnees$Girth,donnees$Height))
    y<-as.matrix(donnees$Volume)
    A=t(x)%*%x+(n-1)*lambda*diag(c(1,1,1))
    B=t(x)%*%y
    theta=solve(A,B)
    test<-trees[v,]
    x<-as.matrix(cbind(1,test$Girth,test$Height))
    T<-T+(test$Volume-x%*%theta)^2
  }
  
  
  
  return(T)
}
lambda=seq(0.00005,0.0004,length=50)
plot(lambda,sapply(lambda,erreurlambda))