coords=expand.grid(seq(0,1,length=12),seq(0,1,length=14))

library(fields)
library(mvnfast)

  Cor_mat=Exponential(rdist(coords),range=1)
  
  Z=rmvn(n=11000,mu=rep(0,nrow(Cor_mat)),sigma=Cor_mat)
  
  X<-array(dim=c(11000,12,14,10))
  for(i in 1:10){
    Cor_mat=Exponential(rdist(coords),range=runif(1,0.5,1.5))
    
    Z=rmvn(n=11000,mu=rep(0,nrow(Cor_mat)),sigma=Cor_mat)
    dim(Z)=c(11000,12,14)
    
    X[,,,i]=Z
  }
image(X[1,,,1])

m=Z

for(i in 1:11000){
  for(j in 2:11){
    for(k in 2:13){
      m[i,j,k] <- mean(2+exp(-4+X[i,(j-1):(j+1),(k-1):(k+1),2]+X[i,(j-1):(j+1),(k-1):(k+1),3]-X[i,(j-1):(j+1),(k-1):(k+1),3])+cos(X[i,(j-1):(j+1),(k-1):(k+1),1]+X[i,(j-1):(j+1),(k-1):(k+1),5]-X[i,(j-1):(j+1),(k-1):(k+1),4])-
        sin(X[i,(j-1):(j+1),(k-1):(k+1),6]-X[i,(j-1):(j+1),(k-1):(k+1),1])*(X[i,(j-1):(j+1),(k-1):(k+1),8])-sqrt(X[i,(j-1):(j+1),(k-1):(k+1),2]^2+X[i,(j-1):(j+1),(k-1):(k+1),5]^2+X[i,(j-1):(j+1),(k-1):(k+1),7]^2+X[i,(j-1):(j+1),(k-1):(k+1),1]^2)+
        exp(-4+X[i,(j-1):(j+1),(k-1):(k+1),10]+X[i,(j-1):(j+1),(k-1):(k+1),3]-X[i,(j-1):(j+1),(k-1):(k+1),7])+cos(X[i,(j-1):(j+1),(k-1):(k+1),9]+X[i,(j-1):(j+1),(k-1):(k+1),1]-X[i,(j-1):(j+1),(k-1):(k+1),8])-
        sin(X[i,(j-1):(j+1),(k-1):(k+1),8]-X[i,(j-1):(j+1),(k-1):(k+1),2])*(X[i,(j-1):(j+1),(k-1):(k+1),3])-sqrt(X[i,(j-1):(j+1),(k-1):(k+1),9]^2+X[i,(j-1):(j+1),(k-1):(k+1),3]^2+X[i,(j-1):(j+1),(k-1):(k+1),7]^2+X[i,(j-1):(j+1),(k-1):(k+1),10]^2)
      )
    }
  }
}

m<-m[,-c(1,12),-c(1,14)]
X<-X[,-c(1,12),-c(1,14),]
#Simulate Gaussian data with mean equal to m
Y <- apply(exp(m)/(1+exp(m)),1:3,function(x) rbinom(1,1,x))
par(mfrow=c(1,2))
hist(exp(m)/(1+exp(m)))
hist(Y)

Y_train<-Y[1:10000,,]
X_train<-X[1:10000,,,]

Y_test<-Y[10001:11000,,]
X_test<-X[10001:11000,,,]

save(Y_train,X_train,file="logistic_train_df.Rdata")


save(Y_test,X_test,file="logistic_test_df.Rdata")

Y <- apply(m,1:3,function(x) rnorm(1,mean=x[1],sd=2))
hist(Y)

Y_train<-Y[1:10000,,]
X_train<-X[1:10000,,,]

Y_test<-Y[10001:11000,,]
X_test<-X[10001:11000,,,]

save(Y_train,X_train,file="prediction_train_df.Rdata")


save(Y_test,X_test,file="prediction_test_df.Rdata")

library(evd)
Y <- apply(m,1:3,function(x) rgamma(1,1,2)+rgpd(1,scale=exp(x/20),shape=0.1))
hist(Y)

Y_train<-Y[1:10000,,]
X_train<-X[1:10000,,,]

Y_test<-Y[10001:11000,,]
X_test<-X[10001:11000,,,]

save(Y_train,X_train,file="qregress_train_df.Rdata")


save(Y_test,X_test,file="qregress_test_df.Rdata")
