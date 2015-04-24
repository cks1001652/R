library(mvtnorm)
#orignial data
x <- matrix(NA,12,5)
x[,1]=1
x[,2]=c(1,0,1,0,1,0,-1,0,-1,0,-1,0)
x[,3]=c(-1,0,0,1,0,1,1,0,0,-1,0,-1)
x[,4]=c(0,1,-1,0,0,-1,0,-1,1,0,0,1)
x[,5]=c(0,-1,0,-1,-1,0,0,1,0,1,1,0)
x[1,]
xtranspose<-t(x)

##########
for(i in 1:12){
	beta0<-matrix(c(0,0,0,0,0),ncol=1,nrow=5)
	mu<- x%*%beta0	
}
sigmaalpha<-1
sigmatheta1<-1
sigmatheta2<-1
sigmatheta3<-1
sigmatheta4<-1
N<-100000
y<-matrix(NA,N,5)
for(n in 1:N){
	for (i in 1:12){
#draw psi from N(mu_g,1)
psi <- matrix (rnorm(1,mu[i,],1),12,1)
}
#draw B from N(mubeta,sigmabeta)
carat <- diag(c(sigmaalpha,sigmatheta1,sigmatheta2,sigmatheta3,sigmatheta4))
carat1<-solve(carat)
z<-solve(carat1+xtranspose%*%x)
mubeta <- z%*%xtranspose%*%psi
sigmabeta<- z
beta1<-rmvnorm(1,mubeta,sigmabeta)
y[n,]<-beta1
}

#proby1=pnorm(mu[1])
#proby1

#LBPSI<-function(){exp^(-1/2*(PSIg-X[i,]%*%B)^2)/sqrt(2*pi)
	
#}
#probBPSI_Y

#draw psi from N(mu_g,1)
par(mfrow=c(4,4))
plot(y[,1],y[,2])
plot(y[,1],y[,3])
plot(y[,1],y[,4])
plot(y[,1],y[,5])
plot(y[1:1000,2])

plot(y[1:1000,3])

plot(y[1:1000,4])
plot(y[1:1000,5])


hist(y[,1])
hist(y[,2])
hist(y[,3])
hist(y[,4])
hist(y[,5])