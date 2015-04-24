getwd()
setwd("/Users/cheeseloveicecream/Documents/missouri/STAT4999")
nbaraw<-data.frame(read.csv("nbadata.csv",sep=",",quote=""))
nbaraw1<-nbaraw[,-7:-8]
nbaraw1<-nbaraw1[,-1:-2]
attach(nbaraw1)
nbaraw1$diff<-nbaraw1[,2]-nbaraw1[,4]
nbaraw1<-data.matrix(nbaraw1)
####
i<-1
x<- matrix(0,1230,31)


	for(i in 1:1230){
		if( nbaraw1[i,5]>=0){
			x[i,(nbaraw1[i,1]+1)]=1
			x[i,(nbaraw1[i,3]+1)]=-1
			}
		else{
			x[i,(nbaraw1[i,1]+1)]=-1
			x[i,(nbaraw1[i,3]+1)]=1
			}
					}
					x[,1]=1

	library(mvtnorm)
	library(truncnorm)
	xtranspose<-t(x)
	beta0<-matrix(0,ncol=1,nrow=31)
	mu<- x%*%beta0	
	set.seed(23457)
	phi<-rep(0,1230)
	sigmamatrix<-diag(1,31,31)
	
	
	N<-10000
	y<-matrix(NA,N,31)
	Lambda <- sigmamatrix
	Lambda1<-solve(Lambda)
	z<-solve(Lambda1+xtranspose%*%x)
	v<-z%*%xtranspose	
					
#gibbs sampling
	for(n in 1:N){
		for (i in 1:1230){
							if(nbaraw1[i,5]>=0){
							#draw phi from N+(mu_g,1)
							# phi[i]<-mu[i]+qnorm((1-runif(1)*pnorm(mu[i],mu[i],1)),mu[i],1)
							phi[i]<- rtruncnorm(1,a=0,b=Inf,mean=mu[i],sd=1)

												}
							else{
							#draw phi from N-(mu_g,1)
							# phi[i]<-mu[i]-qnorm((pnorm(mu[i],mu[i],1)*runif(1)),mu[i],1)
							 phi[i]<- rtruncnorm(1,a=-Inf,b=0,mean=mu[i],sd=1)

							     }
				 		  }
					#draw B from N(mubeta,sigmabeta)
						
						sigmabeta<- z
						mubeta <- v%*%phi
						beta1<-rmvnorm(1,mubeta,sigmabeta)
						y[n,]<-beta1	
						mu<-x%*%t(beta1)

					}
						
igood <- 1:N				 
as.matrix(levels(nbaraw[,3],col=1))
as.matrix(apply(y[igood, ], 2, mean),col=1)
# igood <- 1:N
# apply(y[igood, ], 2, mean)
# max(apply(y[igood, 2:31], 2, mean))
plot(y[,4])
