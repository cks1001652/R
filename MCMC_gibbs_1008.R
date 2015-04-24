#input and rough analyze 
getwd()
setwd("/Users/cheeseloveicecream/Documents/missouri/STAT4999")
rawdata<-data.frame(read.csv("data.csv",sep=",",quote=""))
rawdata <- rawdata[,-1:-4]
rawdata<-rawdata[,-6:-9]
attach(rawdata)
rawdata$home<-0
rawdata_num1<-as.matrix(sapply(rawdata[,2],as.numeric))
rawdata_num2<-data.matrix(rawdata[,4:6])
rawdata_cha1<-rawdata[,1]
rawdata_cha2<-rawdata[,3]
rawdata1<-data.frame(rawdata_cha1,rawdata_num1,rawdata_cha2,rawdata_num2)
rawdata1<-data.matrix(rawdata1)

i<-1
for (i in 1:256){
				if(rawdata1[i,2]==1){
									rawdata1[i,6]<-1
									}
				else{
					rawdata1[i,6]<-0
					}
}
#extract crutial data
data<-matrix(0,256,2)
i<-1
for(i in 1:256){
				if(rawdata1[i,6]==1){
									data[i,1]<-1
									}
				else{
					data[i,2]<-1
					}
				}
x<- matrix(0,256,33)


	for(i in 1:256){
		if( rawdata1[i,6]==1){
			x[i,(rawdata1[i,1]+1)]=1
			x[i,(rawdata1[i,3]+1)]=-1
			}
		else{
			x[i,(rawdata1[i,1]+1)]=-1
			x[i,(rawdata1[i,3]+1)]=1
			}
					}
					x[,1]=1
#levels(rawdata1[,1])
###############################################
# #package
	library(mvtnorm)
	library(truncnorm)

# #Home and Away information
	# x <- matrix(NA,12,5)
	# x[,1]=1
	# x[,2]=c(1,0,1,0,1,0,-1,0,-1,0,-1,0)
	# x[,3]=c(-1,0,0,1,0,1,1,0,0,-1,0,-1)
	# x[,4]=c(0,1,-1,0,0,-1,0,-1,1,0,0,1)
	# x[,5]=c(0,-1,0,-1,-1,0,0,1,0,1,1,0)
	 xtranspose<-t(x)

# #Win and Loss information for the home and away teams
	# data <- matrix(NA,12,2)
	# data[,1]=c(1,1,0,1,0,1,0,0,1,0,0,1)
	# data[,2]=c(0,0,1,0,1,0,1,1,0,1,1,0)

#time(0)  
	beta0<-matrix(0,ncol=1,nrow=33)
	mu<- x%*%beta0	

#preparation
	set.seed(23457)
	phi<-rep(0,256)
	diagsigma<-diag(1,33,33)
	
	
	N<-10000
	y<-matrix(NA,N,33)
	Lambda <- diagsigma
	Lambda1<-solve(Lambda)
	z<-solve(Lambda1+xtranspose%*%x)
	v<-z%*%xtranspose	
#gibbs sampling
	ptm <- proc.time()

	for(n in 1:N){
							#draw phi from N+(mu_g,1)
							# phi[i]<-qnorm(runif(1)*(1-pnorm(mu[i],1)),mu[i],1)
							# phi[i]<- mu[i]+qnorm((1-runif(1)*pnorm(mu[i],mu[i],1)),mu[i],1)
				phi[(data[,1])==1]<- rtruncnorm(sum(data[,1]==1),a=0,b=Inf,mean=mu[(data[,1])==1],sd=1)
							#draw phi from N-(mu_g,1)
							# phi[i]<-qnorm((pnorm(mu[i],1)*runif(1)),mu[i],1)
							# phi[i]<- mu[i]-qnorm(runif(1)*pnorm(mu[i],mu[i],1),mu[i],1)
				phi[(data[,1])==0]<- rtruncnorm(sum(data[,2]==1),a=-Inf,b=0,mean=mu[(data[,1])==0],sd=1)
						
						
		
			#draw B from N(mubeta,sigmabeta)
					
			mubeta <- v%*%phi
		    sigmabeta<- z
		    beta1<-rmvnorm(1,mubeta,sigmabeta)
			y[n,]<-beta1
			mu<-x%*%t(beta1)
				 }
proc.time()-ptm
levels(rawdata[,3])
igood <- 1000:10000
apply(y[igood, ], 2, mean)
colnames(y)<-c("home",levels(rawdata_cha1))


# max(apply(y[igood, 2:33], 2, mean)
# )

# par(mfrow=c(3, 2))
# plot(y[,2])