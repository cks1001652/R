library(mvtnorm)
library(truncnorm)
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
####
score.diff<-rep(NA,256)
for(i in 1:256){
	if(rawdata1[i,2]==1){score.diff[i]<-(rawdata[i,4]-rawdata[i,5])}
	else{score.diff[i]<-(rawdata[i,5]-rawdata[i,4])}
}
y<-rep(NA,256)
for(i in 1:256){
	if(score.diff[i]< (-4.25)){y[i]=1}
	else if(score.diff[i]< 3 &&score.diff[i]>= -4.25){y[i]=2}
	else if(score.diff[i]<= 13 &&score.diff[i]> 3){y[i]=3}
	else {y[i]=4}
}
####
set.seed(1234567)
xtranspose<-t(x)
beta0<-matrix(0,ncol=1,nrow=33)
mu<-x%*%beta0
diagsigma<-diag(1,33,33)
Lambda<-diagsigma
Lambda1<-solve(Lambda)
phi<-rep(NA,256)
z<-solve(Lambda1+xtranspose%*%x)
v<-z%*%xtranspose	
output<-matrix(NA,N,33)
#c_j~N(0,sigma_c^2)
#sigmac<-1
#full conditional

#phi[i]~N(mu[i],1)
# m<-rep(c(-5,-1,1,5),1)
# M<-rep(c(-10,-4,1,10),1)
C1<-rep(0,N)
C3<-rep(0,N)
c1<--1
c3<- 1
c2<-0
C1[1]<-c1
C3[1]<-c3
N<-1000
n<-1
s1<- 0.5
s3<- 0.5
for(n in 1:N){			
					phi[y==1]<-rtruncnorm(length(y[y==1]),a=-Inf,b=C1[n],mean=mu[y==1],sd=1)
					phi[y==2]<-rtruncnorm(length(y[y==2]),a=C1[n],b=c2,mean=mu[y==2],sd=1)
					phi[y==3]<-rtruncnorm(length(y[y==3]),a=c2,b=C3[n],mean=mu[y==3],sd=1)
					phi[y==4]<-rtruncnorm(length(y[y==4]),a=C3[n],b=Inf,mean=mu[y==4],sd=1)
					m<-rep(c(max(phi[y==1]),max(phi[y==2]),max(phi[y==3]),max(phi[y==4])),1)	
					M<-rep(c(min(phi[y==1]),min(phi[y==2]),min(phi[y==3]),min(phi[y==4])),1)
					# c1<-rtruncnorm(1,a=m[1],b=M[2],mean=0,sd=1)
					# c2<-0
				 	# c3<-rtruncnorm(1,a=m[3],b=M[4],mean=0,sd=1)
				 	c1star<-rnorm(1,C1[n],s1)
					c3star<-rnorm(1,C3[n],s3)
					if(runif(1)<(dtruncnorm(c1star,a=m[1],b=M[2],mean=0,sd=1)/dtruncnorm(C1[n],a=m[1],b=M[2],mean=0,sd=1))){C1[n+1]<-c1star}
					else {C1[n+1]<-C1[n]}
					if(runif(1)<(dtruncnorm(c3star,a=m[3],b=M[4],mean=0,sd=1)/dtruncnorm(C3[n],a=m[3],b=M[4],mean=0,sd=1)))
					{C3[n+1]<-c3star}
					else{C3[n+1]<-C3[n]}


			mubeta <- v%*%phi
		    sigmabeta<- z
		    beta1<-rmvnorm(1,mubeta,sigmabeta)
			output[n,]<-beta1
			mu<-x%*%t(beta1)

}

colnames(output)<-c("home",levels(rawdata[,3]))
igood<-1000:N
apply(output[igood,],2,mean)
# plot(output[,10])

out<-rep(NA,33)
out<-apply(output[igood,],2,mean)
team<-rep(out[2:32],1)
winprob_xbeaty_xhome<-function(x,y){
	pnorm((team[x]-team[y]),mean=0,sd=1)
}
winprob_xbeaty_xhome(28,10)
#seahawks beats brocons, game in third place

#prob=0.589938

#summary(score.diff)
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -35.000  -4.250   3.000   3.105  13.000  43.000 

