library(truncnorm)
library(mvtnorm)

getwd()
setwd("/Users/cheeseloveicecream/Documents/missouri/STAT4999/NBA")
n.raw<-data.frame(read.csv("ndata.csv",sep=",",quote=""))
name1<-data.frame(read.csv("name.csv",sep=",",quote=""))


n.raw1<-data.matrix(n.raw)
colnames(n.raw1)<-c("teamH","PtsH","teamA","PtsA")

x<- matrix(0,1230,31)
x[,1]<-1
for(i in 1:1230){
				if(n.raw1[i,2]<n.raw1[i,4]){
				x[i,(n.raw1[i,1]+1)]<- 1
				x[i,(n.raw1[i,3]+1)]<- -1
								 }
				else{  
				x[i,(n.raw1[i,1]+1)]<- -1
				x[i,(n.raw1[i,3]+1)]<- 1
						 		  }
			   }
			   


 set.seed(123456789)
xtranspose<-t(x)
beta0<-matrix(0,ncol=1,nrow=31)
mu<-x%*%beta0
diagsigma<-diag(1,31,31)
Lambda<-diagsigma
Lambda1<-solve(Lambda)
phi<-rep(NA,1230)
z<-solve(Lambda1+xtranspose%*%x)
v<-z%*%xtranspose	
N<-10000
output<-matrix(NA,N,31)
sum1<-sum(n.raw1[,2]>n.raw1[,4])
sum2<-sum(n.raw1[,2]<n.raw1[,4])
ptm<- proc.time()

for(n in 1:N){
	#draw phi from N+(mu_g,1)
	# phi[i]<-qnorm(runif(1)*(1-pnorm(mu[i],1)),mu[i],1)
	# phi[i]<- mu[i]+qnorm((1-runif(1)*pnorm(mu[i],mu[i],1)),mu[i],1)
	phi[n.raw1[,2]>n.raw1[,4]]<- rtruncnorm(sum1,a=0,b=Inf,mean=mu[(n.raw1[,2]>n.raw1[,4])],sd=1)
	#neutral stadium counts as win team being home team cause homeadv==0	
	#draw phi from N-(mu_g,1)
	# phi[i]<-qnorm((pnorm(mu[i],1)*runif(1)),mu[i],1)
	# phi[i]<- mu[i]-qnorm(runif(1)*pnorm(mu[i],mu[i],1),mu[i],1)
	phi[n.raw1[,2]<n.raw1[,4]]<- rtruncnorm(sum2,a=-Inf,b=0,mean=mu[(n.raw1[,2]< n.raw1[,4])],sd=1)	
	# #draw B from N(mubeta,sigmabeta)	
	mubeta <- v%*%phi
	sigmabeta <- z
	beta1<-rmvnorm(1,mubeta,sigmabeta)
	output[n,]<-beta1
	mu<-x%*%t(beta1)}
	proc.time() -ptm
	
	
estbg <- as.matrix(apply(output, 2, mean))
estbg1<- data.frame(c("home",levels(name1[,1])),estbg)
names(estbg1)[1]<- paste("team")
names(estbg1)[2]<- paste("ability")
attach(estbg1)
estbg2<- estbg1[order(ability,team,decreasing=TRUE),]
write.table(estbg2,"nba_gibbs.txt",sep="\t")
estbg2	