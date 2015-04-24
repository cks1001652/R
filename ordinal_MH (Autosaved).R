getwd()
setwd("/Users/cheeseloveicecream/Documents/missouri/STAT4999/college football")
n.raw<-data.frame(read.csv("ndata.csv",sep=",",quote=""))
name1<-data.frame(read.csv("name1.csv",sep=",",quote=""))
name1<-name1[-2]

n.raw1<-data.matrix(n.raw)
colnames(n.raw1)<-c("teamW","PtsW","home","teamL","PtsL")
ptm<-proc.time()
x<- matrix(0,694,209)
x[n.raw1[,3]!=0,1]<-1
x[(n.raw1[,3]==0),1]<-0
for(i in 1:694){
				if(n.raw1[i,3]==1){
				x[i,(n.raw1[i,1]+1)]<- 1
				x[i,(n.raw1[i,4]+1)]<- -1
								 }
				if( n.raw1[i,3]==2){  
				x[i,(n.raw1[i,1]+1)]<- -1
				x[i,(n.raw1[i,4]+1)]<- 1
						 		  }
			   }
			   
score.diff<-rep(NA,694)
for(i in 1:694){
	if(n.raw1[i,3]==2){score.diff[i]<-(n.raw1[i,5]-n.raw1[i,2])}
	else{score.diff[i]<-(n.raw1[i,2]-n.raw1[i,5])}
}
#summary(score.diff)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-56.000  -7.000   7.000   8.006  22.000  74.000 
y<-rep(NA,694)
for(i in 1:694){
	if(score.diff[i]< (-7)){y[i]=1}
	else if(score.diff[i]< 7 &&score.diff[i]>= -7){y[i]=2}
	else if(score.diff[i]<= 22 &&score.diff[i]> 7){y[i]=3}
	else {y[i]=4}
}	
###
set.seed(1234567)
xtranspose<-t(x)
beta0<-matrix(0,ncol=1,nrow=209)
mu<-x%*%beta0
diagsigma<-diag(1,209,209)
Lambda<-diagsigma
Lambda1<-solve(Lambda)
phi<-rep(NA,694)
z<-solve(Lambda1+xtranspose%*%x)
v<-z%*%xtranspose	
output<-matrix(NA,N,209)

####

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

N<-10000
n<-1

for(n in 1:N){			
					phi[y==1]<-rtruncnorm(length(y[y==1]),a=-Inf,b=c1,mean=mu[y==1],sd=1)
					phi[y==2]<-rtruncnorm(length(y[y==2]),a=c1,b=c2,mean=mu[y==2],sd=1)
					phi[y==3]<-rtruncnorm(length(y[y==3]),a=c2,b=c3,mean=mu[y==3],sd=1)
					phi[y==4]<-rtruncnorm(length(y[y==4]),a=c3,b=Inf,mean=mu[y==4],sd=1)
					m<-rep(c(max(phi[y==1]),max(phi[y==2]),max(phi[y==3]),max(phi[y==4])),1)	
					M<-rep(c(min(phi[y==1]),min(phi[y==2]),min(phi[y==3]),min(phi[y==4])),1)
					c1<-rtruncnorm(1,a=m[1],b=M[2],mean=0,sd=1)
				 	c2<- 0
				 	c3<-rtruncnorm(1,a=m[3],b=M[4],mean=0,sd=1)
				 	C1[n]<-c1
				 	C3[n]<-c3
				 	# c1star<-rnorm(1,C1[n],s1)
					# c3star<-rnorm(1,C3[n],s3)
					# if(runif(1)<(dtruncnorm(c1star,a=m[1],b=M[2],mean=0,sd=1)/dtruncnorm(C1[n],a=m[1],b=M[2],mean=0,sd=1))){C1[n+1]<-c1star}
					# else {C1[n+1]<-C1[n]}
					# if(runif(1)<(dtruncnorm(c3star,a=m[3],b=M[4],mean=0,sd=1)/dtruncnorm(C3[n],a=m[3],b=M[4],mean=0,sd=1)))
					# {C3[n+1]<-c3star}
					# else{C3[n+1]<-C3[n]}


			mubeta <- v%*%phi
		    sigmabeta<- z
		    beta1<-rmvnorm(1,mubeta,sigmabeta)
			output[n,]<-beta1
			mu<-x%*%t(beta1)

}

colnames(output)<-c("home",levels(name1[,1]))
igood<-1000:N
a<-apply(output[igood,],2,mean)
# plot(output[,10])
sort(a)
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

