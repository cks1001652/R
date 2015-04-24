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
			   
score.diff<-rep(NA,1230)
for(i in 1:1230){
	score.diff[i]<-(n.raw1[i,2]-n.raw1[i,4])}

#summary(score.diff)
# Min. 1st Qu.  Median    Mean    3rd Qu.    Max. 
#-48.000  -7.000   3.000   2.597  11.000  45.000 
y<-rep(NA,694)
for(i in 1:694){
	if(score.diff[i]< (-7)){y[i]=1}
	else if(score.diff[i]< 3 &&score.diff[i]>= -7){y[i]=2}
	else if(score.diff[i]<= 11 &&score.diff[i]> 3){y[i]=3}
	else {y[i]=4}
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

c2<-0



ymax<-as.matrix(y,1230,1)
y.ij<-matrix(0,1230,4)

y.ij[ymax==1,1]<-1
y.ij[ymax==2,2]<-1
y.ij[ymax==3,3]<-1
y.ij[ymax==4,4]<-1
C1<--1
C3<- 1
c2<-0
A<-matrix(NA,1230,4)
Astar<-matrix(NA,1230,4)

#(c1,c3|y,beta) ~ N_(c1|0,1)N_+(c3|0,1)p(y|beta,c)

C1.mc <- matrix(NA,nrow= N)
C3.mc <- matrix(NA,nrow= N)

accept1 <- rep(0, N)
accept3<- rep(0,N)


				 

# p.y1 <- function(B1,B2){exp(sum(B1)+sum(B2))}
# p.y1star<-function(B1,B2){exp(sum(B1star)+sum(B2star))}

# p.y3 <- function(B3,B4){exp(sum(B3)+sum(B4))}
# p.y3star <- function(B3,B4){exp(sum(B3star)+sum(B4star))}

s1<-0.12
s2<- 0.12
n<-1

yij1<- y.ij[,1]==1
yij2<- y.ij[,2]==1
yij3<- y.ij[,3]==1
yij4<- y.ij[,4]==1
ptm<- proc.time()
for(n in 1:N){		
	
					phi[yij1]<-rtruncnorm(length(y[yij1]),a=-Inf,b=C1,mean=mu[yij1],sd=1)
					phi[yij2]<-rtruncnorm(length(y[yij2]),a=C1,b=c2,mean=mu[yij2],sd=1)
					phi[yij3]<-rtruncnorm(length(y[yij3]),a=c2,b=C3,mean=mu[yij3],sd=1)
					phi[yij4]<-rtruncnorm(length(y[yij4]),a=C3,b=Inf,mean=mu[yij4],sd=1)
					# m<-rep(c(max(phi[y==1]),max(phi[y==2]),max(phi[y==3]),max(phi[y==4])),1)	
					# M<-rep(c(min(phi[y==1]),min(phi[y==2]),min(phi[y==3]),min(phi[y==4])),1)
					# c1<-rtruncnorm(1,a=m[1],b=M[2],mean=0,sd=1)
					# c2<-0
				 	# c3<-rtruncnorm(1,a=m[3],b=M[4],mean=0,sd=1)
					
#					A[,1]<-pnorm(C1+mu,0,1)-pnorm(-Inf,0,1)
					A[,1]<-pnorm(C1+mu,0,1)
					A[,2]<-pnorm(mu,0,1)-pnorm(C1+mu,0,1)
					A[,3]<-pnorm(C3+mu,0,1)-pnorm(mu,0,1)
#					A[,4]<-pnorm(Inf,0,1)-pnorm(C3+mu,0,1)
					A[,4]<- 1 - pnorm(C3+mu,0,1)

					
					# B1 <- log(A[1:sum(y.ij[,1]==1),1])
					# B2 <- log(A[1:sum(y.ij[,2]==1),2])
					# B3 <- log(A[1:sum(y.ij[,3]==1),3])
					# B4 <- log(A[1:sum(y.ij[,4]==1),4])
					B1 <- log(A[yij1,1])
					B2 <- log(A[yij2,2])
					B3 <- log(A[yij3,3])
					B4 <- log(A[yij4,4])
			
			##################	 	
			
				 	C1star<- C1 + rnorm(1,0,s1)
				 	if (C1star > c2) r1 <- 0
				 	else{
#				 	Astar[,1]<-pnorm(C1star+mu,0,1)-pnorm(-Inf,0,1)
				 	Astar[,1]<-pnorm(C1star+mu,0,1)
					Astar[,2]<-pnorm(mu,0,1)-pnorm(C1star+mu,0,1)
					

				 	# B1star <- log(Astar[1:sum(y.ij[,1]==1),1])
					# B2star <- log(Astar[1:sum(y.ij[,2]==1),2])
				 	B1star <- log(Astar[yij1,1])
					B2star <- log(Astar[yij2,2])
					
#					if (C1star > 0) {r1 <- 0}
					# else r1 <- exp((-1/2)*(C1star^2-C1^2)+log(p.y1star(B1star,B2star))-log(p.y1(B1,B2)))
					r1 <- exp((-1/2)*(C1star^2-C1^2)+sum(B1star) + sum(B2star)-sum(B1) - sum(B2))
					
					if (runif(1) < r1){
						C1 <- C1star
						accept1[n] <- 1
									}
					}
					C1.mc[n] <- C1
					
			##################		
					C3star<- C3 + rnorm(1,0,s2)
					if (C3star < c2) r3 <- 0
					else{
					Astar[,3]<-pnorm(C3star+mu,0,1)-pnorm(c2+mu,0,1)
#					Astar[,4]<-pnorm(Inf,0,1)-pnorm(C3star+mu,0,1)
					Astar[,4]<- 1 - pnorm(C3star+mu,0,1)
					# B3star <- log(Astar[1:sum(y.ij[,3]==1),3])
					# B4star <- log(Astar[1:sum(y.ij[,4]==1),4])
					B3star <- log(Astar[yij3,3])
					B4star <- log(Astar[yij4,4])
					if (C3star < 0) {r3 <- 0}
					# else {r3 <- exp((-1/2)*(C3star^2-C3^2)+log(p.y3star(B3star,B4star))-log(p.y3(B3,B4)))}
					r3 <- exp((-1/2)*(C3star^2-C3^2)+ sum(B3star) + sum(B4star) - sum(B3) - sum(B4))
					if (runif(1) < r3){
						C3 <- C3star
						accept3[n] <- 1
									}
					}
					C3.mc[n] <- C3			 	
				 	

			##################
				mubeta <- v%*%phi
		    	sigmabeta<- z
		    	beta1<-rmvnorm(1,mubeta,sigmabeta)
				output[n,]<-beta1
				mu<-x%*%t(beta1)

}
proc.time() -ptm


# plot(C1.mc)

estb <- as.matrix(apply(output, 2, mean))
estb1<- data.frame(c("home",levels(name1[,1])),estb)
names(estb1)[1]<- paste("team")
names(estb1)[2]<- paste("ability")
attach(estb1)
estb2<- estb1[order(ability,team,decreasing=TRUE),]
write.table(estb2,"nba.txt",sep="\t")
estb2