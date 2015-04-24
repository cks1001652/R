library(truncnorm)
library(mvtnorm)
library(Matrix)
#getwd()
setwd("/Users/cheeseloveicecream/Documents/missouri/STAT4999/NBA")
n.raw<-data.frame(read.csv("ndata1.csv",sep=",",quote=""))
name1<-data.frame(read.csv("name.csv",sep=",",quote=""))
n.raw1<-data.matrix(n.raw)
colnames(n.raw1)<-c("Home","PtsH","Away","PtsA")

#week0<-data.frame(read.csv("week0.csv",sep=",",quote=""))
#week1<-data.frame(read.csv("week1.csv",sep=",",quote=""))
#name1<-name1[-2]
#nraw1<-data.matrix(nraw)
#######X-matrix##########################
x<-matrix(0,1230,181)
x[,1]<- 1
for(i in 1:246){
				if(n.raw1[i,2]<n.raw1[i,4]){
											x[i,(6*n.raw1[i,1]-4)]<-1
											x[i,(6*n.raw1[i,3]-4)]<- -1
											}
				else{
											x[i,(6*n.raw1[i,1]-4)]<--1
											x[i,(6*n.raw1[i,3]-4)]<- 1
											}
				}
				
for(i in 247:465){
				if(n.raw1[i,2]<n.raw1[i,4]){
											x[i,(6*n.raw1[i,1]-3)]<-1
											x[i,(6*n.raw1[i,3]-3)]<- -1
											}
				else{
											x[i,(6*n.raw1[i,1]-3)]<--1
											x[i,(6*n.raw1[i,3]-3)]<- 1
											}
				}
				
for(i in 466:693){
				if(n.raw1[i,2]<n.raw1[i,4]){
											x[i,(6*n.raw1[i,1]-2)]<-1
											x[i,(6*n.raw1[i,3]-2)]<- -1
											}
				else{
											x[i,(6*n.raw1[i,1]-2)]<--1
											x[i,(6*n.raw1[i,3]-2)]<- 1
											}
				}
											
for(i in 694:870){
				if(n.raw1[i,2]<n.raw1[i,4]){
											x[i,(6*n.raw1[i,1]-1)]<-1
											x[i,(6*n.raw1[i,3]-1)]<- -1
											}
				else{
											x[i,(6*n.raw1[i,1]-1)]<--1
											x[i,(6*n.raw1[i,3]-1)]<- 1
											}
				}
				
for(i in 871:1107){
				if(n.raw1[i,2]<n.raw1[i,4]){
											x[i,(6*n.raw1[i,1])]<-1
											x[i,(6*n.raw1[i,3])]<- -1
											}
				else{
											x[i,(6*n.raw1[i,1])]<--1
											x[i,(6*n.raw1[i,3])]<- 1
											}
				}
				
for(i in 1108:1230){
				if(n.raw1[i,2]<n.raw1[i,4]){
											x[i,(6*n.raw1[i,1]+1)]<-1
											x[i,(6*n.raw1[i,3]+1)]<- -1
											}
				else{
											x[i,(6*n.raw1[i,1]+1)]<--1
											x[i,(6*n.raw1[i,3]+1)]<- 1
											}
					}										


#  Add indicator for home team wins
home.win<- (n.raw1[, 2]<n.raw1[,4])
n.home.win <- sum(n.raw1[, 2]<n.raw1[,4])
n.home.loss <- sum(n.raw1[, 2]>n.raw1[,4])
#variance########################## 
rho<- 0.2
sigma0<-matrix(rho,6,6)^(abs(outer(1:6,1:6,"-")))
Lambda1<-solve(bdiag(1,sigma0,sigma0,sigma0,sigma0,sigma0,sigma0,sigma0,sigma0,sigma0,sigma0,sigma0,sigma0,sigma0,sigma0,sigma0,sigma0,sigma0,sigma0,sigma0,sigma0,sigma0,sigma0,sigma0,sigma0,sigma0,sigma0,sigma0,sigma0,sigma0,sigma0))

#######mu###########

betanew<-matrix(0,181,1)
mu<-x%*%betanew
#################
xtranspose<-t(x)
set.seed(234567)
phi<-rep(0,1230)
N<-10000

output<-matrix(NA,N,181)
#output<-matrix(NA,N,31)
# R<-chol(Lambda1+xtranspose%*%x)
# Rp<- t(R)
z <- solve(Lambda1+xtranspose%*%x)
z<- as.matrix(z)
sigmabeta <-z 
v<-z%*%xtranspose
# vv<- solve(Rp%*%R)
# vvv<- solve(Rp%*%R)%*%xtranspose	
# sigmabeta<-vv
s1<-0.2
accept<-rep(0,N)
rho.mc<- matrix(NA,nrow=N)
ptm <- proc.time()
	
for(n in 1:N){
							#draw phi from N+(mu_g,1)
							# phi[i]<-qnorm(runif(1)*(1-pnorm(mu[i],1)),mu[i],1)
							# phi[i]<- mu[i]+qnorm((1-runif(1)*pnorm(mu[i],mu[i],1)),mu[i],1)
		phi[home.win]<- rtruncnorm(n.home.win,a=0,b=Inf,mean=mu[home.win],sd=1)
		#neutral stadium counts as win team being home team cause homeadv==0	
							#draw phi from N-(mu_g,1)
							# phi[i]<-qnorm((pnorm(mu[i],1)*runif(1)),mu[i],1)
							# phi[i]<- mu[i]-qnorm(runif(1)*pnorm(mu[i],mu[i],1),mu[i],1)
		phi[!home.win]<- rtruncnorm(n.home.loss,a=-Inf,b=0,mean=mu[!home.win],sd=1)	
		#draw B from N(mubeta,sigmabeta)	
		rhostar<- rho + rnorm(1,0,s1)
		if(rhostar>=1.2 || rhostar<=-1.2)r1<-0
		else{
			r1<-exp(-1/2*rhostar^2)
			if(runif(1) < r1){
				rho<-rhostar
				accept[n]<-1
			}
		}
		rho.mc[n]<-rho
		mubeta <- as.matrix(v%*%phi)
	    beta1<-rmvnorm(1,mubeta,sigmabeta)
		output[n,]<-beta1
		mu<-x%*%t(beta1)
		
		if (trunc(n/100)*100 == n) print(n)
				 }
proc.time() -ptm
###########trace plot/density######
pdf("team1.pdf")
plot(density(output[,3]))#a team in the first period
dev.off()
pdf("team2.pdf")
plot(density(output[,41]))#a team in the second period
dev.off()
pdf("homedensity.pdf")
plot(density(output[,1]))#home advantage
dev.off()
pdf("rhotrace.pdf")
plot(rho.mc)
dev.off()

best0 <- as.matrix(apply(output, 2, mean))

######first period####
best.1<-matrix(0,30,1)
for(i in 1:30){best.1[i]<-best0[6*(i-1)+2]}
best.11<-rbind(best0[1],best.1)
best.12<- data.frame(c("home",levels(name1[,1])),best.11)
names(best.12)[1]<- paste("team")
names(best.12)[2]<- paste("ability")
attach(best.12)
best.p1<-best.12[order(ability,team,decreasing=TRUE),]

#############second period
best.2<-matrix(0,30,1)
for(i in 1:30){best.2[i]<-best0[6*(i-1)+3]}
best.21<-rbind(best0[1],best.2)
best.22<- data.frame(c("home",levels(name1[,1])),best.21)
names(best.22)[1]<- paste("team")
names(best.22)[2]<- paste("ability")
attach(best.22)
best.p2<-best.22[order(ability,team,decreasing=TRUE),]

#############third period
best.3<-matrix(0,30,1)
for(i in 1:30){best.3[i]<-best0[6*(i-1)+4]}
best.31<-rbind(best0[1],best.3)
best.32<- data.frame(c("home",levels(name1[,1])),best.31)
names(best.32)[1]<- paste("team")
names(best.32)[2]<- paste("ability")
attach(best.32)
best.p3<-best.32[order(ability,team,decreasing=TRUE),]

#############fourth period
best.4<-matrix(0,30,1)
for(i in 1:30){best.4[i]<-best0[6*(i-1)+5]}
best.41<-rbind(best0[1],best.4)
best.42<- data.frame(c("home",levels(name1[,1])),best.41)
names(best.42)[1]<- paste("team")
names(best.42)[2]<- paste("ability")
attach(best.42)
best.p4<-best.42[order(ability,team,decreasing=TRUE),]

#############fifth period
best.5<-matrix(0,30,1)
for(i in 1:30){best.5[i]<-best0[6*(i-1)+6]}
best.51<-rbind(best0[1],best.5)
best.52<- data.frame(c("home",levels(name1[,1])),best.51)
names(best.52)[1]<- paste("team")
names(best.52)[2]<- paste("ability")
attach(best.52)
best.p5<-best.52[order(ability,team,decreasing=TRUE),]

#############sixth period
best.6<-matrix(0,30,1)
for(i in 1:30){best.6[i]<-best0[6*(i-1)+7]}
best.61<-rbind(best0[1],best.6)
best.62<- data.frame(c("home",levels(name1[,1])),best.61)
names(best.62)[1]<- paste("team")
names(best.62)[2]<- paste("ability")
attach(best.62)
best.p6<-best.62[order(ability,team,decreasing=TRUE),]

######final period####
best.tfinal<-best.62[,2]+best.52[,2]+best.42[,2]+best.32[,2]+best.22[,2]+best.12[,2]
best.final<-best.tfinal[-1]
best1<- data.frame(c(levels(name1[,1])),best.final)
names(best1)[1]<- paste("team")
names(best1)[2]<- paste("ability")
attach(best1)
best2<-best1[order(ability,team,decreasing=TRUE),]

##############plot#######################################
est.m<- rbind(best.12[-1,2],best.22[-1,2],best.32[-1,2],best.42[-1,2],best.52[-1,2],best.62[-1,2])
pdf("improvement.pdf")
matplot(est.m,type='b')
dev.off()
#####################################

# # igood<- 1:1000
# aout<-as.matrix(apply(output[igood,],2,mean))
# aout1<- data.frame(c("home",levels(name1[,1])),aout)

# names(aout1)[1]<- paste("team")
# names(aout1)[2]<- paste("ability")
# attach(aout1)
# aout2<- aout1[order(ability,team,decreasing=TRUE),]
# write.table(aout2,"CBF_gibbs_0.5.txt",sep="\t")
# output.diff<-matrix(output[,5]-output[,56],N,1)
# cred.int.bama.fsu<-apply(output.diff,2,quantile,probs=c(.025,.975))
#cred.int <- apply(output,2, quantile, probs= c(.025, .975))
#output[1,]