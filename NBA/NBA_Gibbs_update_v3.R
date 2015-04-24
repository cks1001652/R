library(truncnorm)
library(mvtnorm)
#getwd()
#setwd("/Users/cheeseloveicecream/Documents/missouri/STAT4999/NBA")
n.raw<-data.frame(read.csv("ndata.csv",sep=",",quote=""))
name1<-data.frame(read.csv("name.csv",sep=",",quote=""))
#week0<-data.frame(read.csv("week0.csv",sep=",",quote=""))
#week1<-data.frame(read.csv("week1.csv",sep=",",quote=""))


#name1<-name1[-2]

#nraw1<-data.matrix(nraw)
n.raw1<-data.matrix(n.raw)
 period0<-matrix(0,1230,1)   #   look at this
 period1<-matrix(0,1230,1)   #   look at this
period2<-matrix(0,1230,1)
period3<-matrix(0,1230,1)
period4<-matrix(0,1230,1)

colnames(n.raw1)<-c("Home","PtsH","Away","PtsA")
x5<-matrix(0,1230,30)
x4<-matrix(0,1230,30)
x3<-matrix(0,1230,30)
x2<-matrix(0,1230,30)
x1<-matrix(0,1230,30)
x0<- matrix(0,1230,31)
x0[,1]<-1
#  Add indicator for home team wins
home.win<- (n.raw1[, 2]>n.raw1[,4])
n.home.win <- sum(n.raw1[, 2]>n.raw1[,4])
n.home.loss <- sum(n.raw1[, 2]<n.raw1[,4])
period0 <- period1<- period2 <- period3 <- period4<- rep(0, length(home.win))
period0[247:length(home.win)] <- 1
period1[466:length(home.win)] <- 1
period2[694:length(home.win)] <- 1
period3[871:length(home.win)] <- 1
period4[1108:length(home.win)] <- 1

for(i in 1:1230){
				if(n.raw1[i,2]<n.raw1[i,4]){
				x0[i,(n.raw1[i,1]+1)]<- 1
				x1[i,(n.raw1[i,1])]<- period0[i]
				x2[i,(n.raw1[i,1])]<- period1[i]
				x3[i,(n.raw1[i,1])]<- period2[i]
				x4[i,(n.raw1[i,1])]<- period3[i]
				x5[i,(n.raw1[i,1])]<- period4[i]
				
				x0[i,(n.raw1[i,3]+1)]<- -1
				x1[i,(n.raw1[i,3])]<- -(period0[i])
				x2[i,(n.raw1[i,3])]<- -(period1[i])
				x3[i,(n.raw1[i,1])]<- -(period2[i])
				x4[i,(n.raw1[i,1])]<- -(period3[i])
				x5[i,(n.raw1[i,1])]<- -(period4[i])
				
				 							}
				else{  
				x0[i,(n.raw1[i,1]+1)]<- -1
				x1[i,n.raw1[i,1]]<- -(period0[i])
				x2[i,(n.raw1[i,1])]<- -(period1[i])
				x3[i,(n.raw1[i,1])]<- -(period2[i])
				x4[i,(n.raw1[i,1])]<- -(period3[i])
				x5[i,(n.raw1[i,1])]<- -(period4[i])
				
				x0[i,(n.raw1[i,3]+1)]<- 1
				x1[i,n.raw1[i,3]]<- period0[i]	
				x2[i,(n.raw1[i,3])]<- period1[i]
				x3[i,(n.raw1[i,1])]<- period2[i]
				x4[i,(n.raw1[i,1])]<- period3[i]
				x5[i,(n.raw1[i,1])]<- period4[i]
				

					}
			   }

x<-cbind(x0,x1,x2,x3,x4,x5)
#x<-x0

xtranspose<-t(x)
beta0<-matrix(0.5,31,1)
betaa<-matrix(0,30,1)
betab<-matrix(0,30,1)
betac<-matrix(0,30,1)
betad<-matrix(0,30,1)
betae<-matrix(0,30,1)

betanew<-rbind(beta0,betaa,betab,betac,betad,betae)
mu<-x%*%betanew
#mu<-x%*%beta0

set.seed(234567)
phi<-rep(0,1230)

diagsigma<- diag(c(1, rep(1, 30), .01*rep(1, 30),0.01*rep(1,30),0.01*rep(1,30),0.01*rep(1,30),0.01*rep(1,30)))
#diagsigma<- diag(c(1, rep(1, 30)))
	Lambda1<-diag(1/diag(diagsigma))
N<-10000

output<-matrix(NA,N,181)
#output<-matrix(NA,N,31)
# R<-chol(Lambda1+xtranspose%*%x)
# Rp<- t(R)
sigmabeta <- z <-solve(Lambda1+xtranspose%*%x)
v<-z%*%xtranspose
# vv<- solve(Rp%*%R)
# vvv<- solve(Rp%*%R)%*%xtranspose	
# sigmabeta<-vv

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
		mubeta <- v%*%phi
#	    sigmabeta<- z
	    beta1<-rmvnorm(1,mubeta,sigmabeta)
		output[n,]<-beta1
		mu<-x%*%t(beta1)
		if (trunc(n/100)*100 == n) print(n)
				 }
proc.time() -ptm
best <- as.matrix(apply(output, 2, mean))
best1<- data.frame(c("home",levels(name1[,1])),best[1:31])
names(best1)[1]<- paste("team")
names(best1)[2]<- paste("ability")
attach(best1)
best2<-best1[order(ability,team,decreasing=TRUE),]
plot(density(output[,1]))

bbet1 <- data.frame(c(levels(name1[,1])),best[32:61])
bbet2<- data.frame(c(levels(name1[,1])),best[62:91])
bbet3<- data.frame(c(levels(name1[,1])),best[92:121])
bbet4<- data.frame(c(levels(name1[,1])),best[122:151])
bbet5<- data.frame(c(levels(name1[,1])),best[152:181])

btmp <- best1[-1, 2] + 1*bbet1[, 2]+1*(bbet2[, 2])+1*(bbet3[, 2])+1*(bbet4[, 2])+1*(bbet5[, 2])
bseason.end <- data.frame(levels(name1[,1]), btmp)
ix <- order(bseason.end[, 2])
bseason.end[ix, ]
###############result#####################


w0 <- best1[-1, 2]
w1 <- best1[-1, 2] +1*(bbet1[, 2])
w2 <- best1[-1, 2] + 1*bbet1[, 2]+1*(bbet2[, 2])
w3 <- best1[-1, 2] + 1*bbet1[, 2]+1*(bbet2[, 2])+1*(bbet3[, 2])
w4 <- best1[-1, 2] + 1*bbet1[, 2]+1*(bbet2[, 2])+1*(bbet3[, 2])+1*(bbet4[,2])
w5 <- best1[-1, 2] + 1*bbet1[, 2]+1*(bbet2[, 2])+1*(bbet3[, 2])+1*(bbet4[,2])+1*(bbet5[,2])

ww <- cbind(w0,w1, w2,w3,w4,w5)
matplot(t(ww),type='b', xlab="week", ylab="ability")
ix <- w5 > 0.3
data.frame(c(levels(name1[,1])), w0,w1, w2,w3,w4,w5)[ix, ]
matplot(t(ww[ix, ]),type='l', ylim=c(0.05, 0.4), lty=1:sum(ix), col=1:sum(ix))
legend(1.6, 1.3, legend=name1[ix, 1], lty=1:sum(ix), col=1:sum(ix))

detach(est1)
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