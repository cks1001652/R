library(truncnorm)
library(mvtnorm)
#getwd()
#setwd("/Users/cheeseloveicecream/Documents/missouri/STAT4999/college football")
n.raw<-data.frame(read.csv("ndata2.csv",sep=",",quote=""))
name1<-data.frame(read.csv("team.csv",sep=",",quote=""))
week<-data.frame(read.csv("week.csv",sep=",",quote=""))

name1<-name1[-2]

#nraw1<-data.matrix(nraw)
n.raw1<-data.matrix(n.raw)
m.week<-data.matrix(week)  #   look at this
x1<-matrix(0,828,208)
x0<- matrix(0,828,209)
x0[n.raw1[,3]!=0,1]<-1
x0[(n.raw1[,3]==0),1]<-0
colnames(n.raw1)<-c("teamW","PtsW","home","teamL","PtsL")
#  Add indicator for home team wins
home.win <- n.raw1[, 3] <= 1
n.home.win <- sum(home.win)
n.home.loss <- sum(!home.win)


for(i in 1:828){
				if(n.raw1[i,3]<=1){
				x1[i,(n.raw1[i,1])]<- (m.week[i])-8 
				x1[i,(n.raw1[i,4])]<- -(m.week[i] )-8
								 }
				if( n.raw1[i,3]==2){  
				x1[i,(n.raw1[i,1])]<- -(m.week[i] )-8
				x1[i,(n.raw1[i,4])]<- m.week[i] -8
						 		  }
			   }
			   

for(i in 1:828){
				if(n.raw1[i,3]<=1){
				x0[i,(n.raw1[i,1]+1)]<- 1
				x0[i,(n.raw1[i,4]+1)]<- -1
								 }
				if( n.raw1[i,3]==2){  
				x0[i,(n.raw1[i,1]+1)]<- -1
				x0[i,(n.raw1[i,4]+1)]<- 1
						 		  }
			   }
x<-cbind(x0,x1)

xtranspose<-t(x)
beta0<-matrix(0.5,209,1)
betab<-matrix(0,208,1)
betanew<-rbind(beta0,betab)
mu<-x%*%betanew

set.seed(234567)
phi<-rep(0,828)
diagsigma<- diag(c(1, rep(1, 208), .01*rep(1, 208)))
	Lambda1<-diag(1/diag(diagsigma))
N<-10000

output<-matrix(NA,N,417)
sigmabeta <- z <-solve(Lambda1+xtranspose%*%x)
v<-z%*%xtranspose	
	
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
#proc.time() -ptm
est <- as.matrix(apply(output, 2, mean))
est1<- data.frame(c("home",levels(name1[,1])),est[1:209])
names(est1)[1]<- paste("team")
names(est1)[2]<- paste("ability")
attach(est1)
est2<-est1[order(ability,team,decreasing=TRUE),]
plot(density(output[,1]))

bet <- data.frame(c(levels(name1[,1])),est[210:417])
tmp <- est1[-1, 2] + 7*bet[, 2]
season.end <- data.frame(levels(name1[,1]), tmp)
ix <- order(season.end[, 2])
season.end[ix, ]
write.table(season.end[ix, ], "CF_linear.txt", sep="\t")


w1 <- est1[-1, 2] - 7*bet[, 2]
w15 <- est1[-1, 2] + 7*bet[, 2]
ww <- cbind(w1, w15)
matplot(c(1, 15), t(ww),type='l', xlab="week", ylab="ability")
ix <- w15 > 1.2
data.frame(c(levels(name1[,1])), w1, w15)[ix, ]
matplot(t(ww[ix, ]),type='l', ylim=c(-1, 2.5), lty=1:sum(ix), col=1:sum(ix))
legend(1.6, 1.3, legend=name1[ix, 1], lty=1:sum(ix), col=1:sum(ix))

detach(est1)


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