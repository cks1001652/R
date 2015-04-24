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
# data<-matrix(0,256,2)
# i<-1
# for(i in 1:256){
				# if(rawdata1[i,6]==1){
									# data[i,1]<-1
									# }
				# else{
					# data[i,2]<-1
					# }
				# }
i<-1
x<- matrix(0,256,33)
					x[,1]=1

		for(i in 1:256){
						if( rawdata1[i,2]==1){

											x[i,(rawdata1[i,1]+1)]<- 1
											x[i,(rawdata1[i,3]+1)]<- -1
											}
						else{
			x[i,(rawdata1[i,1]+1)]<- -1
			x[i,(rawdata1[i,3]+1)]<- 1
							}
						}
			x[,1]<-1
#levels(rawdata1[,1])
######################################################
	library(mvtnorm)
	
	xtranspose<-t(x)
#preparation
	set.seed(23457)
	
	beta0<-matrix(1,ncol=1,nrow=33)
	mu<- x%*%beta0	
	s <- 1
phi<- matrix(NA,N,256)

game<- 0
game1<- 0
	sigmaalpha<-1
	sigmatheta1<-1
	sigmatheta2<-1
	sigmatheta3<-1
	sigmatheta4<-1
	sigmatheta5<-1
	sigmatheta6<-1
	sigmatheta7<-1
	sigmatheta8<-1
	sigmatheta9<-1
	sigmatheta10<-1
	sigmatheta11<-1
	sigmatheta12<-1
	sigmatheta13<-1
	sigmatheta14<-1
	sigmatheta15<-1
	sigmatheta16<-1
	sigmatheta17<-1
	sigmatheta18<-1
	sigmatheta19<-1
	sigmatheta20<-1
	sigmatheta21<-1
	sigmatheta22<-1
	sigmatheta23<-1
	sigmatheta24<-1
	sigmatheta25<-1
	sigmatheta26<-1
	sigmatheta27<-1
	sigmatheta28<-1
	sigmatheta29<-1
	sigmatheta30<-1
	sigmatheta31<-1
	sigmatheta32<-1
#
	N<-10000
	y<-matrix(NA,N,33)
	Lambda <-diag(c(sigmaalpha,sigmatheta1,sigmatheta2,sigmatheta3,sigmatheta4,sigmatheta5,sigmatheta6,sigmatheta7,sigmatheta8,sigmatheta9,sigmatheta10,sigmatheta11,sigmatheta12,sigmatheta13,sigmatheta14,sigmatheta15,sigmatheta16,sigmatheta17,sigmatheta18,sigmatheta19,sigmatheta20,sigmatheta21,sigmatheta22,sigmatheta23,sigmatheta24,sigmatheta25,sigmatheta26,sigmatheta27,sigmatheta28,sigmatheta29,sigmatheta30,sigmatheta31,sigmatheta32))
	Lambda1<-solve(Lambda)
	z<-solve(Lambda1+xtranspose%*%x)
	v<-z%*%xtranspose								
	

	for(i in 1 : 256){	
	if ((data[i,1]-data[i,2])>0){
game<-game+1
								}

else{
game1<-game1+1
	}
				  }
	g<-game+1
	G<-game+game1
	for (n in 1:N){
phi[n,1:game]<- mu[1:game] + s*qnorm(1 - runif(game)*pnorm(mu[1:game]/s),mu[1:game],1)
phi[n,g:G]<- mu[g:G] - s*qnorm(runif(game1)*pnorm(mu[g:G]/s),mu[g:G],1)
						#draw B from N(mubeta,sigmabeta)
						mubeta <- v%*%phi[n,]
						sigmabeta<- z
						beta1<-rmvnorm(1,mubeta,sigmabeta)
						
						y[n,]<-beta1}
						
						
levels(rawdata[,3])

igood <- 1:10000
apply(y[igood, ], 2, mean)
max(apply(y[igood, 2:33], 2, mean))
plot(y[,2])
						
##################################################
# for(i in 1 : 256){	
	# if ((data[i,1]-data[i,2])>0){
# game<-game+1
								# }

# else{
# game1<-game1+1
	# }
				  # }
# g<-game+1
# G<-game+game1
# for (n in 1:N){
# phi[1:game,n]<- mu + s*qnorm(1 - runif(game)*pnorm(mu/s))
# phi[g:G,n]<- mu - s*qnorm(runif(game1)*pnorm(mu/s))
# }







# # 







# s<- matrix(NA,4,2)
# s[1:2,1]<-1
# s[3:4,1]<-2







	# ###
	# # Example with ANOVA setup and factors:

# dd <- data.frame(a = gl(3,4), b = gl(4,1,12)) # balanced 2-way
# model.matrix(~ a + b, dd)

# #  Regression example
# model.matrix(log(Volume) ~ log(Height) + log(Girth), trees)

# #  Efficient sampling from truncated normal

# mu <- 1
# s <- 1

# x <- mu + s*qnorm(1 - runif(3)*pnorm(mu/s))

# X <- matrix(NA, 33, 1000)

# for (i in 1:1000) X[, i] <- mu + s*qnorm(1 - runif(33)*pnorm(mu/s))

# hist(X[1, ])
# hist(X[2, ])
# hist(X[3, ])
# ####
