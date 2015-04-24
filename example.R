gibbs <- function(n.sims,beta.start,alpha,gamma,deta,y,r,burnin<-0,thin<-1){
	beta.draws<- c()
	lambda.draws<-matrix(NA,nrow<-n.sims,ncol<-length(y))
	beta.cur<-beta.start`
	lambda.update<-function(alpha,beta,y,t){
		rgama(length(y),y+lpha,t+beta)
	}
beta.update<-function(alpha,gamma,delta,lambda,y){
	rgama(1,length(y)*alpha+gamma,delta+sum(lambda))
}
for(i in 1:n.sims){
	lambda.cur<- lambda.update(alpha<-alpha,beta<-beta.cur,y<-y,t<-t)
	beta.cur<-beta.update(alpha<-alpha,gamma<-gamma,delta<-delta,lambda<-lambda.cury<-y)
if (i>burnin & (i-burnin)%%thin<-0){
	lambda.draws[(i-burnin)/thin,]<-lambda.cur
	beta.draws[(i-burnin)/thin]<-betacur
}
}
return(list(lambda.draws <- lambda.draws, beta.draws<-beta.draws))
}