#P(Y_{ijt}=1)=F(x_{ijt}aplha+\theta_i-theta_j),which means team i beat team j;
#Xijt = 1, which means team i is in home game;
#let $\psi_{ijt}=\mu_{ijt}+\epsilon_{ijt}$
#epsilon_{ijt}~F(),which F() is symmetric
#Pr(\psi_{ijt}>=0)=Pr(\mu_{ijt}+\epsilon_{ijt}>=0)=Pr(\epsilon>=-\mu_{ijt})=F(\mu_{ijt})
#
#
#
N <-4
x <- matrix (NA,6,N)
for (t in 1:N){

thetaM<-function(thetaA,thetaB,thetaC,psiij,alpha1){rnorm(1,-2/5*(runif(1)+3*alpha1-2*thetaA+4),-1/5)}

thetaA<-function(thetaM,thetaB,thetaC,psiij,alpha1){rnorm(1,-9/2+3*runif(1)-3*alpha1+2*thetaM-2*thetaC,-1)}

thetaB<-function(thetaA,thetaM,thetaC,psiij,alpha1){rnorm(1,-9/2-3*alpha1,-1)}

thetaC<-function(thetaA,thetaB,thetaM,psiij,alpha1){rnorm(1,5-runif(1)+3*alpha1+2*thetaA,1/5)}

psiij<-function(thetaA,thetaB,thetaC,thetaM,alpha1){(1,	qnorm((1/2*rnuif(1)+1/2),0,?),1)}

sigma_aplha1<- (1/(1+6))#6=sigma{x_{ij}}
alpha1<-function(thetaA,thetaB,thetaC,psiij,thetaM){(1,,sigma_alpha1)}
}
plot(x[1,],x[2,],x[3,],x[4,],x[5,],x[6,])