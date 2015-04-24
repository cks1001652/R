##home away generator
#count variable
game<-0
#team number
m<- 10
#they play by each oother 2 twice
n<- team*(team-1)
x<- matrix(0,n,team)
home <- away <- 1
for (home in 1:team){
	for (away in 1:team){
		if (home != away){
						 game <- game +1
						 x[game,home]<- 1
						 x[game,away]<- -1
						 }
					  }
				 }

x

#win loss generator

#game
n
#
s<-sample(0:3)