library(gdata)
wldata=read.xls("winlossdata.xls")
#prep
x<-matrix(0,12,5)
x[,1]=1
data<-matrix(NA,12,2)



for (i in 1:12){
	#extract home away information
if (wldata[i,1]=="A"){
		x[i,2]=1}
	else if(wldata[i,1]=="B"){
		x[i,3]=1}
	else if(wldata[i,1]=="C"){
		x[i,4]=1}
	else if(wldata[i,1]=="D"){
		x[i,5]=1}
if(wldata[i,3]=="A"){
					x[i,2]=-1}
	else if(wldata[i,3]=="B"){
							 x[i,3]=-1}
	else if(wldata[i,3]=="C"){
							 x[i,4]=-1}
	else if(wldata[i,3]=="D"){
							 x[i,5]=-1}
#extract win loss information				
if (wldata[i,2]>wldata[n,4]){
				data[i,1]<-1
				data[i,2]<-0
										}
			# else if (wldata[i,2]<wldata[i,4]){
				# data[i,1]<-0
				# data[i,2]<-1
		 		# }				
				}
data