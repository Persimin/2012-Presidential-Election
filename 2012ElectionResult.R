library(XML)

## Source 1

state = read.csv("http://www.stat.berkeley.edu/~nolan/data/Project2012/countyVotes2012/stateNames.txt")

link = "http://www.stat.berkeley.edu/users/nolan/data/Project2012/countyVotes2012/xxx.xml"

statename = as.vector(state[,1])


a = xmlParse(gsub("xxx", statename[1], link))
b = xmlRoot(a)

c2 = xpathSApply(b,"//tbody",xmlGetAttr,"id")
ID = gsub("county","",c2)

c = xpathSApply(b,"//th[@class ='results-county']",xmlValue)
c1 = gsub("Reporting","",c)

d1 = gsub("[[:space:][:digit:]]+[[:punct:]]","",c1)
d2 = gsub("[[:blank:]]$","",d1)
d2=paste(d2,", Alabama")

County = d2[-1]

Democrat = xpathSApply(b,"//tr[@class = 'party-democrat']/td[@class = 'results-percentage'] | //tr[@class = 'party-democrat race-winner']/td[@class = 'results-percentage']",xmlValue)

Republican = xpathSApply(b,"//tr[@class = 'party-republican']/td[@class = 'results-percentage'] | //tr[@class = 'party-republican race-winner']/td[@class = 'results-percentage']",xmlValue)

source1 = data.frame(County, ID, Republican, Democrat)


for(i in 3:length(statename)) {
  
  a = xmlParse(gsub("xxx", statename[i], link))
  b = xmlRoot(a)
  
  c2 = xpathSApply(b,"//tbody",xmlGetAttr,"id")
  ID = gsub("county","",c2)
  
  c = xpathSApply(b,"//th[@class ='results-county']",xmlValue)
  c1 = gsub("Reporting","",c)
  
  d1 = gsub("[[:space:][:digit:]]+[[:punct:]]","",c1)
  d2 = gsub("[[:blank:]]$","",d1)
  d2=paste(d2,",",statename[i])
  County = d2[-1]
  
  
  Democrat = xpathSApply(b,"//tr[@class = 'party-democrat']/td[@class = 'results-percentage'] | //tr[@class = 'party-democrat race-winner']/td[@class = 'results-percentage']",xmlValue)
  
  Republican = xpathSApply(b,"//tr[@class = 'party-republican']/td[@class = 'results-percentage'] | //tr[@class = 'party-republican race-winner']/td[@class = 'results-percentage']",xmlValue)
  
  g = data.frame(County, ID, Republican, Democrat)
  
  source1 = rbind(source1, g)
  
}
################################

## Source 2 
raceinfo=read.csv("http://www.stat.berkeley.edu/~nolan/data/Project2012/census2010/B01003.csv")
sociodata=read.csv("http://www.stat.berkeley.edu/~nolan/data/Project2012/census2010/DP02.csv")
econdata=read.csv("http://www.stat.berkeley.edu/~nolan/data/Project2012/census2010/DP03.csv")

#create race information dataframe
WPop=raceinfo[raceinfo$POPGROUP.id==2,c(1,6:7)]
names(WPop)[names(WPop)=="HD01_VD01"]="White.Pop"
AAPop=raceinfo[raceinfo$POPGROUP.id==4,c(1,6:7)]
names(AAPop)[names(AAPop)=="HD01_VD01"]="Black.AfricanAmerican.Pop"
TPop=raceinfo[raceinfo$POPGROUP.id==1,-7]
names(TPop)[names(TPop)=="HD01_VD01"]="Total.Pop"
raceinfo1=merge(TPop,WPop,by="GEO.id",all.x=TRUE)
raceinfo2=merge(raceinfo1,AAPop,by="GEO.id",all.x=TRUE)

#combaine socio and economic dataframes together
comnames=c("GEO.id","GEO.id2","GEO.display.label","POPGROUP.id","POPGROUP.display.label")
ESdata=merge(econdata,sociodata,by=comnames,all.x=TRUE)

#dataframe with race,economic,socio information
Cendata=merge(ESdata,raceinfo2,by=comnames,all.x=TRUE)


ex=grep(".*Puerto[[:blank:]]Rico|Alaska",Cendata$GEO.display.label,perl=TRUE)
names(Cendata)[3]="County"
names(Cendata)[2]="ID"
source2 = Cendata[-ex,] #doesn't contain Puerto Rico and Alaska

#merge part 1 and part 2
source12 = merge(source1,source2,by="ID",all.x=TRUE)
names(source12)[6]="County"
w=which(is.na(source12$County))
m=as.character.factor(source12$County.x[w])
m=toupper(m)
m=gsub(" ","",m)
m=gsub(",","COUNTY,",m)
j=as.character.factor(source12$County)
j[1773]="DONAANACOUNTY,NEWMEXICO"
j=toupper(j)
j[w]=m
j=gsub("-","",j)
j=gsub(" ","",j)
source12$County=j

source12$County.x=NULL

#####################################

## Source 3 

p=xmlParse("http://www.stat.berkeley.edu/~nolan/data/Project2012/counties.gml")
root=xmlRoot(p)
state=xpathSApply(root,"//state/gml:name",xmlValue)
state=gsub("\n   ","",state)

num=numeric()
for(i in 1:length(state)){
  num[i]=xmlSize(root[[i]])-1}

c=rep(state,times=num)

name=xpathSApply(root,"//county/gml:name",xmlValue)
name1=gsub("\n    ","",name)
X=xpathSApply(root,"//gml:X",xmlValue)
x1=gsub("\n      ","",X)
Y=xpathSApply(root,"//gml:Y",xmlValue)
y1=gsub("\n      ","",Y)

County=numeric()
for(i in 1:length(c)){
  County[i]=paste(name1[i],",",c[i])}

ex=grep(".*ALASKA",County)

source3=data.frame(County=County,Latitude=x1,Longitude=y1)
source3=source3[-ex,]
l=toupper(source3$County)
l=gsub("-","",l)
l=gsub(" ","",l)

source3$County=l

#merge all three parts
finalDF = merge(source3,source12 ,by="County",all.x=TRUE)

# Our final data frame is finalDF.

################################################ Step 3 ###########################################################

result2004 = read.csv("http://www.stat.berkeley.edu/~nolan/data/Project2012/countyVotes2004.txt",sep = "")
# 2975 data
result = vector()
for (i in 1:nrow(result2004)) {
  if (result2004[i,2] > result2004[i,3]) {
    result[i] = "Republican"
  } else {
    result[i] = "Democrat"
  }
}

c = toupper(result2004[,1])
county1 = gsub(".+,","",c)
state1 = gsub(",[[:alpha:]]+[[:space:]]*[[:alpha:]]*[[:space:]]*[[:alpha:]]*[[:space:]]*[[:alpha:]]*$","",c)
fullname = paste(county1,state1,sep=",")
fullname = gsub(" ","",fullname)

winner2004 = data.frame(fullname,result)

names(winner2004) = c("County","2004 Winner")
winner2004 = winner2004[order(winner2004$County),]

# race, income, 
x = (finalDF$White.Pop / finalDF$Total.Pop)*100
p = finalDF[,c("County","Republican","Democrat","Latitude","Longitude","HC03_VC08.x","HC03_VC10.x","HC03_VC41",
               "HC03_VC62.x","HC03_VC54.x","HC03_VC58","HC03_VC59","HC03_VC75.x",
               "HC03_VC84.x","HC03_VC90.x","HC03_VC92","HC03_VC95","HC03_VC97","HC03_VC157",
               "HC03_VC168.y","HC03_VC134","HC03_VC91.y")]

preddf = data.frame(p, x)

names(preddf) = c("County","Republican","Democrat","Latitude","Longitude","Unemployed.pct","Not.in.LF","MgtJob","PublicWorker","Retail",
                  "ScienceField","SocialWorker","Below10K","Above200K","SocialSecurity","RetireIncome","SSI",
                  "CashPubAssistance","Poverty","NonEnglish","ForeignBorn","MasterPHD","WhitePop")

a = finalDF$Republican
a = as.numeric(gsub("%","",a))
b = finalDF$Democrat
b = as.numeric(gsub("%","",b))

#######################
a=preddf[,1]
a=gsub("COUNTY","",a)
b=gsub("PARISH","",a)
c=gsub("CITY","",b)
d=gsub("^ST[[:punct:]]","ST",c)
d = gsub("'","",d)
preddf[,1]=d
x=winner2004[,1]
x = gsub("CITY","",x)
winner2004[,1]=x
g=merge(winner2004,preddf,by="County",all.x=F)
g$Latitude=as.numeric(levels(g$Latitude))[g$Latitude]/1000000
g$Longitude=as.numeric(levels(g$Longitude))[g$Longitude]/1000000
l=c(266,667,2523,2800)
g = g[-l,]



rep12 = g$Republican
rep12 = as.numeric(gsub("%","",rep12))

dem12 = g$Democrat
dem12 = as.numeric(gsub("%","",dem12))


result12 = vector()
for (i in 1:length(g$Republican)) {
  if (rep12[i] > dem12[i]) {
    result12[i] = "Republican"
  } else {
    result12[i] = "Democrat"
  }
}

g = data.frame(g, result12)

comdf = g[,c(1,2,25,5:24)]
names(comdf)[names(comdf)=="X2004.Winner"]="Winner2004"
names(comdf)[names(comdf)=="result12"] = "Winner2012"

#### Step 3 Part A Recursive Partitioning  ########## 

library(rpart)
library(rpart.plot)



fit <- rpart(Winner2004~Latitude+Longitude+Unemployed.pct+MgtJob+PublicWorker+Not.in.LF+MgtJob+Retail+ScienceField+
               SocialWorker+Below10K+Above200K+SocialSecurity+RetireIncome+SSI+CashPubAssistance+
               Poverty+NonEnglish+ForeignBorn+MasterPHD+WhitePop,
             data=comdf, method="class",
             control=rpart.control(minsplit=40, cp=0.0045))


rpart.plot(fit, type = 0, cex=0.7)



#######################################Step3PartB############################################
library(class)
N=logical()
for(i in 1:ncol(comdf)){
  N=is.na(comdf[,i])
}
n=which(N)
comdf=comdf[-n,]

prediction2012=knn.cv(train=comdf[,4:23],cl=comdf$Winner2004,k=7,prob=TRUE)



#################### Step 4 Part A #####################

predr = predict(fit, newdata = comdf, type = c("prob"))
predr = as.data.frame(predr)


rprediction = vector()
for (i in 1:length(predr$Republican)) {
  if (predr$Republican[i] > predr$Democrat[i]) {
    rprediction[i] = "Republican"
  } else { 
    rprediction[i] = "Democrat"
  }
}

actual12 = comdf$Winner2012

correctpred = length(which(actual12 == rprediction))

accuracy = correctpred/nrow(comdf)

# "accuracy" is the accuracy rate of rpart model in predicting result of 2012 election results. 
# Prediction of 2012 election based on rpart model is 88.67% accurate. 


###############Step 4 Part A Plots #############
###plot based on different values of k
kvalues=1:20
prediction=sapply(kvalues,function(x)(knn.cv(train=comdf[,4:23],cl=comdf$Winner2004,k=x,prob=TRUE)))
err=apply(prediction,2,function(x)mean(comdf$Winner2012!=x))
plot(err~kvalues,type="l",xlab="Number of K-Values",ylab="Error Rate in Predcition",ylim=c(0.12,0.17),main="Prediction Error Rate for Different K-Values(1:20)",xaxt="n",col="blue",lwd=5)
axis(1, at = seq(1, 20, by = 1), las=2,font=0.5)
###plot based on different number of variables
colnumber=4:23
nvar=1:20
prednvar=sapply(colnumber,function(x)(knn.cv(train=comdf[,4:x],cl=comdf$Winner2004,k=7,prob=TRUE)))
errnvar=apply(prednvar,2,function(x)mean(comdf$Winner2012!=x))
plot(errnvar~nvar,type="l",main="Error Rate on Different Number of Variables",ylab="Error Rate in Predcition",xlab="Number of Variables",
     xaxt="n",col="red",lwd=5)
axis(1, at = seq(1, 20, by = 1), las=2,font=0.5)

###plot error rate based on each variable
varnumb=4:23
predvar=sapply(varnumb,function(x)(knn.cv(train=comdf[,x],cl=comdf$Winner2004,k=7,prob=TRUE)))
errvar=apply(predvar,2,function(x)mean(comdf$Winner2012!=x))
var=names(comdf[varnumb])
plot(errvar~varnumb,type="p",main="Error Rate of Different Individual Variable",ylab="Error Rate",xlab="Individual Variable",cex=0.8,pch=19,xaxt="n",col.axis="green")
axis(1, at = seq(4, 23, by = 1), las=2,font=0.5,col.axis="purple",cex.axis=0.5,labels=c("Latitude","Longitude","Unemployed.pct","Not.in.LF","MgtJob","PublicWorker","Retail",
                                                                                        "ScienceField","SocialWorker","Below10K","Above200K","SocialSecurity","RetireIncome","SSI","CashPubAssistance","Poverty","NonEnglish","ForeignBorn","MasterPHD","WhitePop"))


###########step4 PartB################

resultBush = vector()
resultKerry = vector()
for (i in 1:nrow(result2004)) 
{
  resultBush[i] = result2004[i,2] / (result2004[i,2] + result2004[i,3])
  resultKerry[i] = result2004[i,3] / (result2004[i,2] + result2004[i,3])
}
percentage2004 = data.frame(fullname,resultBush,resultKerry)
names(percentage2004) = c("County","Republican2004","Democrat2004")
percentage2004 = percentage2004[order(percentage2004$County),]
percentage2012=preddf[,1:5]
percentage2012$Latitude=as.numeric(levels(percentage2012$Latitude))[percentage2012$Latitude]/1000000
percentage2012$Longitude=as.numeric(levels(percentage2012$Longitude))[percentage2012$Longitude]/1000000
percentage2012$Republican = as.numeric(gsub("%","",percentage2012$Republican))/100
percentage2012$Democrat = as.numeric(gsub("%","",percentage2012$Democrat))/100


dfpt4=merge(percentage2004,percentage2012,by="County")


names(dfpt4) = c("County","Republican2004","Democrat2004","Republican2012","Democrat2012","Latitude","Longitude")


################## Map #############

install.packages("maps")

library(maps)


map(database = "state", col="gray90",fill=TRUE)

for(i in 1:nrow(dfpt4)){
  if(dfpt4[i,2] > dfpt4[i,3] & dfpt4[i,4] > dfpt4[i,5]){
    points(dfpt4[i,6],dfpt4[i,7],pch=16, col = "red", cex = 0.7)
  }
  else if(dfpt4[i,2] < dfpt4[i,3] & dfpt4[i,4] < dfpt4[i,5])
  {
    points(dfpt4[i,6], dfpt4[i,7],col="blue",pch=16, cex = 0.7)
  }  
  else if(dfpt4[i,2] < dfpt4[i,3] & dfpt4[i,4] > dfpt4[i,5])
  {
    points(dfpt4[i,6], dfpt4[i,7],col="green",pch=16, cex = 0.7)
  }
  else if(dfpt4[i,2] > dfpt4[i,3] & dfpt4[i,4] < dfpt4[i,5])
  {
    points(dfpt4[i,6], dfpt4[i,7],col="yellow",pch=16, cex = 0.7)
  }
}

legend(-125,30,cex=0.7,c("Republican wins in 2004 and 2012","Democrat wins in 2004 and 2012", "Democrat wins in 2004, but loses in 2012",
                         "Republican wins in 2004, but loses in 2012"
),col=c("Red","Blue","green","yellow"),pch=c(16,16,16,16))
title("United States Presidential Election Results and Changes in 2004 and 2012")
