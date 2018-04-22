load("D:\\study\\Praxis material\\Machine Learning\\Data\\R data.RData")
data=na.omit(bankloan[,c(1,3,4,5,6,7)])
head(data)

col=sapply(2:20,FUN = function(x)kmeans(scale(data),centers = x,nstart = 40)$tot.withinss)
plot(2:20,col,type='b')

#kmeans will give good results if data is linearly seperable
#Silhoutte avg distance
#Tries to find out what is seperation between (a point and distance between 
# all other points of all clusters ) labelled as 'b' and also how much seperation between that 
# point and other points of same cluster labelled as 'a'
#formula: (b-a)/max(b,a)

fit1=kmeans(scale(data),centers = 5,nstart = 50,algorithm = 'Lloyd',iter.max = 100)
??silhoutte
#install.packages('cluster')
require(cluster)
s=silhouette(x = fit1$cluster,dist = dist(scale(data)),iter.max=100)
s

avg_sil=function(centers,dataset,scale=T){
  
  if(scale == T){
    df=scale(dataset)
  }else df=dataset
  
  fit=kmeans(df,centers = centers,nstart = 100,algorithm = 'Lloyd',iter.max = 100)
  cluster=fit$cluster
  s=silhouette(x=cluster,dist = dist(df))
  mean(s[,3])
}

avg_sil(centers = 5,dataset = data,scale = T)
#silhouette(fit1$cluster,dist = dist(data))
sapply(2:20,avg_sil,dataset=data,scale=T)


fit2=kmeans(scale(data),centers = 50,iter.max = 100,nstart = 100,algorithm = 'Lloyd')
sort(fit2$size)
sum(fit2$size)
sum(fit1$size)
data[fit2$cluster==11,]
fit2$size
fit2$cluster
#####################################

data1=insurance[,c(5,9,17)]

require(lubridate)
data1$age=as.numeric(substring(dmy(x = as.character(insurance$incident_date)),1,4))-as.numeric(substring(dmy(x = as.character(insurance$dob)),1,4))

col=sapply(2:10,FUN = function(x)kmeans(scale(data1),centers = x,nstart = 40)$tot.withinss)

plot(2:10,col,type = 'b')

sapply(2:10,avg_sil,dataset=data1,scale=T)
