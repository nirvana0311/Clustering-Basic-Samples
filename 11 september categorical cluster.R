data=read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/cpu-performance/machine.data",header = F)
data=read.csv()
file.choose()
#hierarchial clustering is good only when data points is low, like 5000, max 10000
#Bcoz R has a tough time with many data points
#K-means and hierarchial clustering isn't good for categorical data
#
cluster_data=data[,-c(1,2,9,10)]
hc=hclust(dist(scale(cluster_data)))
plot(hc,hang = -1)
rect.hclust(hc,k = 3)
clusters=cutree(tree = hc,h = 6)
table(clusters)

cluster_data[clusters==1,]
cluster_data[clusters==2,]
cluster_data[clusters==3,]
cluster_data[clusters==10,]


cluster_data[c(T,T,T,F),]
head(cluster_data)

require(cluster)
s=silhouette(x = clusters,dist = dist(scale(cluster_data)))
mean(s[,3])

clusters_6=cutree(tree = hc,k = 6)
s=silhouette(x = clusters_6,dist = dist(scale(cluster_data)))
mean(s[,3])


clusters_3=cutree(tree = hc,k = 3)
s=silhouette(x = clusters,dist = dist(scale(cluster_data)))
mean(s[,3])



glassData=read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/glass/glass.data",header = F)
head(glassData)
glassData=glassData[,-c(1,11)]
hc1=hclust(d = dist(scale(glassData)))
plot(hc1,hang=-1)
rect.hclust(hc1,k = 3)
clusters1=cutree(tree = hc1,k = 3)

for(i in 2:10){
  clusters1=cutree(tree = hc1,k = i)
s1=silhouette(x = clusters1,dist = dist(scale(glassData)))
print(i)
print(mean(s1[,3]))
}

table(clusters1)
#Try density based clustering to remove outliers and focus on meat of clusters in center
tail(glassData)
glassData=glassData[!c(clusters1==10),]
glassData=glassData[!c(clusters1==9),]
glassData=glassData[!c(clusters1==4),]
glassData=glassData[!c(clusters1==5),]
glassData=glassData[!c(clusters1==8),]
glassData=glassData[!c(clusters1==7),]
glassData

