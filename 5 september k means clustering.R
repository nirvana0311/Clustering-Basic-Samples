data1=na.omit(autompg[1:100,3:4])
plot(data1,pch=20)

kmeans_fit=kmeans(scale(data1),centers = 3)
plot(data1,pch=20,col=kmeans_fit$cluster)
kmeans_fit$totss
kmeans_fit$tot.withinss

kmeans_fit1=kmeans(scale(data1),centers = 3,nstart = 40)
plot(data1,pch=20,col=kmeans_fit1$cluster)
plot(kmeans_fit1$centers)
kmeans_fit1$totss
kmeans_fit1$tot.withinss

boxplot(data1$displacement)
boxplot(data1$horsepower)

#install.packages('Rlof')
require(Rlof)
#To find multivariate outlier use above package

#To decide optimum number of k

sst1=sapply(2:20,function(x) kmeans(scale(data1),centers = x,nstart = 40)$tot.withinss)
plot(2:20,sst1,type = 'b')





#without scale
lol=sapply(2:20,FUN = function(x)kmeans(data1,centers = x,nstart = 40)$tot.withinss)
plot(2:20,lol,type = 'b')

data("iris")
data=iris[,1:4]

kol=kmeans(data,centers = 3,nstart = 40)


plot(iris$Sepal.Width,iris$Petal.Width,col=kol$cluster,pch=20)
kol$cluster

iris$Species=ifelse(iris$Species=='setosa',2,ifelse(iris$Species=='versicolor',3,1))

table(kol$cluster,iris$Species)
require(caret)
confusionMatrix(kol$cluster,iris$Species)


(36+50+48)/150

