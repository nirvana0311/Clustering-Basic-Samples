#k-means clustering gets affected by outliers
#Four different linkage functions available for hierarchial clustering
#Single,complete,group,ward
#Linkage is used Tofind out distance between two clusters
#Single linkage will calculate the lowest distance between 2 points from 2 different clusters
#Complete Linkage is opposite of single linkage
#minimum similarity and maximum linkage
#With single linkage you always end up with chain like structure
#Lowest of maximum distance will be clubbed in one cluster
#In Group average linkage distance between centroid is calculated
#Dendogram

hc1=hclust(dist(scale(bankloan[250:300,-c(9)])),method = 'complete')
?hclust
plot(hc1,hang = -1)

rect.hclust(hc1,k = 3)
cutree(tree = hc1,k = 4)
 
table(cutree(hc1,k = 3))
table(cutree(hc1,k = 3),cutree(hc1,k = 4))
