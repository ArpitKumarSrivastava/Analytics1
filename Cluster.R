# Clustering

set.seed(1234)
subject1 = trunc(rnorm(30, mean = 60, sd = 15))
range(subject1)
subject1

#cluster them

marks = data.frame(subject1)
head(marks)

# cluster is unsupervisied algorithms 
#creating cluster
#-------------------------------#
k2 = kmeans(marks, centers = 2)     #use this algorithm || kmean is used to create the cluster
#-------------------------------#
k2

#determining cluster summary manually
k2$size
marks[k2$cluster==1,]     # if we want to look at the elements of cluster 1
marks[k2$cluster==2,]     # if we want to look at the elements of cluster 2

length(marks[k2$cluster==1,] )     # no. of elements in cluster 1 

k2$centers    # give the mean of both the cluster seperately

k2
