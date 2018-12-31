
######################################
# section 4.2.3
######################################

# Using R to Perform a K-Means Analysis

# install packages, if necessary
library(plyr)
library(ggplot2)
library(cluster)
library(lattice)
library(graphics)
library(grid)
library(gridExtra)

setwd("Z:/SDSU/Courses/2018/Fall/MIS 620/Week 4/")
#import the student grades
grade_input <- as.data.frame(read.csv("grades_km_input.csv"))

names(grade_input)



kmdata_orig <- as.matrix(grade_input[,c("Student","English", "Math","Science")])
kmdata <- kmdata_orig[,2:4]




clust<-kmeans(kmdata, centers=3, nstart=25)
class(clust)
names(clust)
clust$withinss
clust$cluster


cbind(clust$cluster, grade_input)


#searching for optimal K
wss <- numeric(15) 
for (k in 1:15) 
{
  clust <- kmeans(kmdata, centers=k, nstart=25)
  
  wss[k] <- sum(clust$withinss)
}
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within Sum of Squares") 




set.seed(200)
km = kmeans(kmdata,3, nstart=25)
km
km$cluster
c( wss[3] , sum(km$withinss) )

#prepare the student data and clustering results for plotting
df <-  as.data.frame(kmdata_orig[,2:4])
df$cluster <-  factor(km$cluster)
centers <- as.data.frame(km$centers)


#show_guide now show.legend!!
g1 <-  ggplot(data=df, aes(x=English, y=Math, color=cluster )) + 
  geom_point() + theme(legend.position="right") +
  geom_point(data=centers, aes(x=English,y=Math, color=as.factor(c(1,2,3))), 
             size=10, alpha=.3, show.legend=FALSE)

g2 <- ggplot(data=df, aes(x=English, y=Science, color=cluster )) + 
  geom_point() + 
  geom_point(data=centers, aes(x=English,y=Science, color=as.factor(c(1,2,3))), 
             size=10, alpha=.3, show.legend=FALSE)

g3 <-  ggplot(data=df, aes(x=Math, y=Science, color=cluster )) + 
  geom_point() +
  geom_point(data=centers, aes(x=Math,y=Science, color=as.factor(c(1,2,3))), 
             size=10, alpha=.3, show.legend=FALSE)

tmp <-  ggplot_gtable(ggplot_build(g1)) 

#no MAIN parameter only top!!!!

grid.arrange(arrangeGrob(g1 + theme(legend.position="none"),
                         g2 + theme(legend.position="none"),
                         g3 + theme(legend.position="none"),
                         top ="High School Student Cluster Analysis", ncol=1))




