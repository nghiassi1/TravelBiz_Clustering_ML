###Key Questions

##What patterns do you observe in the data (exploratory data analysis)? (5 points)
##PCA: What type of relationships do you see among the variables? What are the outcomes of a Principal Component Analysis? (25 points)
##CLUSTERING: Build a Machine Learning Clustering Model to identify and describe segments of users based on their ratings of attributes. (50 points)
##Provide your recommendations on travel product packages, positioning and targeting of potential customers to the travel management team based on your analysis. (20 points)

library(tidyr)
library(dplyr)
library(ggplot2)
library(factoextra) #For PCA
library(corrplot) #For some correlation and PCA plots
library(cluster)
library(NbClust) #For finding optimal number of clusters
library(clValid) #For cluster validation

## Read in Data
setwd("C:/Users/nghiassi/Documents/MSBA")
travel_data <- readxl::read_xlsx("Travel_Review.xlsx")
## Summary
summary(travel_data)
travel_data_c <- travel_data[,2:25]

###Histograms
library(Hmisc)

hist.data.frame(travel_data_c)
colSums(is.na(travel_data_c))

####Because all attributes are on the same 0-5 scale and low scores or lack of ratings are both valid 
##It was decided to not remove any outliers. There was 1 row with an N/A and that was removed for data integrity

####only one record has NA values; drop record
travel_data_c <-drop_na(travel_data_c)

###PCA 1

trav_pca = prcomp(travel_data_c, scale. = TRUE)
trav_pca
#The first 3 components account for 41.9% of variation
get_eig(trav_pca) #Obtain eigenvalues
fviz_eig(trav_pca, addlabels=TRUE)

#Obtaining some results for variable analysis:
var = get_pca_var(trav_pca)

#graph of variables mapped over the first two components
###Looks like 
fviz_pca_var(trav_pca,
             repel=TRUE) 


#Contribution of variables to PCs:
corrplot(var$contrib, is.corr=FALSE) #Visualize quality of variables


#Bar plots of variables contributions, sorted from highest to lowest
fviz_contrib(trav_pca, choice = "var", axes = 1) # Contributions of variables to PC1
fviz_contrib(trav_pca, choice = "var", axes = 2) # Contributions of variables to PC2
fviz_contrib(trav_pca, choice = "var", axes = 3) # Contributions of variables to PC3
fviz_contrib(trav_pca, choice = "var", axes = 4) # Contributions of variables to PC4
fviz_contrib(trav_pca, choice = "var", axes = 5) # Contributions of variables to PC5



### Clustering
##Scale data
travel_data_s <- scale(travel_data_c)
###find proximity matrix
distance = get_dist(travel_data_s)


set.seed(2023) # Set a seed if you want to make the results reproducible
k3 = kmeans(travel_data_s, centers=3, nstart=25) ###Inital K = 3 K-means

k3 #Print the results
k3$size #size of each cluster
k3$centers #centroid of each cluster

#Scatter plot of pizzas using two of the variables,
#color-coded by cluster labels
fviz_cluster(k3, data=travel_data_s)

###Elbow Method
fviz_nbclust(travel_data_s, kmeans, k.max=10, nstart=25, method="wss")

####Silhouette
fviz_nbclust(travel_data_s, kmeans, k.max=10, nstart=25, method="silhouette")


### Clustering K = 2

set.seed(2023) # Set a seed if you want to make the results reproducible
k2 = kmeans(travel_data_s, centers=2, nstart=25)

k2 #Print the results
k2$size #size of each cluster
k2$centers #centroid of each cluster

fviz_cluster(k2, data=travel_data_s)

##K - 4

set.seed(2023) # Set a seed if you want to make the results reproducible
k4 = kmeans(travel_data_s, centers=4, nstart=25)

k4 #Print the results
k4$size #size of each cluster
k4$centers #centroid of each cluster

fviz_cluster(k4, data=travel_data_s)


##K - 5

set.seed(2023) # Set a seed if you want to make the results reproducible
k5 = kmeans(travel_data_s, centers=5, nstart=25)

k5 #Print the results
k5$size #size of each cluster
k5$centers #centroid of each cluster

fviz_cluster(k5, data=travel_data_s)


sile = silhouette(k4$cluster, dist(travel_data_s))
fviz_silhouette(sile)

##Pam 5
pam5<-pam(travel_data_s,5)
fviz_cluster(pam5, data = travel_data_s)

##Pam 3
pam3<-pam(travel_data_s,3)
fviz_cluster(pam3, data = travel_data_s)


###Hierarchical 

hclus = hclust(d=distance, method="complete")

#dendrogram: cex controls label size
fviz_dend(hclus, cex=0.1)

##cut tree into 4
cut_hc_4 = cutree(hclus, k=4)

table(cut_hc) #number of members in each cluster
fviz_cluster(list(data = travel_data_s, cluster = cut_hc_4),
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
             ellipse.type = "convex", # Concentration ellipse
             repel = FALSE, # Allow label overplotting (slow)
             show.clust.cent = FALSE)

##cut tree into 3
cut_hc_3 = cutree(hclus, k=3)

table(cut_hc_3) #number of members in each cluster
fviz_cluster(list(data = travel_data_s, cluster = cut_hc_3),
             palette = c("#FC4E07", "#2E9FDF", "#E7B800"),
             ellipse.type = "convex", # Concentration ellipse
             repel = FALSE, # Allow label overplotting (slow)
             show.clust.cent = FALSE)


# Attach cluster membership info to the original dataset and compare variable averages of each group
travel_clus_4 = cbind(travel_data_c, cluster=k4$cluster)
head(travel_clus) 

#profile clusters taking mean of the numerical variables
aggregate(travel_clus_4, 
          by=list(travel_clus$cluster), 
          mean)

####Cluster 3 

travel_clus_3 = cbind(travel_data_c, cluster=k3$cluster)
head(travel_clus_3) 

#profile clusters taking mean of the numerical variables
aggregate(travel_clus_3, 
          by=list(travel_clus_3$cluster), 
          mean)

write.csv(travel_clus_3, "cluster3.csv")
write.csv(travel_clus_4,"cluster4.csv")

###Cluster 2
travel_clus_2 = (cbind(travel_data_c, cluster = k2$cluster))
write.csv(travel_clus_2,"cluster2.csv")


clmethods <- c("hierarchical","kmeans","pam")
intern <- clValid(travel_data_s, nClust = 2:6, 
                  clMethods = clmethods, validation = "internal")

summary(intern)

cut_hc_2 = cutree(hclus, k=2)

fviz_cluster(list(data = travel_data_s, cluster = cut_hc_2),
             palette = c("#2E9FDF", "#FC4E07"),
             ellipse.type = "convex", # Concentration ellipse
             repel = FALSE, # Allow label overplotting (slow)
             show.clust.cent = FALSE)

travel_clus_2_Hier = (cbind(travel_data_c, cluster = cut_hc_2))

aggregate(travel_clus_2_Hier, 
          by = list(travel_clus_2_Hier$cluster),mean)


fviz_silhouette(silhouette(k4$cluster, dist(travel_data_s))) ##k=4
fviz_silhouette(silhouette(k3$cluster, dist(travel_data_s))) ## k=3
fviz_silhouette(silhouette(k2$cluster, dist(travel_data_s))) ##K = 2
fviz_silhouette(silhouette(pam3$cluster, dist(travel_data_s))) ##pam k = 3
fviz_silhouette(silhouette(pam5$cluster, dist(travel_data_s))) ##pam k = 5
