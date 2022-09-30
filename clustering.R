#Cluster analysis
library(factoextra)
#Data preparation
#To make sure we have consistent results for clustering, we only keep the complete responeses
#for all scale questions (meaning the ones who completed all 3 scales)
setwd("~/Desktop/covid19_survey")
covid = read.csv("covid19_data.csv")
#First remove the ones that didn't complete CAS.
covid_clu_1 = covid[-c(which(is.na(covid$CAS_1)==TRUE|is.na(covid$CAS_2)==TRUE|is.na(covid$CAS_3)==TRUE|
                             is.na(covid$CAS_4)==TRUE|is.na(covid$CAS_5)==TRUE)),]#1911 left
#From covid_clu_1, delete the ones that didn't complete PSS
covid_clu_2 = covid_clu_1[-c(which(is.na(covid_clu_1$PSS_1)==TRUE|is.na(covid_clu_1$PSS_2)==TRUE|is.na(covid_clu_1$PSS_3)==TRUE|
                             is.na(covid_clu_1$PSS_4)==TRUE|is.na(covid_clu_1$PSS_5)==TRUE|is.na(covid_clu_1$PSS_6)==TRUE|is.na(covid_clu_1$PSS_7)==TRUE|is.na(covid_clu_1$PSS_8)==TRUE|is.na(covid_clu_1$PSS_9)==TRUE
                           |is.na(covid_clu_1$PSS_10)==TRUE)),]
#1726 responese left
#From covid_clu_2, delete the ones that didn't complete MDI
covid_clu = covid_clu_2[-c(which(is.na(covid_clu_2$MDI_1)==TRUE|is.na(covid_clu_2$MDI_2)==TRUE|is.na(covid_clu_2$MDI_3)==TRUE|
                             is.na(covid_clu_2$MDI_4)==TRUE|is.na(covid_clu_2$MDI_5)==TRUE|is.na(covid_clu_2$MDI_6)==TRUE|is.na(covid_clu_2$MDI_7)==TRUE
                             |is.na(covid_clu_2$MDI_8)==TRUE|is.na(covid_clu_2$MDI_9)==TRUE|is.na(covid_clu_2$MDI_10)==TRUE|is.na(covid_clu_2$MDI_11)==TRUE|is.na(covid_clu_2$MDI_12)==TRUE)),]
#1701 responses left for covid_clu. covid_clu is the data we're using for clustering
#Since the variables selection was done, we chose the following demographics question to see if we 
#are able to get the same cluster as the PSS score indicates. 
#From covid_clu_1, delete the ones that didn't complete PSS
covid_clu_pss = covid_clu_2
#Select Q5,6,11,24, 25, 26A, 1.1, make sure 
covid_clu_pss = covid_clu_pss[-c(which(is.na(covid_clu_pss$Q5)==TRUE|is.na(covid_clu_pss$Q6)==TRUE|is.na(covid_clu_pss$Q1.1)==TRUE|is.na(covid_clu_pss$Q11)==TRUE)),]
attach(covid_clu_pss)
covid_clu_pss$PSS_total = (PSS_1+PSS_2+PSS_3+ PSS_6+PSS_9+PSS_10 -6)+(5-PSS_4)+(5-PSS_5)+(5-PSS_7)+(5-PSS_8) 
detach(covid_clu_pss)
covid_clu_pss$PSS_total<=13
covid_clu_pss$PSS_total>13 & covid_clu_pss$PSS_total<27
covid_clu_pss$PSS_total>=27
#1688 features left
demo_clu_pss = covid_clu_pss[,c(29,36,37,44)]
demo_clu_pss.scaled = scale(demo_clu_pss) 
fviz_nbclust(demo_clu_pss.scaled, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)+ggtitle("Elbow Method (PSS demographics)")
demo_pss.res <- kmeans(demo_clu_pss.scaled, 3, nstart = 10)
fviz_cluster(demo_pss.res, demo_clu_pss , geom = "point",ellipse.type = "norm",main = "Clustering using 7 demographics features")
cl4 = demo_pss.res$cluster
## Calculate the mean PSS for cluster 1
mean(covid_clu_pss$PSS_total[c(which(cl4 ==1))])
mean(covid_clu_pss$PSS_total[c(which(cl4 ==2))])
mean(covid_clu_pss$PSS_total[c(which(cl4 ==3))])
#Using CAS features
cas_clu = covid_clu[,2:6]
cas_clu = cas_clu-1 
cas_clu.scaled = scale(cas_clu) 
#Determine the no of clusters
# Elbow method for kmeans
fviz_nbclust(cas_clu.scaled, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)+ggtitle("Elbow Method (CAS)")
#3 clusters were recommended
cas.res <- kmeans(cas_clu.scaled, 3, nstart = 10)
#Plotting
fviz_cluster(cas.res, cas_clu, ellipse.type = "norm")
fviz_cluster(cas.res, cas_clu, geom = "point",ellipse.type = "norm",main = "Clustering using 5 features from CAS")
# Average silhouette for kmeans
#fviz_nbclust(cas_clu.scaled, kmeans, method = "silhouette")

#Using MDI features
mdi_clu = covid_clu[,7:18]
### The true coding is 5 to 0, while the default coding is 1 to 6. 
mdi_clu = 6 - mdi_clu
mdi_clu.scaled = scale(mdi_clu) 
#Determine the no of clusters
# Elbow method for kmeans
fviz_nbclust(mdi_clu.scaled, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)+ggtitle("Elbow Method (MDI)")
#3 clusters were recommended
mdi.res <- kmeans(mdi_clu.scaled, 3, nstart = 10)
#Plotting
fviz_cluster(mdi.res, mdi_clu, ellipse.type = "norm")
fviz_cluster(mdi.res, mdi_clu, geom = "point",ellipse.type = "norm",main = "Clustering using 5 features from MDI")

#Using PSS features
pss_clu = covid_clu[,19:28]
pss_clu$PSS_1 = pss_clu$PSS_1-1
pss_clu$PSS_2 = pss_clu$PSS_2-1
pss_clu$PSS_3 = pss_clu$PSS_3-1
pss_clu$PSS_6 = pss_clu$PSS_6-1
pss_clu$PSS_9 = pss_clu$PSS_9-1
pss_clu$PSS_10 = pss_clu$PSS_10-1
pss_clu$PSS_4 = 5-pss_clu$PSS_4
pss_clu$PSS_5 = 5-pss_clu$PSS_5
pss_clu$PSS_7 = 5-pss_clu$PSS_7
pss_clu$PSS_8 = 5-pss_clu$PSS_8
pss_clu.scaled = scale(pss_clu) 
fviz_nbclust(pss_clu.scaled, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)+ggtitle("Elbow Method (PSS)")
pss.res <- kmeans(pss_clu.scaled, 3, nstart = 10)
fviz_cluster(pss.res, pss_clu, ellipse.type = "norm")
fviz_cluster(pss.res, pss_clu, geom = "point",ellipse.type = "norm",main = "Clustering using 10 features from PSS")
#Notice the clusterings are saved in cas.res, pss.res, mdi.res, we want to calculate pairwise 
#AMI
library(aricode)
#Calculate AMI
cl1 = cas.res$cluster
cl2 = mdi.res$cluster
cl3 = pss.res$cluster
AMI(cl1,cl2)
AMI(cl2,cl3)
AMI(cl1,cl3)
### should an inclusion of other features into the clustering be done? 
### Or a comparison with the labeling with suggested threshold in CAS, PSS and MDI
### True labels

