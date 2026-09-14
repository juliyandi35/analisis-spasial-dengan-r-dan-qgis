Data <- read.csv("uas_dbscan_1.csv")
head(Data)

library(factoextra)
library(ggplot2)
library(ggthemes)
Data <- Data[,2:3]
ggplot(data = Data, aes(x = X1, y = X2)) +
  geom_point(col = "firebrick4") +
  theme_pander()

library(dbscan)
db_clust <- dbscan(Data, eps = 3, minPts = 3)
db_clust

Data <- Data %>% 
  mutate(clust = db_clust$cluster, 
         clust = ifelse(clust==0,"Noise",clust)) 

ggplot(data =Data, aes(x = X1, y = X2)) +
  geom_point(aes(col = as.factor(clust))) +
  theme_pander() +
  labs(col = "Cluster")

