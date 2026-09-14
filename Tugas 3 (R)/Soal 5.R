Data <- read.csv("uas_dbscan_2.csv")
Data <- Data[,-1]

# Uji Multikolinieritas
library(car)
mult1=vif(lm(X1~X2+X3+X4,Data))
mult1
mult2=vif(lm(X2~X1+X3+X4,Data))
mult2
mult3=vif(lm(X3~X1+X2+X4,Data))
mult3
mult4=vif(lm(X4~X1+X2+X3,Data))
mult4
Result <- data.frame(mult1, mult2, mult3, mult4)
rownames(Result) <- NULL
Result

# Clustering
library(dbscan)
db_clust <- dbscan(Data, eps = 3.5, minPts = 3)
db_clust

# Nilai Silhouette
library(cluster)
silhouette_result <- silhouette(db_clust$cluster, dist(Data))
silhouette_result
mean(silhouette_result[,'sil_width'])

# Simulasi variasi nilai minPts dan eps
Result <- matrix(NA,nrow = 10,ncol = 14)
for (i in 1:10) {
  for (j in 2:15) {  
    db_clust <- dbscan(Data, eps = j, minPts = i)
    silhouette_result <- silhouette(db_clust$cluster, dist(Data))
    Result[i, (j-1)] <- mean(data.frame(silhouette_result)$sil_width)
  }
}
rownames(Result) <- 1:10
colnames(Result) <- 2:15
Result
max(Result,na.rm = TRUE)

# Terpilih minPts = 3 dan eps = 4
db_clust_best <- dbscan(Data, eps = 4, minPts = 3)
db_clust_best
silhouette_best_result <- silhouette(db_clust_best$cluster, dist(Data))
mean(data.frame(silhouette_best_result)$sil_width)

# X1 X2
X12 <- ggplot(data = Data, aes(x = X1, y = X2, color = factor(db_clust_best$cluster))) +
  geom_point() +
  labs(title = "X1 and X2 Clustering Results", x = "X1", y = "X2",color="Cluster") +
  scale_color_manual(values = c("red", "blue", "green"))
# X1 X3
X13 <- ggplot(data = Data, aes(x = X1, y = X3, color = factor(db_clust_best$cluster))) +
  geom_point() +
  labs(title = "X1 and X3 Clustering Results", x = "X1", y = "X3",color="Cluster") +
  scale_color_manual(values = c("red", "blue", "green"))
# X1 X4
X14 <- ggplot(data = Data, aes(x = X1, y = X4, color = factor(db_clust_best$cluster))) +
  geom_point() +
  labs(title = "X1 and X4 Clustering Results", x = "X1", y = "X4",color="Cluster") +
  scale_color_manual(values = c("red", "blue", "green"))
# X2 X3
X23 <- ggplot(data = Data, aes(x = X2, y = X3, color = factor(db_clust_best$cluster))) +
  geom_point() +
  labs(title = "X2 and X3 Clustering Results", x = "X2", y = "X3",color="Cluster") +
  scale_color_manual(values = c("red", "blue", "green"))
# X2 X4
X24 <- ggplot(data = Data, aes(x = X2, y = X4, color = factor(db_clust_best$cluster))) +
  geom_point() +
  labs(title = "X2 and X4 Clustering Results", x = "X2", y = "X4",color="Cluster") +
  scale_color_manual(values = c("red", "blue", "green"))
# X3 X4
X34 <- ggplot(data = Data, aes(x = X3, y = X4, color = factor(db_clust_best$cluster))) +
  geom_point() +
  labs(title = "X3 and X4 Clustering Results", x = "X3", y = "X4",color="Cluster") +
  scale_color_manual(values = c("red", "blue", "green"))
library(ggpubr)
ggarrange(X12,X13,X14,X23,X24,X34)
