# K-means NBA
df = read.csv('/Users/alston/Desktop/Final Project_G9/NBA_dataset1.csv')
df_col = c(6, 10, 12, 13, 15, 16, 18, 19, 23, 27, 28, 62)
NBA_df = df[,df_col]

set.seed(5566)

# group in 5
NBA_seg_5 = kmeans(scale(NBA_df), centers=5, nstart = 50, iter.max = 100)
NBA_seg_5
NBA_seg_5$cluster
NBA_seg_5$size

k=10
WGSS=c()
for(i in 1:k){
  WGSS[i]=sum(kmeans(scale(NBA_df), centers=i)$withinss)
}

plot(1:k, WGSS, type="b")

# group in 4
NBA_seg_4 = kmeans(scale(NBA_df), centers=4, nstart = 50, iter.max = 100)
NBA_seg_4
NBA_seg_4$cluster
NBA_seg_4$size

# group in 3
NBA_seg_3 = kmeans(scale(NBA_df), centers=3, nstart = 50, iter.max = 100)
NBA_seg_3
NBA_seg_3$cluster
NBA_seg_3$size

# PCA to plot the cluset
install.packages("ggfortify")
library(ggfortify)
install.packages("ggbiplot")
library(ggbiplot)
install.packages("cluster")
library(cluster)
pca_res <- prcomp(NBA_df, scale. = TRUE)
autoplot(clara(NBA_df, k = 3))