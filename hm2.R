library(dplyr)
#install.packages("caret")
library(caret)

# loading data
file <- file.choose()
essay <- read.csv(file, header = T, stringsAsFactors = F)
# data pre-processing: remove NA, z-score normalization
df <- data.frame(essay)
sum(!complete.cases(df))
df <- na.omit(df)
summary(df)
df[,3:72] <- scale(df[,3:72], center = T, scale = T)
summary(df)
View(df)

# set the data with author == 'dispt' as testing data ???????????????????????????dispt label???row,??????????????????author
testing <- df[df$author == "dispt", ]
sample_data <- df[df$author != "dispt", ]
# split the rest data into traning dataset and validation dataset 
set.seed(100)
train_size = floor(0.8 * nrow(sample_data)) 
train_int <- sample(seq_len(nrow(sample_data)), size = train_size)
train <- sample_data[train_int, ]
valid <- sample_data[-train_int, ]


library(factoextra)
# distance function and visualization
distance <- get_dist(sample_data, method = "euclidean")
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

# k-means function
# for 74 identified author essay: 51 essays written by Hamilton, 15 by Madison, 3 by Hamilton and Madison, 5 by Jay

# k-means: k = 6
km_output <- kmeans(train[, 3:72], centers = 6, nstart = 25, iter.max = 100, algorithm = "Hartigan-Wong")
str(km_output)
sum(c(km_output$withinss, km_output$betweenss))
fviz_cluster(km_output, data = train[, 3:72])
cluster_df <- data.frame(author = train$author, cluster = unname(km_output$cluster))
table(cluster_df)
# K-means on validation dataset
km_predic <- kmeans(valid[, 3:72], centers = 6, nstart = 25, iter.max = 100, algorithm = "Hartigan-Wong")
cluster_predic <- data.frame(author = valid$author, cluster = unname(km_predic$cluster))
table(cluster_predic)
# ??????cluster lable???????????????table(cluster_df), ?????????training dataset?????????cluster(????????????cluster??????????????????,??????????????????,??????????????????????????????)
cluster_predic$valid_pred[cluster_predic$cluster == 1] <- "Hamilton"
cluster_predic$valid_pred[cluster_predic$cluster == 2] <- "Jay"
cluster_predic$valid_pred[cluster_predic$cluster == 3] <- "Hamilton"
cluster_predic$valid_pred[cluster_predic$cluster == 4] <- "Hamilton"
cluster_predic$valid_pred[cluster_predic$cluster == 5] <- "HM"
cluster_predic$valid_pred[cluster_predic$cluster == 6] <- "Madison"

