getwd()
setwd("/Users/kimjisu/Desktop/Data_mining/")
utilities.df <- read.csv("Utilities.csv")
str(utilities.df)
# set row names to the utilities column
row.names(utilities.df) <- utilities.df[,1]
# remove the utility column
utilities.df <- utilities.df[,-1]
# compute Euclidean distance
d <- dist(utilities.df, method = "euclidean")
d
##########################Normalization##############################
# normalize input variables
utilities.df.norm <- sapply(utilities.df, scale)
utilities.df.norm
# add row names: utilities
row.names(utilities.df.norm) <- row.names(utilities.df)
# compute normalized distance based on Sales (column 6) and Fuel Cost (column 8)
d.norm <- dist(utilities.df.norm[,c(6,8)], method = "euclidean") # c(6,8) -> variable 2
d.norm
# in hclust() set argument method =
# to "ward.D", "single", "complete", "average", "median", or "centroid“
d.norm <- dist(utilities.df.norm, method = "euclidean")
d.norm
hc1 <- hclust(d.norm, method = "single") #clustering single
plot(hc1, hang = -1, ann = FALSE)
hc2 <- hclust(d.norm, method="average")
plot(hc2, hang=-1, ann=FALSE)
memb_s <- cutree(hc1, k=6)
memb_s # single
memb_c <- cutree(hc2, k=5) # define group difference
memb_c
row.names(utilities.df.norm) <- paste(memb_c,": ", row.names(utilities.df), sep="") # single linkage
heatmap(as.matrix(utilities.df.norm), Colv=NA, hclustfun = hclust, col=rev(paste("gray",1:99,sep = "")))
## data low : bright color, data high : dark color

######################### K-Means clustering ##################################
km <- kmeans(utilities.df.norm, 6)
str(km)
# show cluster membership
km$cluster
km$centers
# plot an empty scatter plot
# ylab = X, type = line, ylim range : center min~ max, xlim range : 0~8
# draw rectangle
plot(c(0), xaxt = 'n', ylab = "", type = "l", ylim = 
       c(min(km$centers), max(km$centers)), xlim = c(0, 8))

#xaxt: x축에 좌표를 표시하지 못하게 함
# label x-axes
axis(1, at = c(1:8), labels = names(utilities.df)) #1:x축에 tick-marks (1~8)
# plot centroids
for (i in c(1:6))
  lines(km$centers[i,], lty = i, lwd = 2, col = ifelse(i %in% c(1, 3, 5),"black", "dark grey"))
# name clusters
text(x = 0.5, y = km$centers[, 1], labels = paste("Cluster", c(1:6)))
