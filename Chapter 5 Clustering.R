###Chapter 5 Clustering

###k-means: How Well Did You Do Earlier?
# seeds and seeds_type are pre-loaded in your workspace

# Set random seed. Don't remove this line.
set.seed(100)

# Do k-means clustering with three clusters, repeat 20 times: seeds_km
seeds_km <- kmeans(seeds, centers = 3, nstart = 20)

# Print out seeds_km
seeds_km

# Compare clusters with actual seed types. Set k-means clusters as rows
table(seeds_km$cluster, seeds_type)

# Plot the length as function of width. Color by cluster
plot(seeds$width, seeds$length, xlab = "Length", ylab = "Width", col = seeds_km$cluster)

###The Influence of Starting Centroids
# seeds is pre-loaded in your workspace

# Set random seed. Don't remove this line.
set.seed(100)

# Apply kmeans to seeds twice: seeds_km_1 and seeds_km_2
seeds_km_1 <- kmeans(seeds, 5, nstart = 1)
seeds_km_2 <- kmeans(seeds, 5, nstart = 1)

# Return the within cluster sum of squares
seeds_km_1$tot.withinss / seeds_km_2$tot.withinss

# Compare the resulting clusters
table(seeds_km_1$cluster, seeds_km_2$cluster)

###Making A Scree Plot!
# The dataset school_result is pre-loaded

# Set random seed. Don't remove this line.
set.seed(100)

# Explore the structure of your data
str(school_result)

# Initialise ratio_ss
ratio_ss <- rep(0, 7)

# Finish the for-loop
for (k in 1:7) {
  
  # Apply k-means to school_result: school_km
  school_km <- kmeans(school_result, k, nstart = 20)
  
  # Save the ratio between of WSS to TSS in kth element of ratio_ss
  ratio_ss[k] <- school_km$tot.withinss / school_km$totss
  
}

# Make a scree plot with type "b" and xlab "k"
plot(ratio_ss, type = "b", xlab = "k")

###Standardized vs Non-Standardized Clustering (1)
# The dataset run_record has been loaded in your workspace

# Set random seed. Don't remove this line.
set.seed(1)

# Explore your data with str() and summary()
str(run_record)
summary(run_record)

# Cluster run_record using k-means: run_km. 5 clusters, repeat 20 times
run_km <- kmeans(run_record, 5, nstart = 20)

# Plot the 100m as function of the marathon. Color using clusters
plot(run_record$marathon, run_record$X100m, col = run_km$cluster,
     xlab = "marathon", ylab ="100m", main = "Run Records")

# Calculate Dunn's index: dunn_km. Print it.
dunn_km <- dunn(clusters = run_km$cluster, Data = run_record)
print(dunn_km)

###Standardized vs Non-Standardized Clustering (2)
# The dataset run_record as well as run_km are available

# Set random seed. Don't remove this line.
set.seed(1)

# Standardize run_record, transform to a dataframe: run_record_sc
run_record_sc <- as.data.frame(scale(run_record))

# Cluster run_record_sc using k-means: run_km_sc. 5 groups, let R start over 20 times
run_km_sc <- kmeans(run_record_sc, 5, nstart = 20)

# Plot records on 100m as function of the marathon. Color using the clusters in run_km_sc
plot(run_record$marathon, run_record$X100m, col = run_km_sc$cluster,
     xlab = "marathon", ylab ="100m", main = "Run Records")

# Compare the resulting clusters in a nice table
table(run_km$cluster, run_km_sc$cluster)

# Calculate Dunn's index: dunn_km_sc. Print it.
dunn_km_sc <- dunn(clusters = run_km_sc$cluster, Data = run_record_sc)
dunn_km_sc

###Single Hierarchical Clustering
# The dataset run_record_sc has been loaded in your workspace

# Apply dist() to run_record_sc: run_dist
run_dist <- dist(run_record_sc)

# Apply hclust() to run_dist: run_single
run_single <- hclust(run_dist, method = "single")

# Apply cutree() to run_single: memb_single
memb_single <- cutree(run_single, k = 5)

# Apply plot() on run_single to draw the dendrogram
plot(run_single)

# Apply rect.hclust() on run_single to draw the boxes
rect.hclust(run_single, k = 5, border = 2:6)

###Complete Hierarchical Clustering
# run_record_sc is pre-loaded

# Code for single-linkage
run_dist <- dist(run_record_sc, method = "euclidean")
run_single <- hclust(run_dist, method = "single")
memb_single <- cutree(run_single, 5)
plot(run_single)
rect.hclust(run_single, k = 5, border = 2:6)

# Apply hclust() to run_dist: run_complete
run_complete <- hclust(run_dist, method = "complete")

# Apply cutree() to run_complete: memb_complete
memb_complete <- cutree(run_complete, k = 5)

# Apply plot() on run_complete to draw the dendrogram
plot(run_complete)

# Apply rect.hclust() on run_complete to draw the boxes
rect.hclust(run_complete, k = 5, border = 2:6)

# table() the clusters memb_single and memb_complete. Put memb_single in the rows
table(memb_single, memb_complete)

###Hierarchical vs k-means
# run_record_sc, run_km_sc, memb_single and memb_complete are pre-calculated

# Set random seed. Don't remove this line.
set.seed(100)

# Dunn's index for k-means: dunn_km
dunn_km <- dunn(clusters = run_km_sc$cluster, Data = run_record_sc)

# Dunn's index for single-linkage: dunn_single
dunn_single <- dunn(clusters = memb_single, Data = run_record_sc)

# Dunn's index for complete-linkage: dunn_complete
dunn_complete <- dunn(clusters = memb_complete, Data = run_record_sc)

# Compare k-means with single-linkage
table(run_km_sc$cluster, memb_single)

# Compare k-means with complete-linkage
table(run_km_sc$cluster, memb_complete)

###Clustering US States Based on Criminal Activity
# Set random seed. Don't remove this line.
set.seed(1)

# Scale the dataset: crime_data_sc
crime_data_sc <- scale(crime_data)

# Perform k-means clustering: crime_km
crime_km <- kmeans(crime_data_sc, 4, nstart = 20)

# Perform single-linkage hierarchical clustering
## Calculate the distance matrix: dist_matrix
dist_matrix <- dist(crime_data_sc)

## Calculate the clusters using hclust(): crime_single
crime_single <- hclust(dist_matrix, method = "single")

## Cut the clusters using cutree: memb_single
memb_single <- cutree(crime_single, 4)

# Calculate the Dunn's index for both clusterings: dunn_km, dunn_single
dunn_km <- dunn(clusters = crime_km$cluster, Data = crime_data_sc)
dunn_single <- dunn(clusters = memb_single, Data = crime_data_sc)

# Print out the results
print(dunn_km)
print(dunn_single)
