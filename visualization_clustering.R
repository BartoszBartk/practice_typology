#########################
# Code for visualizing results from 12 interviews with experts from science, authorities and advisory services
# on relevance of 17 behavioural factors for 18 agri-environmental practices
# Includes code for clustering the practices by behavioural factor scores
# Data collection by Nina Büttner; code by Bartosz Bartkowski
# Contact: bartosz.bartkowski@ufz.de
#########################

#if necessary install.packages("factoextra","cluster","here","ggpubr","ggplot2")
require(factoextra) # needed only for clustering
require(cluster) # needed only for clustering
require(here)
require(ggpubr)
require(ggplot2)
require(dplyr)


#############
# Read in all required data (otherwise the function with plyr will mess up here())
#############

# basic data set for visualizations
aep_ratings <- read.csv(here("data_interviews.csv"), header = T, sep = ";", dec = ",", na.strings = "n/a")
# reshape
aep_ratings_long <- reshape(aep_ratings, varying = list(colnames(aep_ratings[,3:19])), 
                            times = colnames(aep_ratings[,3:19]), idvar = c("Interview", "Practice"), 
                            v.names = "Rating", direction = "long")

# mapping of TDFs to COM-B
tdf_comb <- read.csv(here("tdf_com_b_mapping.csv"), header = T, sep = ";")

# dataset for clustering
data <- read.csv(here("data.csv"), sep = ";", header = T, dec = ",")
data_ <- data[,c(2:17)] #remove first column with TDFs
rownames(data_) <- data$TDF #add TDFs as row names
data_ <- as.data.frame(t(data_)) #transpose data frame to have practices in columns
#note: first column provides the number of responses, it should not be used for clustering

############
# Basic visualizations
############

# boxplots of TDFs across all practices and interviews
boxplot_all <- ggplot(aep_ratings_long, aes(x = time, y = Rating)) + 
  geom_boxplot(fill = 'grey') +
  theme_minimal() +
  guides(x = guide_axis(angle = 90))
boxplot_all

# bars as alternative
# calculate means and sds (http://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization#barplot-with-error-bars)
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}
# make bar plot with error bars
aep_simple <- data_summary(aep_ratings_long, varname = "Rating", 
                    groupnames = c("time"))
bars_all <- ggplot(aep_simple, aes(x = time, y = Rating)) +
  geom_bar(stat = "identity", fill = 'grey') +
  geom_errorbar(aes(ymin = Rating - sd, ymax = Rating + sd), width=.2,
                position=position_dodge(.9)) +
  theme_minimal() +
  guides(x = guide_axis(angle = 90)) +
  labs(title = "Mean ratings of TDFs across practices", 
       x = "", y = "Rating") +
  theme(plot.title = element_text(hjust = 0.5))
bars_all

# add colouring by COM-B categories
# merge with mapping of TDF and COM-B
aep_simple_comb <- merge(aep_simple, tdf_comb, by.x = "time", by.y = "TDF")
# add variable to reorder
aep_simple_comb$order <- ifelse(aep_simple_comb$COMB == "Capability", 1,
                                ifelse(aep_simple_comb$COMB == "Opportunity", 2, 3))
aep_simple_comb <- aep_simple_comb[order(aep_simple_comb$order),]
# plot
bars_all_comb <- ggplot(aep_simple_comb, aes(x = time, y = Rating, fill = COMB)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = Rating - sd, ymax = Rating + sd), width=.2,
                position=position_dodge(.9)) +
  theme_minimal() +
  guides(x = guide_axis(angle = 90)) +
  labs(title = "Mean ratings of TDFs across practices", 
       x = "", y = "Rating") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(limits = aep_simple_comb$time) +
  scale_fill_discrete(breaks = c("Capability", "Opportunity", "Motivation"))
bars_all_comb

############
# Clustering for all practices
############

#remove "responses" column
data_all <- data_[,c(2:18)]

#k-means
fviz_nbclust(data_all, kmeans, method = "wss") #compute total within sum of squares statistics for different n's of clusters
gap_stat <- clusGap(data_all,
                    FUN = kmeans,
                    nstart = 25,
                    K.max = 10,
                    B = 50) #compute gaps statistic for different n's of clusters
fviz_gap_stat(gap_stat) #visualize
#6 clusters seems a good choice
km6 <- kmeans(data_all, 6, 25)
km6
fig_km6 <- fviz_cluster(km6, data_all, repel = T, geom = "point", main = "", ggtheme = theme_minimal())
fig_km6 <- fig_km6 + 
  geom_text(data = fig_km6$data, aes(x=x, y=y, label=name, colour=cluster), show.legend = F, nudge_y = -0.2) + #remove text from legend & move data point labels down
  xlim(-4.5,5) #modify xlim to make all text readable
fig_km6

#k-medoids (supposedly more robust, as relying on medians, not means)
fviz_nbclust(data_all, pam, method = "wss")
gap_stat_2 <- clusGap(data_all,
                      FUN = pam,
                      nstart = 25,
                      K.max = 10,
                      B = 50)
fviz_gap_stat(gap_stat_2)
#check 6 clusters
kmed6 <- pam(data_all, 6)
kmed6
fig_kmed6 <- fviz_cluster(kmed6, data = data_all, repel = T, geom = "point", main = "", ggtheme = theme_minimal())
fig_kmed6 <- fig_kmed6 + 
  geom_text(data = fig_kmed6$data, aes(x=x, y=y, label=name, colour=cluster), show.legend = F, nudge_y = -0.2) +
  xlim(-4.5,5)
fig_kmed6

############
# Subset of practices
############

#only practices with >2 votes
data_min3 <- subset(data_, Responses>2)[,c(2:18)]
#k-means
fviz_nbclust(data_min3, kmeans, method = "wss")
gap_stat <- clusGap(data_min3,
                    FUN = kmeans,
                    nstart = 25,
                    K.max = 10,
                    B = 50)
fviz_gap_stat(gap_stat)
#4 clusters seems fine
km4_ <- kmeans(data_min3, 4, 25)
km4_
fig_km4_ <- fviz_cluster(km4_, data_min3, repel = T, geom = "point", main = "", ggtheme = theme_minimal())
fig_km4_ <- fig_km4_ + 
  geom_text(data = fig_km4_$data, aes(x=x, y=y, label=name, colour=cluster), show.legend = F, nudge_y = 0.2) +
  xlim(-4.5,NA) #+
  #labs(title = "Clusters of practices with at least three responses") +
  #theme(plot.title = element_text(hjust = 0.5))
fig_km4_

#k-medoids
fviz_nbclust(data_min3, pam, method = "wss")
gap_stat_2 <- clusGap(data_min3,
                      FUN = pam,
                      nstart = 25,
                      K.max = 10,
                      B = 50)
fviz_gap_stat(gap_stat_2)
#check 4 clusters
kmed_ <- pam(data_min3, 4)
kmed_
fig_kmed_ <- fviz_cluster(kmed_, data = data_min3, repel = T, geom = "point", main = "", ggtheme = theme_minimal())
fig_kmed_ <- fig_kmed_ + 
  geom_text(data = fig_kmed_$data, aes(x=x, y=y, label=name, colour=cluster), show.legend = F, nudge_y = 0.2) +
  xlim(-4.5,NA)
fig_kmed_

############
# Visualization of clusters
############

# plot multiple figures
ggarrange(fig_km6, fig_km4_, fig_kmed6, fig_kmed_,
          labels = c("A: k-means, 6 clusters", "C: k-means, 4 clusters (min. 3 responses)", "B: k-medoids, 6 clusters", "D: k-medoids, 4 clusters (min. 3 responses)"),
          label.x = c(-0.05, -0.1, -0.05, -0.1),
          ncol = 2, nrow = 2)

############
# TDF bar charts for individual clusters
############

# add cluster numbers to practices
practices_clusters <- as.data.frame(km4_$cluster)
colnames(practices_clusters) <- "cluster"
practices_clusters$Practice <- rownames(practices_clusters)
ratings_clusters <- merge(aep_ratings, practices_clusters, by = "Practice", all.x = T)

# create datasets for each cluster
cluster1 <- subset(ratings_clusters, cluster == 1)
cluster2 <- subset(ratings_clusters, cluster == 2)
cluster3 <- subset(ratings_clusters, cluster == 3)
cluster4 <- subset(ratings_clusters, cluster == 4)

## cluster 1
# reshape
cluster1_long <- reshape(cluster1, varying = list(colnames(aep_ratings[,3:19])), 
                         times = colnames(aep_ratings[,3:19]), idvar = c("Interview", "Practice"), 
                         v.names = "Rating", direction = "long")
# use the function defined above
cluster1_simple <- data_summary(cluster1_long, varname = "Rating", 
                           groupnames = c("time"))
# merge
cluster1_simple <- merge(cluster1_simple, tdf_comb, by.x = "time", by.y = "TDF")
# add variable to reorder
cluster1_simple$order <- ifelse(cluster1_simple$COMB == "Capability", 1,
                                ifelse(cluster1_simple$COMB == "Opportunity", 2, 3))
cluster1_simple <- cluster1_simple[order(cluster1_simple$order),]
# plot
bars_cluster1 <- ggplot(cluster1_simple, aes(x = time, y = Rating, fill = COMB)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = Rating - sd, ymax = Rating + sd), width=.2,
                position=position_dodge(.9)) +
  theme_minimal() +
  guides(x = guide_axis(angle = 90)) +
  labs(#title = "Mean ratings of TDFs within cluster 1", 
       x = "", y = "Rating") +
  #theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(limits = aep_simple_comb$time) +
  scale_fill_discrete(breaks = c("Capability", "Opportunity", "Motivation")) +
  scale_fill_manual(values=c("#D9E2F3", 
                             "#EFE599", 
                             "#C5E0B3")) 
bars_cluster1

## cluster 2
# reshape
cluster2_long <- reshape(cluster2, varying = list(colnames(aep_ratings[,3:19])), 
                         times = colnames(aep_ratings[,3:19]), idvar = c("Interview", "Practice"), 
                         v.names = "Rating", direction = "long")
# use the function defined above
cluster2_simple <- data_summary(cluster2_long, varname = "Rating", 
                                groupnames = c("time"))
# merge
cluster2_simple <- merge(cluster2_simple, tdf_comb, by.x = "time", by.y = "TDF")
# add variable to reorder
cluster2_simple$order <- ifelse(cluster2_simple$COMB == "Capability", 1,
                                ifelse(cluster2_simple$COMB == "Opportunity", 2, 3))
cluster2_simple <- cluster2_simple[order(cluster2_simple$order),]
# plot
bars_cluster2 <- ggplot(cluster2_simple, aes(x = time, y = Rating, fill = COMB)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = Rating - sd, ymax = Rating + sd), width=.2,
                position=position_dodge(.9)) +
  theme_minimal() +
  guides(x = guide_axis(angle = 90)) +
  labs(#title = "Mean ratings of TDFs within cluster 2", 
       x = "", y = "Rating") +
  #theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(limits = aep_simple_comb$time) +
  scale_fill_discrete(breaks = c("Capability", "Opportunity", "Motivation")) +
  scale_fill_manual(values=c("#D9E2F3", 
                             "#EFE599", 
                             "#C5E0B3")) 
bars_cluster2

## cluster 3
# reshape
cluster3_long <- reshape(cluster3, varying = list(colnames(aep_ratings[,3:19])), 
                         times = colnames(aep_ratings[,3:19]), idvar = c("Interview", "Practice"), 
                         v.names = "Rating", direction = "long")
# use the function defined above
cluster3_simple <- data_summary(cluster3_long, varname = "Rating", 
                                groupnames = c("time"))
# merge
cluster3_simple <- merge(cluster3_simple, tdf_comb, by.x = "time", by.y = "TDF")
# add variable to reorder
cluster3_simple$order <- ifelse(cluster3_simple$COMB == "Capability", 1,
                                ifelse(cluster3_simple$COMB == "Opportunity", 2, 3))
cluster3_simple <- cluster3_simple[order(cluster3_simple$order),]
# plot
bars_cluster3 <- ggplot(cluster3_simple, aes(x = time, y = Rating, fill = COMB)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = Rating - sd, ymax = Rating + sd), width=.2,
                position=position_dodge(.9)) +
  theme_minimal() +
  guides(x = guide_axis(angle = 90)) +
  labs(#title = "Mean ratings of TDFs within cluster 3", 
       x = "", y = "Rating") +
  #theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(limits = aep_simple_comb$time) +
  scale_fill_discrete(breaks = c("Capability", "Opportunity", "Motivation")) +
  scale_fill_manual(values=c("#D9E2F3", 
                             "#EFE599", 
                             "#C5E0B3")) 
bars_cluster3

## cluster 4
# reshape
cluster4_long <- reshape(cluster4, varying = list(colnames(aep_ratings[,3:19])), 
                         times = colnames(aep_ratings[,3:19]), idvar = c("Interview", "Practice"), 
                         v.names = "Rating", direction = "long")
# use the function defined above
cluster4_simple <- data_summary(cluster4_long, varname = "Rating", 
                                groupnames = c("time"))
# merge
cluster4_simple <- merge(cluster4_simple, tdf_comb, by.x = "time", by.y = "TDF")
# add variable to reorder
cluster4_simple$order <- ifelse(cluster4_simple$COMB == "Capability", 1,
                                ifelse(cluster4_simple$COMB == "Opportunity", 2, 3))
cluster4_simple <- cluster4_simple[order(cluster4_simple$order),]
# plot
bars_cluster4 <- ggplot(cluster4_simple, aes(x = time, y = Rating, fill = COMB)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = Rating - sd, ymax = Rating + sd), width=.2,
                position=position_dodge(.9)) +
  theme_minimal() +
  guides(x = guide_axis(angle = 90)) +
  labs(#title = "Mean ratings of TDFs within cluster 4", 
       x = "", y = "Rating") +
  #theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(limits = aep_simple_comb$time) +
  scale_fill_discrete(breaks = c("Capability", "Opportunity", "Motivation")) +
  scale_fill_manual(values=c("#D9E2F3", 
                             "#EFE599", 
                             "#C5E0B3")) 
bars_cluster4

# all in one plot
ggarrange(bars_cluster1, bars_cluster2, bars_cluster3, bars_cluster4,
          labels = c("A: Cluster 1 (Reduced soil pressure)",
                     "B: Cluster 2 (Legumes, Reduced mowing, Reduced tillage, Rotational fallow)",
                     "C: Cluster 3 (Buffer strips, Cover crops, Extensive grazing, Flower strips, No mineral fertilizer)",
                     "D: Cluster 4 (Agroforestry, Hedges, Permanent grassland)"),
          hjust = -0.05,
          vjust = 1.0,
          ncol = 2, nrow = 2)
