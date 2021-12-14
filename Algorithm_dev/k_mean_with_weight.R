#####################
# Define parameters #
#####################
#load package
library(data.table)
library(here)
#define parameter
weight <- 'ECY'
longitude <- 'Ind'
latitude <- ''
#number of clusters
K = 3
max_iteration = 50
num_of_test = 100
#load data
change_here <- function(new_path){
  new_root <- here:::.root_env
  
  new_root$f <- function(...){file.path(new_path, ...)}
  
  assignInNamespace(".root_env", new_root, ns = "here")
}
change_here("~/Document/")

df <- fread(here("Data","cargo_cmdty_grp_analysis.csv"))

#####################
# Clustering #
#####################
# Standardrize df
if (trimws(longitude) == '') {df[,longitude:=0]}
if (trimws(latitude) == '') {df[,latitude:=0]}

if (trimws(longitude) != '') {setnames(df,longitude,"longitude")}
if (trimws(latitude) != '') {setnames(df,latitude,"latitude")}
if (trimws(weight) != '') {setnames(df,weight,"weight")}

setcolorder(df,c("longitude","latitude","weight"))

#initial centroids by random
for (test in 1:num_of_test){
    init_centroids_index = sample(nrow(df),K)
    #initiate containers
    distance_matrix = matrix(data = NA, nrow = nrow(df), ncol = K)
    cluster = vector()
    centroid_long = vector()
    centroid_lat = vector()
    #compute distance between cities and initial centroids
    for (k in c(1:K)) {
      for (i in c(1:nrow(df))) {
        obs_i = as.numeric(df[i,1:2])
        centroid_k = as.numeric(df[init_centroids_index[k],1:2])
        distance_matrix[i,k] = dist(rbind(obs_i,centroid_k))
      }
    }
    #initial cluster assignment for each city
    for (i in c(1:nrow(df))) {
      cluster[i] = which.min(distance_matrix[i,])
    }
    
    #iteration baseline
    old_cluster = vector(length = length(cluster))
    new_cluster = cluster
    #iterations
    iteration <- 1
    while ((!all(old_cluster == new_cluster)) & iteration<max_iteration) {
      #update old cluster assignment
      old_cluster = new_cluster
      #calculate centroids using weighted average
      for (k in c(1:K)) {
        cluster_k = which(old_cluster == k) #index of cluster k
        centroid_long[k] = weighted.mean(df$longitude[cluster_k], df$weight[cluster_k])
        centroid_lat[k] = weighted.mean(df$latitude[cluster_k], df$weight[cluster_k])
      }
      df_centroid = as.data.frame(cbind(centroid_long, centroid_lat))
      #compute distance between cities and centroids
      for (k in c(1:K)) {
        for (i in c(1:nrow(df))) {
          obs_i = as.numeric(df[i,1:2])
          centroid_k = as.numeric(df_centroid[k,])
          distance_matrix[i,k] = dist(rbind(obs_i,centroid_k))
        }
      }
      #update cluster assignment for each city
      for (i in c(1:nrow(df))) {
        cluster[i] = which.min(distance_matrix[i,])
      }
      #update new_cluster
      new_cluster = cluster
      #update iteration
      print(paste0("iteration:", iteration))
      iteration = iteration+1
      }
    
    #combine cluster result (drop if there's less than 3 groups)
    if (length(unique(cluster))==3){
      df <- cbind(df,cluster)
      df[,(paste0("cluster_",test)):=.GRP, by=cluster]
      df[,cluster:=NULL]
      print(paste0("test run ", test, " complete"))
      } 
    else {
      print(paste0("test run ", test, " complete"))
      print("less then 3 groups, so dropped")
      }
}

#summary cluster info
df$as_1 <- rowSums(df[,c(names(df)[grep("cluster",names(df))]), with=FALSE] ==1)
df$as_2 <- rowSums(df[,c(names(df)[grep("cluster",names(df))]), with=FALSE] ==2)
df$as_3 <- rowSums(df[,c(names(df)[grep("cluster",names(df))]), with=FALSE] ==3)

#export result
fwrite(df,here("Analysis","CMDTY_4_GRP_CLUSTER_analysis.csv"))

