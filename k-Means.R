# Creates random dataset of two attributes.
# Recommended to run a few times from start to bottom in case you get a bad sample.
createx = c(rnorm(100,0,0.5),rnorm(50,-1,0.25),rnorm(50,1,0.25))
createy = c(rnorm(100,2,0.5),rnorm(50,0,0.25),rnorm(50,0,0.25))
dataset = data.frame(x = createx, y = createy)
plot(dataset, main = "Iteration 0") # Plots dataset, iteration = 0.

k = 3 # k is hard-coded specifically for 3 clusters.
Iterations = 10 # Set how many iterations you want to loop for k-means clustering.
minx = min(dataset[,1])
maxx = max(dataset[,1])
miny = min(dataset[,2])
maxy = max(dataset[,2])
clustersk = data.frame(x = c(runif(k,minx,maxx)), y = c(runif(k,miny,maxy))) # Chooses 3 random points on plot to designate as 3 centroids for 3 clusters.
euclidean = function(a, b) sqrt(sum((a - b)^2)) # Function for euclidean.
for (i in 1:k){ # Initialize colors for 3 clusters.
  if(i==1){
    clustersk[c(i),"col"] = "red"
  }
  if(i==2){ 
      clustersk[c(i),"col"] = "blue"
  }
  if(i==3){ 
    clustersk[c(i),"col"] = "green"
  }
}

# Iterations loop start here.
t = 0
while (t < Iterations){
for (i in seq_along(dataset[,1])){ # For loop to calculate which points are closest to which centroid, and assigns color to them.
  closest = Inf
  pcolor = ""
  for (v in 1:k){
    if (euclidean(dataset[i,1:2],clustersk[v,1:2]) < closest){
      closest = euclidean(dataset[i,1:2],clustersk[v,1:2])
      pcolor = clustersk[v,3]
    }
  }
  dataset[i,"col"] = pcolor
}
plot(dataset[,1], dataset[,2], col = dataset[,3], main = paste0("Iteration ", t+1), xlab = "x-Axis", ylab = "y-Axis") # Plots points with their respective colors to show their cluster part.
points(clustersk[,1],clustersk[,2], pch = 3, cex = 5) # Plots clusters' centroids.

newk1 = data.frame()
newk2 = data.frame()
newk3 = data.frame()
for (i in 1:k){ # Computes new centroids for three clusters based on mean of their cluster points.
  temp = data.frame()
  for (v in seq_along(dataset[,1])){
    if (dataset[v,3]=="red" && i==1){
      temp = rbind(temp,dataset[v,1:2])
    }
    if (dataset[v,3]=="blue" && i==2){
      temp = rbind(temp,dataset[v,1:2])
    }
    if (dataset[v,3]=="green" && i==3){
      temp = rbind(temp,dataset[v,1:2])
    }
  }
  meanx = mean(temp[,1])
  meany = mean(temp[,2])
  if (i==1){
    newk1 = data.frame(meanx, meany, col = "red")
  }
  if (i==2){
    newk2 = data.frame(meanx, meany, col = "blue")
  }
  if (i==3){
    newk3 = data.frame(meanx, meany, col = "green")
  }
}
clustersk = data.frame(x = c(newk1[,1], newk2[,1], newk3[,1]), y = c(newk1[,2], newk2[,2], newk3[,2]), col = c(newk1[,3], newk2[,3], newk3[,3])) # Sets new centroids for the three clusters.
t = t + 1 # Adds 1 to t counter as iteration counter.
}
