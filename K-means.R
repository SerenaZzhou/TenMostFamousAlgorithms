#K-means
my_kmeans<-function(data,k,iter.max=10){
  rows<-nrow(data)
  cols<-ncol(data)
  between_distance<-0
  within_distance<-matrix(0,k,1)
  index<-matrix(0,rows,2)
  center<-matrix(0,k,cols)
  iter<-0
  
  #generate a random center
  randCenter<-as.vector(sample(1:rows,size=k))
  
  for(i in 1:k){
    index[randCenter[i],1]<-i
    center[randCenter[i],]<-data[randCenter[i],]
    center<-as.matrix(center,k,cols)
  }
  changed=TRUE
  
  while (changed) {
    if(iter>=iter.max)
      break
    changed=FALSE
    
    for (i in 1:rows) {
      initialDistance<-10000
      previousCluster<-index[i,1]
      
      for (j in 1:k) {
        currentDistance<-sum((data[i,]-center[j,])^2)^0.5
        if (currentDistance<initialDistance) {
          initialDistance<-currentDistance
          index[i,1]<-j
          index[i,2]<-currentDistance
        }
      }
      if (previousCluster!=index[i,1]) {
        changed=TRUE
      }
      for (m in 1:k) {
        newcenter<-data[index[,1]==m,]
        newcenter<-as.matrix(newcenter)
        if (nrow(newcenter)>0) {
          center[m,]<-colMeans(newcenter)
        }else{
          center[m,]<-center[m,]
        }
      }
    }
    iter=(iter+1)
  }
  ss<-function(x)  sum(scale(x,scale=FALSE)^2)
  between_distance<-ss(center[index[,1],])
  within_distance<-sapply(split(as.data.frame(data),index[,1]),ss)
  total_within_distance<-sum(within_distance)
  
  result<-list(cluster=index[,1],total_within_distance=total_within_distance,between_distance=between_distance,iteration=iter)
  return(result)
}

#Testing
data(iris)
X<-as.matrix(iris[,1:4])
scaled<-function(x){
  (x-min(x))/(max(x)-min(x))
}
normX<-X
for (i in 1:ncol(X)) {
  normX[,i]<-scaled(X[,i])
}
data<-as.matrix(normX)
test<-my_kmeans(data,3)

table(iris[,5],test$cluster)
mean(as.integer(iris[,5])==test$cluster)

#Visually
plot(iris$Sepal.Width,iris$Sepal.Length,col=test$cluster,pch=19,xlim=c(1,5))

