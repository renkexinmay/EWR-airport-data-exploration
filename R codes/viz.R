#Part 1 import target parameters Root GDP
Dim = root2[,128:721]
norm.data=Dim
norm.data[] = lapply(norm.data, function(x) as.numeric(as.character(x)))

#scale data
#arr两行需要一起scale
#CW两行需要一起scale

min.max.norm <- function(x){
  (x-min1)/(max1-min1)
}

que=c(1:2,5:9)
for(n in que) {
  i1 = 66*(n-1)+1
  i2=i1+65
  max1 = max(na.omit(unlist(norm.data[,i1:i2])))
  min1 = min(na.omit(unlist(norm.data[,i1:i2])))
  for(j in i1:i2) {
    focus = which(!is.na(norm.data[,j]))
    norm.data[focus,j] = min.max.norm(norm.data[focus,j])
  }
}

n1=3
n2=4
i1 = 66*(n1-1)+1 #133
i2=66*(n2-1)+1+65 #264
max1 = max(na.omit(unlist(norm.data[,i1:i2])))
min1 = min(na.omit(unlist(norm.data[,i1:i2])))
for(j in i1:i2) {
  focus = which(!is.na(norm.data[,j]))
  norm.data[focus,j] = min.max.norm(norm.data[focus,j])
}

for(j in 1:ncol(norm.data)) {
  focus = which(is.na(norm.data[,j]))
  norm.data[focus,j] = 0
}

write.csv(x = norm.data, file="tt.csv")


#3PCA（使用标准化数据）
r<-princomp(norm.data[,1:ncol(norm.data)])
#r<-prcomp(norm.data[,1:ncol(norm.data)])
dim = cbind(norm.data, r$scores[,1:3])
#dim = cbind(norm.data, r$x[,1:3])
library(rgl)
plot3d(dim[,10:12], col=r1[,9])
browseURL(paste("file://", writeWebGL(dir=file.path(tempdir(), "webGL"), width=1000), sep=""))

results:
  
  Importance of components:
  PC1     PC2     PC3     PC4
Standard deviation     4.0305 1.81430 1.54954 1.50583
Proportion of Variance 0.4603 0.09327 0.06804 0.06425

Only 55%

#2自动编码器（使用）

delNo = numeric()
for(j in 1:9) {
  delNo[j]=66*j
}
norm.data1 = norm.data[,-delNo]

#norm.data ->用于vis，带有一列空格
#norm.data1->用于AE，没有空格

library(h2o)
localH2O <- h2o.init(ip = "localhost", port = 54321)

write.csv(x = norm.data1, file="Dim1.csv")
mfile = "C:\\Users\\Qian Fu\\Documents\\R-TMI\\new-feb\\Dim1.csv"
# mfile = "C:\\Users\\Kexin\\Documents\\Dim1.csv"
mydata = h2o.importFile(path = mfile)

set.seed(1)

NN_model <- h2o.deeplearning(
  x = 2:586,
  training_frame = mydata,
  hidden = c(300,2,200),
  epochs = 100,
  activation = "Tanh",
  autoencoder = TRUE,
  export_weights_and_biases=T
)

Weights1<-h2o.weights(NN_model,matrix_id = 1)
Weights2<-h2o.weights(NN_model,matrix_id = 2)
write.csv (x = as.data.frame(Weights1), file = "w1.csv")
write.csv (x = as.data.frame(Weights2), file = "w2.csv")

train_supervised_features = h2o.deepfeatures(NN_model, mydata, layer=2)
plotdata = as.data.frame(train_supervised_features)
library(rgl)
#plot.new()
#par( mfrow = c(1,1))
plot3d (plotdata[,1:3])
browseURL(paste("file://", writeWebGL(dir=file.path(tempdir(), "webGL"), width=1000), sep=""))

#NN saved









200-2-200
MSE = 0.03328308
RMSE = 0.1824365

3D: C:\Users\Qian Fu\Dropbox\work\【1】TMI\LIT REVIEW/0504.html


300-2-200
Mse = 0.03256412
RMSE = 0.1804553

特点：两种在某方面相反的clusters占大多数， 中间的散乱

300-2-300
0.0334479
0.1828877


c(200,50, 2, 50, 200)
0.02695122
0.1641683

100-2-100
0.03332863
0.1825613

hopkins(plotdata, n = nrow(plotdata)-1)
$H
[1] 0.320695


#choose k
#elbow
##k-means elbow
wss <- (nrow(x)-1)*sum(apply(x,2,var))
for (i in 2:20) 
{set.seed(1234)
  wss[i] <- sum(kmeans(x,centers=i)$withinss)}
plot(1:20, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

##k-means elbow
library(factoextra)
fviz_nbclust(x, kmeans, method = "wss", k.max = 15)
#pam elbow
fviz_nbclust(x, pam, method = "wss", k.max = 15)
#hc elbow
fviz_nbclust(x, hcut, method = "wss", k.max = 15)


#The average silhouette approach we’ll be described comprehensively in the chapter cluster validation statistics. Briefly, it measures the quality of a clustering. That is, it determines how well each object lies within its cluster. A high average silhouette width indicates a good clustering.
##kmeans 
#require(vegan)
fit <- cascadeKM(scale(x, center = TRUE,  scale = TRUE), 1, 30, iter = 1000)
plot(fit, sortg = TRUE, grpmts.plot = TRUE)
calinski.best <- as.numeric(which.max(fit$results[2,]))
cat("Calinski criterion optimal number of clusters:", calinski.best, "\n")

library(mclust)
d_clust <- Mclust(as.matrix(x), G=1:20)
m.best <- dim(d_clust$z)[2]
cat("model-based optimal number of clusters:", m.best, "\n")
# 4 clusters
plot(d_clust)

##kmeans- Silhouette Width metric
#install.packages("fpc")
# library("fpc")
K <- 2:20
round <- 30 # 每次迭代30次，避免局部最优
rst <- sapply(K, function(i){
  print(paste("K=",i))
  mean(sapply(1:round,function(r){
    print(paste("Round",r))
    result <- kmeans(x, i)
    stats <- cluster.stats(dist(x), result$cluster)
    stats$avg.silwidth
  }))
})
plot(K,rst,type='l',main=' Silhouette Coefficient vs. K', ylab=' Silhouette Coefficient')

library(cluster)
sil <- rep(0, 20)
for(i in 2:20){
  km.res <- kmeans(x, centers = i, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(x))
  sil[i] <- mean(ss[, 3])
}
# Plot the  average silhouette width
plot(1:20, sil, type = "b", pch = 19, 
     frame = FALSE, xlab = "Number of clusters k",ylab= "average silhouette width")
#abline(v = which.max(sil), lty = 2)





Elbow - 12
Sil-5,6,10~13
Gap – 6,12
PF-6~8,15
Km
-6,12
8,12
5,6,11,7
6,11.12
7.15
Pm
-12,6
5, 12
10.12,6
6,12
6.12
Hc
- 6
6,
5.8.13
12.9.6
8,15


* 2 proposed  0 as the best number of clusters
* 1 proposed  1 as the best number of clusters
* 1 proposed  2 as the best number of clusters
* 3 proposed  3 as the best number of clusters
* 9 proposed  4 as the best number of clusters
* 2 proposed  5 as the best number of clusters
* 2 proposed  8 as the best number of clusters
* 1 proposed  13 as the best number of clusters
* 2 proposed  14 as the best number of clusters
* 3 proposed  15 as the best number of clusters




6
8
12
km



pm



hc





##粘标签
##kmeans, k=5
cl=kmeans(x,10)
root3=cbind(root2,x,KMlabel_5=cl$cluster)
##hc k =5
hc <- hclust(dist(x), method="ward.D")
x1=cbind(x, cluster = as.numeric(as.character(cutree(hc,k=10))))
root3= cbind(root3,HClabel_5=x1$cluster)

##pam
pamc<-pam(plotdata,12)
比较root2和root3的顺序
finaldata= cbind(root2, PMlabel_12 =pamc$clustering)
write.csv(x = finaldata, file="finaldata.csv")

#3 clustering 画图
##hc
x=plotdata
hc <- hclust(dist(x), method="ward.D")
x1=cbind(x, cluster = as.numeric(as.character(cutree(hc,k=10))))
colnames(x1)
plot3d (x1[,1:3],col=x1$cluster)
plot (x1[,1:2],col=x1$cluster, main = "HC, k=10")

browseURL(paste("file://", writeWebGL(dir=file.path(tempdir(), "webGL"), width=1000), sep=""))

##kmeans
cl=kmeans(x,10)
dim = cbind(x, clusterNum =cl$cluster) 
plot3d(dim[,1:3],col=dim$clusterNum)
plot(dim[,1:2],col=dim$clusterNum, main ="KM, k=10" )
browseURL(paste("file://", writeWebGL(dir=file.path(tempdir(), "webGL"), width=1000), sep=""))

cl=kmeans(x,9)
dim = cbind(x, clusterNum =cl$cluster) 
plot(dim[,1:2],col=dim$clusterNum, main ="KM, k=9" )
root3=cbind(root3,x,KM9=cl$cluster)

cl=kmeans(x,10)
dim = cbind(x, clusterNum =cl$cluster) 
root3=cbind(root3,x,KM10=cl$cluster)
plot(dim[,1:2],col=dim$clusterNum, main ="KM, k=10" )

cl=kmeans(x,11)
dim = cbind(x, clusterNum =cl$cluster) 
root3=cbind(root3,x,KM11=cl$cluster)
plot(dim[,1:2],col=dim$clusterNum, main ="KM, k=11" )

write.csv(x = root3, file="EWR_GDP18-1.csv")

##pam
pamc=pam(x,10)
pamc1 = cbind(x, clusterNum =pamc$clustering) 
plot(pamc1[,1:2],col=pamc1$clusterNum, main ="PM, k=10" )
 
norm.data2=cbind(norm.data, root3$KM9, root3$KM10, root3$KM11)
colnames(norm.data2)[595]= "KM9"
colnames(norm.data2)[596]= "KM10"
colnames(norm.data2)[597]= "KM11"

usr <- par( "usr" )
par( mfrow = c(1,1))
plot.new()
norm.data3=norm.data2[order(norm.data2$KM9,decreasing=FALSE),]
norm.data4=norm.data2[order(norm.data2$KM10,decreasing=FALSE),]
norm.data5=norm.data2[order(norm.data2$KM11,decreasing=FALSE),]
par( mfrow = c(10,10), mai = c(0,0,0,0))
#KM
for(i in 1:nrow(norm.data3)){
  y = as.matrix(norm.data3[i, 1:594])
  dim(y) = c(66, 9)
  image(y[,ncol(y):1],axes = FALSE, col = gray(255:0 / 255))
  box(lty = 'solid', col = 'red')
  text( usr[ 2 ], usr[ 4 ], norm.data3[i,595],    adj = c( 2, 1 ), col = "blue" )
}
par( mfrow = c(10,10), mai = c(0,0,0,0))
for(i in 1:nrow(norm.data4)){
  y = as.matrix(norm.data4[i, 1:594])
  dim(y) = c(66, 9)
  image(y[,ncol(y):1],axes = FALSE, col = gray(255:0 / 255))
  box(lty = 'solid', col = 'red')
  text( usr[ 2 ], usr[ 4 ], norm.data4[i,596],    adj = c( 2, 1 ), col = "blue" )
}
par( mfrow = c(1,1))
plot.new()
par( mfrow = c(10,10), mai = c(0,0,0,0))
for(i in 1:nrow(norm.data5)){
  y = as.matrix(norm.data5[i, 1:594])
  dim(y) = c(66, 9)
  image(y[,ncol(y):1],axes = FALSE, col = gray(255:0 / 255))
  box(lty = 'solid', col = 'red')
  text( usr[ 2 ], usr[ 4 ], norm.data5[i,597],    adj = c( 2, 1 ), col = "blue" )
}

#HC
plot.new()
for(i in 1:nrow(norm.data4)){
  y = as.matrix(norm.data4[i, 1:594])
  dim(y) = c(66, 9)
  image(y[,ncol(y):1],axes = FALSE, col = gray(255:0 / 255))
  box(lty = 'solid', col = 'red')
  text( usr[ 2 ], usr[ 4 ], norm.data4[i,596],    adj = c( 2, 1 ), col = "red" )
}
root4=cbind(root3, K9  = norm.data3[,595])


#18svaed (k=12)

write.csv(x = allweather12_P, file="EWR_GDP18.csv")

#statistic analysis
STAT<- allweather12_P[,FALSE]
for(j in 1:9) {
  j1=(j-1)*66+1
  j2=j1+64
  stat = as.matrix(allweather12_P[,j1:j2])
  STAT[,j] = rowVars(stat)
  colnames(STAT)[j] =paste("var-",j)
}

write.csv(x = STAT, file="EWR_GDP19.csv")
STAT<- allweather12_P[,FALSE]
for(j in 1:9) {
  j1=(j-1)*66+1
  j2=j1+64
  stat = as.matrix(allweather12_P[,j1:j2])
  STAT[,j] = rowMeans2(stat)
  colnames(STAT)[j] =paste("mean-",j)
}
write.csv(x = STAT, file="EWR_GDP19-Ave.csv")


#Stat
pamc<-pam(plotdata,12)
比较root2和root3的顺序
write.csv(x = allweather12, file="rnorm.csv")
write.csv(x = root2, file="rorig.csv")


finaldata= cbind(root2, PMlabel_12 =pamc$clustering)
write.csv(x = finaldata, file="finaldata.csv")

#Number_Revisions_NoCNX
#EarlyCancelTime
Month	
SendTimePeriod	
BgnTimePeriod	
EndTimePeriod	
WeekdayNo


