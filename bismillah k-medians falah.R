library(tidyverse) 
#Algoritma klastering
library(cluster) 
#Algoritma klastering dan visualisasi
library(factoextra)
library(gridExtra)
library(Gmedian)

#input data
data1=read.delim("clipboard")
data1

#Deskriptif Data
summary(data1)

#Mengambil kolom yang digunakan
data2=data.frame(data1[2:3])   
data2

#Melihat data outlier
boxplot(data2[,2:3])

#Standarisasi atau scalling  data
datafix <- scale(data2)       
datafix

#Jarak Korelasi
distance <- get_dist(datafix)
distance
fviz_dist(distance, gradient = list(low = "green", mid = "white", high = "red"))

#== Mencari Nilai K Optimal ==#
# METODE  ELBOW atau WSS
fviz_nbclust(datafix, kGmedian, method = "wss")
# menetukan jumlah cluster terbaik metode silhowette
fviz_nbclust(datafix, kGmedian, method = "silhouette")

#== Membuat Cluster K Median ==#
finalfix <- kGmedian(datafix, 3)
print(finalfix)
summary(finalfix)

#ga perlu masuk ke laporan, dibuat tabel ringkas
View(finalfix)
cbind.data.frame(finalfix[["cluster"]])
cbind.data.frame(data1,finalfix[["cluster"]])

dr=data.frame(datafix, finalfix$cluster)
dr


#Tampilan Cluster
fviz_cluster(finalfix, main="hasil produksi mangga & pisang")
fviz_cluster(finalfix, data = datafix, main="hasil produksi mangga & pisang")

# graik
plot(datafix, col=finalfix$cluster, main="hasil produksi mangga & pisang")
points(finalfix$centers, col="blue", pch=8, cex=2)

## melihat deskripsi data setelah diclusterkan berdasarkan cluster
data1%>%
  mutate(Cluster=finalfix$cluster)%>%
  group_by(Cluster)%>%
  summarise_all("mean") 
