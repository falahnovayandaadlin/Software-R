library(fpc)  
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization

#input data
data1=read.delim("clipboard")
data1
head(data1)
str(data1)

#mengambil data yang digunakan 
data2<-(data1[2:3])  
data2
summary(data2)
summary(is.na(data2))  ###mengitung missing value
boxplot(data2)           ###membuat boxplot

#Uji Asusmsi Multikolinearitas/matris korelasi
library(car)
cor(data2)
library(DataExplorer)
plot_correlation(data2)

#Standarisasi atau scalling  data
datafix <- scale(data2)       
datafix 

##cluster dengan fungsi pamk
pamk.hasil <-pamk(datafix)
pamk.hasil
pamk.hasil$nc  ###menampilkan jumlah cluster yang terbentuk

fviz_nbclust(datafix, pam, method = "wss")
fviz_nbclust(datafix, pam, method = "silhouette")

#== Membuat Cluster K Medoid ==#
pam.hasil <- pam(datafix, 3)
summary(pam.hasil)

## melihat deskripsi data setelah diclusterkan berdasarkan cluster
# rata-rata
pam.hasil$medoids
pam.hasil$diss

# dibuat tabel ringkas
data.frame(data1$Kecamatan,pam.hasil$clustering)
a=fviz_cluster(pam.hasil, main="Hasil Produksi Mangga & Pisang")
a
#Tampilan Cluster
fviz_cluster(pam.hasil, main="Hasil Produksi Mangga & Pisang")

# Profilisasi
data1%>%
  mutate(Cluster = pam.hasil$clustering) %>%
  group_by(Cluster) %>%
  summarise_all("mean") 


#Visualisasi bar chart
library(plotly)
datakp=read.delim("clipboard")
datakp
fig <- plot_ly(datakp, x = ~datakp$Kecamatan, y = ~datakp$Mangga..ton.,
               type = 'bar', name = 'Mangga')
fig <- fig %>% add_trace(y = ~datakp$pisang..ton., name = 'Pisang')
fig <- fig %>% layout(yaxis = list(title = 'Count'), barmode = 'group')
fig
 