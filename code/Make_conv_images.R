

library(pbapply)
library(keras)
library(raster)
library(deepLearningRshort)
library(EBImage)
library(OpenImageR)

pic <- readImage("Slides/Images/KAUSTphoto.png")@.Data


  img <- brick(pic)
  crs(img) <- "+proj=tmerc"
  
  plotRGB(img,scale = 1)
  
  tmp<-channel(pic, "gray")[,,1]
  
display(tmp, method="raster")

tmp2<-convolution(tmp,kernel = matrix(rep(1/25,25),5,5))  
display(tmp2, method="raster")

tmp3<-convolution(tmp,kernel = matrix(c(0,0,0,1,0,0,0,0,0),3,3))  
display(tmp3, method="raster")

display(tmp3-tmp, method="raster")

v<-rep(0,25)
v[13]=2
tmp4<-convolution(tmp,kernel = matrix(v,5,5)-matrix(rep(1/25,25),5,5))  
display(tmp4, method="raster")

tmp5<-convolution(tmp,kernel = matrix(c(-1,0,1,-2,0,2,-1,0,1),3,3,byrow=T))  
display(tmp5, method="raster")


tmp7<-convolution(tmp,kernel = matrix(c(0,-1,0,-1,4,-1,0,-1,0),3,3,byrow=F))  
display(tmp7, method="raster")


for(i in 1:4){
tmp8<-convolution(tmp,kernel = matrix(runif(9,-2,2),3,3,byrow=F))  
display(tmp8, method="raster")
}


pic <- readImage("Slides/Images/KAUSTphoto.png")@.Data

img <- brick(pic)
crs(img) <- "+proj=tmerc"

plotRGB(img,scale = 1)

pic=abind(pic,matrix(0,1000,3),along=2)

tmp9<-convolution(pic[,,1],kernel = matrix(c(0,-1,0,-1,4,-1,0,-1,0),3,3,byrow=F))  
image(t(pic[,,1])[,1000:1],
      col = fade(terrain.colors(12, rev = FALSE), 100), axes = F)

image(t(tmp9)[,1000:1],
      col = fade(terrain.colors(12, rev = FALSE), 100), axes = F)

max.pool=tmp9
for(i in seq(1,999,by=2)){
  for(j in seq(1,999,by=2)){
    
    max.pool[i:(i+1),j:(j+1)]=max(tmp9[i:(i+1),j:(j+1)])
  }
}

image(t(max.pool)[,1000:1],
      col = fade(terrain.colors(12, rev = FALSE), 100), axes = F,zlim=range(tmp9))

max.pool=tmp9
for(i in seq(1,996,by=5)){
  for(j in seq(1,996,by=5)){
    
    max.pool[i:(i+4),j:(j+4)]=max(tmp9[i:(i+4),j:(j+4)])
  }
}

image(t(max.pool)[,1000:1],
      col = fade(terrain.colors(12, rev = FALSE), 100), axes = F,zlim=range(tmp9))
