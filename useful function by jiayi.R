##===========================================================================
# this is the funtion I usually used, write by myself 
##=============================================================================


##==================================================================================
# 1. use to sperate merge data to each Species/ calculte species mean (OR other parameter)
##===================================================================================
sperate.sp.fun=function(data)  {
  temp=subset(data,data$species==spp[i])  ##
  fin.dat=data.frame(
    spid=i,species=unique(temp$species),latin=unique(temp$拉丁�?), sp.code=unique(temp$sp.code),
    light=mean(temp$light),RGR=mean(temp$RGR))
  
  if (i==1){                 # use to write down data row by row
    write.table(fin.dat,file="D:/fin",append=T,col.name=T,row.name=F) 
  } else { write.table(fin.dat,file="D:/fin",append=T,col.name=F,row.name=F) }
} 

light.spp=read.csv(file.choose(),header=T)
spp=levels(light.spp$species)
for(i in 1:length(spp)) {sperate.sp.fun(light.spp)}   ### do above program for each species (seperately)


##============================================================================
# 2. use to calculate area of 多边形（凸包�?
##===============================================================================
library(sp)
x1 <- rnorm(100, 0.8, 0.3)
y1 <- rnorm(100, 0.8, 0.3)
hpts <- chull(x = x1, y = y1)
hpts <- c(hpts, hpts[1])
xy.coords <- cbind(x1, y1)
chull.coords <- xy.coords[hpts,]
chull.poly <- Polygon(chull.coords, hole=F)
chull.area <- chull.poly@area
plot(x1,y1,cex=0.5)
lines(chull.coords )


## =======================================================================
# 3. 画图�?2D�?3D
## =======================================================================
x <- y <- 1:3
z <- matrix (nrow = 3, ncol = 3, data = 1:9)
image2D(z, x, y, border = "black")
image2D(z, x, y, rasterImage = TRUE, border = "black")


## =======================================================================
# 4. 另存图片，png，pdf
## =======================================================================
png(file="D:/Histogram of tree transmittance.png",family="GB1",  ##中文
    height=7,width=10,res=300, units = "in")


##---
lightdat=read.csv(choose.files(),header = T)  ##5ha-20151112 - 修改物种名字-去重.csv
spp=levels(lightdat$species)

pdf(file="D:/Histogram of sp tree transmittance.pdf",family="GB1")
par(mfrow=c(2,2))
for (i in 1:length(spp)){
  temp=subset(lightdat,lightdat$species==spp[i])  ##
  n=nrow(temp);
  #if (n<=1){} else {
  hist(temp$light,breaks=200,xlab="Transmittance (%)",
       main="Histogram of transmittance of",sub=paste(unique(temp$species),"( n=",n,")"))
  #}
}
dev.off()

#---------------------------------------------------------------
par(mfrow=c(1,1))
for (i in 1:length(spp)){
  temp=subset(lightdat,lightdat$species==spp[i])
  n=nrow(temp);
  png(file=paste("D:/Histogram of transmittance of",unique(temp$species),".png"),
      family="GB1",height=7,width=10,res=300, units = "in")
  #if (n<=1){} else {
  hist(temp$light,breaks=50,xlab="Transmittance (%)",
       main=paste("Histogram of transmittance of",unique(temp$species),"( n=",n,")"))#}
  dev.off()
}


#===================================================================================
# 排序----
#===================================================================================
library("dplyr")
arrange(mtcars, cyl, disp)


#=============================================================================
# 向绘图区域内部添加文�?
#============================================================================
text( location , “text to place�? , pos ,�? ) 
#坐标轴，要添加的文本，文本相对于位置参数的方位，1234，下左上�?

#============================================================================
# 向图形的四个边界之一添加文本
#============================================================================
mtext(“text to place�? , side , line = n , outer = TRUE or FALSE  …�?) 
#side�?1234 ，意味着放置文本的边下左上右，line 内移或外移文本可取负数�?
#note that a vector adj has a different meaning from text. adj = 0.5 will
#centre the string, but for outer = TRUE on the device region rather than the plot region.

#### R^2 and P in the plot
library("broom")
lm1=lm(light.RGR2$RGR2~light.RGR2$predict.light)


summary(lm1)
png("D:/light vs RGR.png",height=7,width=10,res=300, units = "in");
plot(light.RGR2$predict.light,light.RGR2$RGR2, xlab="Light",ylab="RGR",
     main="",col=1,cex=0.5)
abline(lm1,col=2)
text( 0.24,8,expression(R^"2"),cex=1)   
text( 0.26,8,cex=1,"= 0.0054")
text( 0.253,7.5,cex=1,"P  < 0.001")
dev.off()


###### ggplot中更简�? 集成
library("ggplot2")
s.gg=read.csv(file.choose(),header = T)  ##"dbh-gg.csv"
g1=ggplot(data=s.gg,aes(x=s.gg$rank.x,y=s.gg$rank.y))+
  geom_point(size=0.8,col="blue")+
  facet_grid(g.y~g.x)+geom_smooth(method=lm,col=2,lty=1,lwd=0.5,se = FALSE)+
  labs(x="Species rank of ST ",y="Species rank of ST")
g1  ##\n \n (a)

library(plyr)
lm_labels=function(dat){
  mod=lm(rank.y~rank.x,data=dat)
  formula=sprintf("italic(y)==%.2f%+.2f*italic(x)",
                  round(coef(mod)[1],2),round(coef(mod)[2],2))
  
  r=cor(dat$rank.x,dat$rank.y)
  r2=sprintf("italic(R^2) == %.2f",r^2)
  
  n=dim(dat)[1]
  n=sprintf("italic(n) == %.0f",n)
  data.frame(formula=formula,r2=r2,n=n,stringsAsFactors = FALSE)}

labels=ddply(s.gg,c("g.x","g.y"),lm_labels)
labels

g2=g1+geom_text(x=10,y=130,aes(label=formula),data=labels,parse=T,cex=3,hjust=0)+
  geom_text(x=10,y=120,aes(label=r2),data=labels,parse=T,cex=3,hjust=0)+
  geom_text(x=10,y=107,aes(label=n),data=labels,parse=T,cex=3,hjust=0)
g2

png(file="D:/g2.png",height=7,width=7,res=300, units = "in")
g2
dev.off()


###ggplot 布局，一页多�?
library(grid)
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
png(file="D:/abund and ba vs ST01.png",height=7,width=12,res=300, units = "in")
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2)))
print(g6, vp = vplayout(1, 1))
print(g8, vp = vplayout(1, 2))
dev.off()


#============================================================================
#上下�?
#============================================================================
plot(1,ylab=expression(italic("toto")["subscript"]),
     xlab=expression(italic("toto")^"superscript"))



#============================================================================
#空间格局-g-function�?
#============================================================================

#------------------------------------------------
data=stHSD
spp=levels(stHSD$species)
for(i in 1:length(spp)){
  bb1=subset(data,data$species==spp[i]);nr1=nrow(bb1)
  if (nr1<30) { } else{
    for(j in 1:length(spp)){
      bb2=subset(data,data$species==spp[j]);nr2=nrow(bb2)
      
      if (i==j|nr2<30){ } else{
        bb=rbind(bb1,bb2)
        trees=ppp(bb$gx, bb$gy,c(0, 1000),c(0, 500),marks=factor(bb$species))
        trees1=trees[trees$species==spp[i]]
        trees2=trees[trees$species==spp[j]]
        
        K=Kcross(trees,i=spp[i],j=spp[j],r=c(0,5,10,15,20,25,30,35,40,45,50),correction=c("isotropic"))
        L=sqrt(K$iso/pi)-K$r
        EK=envelope(trees,Kcross,i=spp[i],j=spp[j],r=c(0,5,10,15,20,25,30,35,40,45,50),correction=c("isotropic"))
        g=pcf(trees,i=spp[i],j=spp[j],r=c(0,5,10,15,20,25,30,35,40,45,50),spr=0.5, method="b",correction=c("isotropic"))  ##
        Eg=envelope(trees,pcf,i=spp[i],j=spp[j],r=c(0,5,10,15,20,25,30,35,40,45,50),correction=c("isotropic"))
        
        fin.dt=data.frame(
          spid=i,species=unique(bb0$species),
          
          K5.iso=K$iso[2],K10.iso=K$iso[3],K15.iso=K$iso[4],K20.iso=K$iso[5],K25.iso=K$iso[6],
          K30.iso=K$iso[7],K35.iso=K$iso[8],K40.iso=K$iso[9],K45.iso=K$iso[10],K50.iso=K$iso[11],
          
          L5.iso=L[2],L10.iso=L[3],L15.iso=L[4],L20.iso=L[5],L25.iso=L[6],
          L30.iso=L[7],L35.iso=L[8],L40.iso=L[9],L45.iso=L[10],L50.iso=L[11],
          
          EK5.lo=EK$lo[2],EK10.lo=EK$lo[3],EK15.lo=EK$lo[4],EK20.lo=EK$lo[5],EK25.lo=EK$lo[6],
          EK30.lo=EK$lo[7],EK35.lo=EK$lo[8],EK40.lo=EK$lo[9],EK45.lo=EK$lo[10],EK50.lo=EK$lo[11],
          EK5.hi=EK$hi[2],EK10.hi=EK$hi[3],EK15.hi=EK$hi[4],EK20.hi=EK$hi[5],EK25.hi=EK$hi[6],
          EK30.hi=EK$hi[7],EK35.hi=EK$hi[8],EK40.hi=EK$hi[9],EK45.hi=EK$hi[10],EK50.hi=EK$hi[11],
          
          g5=g$iso[2],g10=g$iso[3],g15=g$iso[4],g20=g$iso[5],g25=g$iso[6],
          g30=g$iso[7],g35=g$iso[8],g40=g$iso[9],g45=g$iso[10],g50=g$iso[11],
          
          Eg5.lo=Eg$lo[2],Eg10.lo=Eg$lo[3],Eg15.lo=Eg$lo[4],Eg20.lo=Eg$lo[5],Eg25.lo=Eg$lo[6],
          Eg30.lo=Eg$lo[7],Eg35.lo=Eg$lo[8],Eg40.lo=Eg$lo[9],Eg45.lo=Eg$lo[10],Eg50.lo=Eg$lo[11],
          Eg5.hi=Eg$hi[2],Eg10.hi=Eg$hi[3],Eg15.hi=Eg$hi[4],Eg20.hi=Eg$hi[5],Eg25.hi=Eg$hi[6],Eg30.hi=Eg$hi[7],
          Eg35.hi=Eg$hi[8],Eg40.hi=Eg$hi[9],Eg45.hi=Eg$hi[10],Eg50.hi=Eg$hi[11])
        
        if (i==1&j==2){                 
          write.table(fin.dt,file="D:/ech pirsp gfun",append=T,col.name=T,row.name=F) 
        } else { write.table(fin.dt,file="D:/ech pirsp gfun",append=T,col.name=F,row.name=F) }
      }
    }
  }
}


##---------------参考资�?
par（col.axis=“black”，cex.axis=0.8，cex.lab=1�? font=3�?
par（mfrow=c�?1�?2))
trees=ppp（bb0$gx�? bb0$gy，c�?0�? 400），c�?0�? 500），
marks=factor（bb0$sp））
semere=trees［trees$marks==“SEMERE”]
mapdata=read.csv（“h:/mapdata.csv”， header=F�?
map=as.matrix（mapdata�?
altitude=matrix（map�? nrow=41�? ncol=51�? dimnames=NULL�?
x=10×�?1�? nrow（altitude））- 10
y=10×�?1�? ncol（altitude））- 10
contour（x，y，altitude，axes=T，nlevels=20，main=“a”， xlab=“x轴x-coordinates（m）”， ylab=“y轴y-coordinates（m）”）
points（semere$x�? semere$y，col=“red”， pch=1，cex= 0.8�? xlim=c�?0�? 400）， ylim=c�?0�? 500））
GA=Gest（semere，correction=c（“border”，“isotropic”，“Ripley”，“translate”）�?
EA=envelope（semere，Gest�?
plot（EA，xlab=“尺度Scale（m）”， ylab=“G值G（r）”，main=“b”）


par（col.axis=“black”，cex.axis=0.8，cex.lab=1�? font=3�?
par（mfrow=c�?1�?2））
trees=ppp（bb0$gx�? bb0$gy，c�?0�? 400），c�?0�? 500），
marks=factor（bb0$sp））
semere=trees［trees$marks==“SEMERE”］
barrpe=trees［trees$marks==“BARRPE”］
mapdata=read.csv（“h:/mapdata.csv”， header=F�?
map=as.matrix（mapdata�?
altitude=matrix（map，nrow=41�? ncol=51�? dimnames=NULL�?
x=10×�?1:nrow（altitude））- 10
y=10×�?1:ncol（altitude））- 10
contour（x，y，altitude，axes=T，nlevels=20，main=“a”， xlab=“x轴x-coordinates（m）”， ylab=“y轴y-coordinates（m）”）
points（semere$x�? semere$y，col=“red”， pch=1，cex=0.8�? xlim=c�?0�? 400）， ylim=c�?0�? 500））
points（barrpe $x，barrpe $y，col=“green”，pch=1，cex=0.8�? xlim=c�?0�? 400）， ylim=c�?0�? 500））
Gm=Gcross（trees�? “SEMERE”， “BARRPE”）
EA=envelope（trees ，Gcross�? i=“SEMERE”， j= “BARRPE”）
plot（EA，xlab=“尺度Scale（m）”， ylab=“G12值G12（r）”，main=“b”，xlim=c�?0�? 30））



##======================================
# 清理内存�? 尽量使用matrix而不是data.frame
##===============================================
rm(object)
gc()


##============================
#
##===============================================================

png("D:/ST vs log(traits)合成.png",height=10,width=10,res=300, units = "in");
par(mfrow=c(2,2))

##===================== 1. abundance (50ha) ==================================
#HSD.AB=read.table(choose.files(),header = T)  ##D:/AB.HSD
#st.HSDAB=merge.data.frame(st3,HSD.AB,by.x="species",by.y="species")
cor.test(st.HSDAB$st3,st.HSDAB$AB.HSD,method="spearman")
lm.HSDAB=lm(log(st.HSDAB$AB.HSD)~log(st.HSDAB$st3))
summary(lm.HSDAB)

plot(log(st.HSDAB$st3),log(st.HSDAB$AB.HSD),xlab="Log (ST)",
     ylab="Log (Abundance)",main="",col=1,cex=0.5)
abline(lm.HSDAB,col=2)
text( -10,9.8,expression(italic(r^"2")),cex=1)   
text( -9,9.8,cex=1," = 0.28")
text( -10,9.2,expression(italic(P)),cex=1) 
text( -9,9.2,cex=1," < 0.001")
##=======================================================
dev.off()


#####


theme_zg <- function(..., bg='white'){
  require(grid)
  theme_classic(...) +
    theme(rect=element_rect(fill=bg),
          plot.margin=unit(rep(0.5,4), 'lines'),
          panel.background=element_rect(fill='transparent', color='black'),
          panel.border=element_rect(fill='transparent', color='transparent'),
          panel.grid=element_blank(),
          axis.title = element_text(color='black', vjust=0.1),
          axis.ticks.length = unit(-0.4,"lines"),
          axis.ticks = element_line(color='black'),
          axis.ticks.margin = unit(0.8,"lines"),
          legend.title=element_blank(),
          legend.key=element_rect(fill='transparent', color='transparent'))
}


####################多图组合
#下载和安装包
#install.packages(c("ggolot2","Rmisc","lattice","plyr"))
library(ggplot2)
library(Rmisc)
library(lattice)
library(plyr)

#绘图
p1 <- ggplot(mtcars,aes(mpg,hp,color=factor(vs)))+geom_point()
p2 <- ggplot(mtcars,aes(mpg,wt,color=factor(vs)))+geom_point()
p3 <- ggplot(mtcars,aes(mpg,drat,color=factor(vs)))+geom_point()
p4 <- ggplot(mtcars,aes(mpg,cyl,color=factor(vs)))+geom_point()

#将多图组合起�?
multiplot(p1,p2,p3,p4,cols = 1)


####图形边框
plot(1:5)
box(which = "outer", col = "red", lwd = 10)

par(omi = c(1, 1, 1, 1))
plot(1:5)
box(which = "outer", col = "red",  lwd = 10)
box(which = "inner", col = "blue", lwd = 5)


par(omi = c(1, 1, 1, 1), mfrow = c(1, 2))
plot(1:5)
box(which = "figure", col = "red", lwd = 2)
plot(1:5)
box(which = "figure", col = "blue", lwd = 2)

par(omi = c(1, 1, 1, 1), mfrow = c(1, 2))
plot(1:5)
box(which = "plot",   col = "red", lwd = 2)
box(which = "figure", col = "red", lwd = 2)
plot(1:5)
box(which = "plot",   col = "blue", lwd = 2)
box(which = "figure", col = "blue", lwd = 2)



#https://www.cnblogs.com/xudongliang/p/6927573.html

par(mfrow=c(1,1))

attach(mtcars)
plot(wt, mpg)
abline(lm(mpg~wt))
title("Regression of MPG on Weight")
box(which = "plot", col = "red", lwd = 2)
box(which = "figure",col = "blue", lwd = 4)

par(oma=c(0,0,0,0), mar=c(0,0,0,0))
attach(mtcars)
plot(wt, mpg)
abline(lm(mpg~wt))
title("Regression of MPG on Weight")
box(which = "plot",  col = "red",    lwd = 2)
box(which = "figure",col = "blue",   lwd = 4)

par(oma=c(2,2,2,2))
attach(mtcars)
plot(wt, mpg)
abline(lm(mpg~wt))
title("Regression of MPG on Weight")
box(which = "plot",  col = "red",    lwd = 2)
box(which = "figure",col = "blue",   lwd = 4)
box(which = "outer", col = "black",  lty = 8)

#a lines from github
