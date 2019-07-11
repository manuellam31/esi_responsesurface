#ESI 3200 QTRAP optimization with Box-Behnken design

library(rsm)
library(Vdgraph)

#Optimazing temperature, esi voltage and curtain gas
bbdmodel<-bbd(y~x1+x2+x3,block = FALSE,n0 = 3, coding=list(x1~(TEM-600)/100, x2~(ESIV-4500)/500, x3~(CUR-20)/10),randomize=FALSE)
head(bbdmodel)
Vdgraph(bbdmodel[ , 3:5]) #variance dispersion graph

bbdmodel$y <- rnorm(n = 15, mean = 1000,sd = 300) #test #you should use your data
qdmodel <-rsm(y~SO(x1, x2, x3), data = bbdmodel) 
summary(qdmodel)
contour(qdmodel, ~ x1+x2+x3, image = TRUE )
persp(qdmodel, ~ x1+x2+x3, zlab="Compound Intensity",contours = "col", col = terrain.colors(50))
