library(raster)
library(rgdal)
library(spdep)

p <- shapefile("D://Tugas 3. Moran's I/TugasMoransI.shp")
p <- p[p$PROVINSI=="JAWA TENGAH", ]

data.frame(p)

par(mai=c(0,0,0,0))
plot(p, col=2:7)
xy <- coordinates(p)
points(xy, cex=1, pch=20, col='white')
text(p, 'DESA', cex=1)

w <- poly2nb(p, row.names = p$Id)
class(w)

summary(w)

wm <- nb2mat(w, style = 'B')

n <- length(p)

y<- p$PengNDWI19
ybar <- mean(y)

dy <- y <- ybar
g <- expand.grid(dy, dy)
yiyj <- g[,1] * g[,2]

pm <- matrix(yiyj, ncol = n)

pmw <- pm * wm

spmw <- sum(pmw)

smw <- sum(wm)
sw <- spmw / smw

vr <- n / sum(dy^2)

MI <- vr * sw
MI

wn <- nb2listw(w)
wn

moran.test(p$X2001, wn,randomisation = F)
moran.test(p$X2002, wn,randomisation = F)
moran.test(p$X2003, wn,randomisation = F)
moran.test(p$X2004, wn,randomisation = F)
moran.test(p$X2005, wn,randomisation = F)


