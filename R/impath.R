impath = function(pref="otocystImageA") {
 pngfn = paste0("images/", pref, ".png")
 system.file(pngfn, package="BMI713pack")
}

plotImage = function(pref="otocystImageA", ...) {
 im = readPNG( impath(pref) )
 grid.raster(im, ...)
}

getOtoExprs = function() {
 xpath = dir(system.file("xlsx", package="BMI713pack"), full=TRUE)
 exsh = read_excel( xpath, sheet = 5 )
 tmp = exsh[-(1:10), -(1:7)] 
 for (i in 1:ncol(tmp)) tmp[,i] = as.numeric(tmp[,i])
 colnames(tmp) = exsh[10,-(1:7)]
 tmp
}

getNdSox = function() {
 oo = getOtoExprs()[,c("Neurod1", "Brip1", "Sox10")]
 for (i in 1:3) oo[,i] = as.numeric(oo[,i])
 oo = oo[oo$Brip1>0,]
 oo[,c(1,3)]
}

vcImages = function() {
  require(png)
  alli = dir(system.file("images", package="BMI713pack"), full=TRUE, pattern="png$")
  bn = basename(alli)
  allim = lapply(alli, readPNG)
  names(allim) = gsub(".png", "", bn)
  allim
}
  
