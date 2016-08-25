impath = function(pref="otocystImageA") {
 pngfn = paste0("images/", pref, ".png")
 system.file(pngfn, package="BMI713pack")
}

plotImage = function(pref="otocystImageA", ...) {
 im = readPNG( impath(pref) )
 grid.raster(im, ...)
}

getOtoSheet = function(shnum=5) {
 xpath = dir(system.file("xlsx", package="BMI713pack"), full=TRUE)
 read_excel( xpath, sheet = shnum )
}

getOtoClass = function() {
 sh = as.data.frame(getOtoSheet())
 data.frame(cellnum=as.numeric(sh[11:392,1]), Aclass=as.character(sh[11:392,3]),
   Bclass=as.character(sh[11:392,4]),
   stringsAsFactors=FALSE)
}

getOtoExprs = function() {
 xpath = dir(system.file("xlsx", package="BMI713pack"), full=TRUE)
 exsh = read_excel( xpath, sheet = 5 )
 cellnum = as.numeric(exsh[11:392,1][[1]])
 tmp = as.data.frame(exsh[-(1:10), -(1:7)])
 for (i in 1:ncol(tmp)) tmp[,i] = as.numeric(tmp[,i])
 colnames(tmp) = exsh[10,-(1:7)]
 tmp = data.frame(cellnum=cellnum, as.data.frame(tmp))
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

exprStateTab = function(genename) {
 require(dplyr)
 ex = as.data.frame(getOtoExprs())
 cc = as.data.frame(getOtoClass())
 d = merge(ex, cc, by="cellnum")
 list(Atab = table(d$Aclass, d[[genename]]>0),
      Btab = table(d$Bclass, d[[genename]]>0))
}
