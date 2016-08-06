impath = function(pref="otocystImageA") {
 pngfn = paste0("images/", pref, ".png")
 system.file(pngfn, package="BMI713pack")
}

plotImage = function(pref="otocystImageA", ...) {
 im = readPNG( impath(pref) )
 grid.raster(im, ...)
}
