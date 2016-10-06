
bvnShine = function() {
require(shiny)

# visualizer
bvncont = function(npts=50, r=0, v1=2, v2=2, render="contour") {
 require(mvtnorm)
 vv = max(c(1,v1,v2)) # don't allow below 1
 x = seq(-2*vv,2*vv,length=npts)
 dom = matrix(0, nr=npts, nc=npts)
 sig = matrix(c(v1, r*sqrt(v1*v2), r*sqrt(v1*v2), v2), 2)
 for (i in 1:npts) {
  dom[i,i] = dmvnorm(c(x[i], x[i]), sig=sig)
  if (i == npts) break
  for (j in (i+1):npts) {
    dom[i,j] = dmvnorm(c(x[i], x[j]), sig=sig)
    dom[j,i] = dmvnorm(c(x[j], x[i]), sig=sig)
  }
 }
 if (render == "contour") {
  contour(x=x, y=x, dom, main=paste("corr = ", r, ", v1= ", v1,
    ", v2= ", v2, sep=""), labcex=.9, asp=1)
  }
 else {
   require(threejs)
   mm = data.matrix(expand.grid(x,x))
   mm = cbind(mm, apply(mm,1,function(x)dmvnorm(x, sig=sig)))
   scatterplot3js( mm, renderer = "webgl" )
   }
 }
 

 #scatterplot3js(x, y, z, height = NULL, width = NULL, axis = TRUE,
 #      num.ticks = c(6, 6, 6), x.ticklabs = NULL, y.ticklabs = NULL,
 #      z.ticklabs = NULL, color = "steelblue", size = 1, labels = NULL,
 #      label.margin = "10px", stroke = "black", flip.y = TRUE, grid = TRUE,
 #      renderer = c("auto", "canvas", "webgl"), signif = 8, bg = "#ffffff",
 #      xlim, ylim, zlim, pch, ...)

    
ui = fluidPage(
 titlePanel("BVN density explorer"),
 helpText("Use sliders to set correlation and marginal variances.  Use D3 tab to see a projection that you can rotate with mouse."),
 sidebarPanel(
  fluidRow(
     sliderInput("corr", "Corr. coeff.", 0, min=-1, max=1, step=.01)
     ),
  fluidRow(
     sliderInput("varx", "Var. X", 2, min=.1, max=50, step=.1)
     ),
  fluidRow(
      sliderInput("vary", "Var. Y", 2, min=.1, max=50, step=.1)
     )
  ), # end sidebar
 mainPanel(
   tabsetPanel(
    tabPanel("contour", plotOutput("cont")),
    tabPanel("D3", scatterplotThreeOutput("D3"))
   )
 ) # end main
)

server = function(input, output) {
  output$cont = renderPlot(
    bvncont( 50, input$corr, input$varx, input$vary ) 
    )
  output$D3 = renderScatterplotThree(
    bvncont( 50, input$corr, input$varx, input$vary, render="D3"  )
  )
}
   
shinyApp(ui, server)
}   
  
