
s2.bands<-readRDS("s2.bands.rds")
band.colors<-c("#680085", "#0036ff", "#09ff00", "#ed0000", "#e70000", "#e70000", 
               "#e70000", "#e70000", "#a20000", "#a20000","#a20000", "#500000", "#500000")
rectangles<-apply(s2.bands, 1, function(x){ 
  list(
    
    type = "rect",
    fillcolor = band.colors[[ as.integer(x[[1]]) ]],
    line = list(color = band.colors[[ as.integer(x[[1]]) ]] ),
    opacity = 0.7,
    x0 =  as.integer(x[['min.wl']]),
    x1 =  as.integer(x[['max.wl']]),
    xref = "x",
    y0 = 0,
    y1 = 0.6,
    yref = "y"
    
  )
} )
