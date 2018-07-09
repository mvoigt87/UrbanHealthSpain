dir() -> pp
cbind(pp)
pp[c(4,14:20)] -> pp
i <- pp[1]
for (i in  pp ) {
 
  dir(path = i, pattern = '*.xml')  
  
}
  
