EDAG.F <-
function(x){
y <- last(x$Age)
a <- first(x$Age)
part.one <- sum(x$dx[-y]*x$ex[-1])
part.two <- 1-(sum(x$dx[-y]*x$ax[-y]))
edagger <- part.one + part.two
#H <- edagger/x$ex[1]
return(edagger)
}


