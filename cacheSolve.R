cacheSolve <- function(x, ...) {       
      inverse <- x$getInverse() 
      if (!is.null(inverse)){ 
            message("getting cached data") 
            return (inverse) 
      }     
      data <- x$get()   
      inverse <- solve(data, ...)  
      x$setInverse(inverse) 
      inverse 
}