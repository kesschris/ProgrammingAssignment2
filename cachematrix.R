## The makeCacheMatrix function works in conjunction with the cacheSolve
## function in order to save the inverse of a matrix in cache for easy
## retreval. 

makeCacheMatrix <- function(x = matrix()) {      
      ## set value of cacheInverse to NULL each time function is called
      cacheInverse <- NULL    
    
      ## Save values of X and initialises the cacheInverse vector in
      ## another environment. This is necessary in order to compare values
      ## later on.
      set <- function (y){ 
            x <<- y 
            cacheInverse <<- NULL 
      } 
     
      ## Gets the matrix 
      get <- function () x
      
      ## Saves the inverted matrix to the other environment.
      setInverse <- function(inverse) cacheInverse <<- inverse   
      
      ## Returs the inverted matrix from the other environment.
      getInverse <- function () cacheInverse 
      
      ## List of functions returned, aka methods, to be used by cacheSolve
      list ( set = set, get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)       
} ## End of makeCacheMatrix

## cacheSolve is to designed to be used in lieu of the Solve function in 
## cases where repeated, yet time consuming, calculations are being performed. 
cacheSolve <- function(x, ...) {
      ## Set value of Inverse
      inverse <- x$getInverse() 
      
      ## Return the inverse matrix if its been cached
      if (!is.null(inverse)){ 
            message("getting cached data") 
            return (inverse) 
      }  
      
      ## Returns the matrix to be calculated
      data <- x$get() 
      ## Calculate the inverse fo the maxtrix and store it in 'data'.
      inverse <- solve(data, ...)  
      ## Save the newly calculated inverse in cache using setInverse.
      x$setInverse(inverse) 
      ## Return the inversed matrix to the promt. 
      inverse 
}