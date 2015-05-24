makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL   # The inverse is set to null
   
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      
      # This returns the object
      get <- function() x
      
      # This sets up the setInverse function and the getInverse function to retrieve it 
      setInverse <- function(inverse) inv <<- inverse
      getInverse <- function() inv
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


# This creates the function for caching the results
cacheSolve <- function(x, ...) {
     
      inv <- x$getInverse()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data)  # Calcule the inverse via solve()
      x$setInverse(inv)
      inv
} #Close off function
