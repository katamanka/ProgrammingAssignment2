## PA2. 
## The two functions provide an example for the caching  
## features in R for time-consuming calculations.
## In this example the result of the computation of the inverse of a matrix is 
## cached when first performed and then recalled when required.

## --- The code is a bit overdocumented; that helped me to figure out what I was doing...  ---

## makeCacheMatrix() provides the architecture for the caching process. 
## It exposes a list of functions to manipulate the input of the matrix and its
## inverse.

makeCacheMatrix <- function(x = matrix()) {
  # the x argument is the input matrix.
  
  # clear the cache
  i <- NULL
  
  # set(): change the input matrix.
  # Update its value and discard the result that was previously cached.
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  # get(): return the input matrix 
  get <- function() x
  
  # setinverse(): set the value of the result. This is invoked by cacheSolve 
  # to store the result in the cache.
  setinverse <- function(inverse) i <<- inverse
  
  # getinverse(): return the contents of the cache
  getinverse <- function() i
  
  # return the list of functions, which will be attributes of the object
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# cacheSolve() performs the actual computation and outputs the result, either 
# recalling it from the cache or computing it if the cache is empty.

cacheSolve <- function(x, ...) {
  # Takes as an argument the object instantiated by makeCacheMatrix
  # and returns the inverse of the matrix stored in the object.
  
  # Get the inverse as stored in the x object. If it is not NULL
  # then return it, notifying the user that the result is obtained 
  # from the cache.
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  # If we got here then the cache is empty. Compute the inverse
  # and store it in the cache using the setinverse function defined in
  # makeCacheMatrix, then return the result.
  data <- x$get()
  # In the real world we should check here if the input matrix is invertible 
  i <- solve(data, ...)
  x$setinverse(i)
  i
}