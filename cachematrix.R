## Pair of functions that cache the inverse of a matrix

##makeCacheMatrix takes in the matrix values and caches its inverse

makeCacheMatrix <- function(x = matrix()) 
  {
    matx <- NULL;
    set <- function(y) 
         {
           x <<- y
           matx <<- NULL
         }
    get <- function() x
    setmatx <- function(solve) matx <<- solve
    getmatx <- function() matx
    list(set = set, get = get, setmatx = setmatx, getmatx = getmatx) 
 } 


# CacheSolve computes the inverse of the input matrix in makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  matx <- x$getmatx()
  if(!is.null(matx)) {
    message("getting cached data")
    return(matx)
  }
  data <- x$get()
  matx <- solve(data)
  x$setmatx(matx)
  matx
}
