##########
#This assignment is about functions that can cache the inverse of a matrix
#Caching is about using memory to avoid excess computation.
#Matrix inversion is usually a costly computation so there may be some benefit to 
#caching the inverse of a matrix rather than compute it repeatedly.
##########

##This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) 
{
  z <- NULL
  set <- function(y){
  x <<- y
  z <<- NULL
  }
  get <- function()x
  setInv <- function(inverse) z <<- inverse
  getInv <- function() z 
  list(set = set, get = get, 
  setInv = setInv, 
  getInv = getInv)
}


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
{
## Return a matrix that is the inverse of 'x'
  z <- x$getInv()
  if(!is.null(z)){
  message("getting cached data")
  return(z)
  }
  mat <- x$get()
  z <- solve(mat,...)
  x$setInv(z)
  z
}
