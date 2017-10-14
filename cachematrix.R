# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. 

#The following 2 functions are to cache the inverse of a matrix.

# makeCacheMatrix creates a special "matrix" object that can cache its inverse
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse of the matrix
# 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
  
  set <- function(y){
      x <<- y
      m <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed, and if the inverse has been computed,
# it returns the result. If not, it computes the inverse, sets the value in the 
# cache via the setinverse function.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(x)
  x$setinverse(m)
  m
}