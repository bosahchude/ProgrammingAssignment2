## The 'makeCacheMatrix' and 'cacheSolve' funtions work together to speed up matrix
## computations by caching the value of the matrix inverse

## This makeCacheMatrix fuction uses the <<- symbol to create a special matrix 
## that can store its inverse so it dosen't need to be computed everytime it's required

makeCacheMatrix <- function(x = matrix()) {
     i <- NULL
     set <- function(y) {
          x <<- y
          i <<- NULL
     }
     
     get <- function() x
     setinverse <- function(inverse) i <<- inverse
     getinverse <- function() i
     
     list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The cacheSolve function checks to see if its argument has a precached version of 
## its inverse before computing it. If the inverse is already cached, it returns that
## cached value. Othewise, the inverse is computed from scratch and stored

cacheSolve <- function(x, ...) {
     
     ## Return a matrix that is the inverse of 'x'
     i <- x$getinverse()
     
     if(!is.null(i)) {
          message("Retrieving cached inverse")
          return(i)
     }
     
     data <- x$get()
     
     i <- solve(data, ...)
     
     x$setinverse(i)
     
     i
}
