#rm(list=ls())#cleanup residuals


#Assignment: Caching the Inverse of a Matrix

#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse


## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#makeCacheMatrix function set up functions as getters and setters for variable matrix (x) and inversed matrix (inv_mat).
makeCacheMatrix <- function(x = matrix()) {
  inv_mat <- NULL
  set <- function(y) {
    x <<- y
    inv_mat <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv_mat <<- inverse
  getinverse <- function() inv_mat
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

#cacheSolve: This function computes the inverse of the special "matrix" 
#returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.


# inverse of the matrix created with the makeCacheMatrix. It first checks to see if the inverse has already been calculated. 
#If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the matrix
##and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_mat <- x$getinverse()
  if(!is.null(inv_mat)) {
    message("getting cached data")
    return(inv_mat)
  }
  data <- x$get()
  inv_mat <- solve(data)
  x$setinverse(inv_mat)
  inv_mat
}
