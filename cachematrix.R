## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix: This function creates a special 
# "matrix" object that can cache its inverse.

# cacheSolve: This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve the 
# inverse from the cache.


## Write a short comment describing this function

# The function, makeVector creates a special matrix which is really a list 
# containing a function to 
# 1.set the value of the matrix
# 2.get the value of the matrix
# 3.set the inverse of the matrix
# 4.get the inverse of the matirx


makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(i) inv <<- i
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)  
  
  
}


## Write a short comment describing this function
# The following function calculates the inverse of the special matrix created
# with the above function. However, it first checks to see if the invers has 
# already been calculated. If so, it gets the inverse from the cache and skips 
# the computation. Otherwise, it calculates the inverse of the data and sets 
# the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  inv <- solve(x$get(), ...)
  x$setinv(inv)
  inv
}

