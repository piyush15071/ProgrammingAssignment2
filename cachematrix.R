##  Set of functions that can store and recall a cached version
##  of a square matrix and its inverse matrix.

makeCacheMatrix <- function(x = matrix()) {

  i <- NULL
  
  set <- function(y){
    x <<- y 
    i <<- NULL  
  }
  
  get <- function() x
  
  setinv <- function(inv) i <<- inv
  
  getinv <- function() i
  
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)    
}


##  Solves the inverse of the cached square matrix created by
##  makeCacheMatrix above. If the inverse of the cached matrix
##  has already been solved, returns the cached version of the
##  inverse matrix. Otherwise, solves the inverse matrix and
##  stores it in the cache.

cacheSolve <- function(x, ...) {
  
  i <- x$getinv()

  if(!is.null(i)) {
    message("Retreiving the cached inverse matrix...")
    return(i)
  }
  
  m <- x$get()
  i <- solve(m, ...)
  x$setinv(i)
  i
}
