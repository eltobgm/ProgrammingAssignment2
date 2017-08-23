## this function is written as a demonstration of the lexical scoping rules 
## for the Data Science, Course 2 R programming, week 3 assignment
## the purpose of the function is to calculate the inverse of matrix after it
## checks the caches to verify that it has not been stored in cache
## the function is split in two nested functions. 
## the syntax should be cachesolve(makeCacheMatrix(a)) , where a is a matrix
## or the function can be used by storing the returned value from makeCacheMatrix(a)
## in a variable that gets called by the function cachesolve


makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}
