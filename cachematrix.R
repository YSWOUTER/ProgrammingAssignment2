## Caching of an inverted matrix
## Programming Assigment 2

## makeCacheMatrix()builds a set of functions and returns the functions within a list to the parent environment

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get, setinverse = setInverse, getinverse = getInverse)
}


## Second function calculates inverse of matrix. 
## First check if inverse has already been calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   
    m <- x$getinverse()
    if(!is.null(m)){
      message("getting cached data")
      return (m)
    }
    
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
  
  