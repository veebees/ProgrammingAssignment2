## Put comments here that give an overall description of what your
## functions do - The attempt here is to reduce computational time by cacheing time
## intensive taks such as calculating the inverse of a matrix. This is a solution
## for square matrices. 

## Write a short comment describing this function - This function calculates the
## inverse of the matrix and caches it

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve(x)) m <<- solve(x)
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function - This function first tries to 
## retrieve the inverse of a matric if it is stored in cache, if not it calculates

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
