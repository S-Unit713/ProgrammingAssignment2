## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  I <- NULL
  set <- function(y){
    x<<-y
    I<<-NULL
  }
  get<- function() x
  Setinverse <- function(inv) I <<- inv
  Getinverse <- function() I
  list(set = set, get = get,
       Setinverse = Setinverse,
       Getinverse = Getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  I <- x$Getinverse()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$Setinverse(m)
  m
}
