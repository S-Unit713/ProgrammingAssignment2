

## create a random matrix and then pass the matrix as makeCacheMatrix(matrix object)

makeCacheMatrix <- function(x = matrix()) {

  i <- NULL
  set <- function(y){
    x<<-y
    i<<-NULL
  }
  get<- function() x
  Setinverse <- function(inv) i <<- inv
  Getinverse <- function() i
  list(set = set, get = get,
       Setinverse = Setinverse,
       Getinverse = Getinverse)
}


## Run functions as >cacheSolve(makeCacheMatrixObject)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  tracemem(x)
  b<-x
  i <- b$Getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data, ...)
  x$Setinverse(i)
  save(i,file = "myfile.Rdata")
  i
}
