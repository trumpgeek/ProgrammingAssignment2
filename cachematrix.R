##LPC
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached matrix data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
#########################
## result of execution
##> b<-makeCacheMatrix()
##> b$set(B)
##> cacheSolve(b)
##[,1]       [,2]
##[1,] -0.3333333  0.6666667
##[2,]  0.6666667 -0.3333333
##> solve(B)
##[,1]       [,2]
##[1,] -0.3333333  0.6666667
##[2,]  0.6666667 -0.3333333
#> 
