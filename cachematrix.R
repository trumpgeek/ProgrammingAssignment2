##LPC
## Do it like it is written in the sample just replacing 'mean'mean with 'solve'
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
## Doing it like it is laid out in the sample except make 'mean' to be 'solve'
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
## Here is an example of execution. Create a matrix called B
##> B
##     [,1] [,2]
##[1,]    1    2
##[2,]    2    1
## result of execution
##> b<-makeCacheMatrix()
## now make it work for B
##> b$set(B)
##> cacheSolve(b)
##[,1]       [,2]
##[1,] -0.3333333  0.6666667
##[2,]  0.6666667 -0.3333333
## Make sure it returns the same thing without the cacheSolve function
##> solve(B)
##[,1]       [,2]
##[1,] -0.3333333  0.6666667
##[2,]  0.6666667 -0.3333333
## The above shows the function appears to be correct
