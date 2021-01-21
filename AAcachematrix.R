makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function()x
  setinv <- function(inverse)inv <<- inverse
  getinv <- function() {
    inv<-ginv(x)
    inv%*%x
  }
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)
}

cacheSolve <- function(x, ...) 
{
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("getting cached data!")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinv(inv)
  inv
}

f<-makeCacheMatrix(matrix(1:4,2,4))
f$get()
f$getinv()