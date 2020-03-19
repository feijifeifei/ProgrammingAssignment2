## This project includes two functions. The "makeCacheMatrix" cach the inverse of a matrix. 
## The "cacheSolve" compute the inverse of a special matix, if it has not been stored in the cache by the previous "makeCacheMatrix" function. 

## This function create a special "matrix" object that can cache its inverse. For example, mm<- makeCacheMatrix(matrix(runif(9), nrow=3))
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## his function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache. For example, cacheSolve(mm) will return a value 
## when run in the first time. But it will return "getting cached data" and the cached value in the 2nd run.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
