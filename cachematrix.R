## The two functions makeCacheMatrix and cacheSolve together create
## a special object that that stores a matrix and its inverse.

## makeCacheMatrix creates a special "vector" that is really a list ## containing functions that
## 1. Set the value of the matrix,
## 2. Get the value of the matrix,
## 3. Set the value of the matrix's inverse, and
## 4. Get the value of the matrix's inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv, getinv = getinv)
}


## cacheSolve calculates the inverse of the special "vector"
## created with the above function. It first checks to see if the ## inverse has already been calculated. If it has, the function
## gets the inverse from the cache and skips the computation.
## Otherwise it calculates the inverse of the matrix and sets the
## value of the inverse in the cache via the setinv function.
## cacheSolve assumes the input matrix is invertible.

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
