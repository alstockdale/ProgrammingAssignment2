## Programming assignment 2 week 3 JHU Programming in R
## Code to cache inverse of Matrix

## Calculates and caches the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  matrix_inv <- NULL
  get <- function() x
  set <- function(y) {
    matrix_inv <<- NULL
    x <<- y
    
  }
  getinv <- function() matrix_inv
  setinv <- function(inverse) {
    matrix_inv <<- inverse
  }
  return(list(
    set = set,
    get = get,
    getinverse = getinv,
    setinverse = setinv
  ))
}

# Cachesolve to store and retrieve result of matrix inversion 
cacheSolve <- function(x,...) {
  inverse <- x$getinverse()
  if (!is.null(inverse)) {
    return(inverse)
  }
  m <- solve(x$get())
  x$setinverse(m)
}

# Test function of matrix
aMatrix <- rbind(c(4,1,1),c(2,1,-1),c(1,1,1))
x <-makeCacheMatrix(aMatrix)
#Retrieve matrix using get function
x$get()
#Use cachesolve function to create inverse of matrix
cacheSolve(x)
# Retrieve result of matrix inversion
x$getinverse()

