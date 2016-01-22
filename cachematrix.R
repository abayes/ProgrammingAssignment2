## These functions create a matrix and compute the inverse of the matrix

## This function creates a special "matrix" object that can cache its inverse
## This function stores a list of 4 functions that are created in this function
## HINT: To use this function, pass it a matrix and assign it to a variable:
## ie: a <- makeCacheMatrix(matrix(c(-1, -2, 3, 3), 2,2))
## 'get' is a function that prints the matrix. It takes no parameters
## 'getsol' is a function that prints the inverse of the matrix. It takes no parameters
## 'set' is a function that can be used to define a new matrix. If the matrix is redefined,
## then the inverse is set to NULL as is hasn't been calculated or set yet.
## 'setsol' is a function that can be used to define the inverse. It does not actually solve.
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  get <- function() x
  getsol <- function() s
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  setsol <- function(solve) s <<- solve
  list(get = get,
       getsol = getsol,
       set = set,
       setsol = setsol)
}

## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache
## If the inverse hasn't been set or solved for, this function solves for the inverse.
## If the inverse has been set, this returns the cached value
cacheSolve <- function(x, ...) {
  s <- x$getsol()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsol(s)
  s
}  

