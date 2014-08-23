## This R file can help to compute the inverse of a matrix, and 
## if you have computed it before in this program, you can get 
## the cached data instead of calculating it repeatedly.
## Attention: assume that the inputted matrix is always 
## INVERTIBLE.


## The first function, makeCacheMatrix creates a special 
## 'matrix', which is a list containing functions to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the reverse
## 4. get the value of the reverse

makeCacheMatrix <- function(x = matrix()) {
  r <- NULL
  set <- function(y){
    x <<- y
    r <<- NULL
  }
  get <- function() x
  set_reverse <- function(reverse) r <<- reverse
  get_reverse <- function() r
  list(set = set, get = get, 
       set_reverse = set_reverse,
       get_reverse = get_reverse)
}

## The second function, cacheSolve calculates the reverse of the
## special 'matrix' created with the above function.
## If the reverse has already been calculated. If so, it gets the 
## reverse from the cache and skips the computation. 
## Otherwise, it calculates the reverse of the matrix and sets the 
## value of the reverse in the cache via the set_reverse function.
cacheSolve <- function(x, ...) {
  r <- x$get_reverse()
  if (!is.null(r)){
    message("getting cached matrix")
    return(r)
  }
  data <- x$get()
  r <- solve(data, ...)
  x$set_reverse(r)
  r
}