## Function makeCacheMatrix will cache the inverse of a matrix

## "makeCacheMatrix" will create a matrix and caches its inverse
##  using "solve" function

makeCacheMatrix <- function(x = matrix()) {
   s <- NULL
   set <- function(y) {
     x <<- y
     s <<- NULL
   }
   get <- function() x
   setsolve <- function(solve) s <<- solve
   getsolve <- function() s
   list(set = set, get = get,
        setsolve = setsolve,
        getsolve = getsolve)
}


## The function "cacheSolve" will calculate the inverse of special matrix
## created by "makeCacheMatrix" function. It first checks to see if the
## inverse is already there (using if(!is.null(s))), Otherwise it calculates
## the inverse and sets the value of the inverse in the cache via 
## "setsolve" function

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)){
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
      
}


