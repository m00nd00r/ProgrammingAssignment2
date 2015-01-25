# This function helps shorten the amount of time taken to find the inverse of a matrix by caching 
# the inverse, if the matrix is not changing, to be used when needed. When a change is made to the 
# matrix, the inverse is updated and cached for future use.

## makeCacheMatrix creates a special "matrix", which is actually a list, that contains functions to:
## 1. Set a matrix
## 2. Get a matrix
## 3. Set a inverse
## 4. Get a inverse

## These values will be stored in an environment different from that in which they were created
## using the '<<-' operator.

makeCacheMatrix <- function(x = matrix()) {
     i <- NULL
     set <- function(x = matrix){
          x <<- y
          i <<- NULL
          
     }
     get <- function() x
     setinverse <- function(solve) i <<- solve
     getinverse <- function () i
     list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
     
}


## cacheSolve calculates the inverse of the special "matrix" created with makeCacheMatrix.
## 1. Check to see if the inverse has already been calculated.
## 2. If yes, it gets the inverse from the cache and skips the calculation.
## 3. If not, it calculates the inverse of the data and sets the value of the inverse in the cache via
##    the setinverse function.

cacheSolve <- function(x, ...) {
     m <- x$getinverse()
     if(!is.null(i)){
          message("getting cached data")
          return(i)
     }
     data <- x$get()
     m <- solve(data, ...)
     x <- setinverse(i)
     m
}
