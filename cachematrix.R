## The overall purpose of the following functions is to take a square matrix as an input, 
## calculate the inverse of that matrix, store the inverse in cache, and output the inverse.
## If the cacheSolve function is called more than once then the output value is returned from
## cache and not calculated again.


## The first function, makeCacheMatrix, returns a list of functions. Its purpose is to 
## store a given matrix and a cached value of the inverse of the given matrix. This 
## function contains the functions that do the following:

## 1. setMatrix: sets the value of the given matrix
## 2. getMatrix: gets the value of the given matrix
## 3. setInverse: sets the value of the inverse matrix
## 4. getInverse: gets the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
      # 'inverse' is the cached value of the inverse of the input matrix, initiated as NULL
      inverse <- NULL
      
      setMatrix <- function(y) {
            x <<- y
            inverse <<- NULL
      }
      getMatrix <- function() x
      setInverse <- function(solve) inverse <<- solve
      getInverse <- function() inverse
      list(setMatrix = setMatrix, getMatrix = getMatrix, 
           setInverse = setInverse, getInverse = getInverse)
}

## The second function calculates the inverse of the special "matrix" created with the above 
## function. However, it first checks to see if the inverse has already been calculated. If so, 
## it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse 
## of the matrix and sets the value of the inverse in the cache via the setInverse function.


cacheSolve <- function(x, ...) {
      # get the cached value of 'inverse' from the 'makeCacheMatrix' function
      inverse <- x$getInverse()
      # if the cached value of 'inverse' is not NULL return it
      if(!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
      }
      # otherwise get the original matrix (data), caclulate its inverse (solve(data)), store it 
      # in the cache, and return the inverse
      data <- x$getMatrix()
      inverse <- solve(data, ...)
      x$setInverse(inverse)
      inverse
}
