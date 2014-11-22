## cachematrix: This script contains 2 functions that can be together to efficiently calculate 
## the inverse of a square matrix.When used together, the user can cache the inverse of a matrix
## then access it multiple times without recalculating it. The user need not be aware of whether the
## inverse has been already calculated, as the script will cacluate the inverse if it has not yet 
## been calculated

## Assumptions:
##    - the matrix, x,  that is provided to be inverted is invertible
## Calling prototype: 
##  cached_matrix <- makeCacheMatrix(x)         // create a cacheing wrapper for the matrix x 
##  myInverse <- cacheSolve(cached_matrix)      // invert it if necessary, or return the existing inverse
## or
## cached_matrix <- makeCacheMatrix()     // create an empty wrapper
## cached_matrix$set_matrix(x)            // put a matrix in the wrapper
## myInverse <- cacheSolve(cached_matrix) // invert it if necessary, or return the existing inverse

## Write a short comment describing this function
## This function creates a special wrapper around a matrix that, when accessed as shown in the 
## prototype calling functions above, 
## will cache the inverse of the matrix. THis inverse can then be retrieved by simply calling the same
## function to invert the matrix again. The special wrapper keeps track of whether the inverse has been 
## calculated or not, and if it has not, it calculates it and makes it available for the next call.

## function: makeCacheMatrix-
## Setting up the function:
## required inputs x  - an invertible matrix
##    - set through the constructor makeCacheMatrix(x)
##    - or through the function set_matrix(x) 
## Calculating the inverse: 
## The inverse can be calculated by 
##    - calling the "cacheSolve" function, descibed below. 
##    - or directly using the function setInverse(x) 
## Outputs - 
##   getInverse(): the inverse of the input matrix
##   get_matrix(): the original matrix, x
##    


makeCacheMatrix <- function(x = matrix()) {
      m_inv <- NULL
      set <- function(y) {
            x <<- y
            m_inv <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) m_inv <<- inverse
      getInverse <- function() m_inv
      list(set_matrix = set, get_matrix = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inverse <- x$getInverse()
      if(!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
      }
      data <- x$get_matrix()
     
       inverse<- solve(data, ...)
     
      x$setInverse(inverse)
      inverse
}
