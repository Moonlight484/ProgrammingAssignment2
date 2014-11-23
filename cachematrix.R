## cachematrix: This script contains 2 functions (makeCacheMatrix and cacheSolve) that can be used 
## together to efficiently calculate 
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

## function: makeCacheMatrix: Create a cacheing wrapper for the matrix x The wrapper caches the inverse of the matrix
## when the setInverse(inverse) function is called. 
## Setting up the function:
## required inputs x  - an invertible matrix
##    - set through the constructor makeCacheMatrix(x)
##    - or through the function set_matrix(x) 
## Calculating the inverse: 
## The inverse can be calculated by 
##    - calling the "cacheSolve" function, descibed below. 
##    - or directly using the function setInverse(x) 
## Outputs - 
##   getInverse(): returns the inverse of the input matrix
##   get_matrix(): returns the original matrix, x
##    

## The constructor simply inputs the matrix that is passed as an argument or an empty matrix if none is provided
## The inverse is initialized to NULL. 
makeCacheMatrix <- function(x = matrix()) {
      m_inv <- NULL
## set: the internal call to set the matrix "y" into the cache wrapper and set the inverse of the matrix 'y" to null
      set <- function(y) {
            x <<- y
            m_inv <<- NULL
      }
      ## get: internal call to return the original input matrix
      get <- function() x
      
      ## setInverse: sets the inverse matrix- can actually be done independently of the input matrix x, so be sure
      ## not to just set the inverse independently of the original matrix!!
      setInverse <- function(inverse) m_inv <<- inverse
      
      ## getInverse the internal call to return the inverse of the input matrix
      getInverse <- function() m_inv 

      ## the "list" function allows the user to set pretty names for the internal functions. The names on the left are 
      ## the exposed names(what the user would see). These are mapped to the internal call names. See "cacheSolve" to see
      ## how to access the external names.
      ## (on the right of the "=" sign).
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
