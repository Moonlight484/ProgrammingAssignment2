## cachematrix: This script contains 2 functions (makeCacheMatrix and cacheSolve) that can be used 
## together to efficiently calculate 
## the inverse of a square matrix. When used together, the user can cache the inverse of a matrix
## then access it multiple times without recalculating it. The user need not be aware of whether the
## inverse has been already calculated, as the script will cacluate the inverse if it has not yet 
## been calculated

## Assumptions:
##    - the matrix, x,  that is provided to be inverted is invertible
## Calling prototype: 
##  cached_matrix_wrapper <- makeCacheMatrix(x)         // create a cacheing wrapper for the matrix x 
##  myInverse <- cacheSolve(cached_matrix_wrapper)      // invert it if necessary, or return the existing inverse
## or
## cached_matrix_wrapper <- makeCacheMatrix()     // create an empty wrapper
## cached_matrix_wrapper$set_matrix(x)            // put a matrix,x in the wrapper
## myInverse <- cacheSolve(cached_matrix_wrapper) // invert it if necessary and return inverse, or return the existing inverse


## makeCacheMatrix: This function creates a special wrapper around a matrix that, when accessed as shown in the 
## prototype calling functions above, 
## will cache the inverse of the matrix. The inverse can  be retrieved by simply calling the getInverse()
## function of the wrapper. The special wrapper keeps track of whether the inverse has been 
## calculated or not, and if it has not, it returns a null. The calling function can then calculate the inverse
## and set it into the wrapper where it will then be available any time after that when the getInverse() function is called.

## function: makeCacheMatrix: Create a cacheing wrapper for the matrix x The wrapper caches the inverse of the matrix
## when the setInverse(inverse) function is called and returns it when getInverse() is called 

## Setting up the function:
## required inputs: none- the wrapper can be created with a matrix or as an "empty" wrapper  
##    - set through the constructor makeCacheMatrix(x)
##    - or create an empty wrapper using cached_matrix <- makeCacheMatrix() and set the matrix using the function cached_matrix$set_matrix(x) 

## Setting the inverse: 
## The inverse can be set by 
##    - calling the "cacheSolve" or similar function, descibed below. 
##    - or  use cached_matrix$setInverse(inverse) which will set an externally calculated inverse of the matrix, 
##                such as the one calculated
##                by cacheSolve(x) into the wrapper. This function will set any matrix that is passed to it  
##                as the inverse. The user must insure that the matrix passed in this way is, indeed, the inverse of the 
##                original matrix.

## Outputs - 
##   cached_matrix$getInverse(): returns the inverse of the input matrix
##   cached_matrix$get_matrix(): returns the original matrix, x
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
      
      ## setInverse: sets the inverse matrix- as in cached_matrix_wrapper$setInverse(inverse)
      setInverse <- function(inverse) m_inv <<- inverse
      
      ## getInverse the internal call to return the inverse of the input matrix - as in cached_matrix_wrapper$getInverse()
      getInverse <- function() m_inv 

      ## the "list" function allows the user to set pretty names for the internal functions. The names on the left are 
      ## the exposed names(what the user would see). These are mapped to the internal call names. See "cacheSolve" to see
      ## how to access the external names.
      ## (on the right of the "=" sign).
      list(set_matrix = set, get_matrix = get,
           setInverse = setInverse,
           getInverse = getInverse) 
}


## cacheSolve: Returns the inverse of the supplied matrix using a special caching function

## function: cacheSolve - sets up the makeCacheMatrix wrapper and then uses it to store the value of the inverse of a matrix
## If it has already calculated the inverse, it simply retrieves it from the wrapper where it had previously stored it
## If it has not already calculated the inverse, it will calculate the inverse and store it in the wrapper.

## Setting up the function:
## Required Inputs: A "wrapped" matrix created using the makeCacheMatrix function (see above for how to do that)-       
##                   myInverse <- cacheSolve(cached_matrix_wrapper)
 
## OUtputs: The inverse of the matrix that was stored in the cached_matrix_wrapper

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x', where x is the cached_matrix_wrapper for the target matrix
      inverse <- x$getInverse()
      ## if the inverse is not null, return the matrix that has been stored in the "inverse" of the wrapper
      ## otherwise, continue below and calculate the inverse and store it into the wrapper for repeated use
      if(!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
      }
      ## It is important to do all three of these steps in this order. First, get the original matrix out of the wrapper
      data <- x$get_matrix()
      
      ## calculate the inverse of the original matrix - 
       inverse<- solve(data, ...)
      
     ## store the inverse of the matrix into the wrapper
     ## doing these steps in this order guarantees that the inverse that is set into the wrapper is 
     ## indeed, the inverse of the matrix that is in the wrapper. 
     ## THe user is responsible for guaranteeing this consistency
      x$setInverse(inverse)
     
     ## return the result
      inverse
}
