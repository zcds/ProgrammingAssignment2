## Calculating the inverse of a matrix may take too long
## especially if it has to be computed repeatedly.
## The below functions leverage the scoping rules of the R language
## to cache a matrix's inverse. Thus, avoiding unnecessary costly 
## computation.

## For a given matrix parameter x, this function
## returns a special "matrix" (actually a list) having the following
## function type elements:
## 1. setMatrix: set the matrix
## 2. getMatrix: get the matrix
## 3. setMatrixInverse: set the matrix inverse
## 4. getMatrixInverse: get the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  
  # create a property to cache inverse of the matrix
  inverse <- NULL
  
  # function to set the original matrix and also resetting the inverse property
  setMatrix <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  # function to get the matrix
  getMatrix <- function() x
  
  # function to cache the inverse matrix in the inverse property
  setMatrixInverse <- function(matrixInverse) inverse <<- matrixInverse
  
  # function to get the cached value of the matrix inverse
  getMatrixInverse <- function() inverse
  
  # return the special "matrix" 
  list(setMatrix = setMatrix, 
       getMatrix = getMatrix,
       setMatrixInverse = setMatrixInverse,
       getMatrixInverse = getMatrixInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  # get the matrix inverse from the special "matrix" object
  inverse <- x$getMatrixInverse()
  
  # Check if the cached inverse is already available...
  if(!is.null(inverse)) {
    # ...if yes, then simply return the cached inverse
    message("getting cached inverse")
    return(inverse)
  }
  
  # ...if the inverse is not yet cached,...
  # 1. ...get the actual matrix data...
  data <- x$getMatrix()
  
  # 2. ...caclculate its inverse...
  inverse <- solve(data, ...)
  
  # 3. ...cache the inverse for subsequent use...
  x$setMatrixInverse(inverse)
  
  # 4. ...return the inverse value.
  inverse
}
