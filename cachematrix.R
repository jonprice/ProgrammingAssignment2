## R Programming Assignment 2 code. 
## File contains functions to create a matrix that can cache its inverse. 
## Code is modeled off the code supplied in the assignment example. 
## code can be tested by running test()

## Create a matrix that can cache the inverse matrix. 
## returned list contains the following functions. 
## get - return the matrix
## set - set the matrix
## getinverse - get the inverse of the matrix if it exists
## setinverse - set the incerse of the matrix
makeCacheMatrix <- function(matrix = matrix()) {
  
  ##clear the cached value. 
  inverseMatrix <- NULL
  
  ## function to set a new matrix
  set <- function(newmatrix) {
    matrix <<- newMatrix
    inverseMatrix <<- NULL
  }
  
  ## get the current matrix
  get <- function() matrix
  
  ## set the inverse matrix
  setinverse <- function(newinverseMatrix) inverseMatrix <<- newinverseMatrix
  
  ## get the inverse matrix
  getinverse <- function() inverseMatrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## Return the inverese of the matrix. 
## If a cached inverse exists it will be return. 
## matrix should be the list created by makeCacheMatrix
## cacheSolve will fail if matric is not invertible. 
cacheSolve <- function(matrix, ...) {
  
  ## check if inverse matrix is cached
  inversematrix <- matrix$getinverse()
  if(!is.null(inversematrix)) {
    message("getting cached data")
    return(inversematrix)
  }
  
  ## get the matrix and caclulate the inverse
  data <- matrix$get()
  inversematrix <- solve(data, ...)
  ## cache the inverse matrix
  matrix$setinverse(inversematrix)
  inversematrix
}


## simple function to test the caching matrix
## expected output will include the message "getting cached data"
test <- function(){
  testmatrix = matrix(   c(2, 4, 3, 1, 5, 7, 5, 6,3),   nrow=3,   ncol=3)
  cachedmatrix <- makeCacheMatrix(testmatrix)
  cacheSolve(cachedmatrix)
  cacheSolve(cachedmatrix)
}

