## These functions can calculate the inverse of a square invertible matrix
## The makeCacheMatrix creates a special "matrix" object that can cache its inverse and the matrix itself.
## R's lexical scoping provides faster computation if the special matrix is reused. 
##
## Usage: data <- makeCacheMatrix(matrix(runif(1,16),4))
##        cacheSolve(data) 

makeCacheMatrix <- function(x = matrix()) {
  # makeCacheMatrix expects a square invertible matrix as it's input argument
  # Usage E.g. data <- makeCacheMatrix(matrix(runif(1,16),4))
  
  m <- NULL      # super assigment cache variable for the inverse
  mat <- NULL    # super assigment cache variable for the special matrix
  
  # get verifies if the input matrix is square and returns it's value
  get <- function(){   
    size <- dim(x)
    if(!identical(size[1],size[2]))
    {
      message("Input a square invertible matrix. E.g. matrix(runif(1,16),4)")
      break
    }
    x
  }
  
  # setsolve calculates the inverse of the input matrix
  setsolve <- function(solve) 
  {m <<- solve}
  
  # checkMatrix checks if the input matrix is being reused and returns a TRUE/FALSE value. 
  checkMatrix <- function(sameMat = FALSE){
    if (!identical(mat,x)){
      mat<<- x
    }
    else {
      sameMat = TRUE
    }
    return(sameMat)
  }
  # getsolve returns the current value of the inverse 
  getsolve <- function() m
  
  # lists all handles in the makeCacheMatrix
  list(get = get,
       setsolve = setsolve,
       getsolve = getsolve,
       checkMatrix = checkMatrix)  
}

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Usage cacheSolve(data)  
  
  dat <- x$get()              # Retrieves the input matrix
  sameMat <- x$checkMatrix()  # Checks if the input matrix is reused
  m <- x$getsolve()           # Calculates inverse
  
  # Has the inverse to this matrix has already been calculated?
  if(sameMat & (!is.null(m))){
    message("Same input matrix. Retrieving cached inverse")
    return(m)
  }
  
  m <- solve(dat, ...)   # Find inverse
  x$setsolve(m)
  message("Calculating inverse for a new matrix")
  m                     # print the inverse  
}
