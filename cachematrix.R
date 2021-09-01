## Put comments here that give an overall description of what your
## functions do

## There are two functions; makeCacheMatrix and cacheSolve
## makeCacheMatrix consist of set, get, setInverse and getInverse
##library(MaSS) calculates the inverse of matrices
library(MASS)
makeCacheMatrix <- function(x = matrix()){
  inv <- NULL             ## Initializing the inverse as NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function(){x}    ## function to get matrix x
  setInverse <- function(inverse){inv <<- inverse}
  getInverse <- function(){inv}     ## function to obtain inverse of the matrix
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This is used to get cache data

cacheSolve <- function(x, ...){  ## gets cache data
  inv <- x$getInverse()
  if(!is.null(inv)){            ## checks to see if inverse is NULL
    message("getting cacehed data")
    return(inv)     ## Returns inverse value
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}