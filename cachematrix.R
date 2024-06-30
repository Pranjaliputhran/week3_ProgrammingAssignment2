## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#there are 2 functions makeChacheMatrix, makeChacheMatrix
#makeChacheMatrix consists of set,get, setinv,getinv
##library(MASS) IS used to calculate inverse for non squared and squared matrices
library(MASS)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL   #INITIALIZING INTIAL AS NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x #FUNCTION TO GET MATRIX X
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function(){
    inver <- ginv(x)
    inver %>% x   #function to obtain inverse of matrix
  } 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
#this is used to Cache data

cacheinverse <- function(x, ...) #gets cache data
  {
  inv <- x$getinverse()
  if(!is.null(inv)) {           #checking whether incerse is NULL
    message("getting cached data")
    return(inv)
  }
  matrix_to_invert <- x$get()
  inv <- solve(matrix_to_invert, ...)    #calculate inverse value
  x$setinverse(inv)
  inv                       #return matrix that is inverse of x
}
