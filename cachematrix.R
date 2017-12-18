# makeCacheMatrix function creates a special matrix object that can cache its inverse.
# it will take coerced data and number of rows and columns as input

makeCacheMatrix <- function(data,nrow,ncol) {  
  x <- matrix(data,nrow,ncol)                    # initialise object
  inv <- NULL                                    # initalise object
  setmatrix <-function(y,nrow,ncol) {            # set matrix
    x <<- y                                        # set matrix to take value for y from glbal enviornment object x
    inv <<- NULL                                   # set inverse matrix to null so that it does not take worng value from cache
  }
  
  getmatrix <- function() x                      # create function to get matrix
  setinv <- function(solve) inv <<- solve        # create function to set inverse matrix
  getinv <- function() inv                       # create function to get inverse function value
  list(setmatrix=setmatrix,getmatrix=getmatrix,setinv=setinv,getinv=getinv) # create new object by returning list
}

# cacheSolve function will create inverse matrix 
# it will check if Inverse matrix already exists in cache 
# this function will take makeCacheMatrix as input
cacheSolve <- function(x,...) {
  inv <- x$getinv()
  if(!is.null(inv)) {                   # check if inverse matrix exist in cache
    message("getting cached data")
    return(inv)                           # return inverse matrix if exists in cache
  }
  data <- x$getmatrix()                 # if inverse matrix does not exist in cache
  inv <- solve(data)                    # solve for inverse matrix
  x$setinv(inv)                         # set inverse matrix in cache
  inv                                   # return inverse matrix
}

# functions tested with following data
# makeCacheMatrix c(.5.-25,-1,.75)
# inverse matrix c(6,2,8,4)