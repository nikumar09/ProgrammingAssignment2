##The first function, makeVector creates a special "vector", which is really a list containing a function to

##set the value of the vector
##get the value of the vector
##set the value of the mean
##get the value of the mean

makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}



##The following function calculates the mean of the special "vector" 
##created with the above function. However, it first checks to see 
##if the mean has already been calculated. If so, it gets the mean 
##from the cache and skips the computation. Otherwise, it 
##calculates the mean of the data and sets the value of the mean 
##in the cache via the setmean function.

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}


## Put comments here that give an overall description of what your
## functions do

makeCacheMatrix <- function(x = matrix()) {
  # initialize to NULL
  inv <- NULL
  
  # create the matrix in the working environment
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # get the value of the matrix
  get <- function() x
  # invert the matrix and store in cache
  setInverse <- function(inverse) inv <<- inverse
  # get the inverted matrix from cache
  getInverse <- function() inv
  
  # return the created functions to the working environment
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


##This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. If the inverse has already
##been calculated (and the matrix has not changed),
##then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  
  if (! is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ##message("Debug1")
  data <- x$get
  ##message("Debug2")
  inv <- solve(data)
  ##message("Debug3")
  x$setinverse(inv)
  ##message("Debug4")
  inv
  
}

##sample Run
##> source("cachematrix.R")
##> a$set(matrix(1:4, 2, 2))
##> cacheSolve(a)
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> cacheSolve(a)
##getting cached data
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

