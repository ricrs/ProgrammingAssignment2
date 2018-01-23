# Put comments here that give an overall description of what your
## functions do

## This is the makeCacheMatrix function.
## Return a list with functions to set data, get  data, 
## set the inverse of the matrix, and get the inverse of the matrix
## This function must be called before cacheSolve function
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverseM) m <<- inverseM
  getinv <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinv = getinv)
}


## Write a short comment describing this function

## Function cacheSolve: 
## 1) Get the data of object x: getinv
## 2) Calculate the inverse of x: get(), solve(), and setinv()
## 3) Return a matrix that is the inverse of 'x': m
cacheSolve <- function(x, ...) {
  
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## Calculate the inverse of x
  mat <- x$get()
  m <- solve(mat)
  x$setinv(m)
  
  
  ## Return a matrix that is the inverse of 'x'
  m
}