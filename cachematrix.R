# Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  d<- -1
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverseM) m <<- inverseM
  getinv <- function() m
  setdet <- function(dtm) d<<-dtm
  getdet <- function() d
  list(set = set, get = get,
       setinverse = setinverse,
       getinv = getinv,
       setdet = setdet,
       getdet=getdet)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  ## Check if determinant of the x matrix is zero -> x isn't invertible! 
  ## Get the data of object x
  data <- x$getdet()
  if (data==0){
    print("Matrix is not invertible")
    return(0)
  }
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## Calculate the inverse of x
  mat <- x$get()
  m <- solve(mat)
  x$setinv(m)
  
  ## Calculate the determinant of x
  x$setdet(m)
  
  ## Return a matrix that is the inverse of 'x'
  m
}