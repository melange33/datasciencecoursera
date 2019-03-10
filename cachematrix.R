## Creates a special matrix to and caches it's inverse, if inverse is already found, returns inverse

makeCacheMatrix <- function(x = matrix()) {
  
 ##set input of the matrix
   m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
    
  }
  
  ##get the input of the matrix
  get <- function()x
  
  ##Set and cache inverse of the matrix, set functions
  setinverse <- function(inverse)m <<- inverse
  getinverse <- function()m
  list(set = set,get = get, setinverse = setinverse, getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  
  ##get the matrix inverse and check if it is cached
  m <- x$getinverse()
  if(!is.null(m)){
    
    ##returns matrix if cached
    message("getting cached data")
    return(m)
  }
  
  ##if not cached, gets the inverse and returns
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}