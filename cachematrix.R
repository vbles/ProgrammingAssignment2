#vbles
#Coursera Assignment

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  # set the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # get the matrix
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  
  # populate the list
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


# to find inverse of matrix (solve or get from cache)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  
  # get it from the cache
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  
  # get the inverse of any matrix using solve()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
