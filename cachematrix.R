## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

# assign an empty object
# set,get,setinverse and getinverse of the square matrix by using solve function
#this makecachematrix is used in cachesolve function
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x
        # checks whether matrix is present if present then get the matrix from previous function'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  #if not then do the inverse of the matrix
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}



