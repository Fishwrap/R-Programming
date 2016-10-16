## The two functions combined will cache inverse value of a matrix specified in the first function
## or call the cache value of the inverse matrix if such inverse matrix was calculated before

## This function sets and gets the value of the matrix and its inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function()x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function()inv
  list(set=set, get=get, setinverse=setinverse,getinverse=getinverse)
}


## This function calls cache value of matrix inverse if exists, otherwise calculates
## matrix inverse and set the value of the matrix inverse in the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cache data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinverse(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
