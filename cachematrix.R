## Matrix inversion is usually a costly computation and 
## especially for very large matirices, there is benefit 
## to caching the inverse of a matrix rather than compute it repeatedly.
## The following functions define a special matrix that caches its inverse.

## makeCacheMatirx creates a special "matrix" object that can cache its inverse. 
## The matric object returned is a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  xInverse <- NULL
  set <- function(y) {
    x <<- y
    xInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) xInverse <<- inverse
  getInverse <- function() xInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cachSolve returns the inverse of the matrix object created with makeCacheMatrix. 
## If the inverse has already been calculated, the function gets the inverse from the 
## cache. If not, it calculates the inverse and sets it value in the cache via 
## the setInverse function.

cacheSolve <- function(x) {
  
  ## check if inverse is in cache and return if it is
  xInverse <- x$getInverse()
  if(!is.null(xInverse)) {
    message("getting cached data")
    return(xInverse)
  }
  
  ## otherwise compute inverse and return
  data <- x$get()
  xInverse <- solve(data)
  x$setInverse(xInverse)
  xInverse
}

## Test
# x = rbind(c(1, -2, 1), c(-2, 1, 1), c(1, 0 , 1))
# m = makeCacheMatrix(x)
# m$get()

## Call cacheSolve the first time
# mInverse <- cacheSolve(m)
# mInverse

## Check if the inverse has been correctly computed
# x %*% mInverse
# mInverse %*% x

## Call cachSolve after inverse has been computed
# cacheSolve(m)