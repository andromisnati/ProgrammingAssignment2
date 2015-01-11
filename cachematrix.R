## 2nd Programming Assignment
## Caching the inverse of a matrix

## Create functions that set,get matrix and inverse matrix

makeCacheMatrix <- function(x = matrix()) {

  invy <- NULL
  set <- function(y) {
    x <<- y
    invy <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invy <<- inverse
  getinverse <- function() invy
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

## Calculates inverse matrix (or catches it if its already calculated)
## X is a matrix created by makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invy <- x$getinverse()
  if(!is.null(invy)) {
    message("getting cached data")
    return(invy)
  }
  data <- x$get()
  invy <- solve(data, ...)
  x$setinverse(invy)
  invy
}

# Tested with:
#   matriz<-matrix(c(1,-0.25,-0.25,1),2,2)
#   matriz2<-makeCacheMatrix(matriz)
#   cacheSolve(matriz2)