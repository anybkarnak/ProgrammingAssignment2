
## makeCacheMatrix
## @set - sets the matrix
## @param [in] y matrix
##
## @get
## @return matrix
##
## @setinverse  - sets the inverse matrix
## @param[in] inverse - inverse matrix
##
## @getinverse
## @return inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
