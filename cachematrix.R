## This functions allows you to cache the values of the inverse of a matrix to avoid unnecesary
## computations unless the matrix change

##The first function, makeCacheMatrix, creates a special "matrix" object that is actually a list containing a function to
## 1. Set the value of a matrix
## 2. Get the values of a matrix
## 3. Set the values of the inverse matrix
## 4. Get the values of the inverse matrix

makeCacheMatrix <- function(X = matrix ()) {
  inverse <- NULL
  set <- function(y) {
    X <<- y
    inverse <<- NULL
  }
  get <- function() X
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse=getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(X, ...) {
  inverse <- X$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  matrix <- X$get()
  inverse <- solve(matrix, ...)
  X$setinverse(inverse)
  inverse
}
