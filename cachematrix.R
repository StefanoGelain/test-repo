# makeCacheMatrix it's my function to create a list containing a function that:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setmatrix <- function(inverse) inv <<- inverse
  getmatrix <- function() inv
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

# In this part the function returns the inverse of the matrix. The steps are:
# 1. checks if the inverse has already been computed
# 2. If so, it gets the result and skips the calculation of the inverse matrix
# 3. If not, it computes the inverse matrix, setting the values in the cache
cacheSolve <- function(x=matrix()) {
  inv <- x$getmatrix()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  matrix <- x$get() 
  inv <- solve(matrix)
  x$setmatrix(inv)
  inv
}