### Together these functions allow the user to find the inverse of matrices 
### and keep these solutions stored in the cache using custom matrix objects


## makeCacheMatrix will create a special matrix object from a standard R matrix

makeCacheMatrix <- function(x = matrix()) {
  # Initialize the inverse matrix
  inv <- NULL
  
  # Set matrix data
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Retrieve the actual matrix
  get <- function() x
  
  # Set the inverse matrix
  setInv <- function(invMat) inv <<- invMat
  
  # Retrieve the inverse matrix
  getInv <- function() inv
  
  # Return the special matrix object
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## cacheSolve will grab the inverse matrix from the cache, if it exists.
## Otherwise, it will solve for the inverse itself

cacheSolve <- function(x, ...) {
  # Grab the inverse matrix from the special matrix object
  inv <- x$getInv()
  
  # If the inverse is NOT null, it was stored in the cache, so return it 
  if(!is.null(inv)) {
    message('Getting Inverse from Cache...')
    return(inv)
  }
  
  # Retrieve the actual matrix
  mat <- x$get()
  
  # Solve for the inverse
  inv <- solve(mat, ...)
  
  # Set the inverse matrix in the special matrix object (so it's now in cache)
  x$setInv(inv)
  
  # Return the inverse matrix
  inv
}
