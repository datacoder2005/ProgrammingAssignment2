makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the inverse property to NULL

  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset the inverse when the matrix is set
  }

  get <- function() x  # Return the matrix

  setInverse <- function(inverse) inv <<- inverse  # Set the inverse

  getInverse <- function() inv  # Get the inverse

  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

