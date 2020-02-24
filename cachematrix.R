## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) { ## Create a special matrix to cache the invesed matrix
  m <- NULL
  set <- function(y) {
    x <<- y ## Assign the input argument to the x object in the parent environment 
    m <<- NULL ## Assign value of NULL to the m object in parent environment
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse ## Assign the inverse to the m object
  getinverse <- function() m ## Find m to retreieve its value (the inverse)
  
  list(set = set, get = get, ## Create matrix and name the list, assign the above functions as an element within the list
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  m <- x$getinverse() ## Retrieve a mean from the object m
  if(!is.null(m)) { ## If not null, return the cached data to the parent environment
    message("getting cached data")
    return(m)
  }
  data <- x$get() ## If it's null, get the vectors from inpu object
  m <- solve(data) %*% data ## Calculate the inverse of the data and assign it to m
  x$setinverse(m) ## Return the value of inverse to the parenet environment 
  m ## Print the inverse object
}
