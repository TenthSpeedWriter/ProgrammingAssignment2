## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  cached <- NULL
  set <- function(val) {
    x <<- val
    # Assigning a new value x should reset the cache
    cached <<- NULL
  }
  get <- function() { x }
  setcache <- function(inverse) {
    cached <<- inverse
  }
  getcache <- function() { cached }
  
  list(set = set, get = get, setcache = setcache, getcache = getcache)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  known_cache = x$getcache()
  
  if(!is.null(known_cache)) {
    # Then return it as is
    result <- known_cache
  } else {
    # Then calculate it, cache the result, and return the result
    solved_matrix <- solve(x$get())
    x$setcache(solved_matrix)
    result <- solved_matrix
  }
  
  result
}
