## write a couple of functions that cache the inverse of a matrix rather than
## to compute it repeatedly

## Function to create a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        mat_inv <- NULL
        set <- function(y){
            x <<- y
            mat_inv <<- NULL
        }
        get <- function() x
        set_inv <- cacheSolve(x)
        get_inv <- function() mat_inv
        list(set = set, get = get,
             set_inv = set_inv,
             get_inv = get_inv)
        
}


## Function to compute the inverse of the special "matrix" returned 
## by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mat_inv <- x$get_inv()
  if (!is.null(mat_inv)) {
  ## Matrix Inverse is already calculated, hence get from cache
    message("getting cached data")
    return(mat_inv)
  }
  ## Matrix Inverse is not present in cache, hence calculate it
  data <- x$get()  
  mat_inv <- Solve(data)
  x$set(mat_inv)
  mat_inv
}
