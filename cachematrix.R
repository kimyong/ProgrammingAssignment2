# Usage : 
# Initialise by calling function makeCacheMatrix with a matrix of size n * n as the input parameter, this will return a list
# containing fucntions to read and set both cachematrix and its inverse

# call the function cacheSolve to return the inverse of the cacheMatrix 
# cacheSolve will use the cache if it exist and calculate the inverse if it doesn't 
# calculated inverse will be cached

# Example:
#> x <- matrix(rexp(4), nrow=2) ## Create a 2 * 2 matrix with random values
#> cx <- makeCacheMatrix(x) ## create a cache matrix with the matrix, x
#> x %*% cacheSolve(cx) ## Multiply the matrix x with the inverse return by the cache matrix
#[,1]          [,2]
#[1,] 1.000000e+00 -6.938894e-18 ## when a matrix is mulitplied with an inverse it should return an identity matrix
#[2,] 2.220446e-16  1.000000e+00 ## there may be some rounding off error
#> x %*% cacheSolve(cx) ## Running it the second time, the function cacheSolve will use the 
#                       ## cached result that was previously generated
#getting cached data
#[,1]          [,2]
#[1,] 1.000000e+00 -6.938894e-18
#[2,] 2.220446e-16  1.000000e+00



## the makeCacheMatrix function takes in a matrix assumed to be invertible
## an return a list containing funcionts to set, get both the matrix and its inverse
## the inverse of the matrix is set null when it is initialised

makeCacheMatrix <- function(x = matrix()) {
  #set the variable i for cache to null
  i <- NULL
  
  # this is the function to set the value of the matrix
  set <- function(y) {
    x <<-y
    i <<- NULL
  }
  
  #this is the function to get the value of the matrix
  get <- function() x
  
  #this is the function to set the value of the inverse
  setinv <- function(inverse) i <<- inverse
  
  #this is the function to get the value of the inverse
  getinv <- function() i
  
  #return the matrix with the defined functions
  list(set=set, get=get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
#get value of inverse of x to variable i
  i <- x$getinv()
  #if there exist a cached copy of the inverse   
  if(!is.null(i)) {
    message("getting cached data")
    ## Return cached matrix that is the inverse of 'x'
    return(i)
  }
  #otherwise calculate the inverse by getting the matrix
  data <- x$get()
  #calculate the inverse
  i <- solve(data, ...)
  #set the calculated inverse to cache
  x$setinv(i)
  #return the inverse
  i
  ## Return a matrix that is the inverse of 'x'
}
