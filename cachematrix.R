## makeCacheMatrix produces a list of four functions to work with the matrix and its cached inverse
## cacheSolve returns the inverse of the matrix

## makeCacheMatrix creates a list of four functions to work with the matrix x
makeCacheMatrix <- function(x = matrix()) {

s <- NULL ## s will be auxiliary variable to store the cached inverse matrix
      
      set <- function(y) {
            x <<- y   ## set the value of the matrix to equal the argument y of set(y)
            s <<- NULL ## erase any previously calculated value of the inverse matrix
      }
      
      get <- function() x 
      ## get the value of matrix x
      
      setsolve <- function(solve) s <<- solve 
      ## set the cached value s to be equal to the parameter solve
      
      getsolve <- function() s 
      ## get the value of the cached inverse
      
      ## return a list of the four functions created here
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)

}


## cacheSolve returns the inverse of x$get(), using the cached value if available

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

      ## first check if s already contains a chached value, if so, return it
      s <- x$getsolve()
      if(!is.null(s)) {
            message("getting cached data")
            return(s)
      }
      
      ## otherwise calculate the inverse
      data <- x$get()
      s <- solve(data, ...)
      x$setsolve(s) #chache the inverse
      s ##and return it
}
