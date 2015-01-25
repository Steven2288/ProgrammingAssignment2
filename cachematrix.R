makeCacheMatrix <- function(x = matrix()) {    ##function to create matrix
  ms <- NULL                   ## make solved matrix = NULL
  set <- function(y) {    ##functon to create the cached data
    x <<- y                 ## assign y to x and caches in parent envrnmt
    ms <<- NULL         ## assign null to ms and caches in parent envrnmt
  }
  get <- function() x                 ## get the x matrix
  setsolve <- function(solve) ms <<- solve   ## calculate inverted matrix
  getsolve <- function() ms          ## call function for inverted matrix
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)                          ## make List for functions
}

Cachesolve <- function(x, ...) {      ## assigns above matrix function to cachesolve
  ms <- x$getsolve()               ## assign solve to ms, from parent environment
  if(!is.null(ms)) {                   ## Decide if ms is not null
    message("getting cached data")   ## if ms is not null, display message
    return(ms)                           ## returns inverse matrix 
  }
  
  data <- x$get()                     ## get x from parent envrnmt, assign to data
  ms <- solve(data, ...)           ## performs solve on data, to inverse matrix
  x$setsolve(ms)                   ## saves solved data/matrix in parent envirnmt
  ms                                        ## displays inverse matrix
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
