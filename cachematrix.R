## These functions calculate and cache the inverse of a matrix
## x variable contains the matrix
## i variable contains the inverse of the matrix

## makeCacheMatrix returns a list of functions:
## set run to essentially start over with a new matrix
    ## sets the value of x 
    ## and also sets the value of i to NULL
## get retrieves the value of x 
## setinverse sets the value of i to the value passed to it
## getinverse retrieves the value i 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) { 
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(d) i <<- d
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve returns a matrix that is the inverse of x. 
## If the inverse has already been calculated, 
   ## the cached calculation is returned
## If the inverse has not already been calculated, 
   ## it is calculated, cached, and returned


cacheSolve <- function(x, ...) {
  li <- x$getinverse()                ## set li to the return value of the getinverse function 
                                         ## which is i
  
  if(!is.null(li)) {                  ## if li is not null (inverse already calculated), 
    message("getting cached data")    ## output message and
     return(li)                       ## return li
  }
  
  data <- x$get()                     ## set data to the return value of the get function 
                                         ## which is m
  
  li <- solve(data, ...)              ## set li to the inverse of data 
  
  x$setinverse(li)                    ## use setinverse function to cache the value of li 
                                        ## in the i variable
  
  li                                  ## return li   
}
