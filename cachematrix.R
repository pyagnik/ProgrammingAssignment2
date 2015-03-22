## caching the inverse of a matrix

## "makeCacheMatrix" creates a special matrix object, and then "cacheSolve" calculates the 
## inverse of the matrix. If the matrix inverse has already been calculated, it will instead
## find it in the cache and return it, and not calculate it again. 


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv) 
}


## The function "cacheSolve" returns the inverse of a matrix A created with the "makeCacheMatrix" 
## function. If the cached inverse is available, "cacheSolve" retrieves it, else, 
## it computes, caches, and returns it. 

cacheSolve <- function(x, ...) { 
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}

  
  