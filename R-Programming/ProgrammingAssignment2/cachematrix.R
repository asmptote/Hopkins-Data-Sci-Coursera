#cache inverse of a matrix

#pair of functions which can be used to invert and cache a square matrix if not already cached and return the
#cached inverse if already inverted

#creates special "matrix" object that can cache its inverse. Returns list of functions

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {  
        x <<- y
        inv <<- NULL
    }
    get <- function() x    
    setinv <- function(inverse) inv <<- inverse  
    getinv <- function() inv               
    list(set=set,get=get,       
         setinv=setinv,
         getinv=getinv)
    
}


# computes inverse of the special "matrix" returned from makeCachematrix functions. Assuming invertible 2x2.
#if inverse has already been calculated and matrix hasn't changed then retrieve from cache

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

