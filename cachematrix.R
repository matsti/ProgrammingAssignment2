## Matrix inversion using a cache to avoid recalculation

## makeCacheMatrix creates an object containing a matrix and its inverse
## once the inverse has been calculated

makeCacheMatrix <- function(x = matrix()) {
        m <-x
        im <- NULL
        
        list(
                set = function(x) {
                        m <<- x
                        im <<- NULL
                        },
                get = function() m,
                setinverse = function(x) im <<- x, # should only be called from cacheSolve
                getinverse = function() im
         )
}


## cacheSolve returns the inverse matrix, from cache if it has already calculated
## it or computes it and stores it into a cache

cacheSolve <- function(x, ...) {

        xim <- x$getinverse()
        if(!is.null(xim)) {
                message("getting cached data")
                return(xim)
        }
        x$setinverse(solve(x$get(), ...))
}
