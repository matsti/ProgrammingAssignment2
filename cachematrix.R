## Matrix inversion using a cache to avoid recalculation

## makeCacheMatrix creates an object containing a matrix and its inverse
## once the inverse has been calculated using the cashSolve function.

makeCacheMatrix <- function(x = matrix()) {
        
        # object variables creation and initialisation
        m <-x
        im <- NULL
        
        # list of the methods implemented in the makeCacheMatrix object
        list(
                set = function(x) {  # allows to set a new matrix value, the inverse is set to null
                        m <<- x
                        im <<- NULL
                        },
                get = function() m,  # return the value of the matrix
                setinverse = function(x) im <<- x, # should only be called from cacheSolve
                getinverse = function() im  # returns the value of the inverse
         )
}


## cacheSolve returns the inverse matrix. The inverse is returned from cache if it has already
## been calculated. If not, the inverse is calculated and stored into a cache.

cacheSolve <- function(x, ...) {

        xim <- x$getinverse()  # checking for a non-null inverse
        if(!is.null(xim)) {  # if an inverse exists, returning the cached matrix with message
                message("getting cached matrix data")
                return(xim)
        }
        x$setinverse(solve(x$get(), ...))  # calculating the inverse using solve and updating the 
        ## value of the inverse in the makeCacheMatrix object
}
