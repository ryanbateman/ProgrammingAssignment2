## The following code contains two functions. One creates a helper object to get
## and set a matrix and its inverse. The other attempts to retrieve the the cache
## of the matrix's inversion and, if it's unable to retrieve, will calculate it
## and cache it.

## This function takes a matrix and returns a list of methods for retrieving it 
## and its inverse.  

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) m <<- inv
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This takes a helper object as returned by the makeCacheMatrix function. It 
## returns the inverse of a matrix. If the inverse has already been calculated 
## and cached, it will return the cached version. If it has not, it will 
## calculate the inverse, cache it and supply it.

cacheSolve <- function(x, ...) {
    inv = x$getInverse()
    if(!is.null(inv)) {
        message("Getting cached inverse")
        return(inv)
    }
    data = x$get()
    inv = solve(data)
    x$setInverse(inv)
    inv
}
