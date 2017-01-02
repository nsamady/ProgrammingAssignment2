## makeCacheMatrix creates a special matrix object, and then cacheSolve 
## will calculate the inverse of the matrix.
## If the matrix inverse has already been calculated, it will instead 
## find it in the cache and return it instead of calculating it again


makeCacheMatrix <- function(x = matrix()) {
    inv = NULL
    set = function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get = function() x
    
    setinv = function(inverse) inv <<- inverse 
    
    getinv = function() inv
    
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## The function cacheSolve returns the inverse of a matrix A created with
## the makeCacheMatrix function.
## If the cached inverse is available, cacheSolve will retrieve it
## while if the is inverse is not availiable, the function will
##compute it, cache it , and then return it

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv = x$getinv()
    
    if (!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    
    mat.data = x$get()
    inv = solve(mat.data, ...)
    
    x$setinv(inv)
    
    return(inv)
}
