## Put comments here that give an overall description of what your
## functions do

## function to create a matrix that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() {x}
        setInver <- function(inver) {inv <<- inver}
        getInver <- function() {inv}
        list(set =set, get=get, setInver=setInver, getInver=getInver)
        
        
}


## function to compute the inverse of the matrix returned by makeCacheMatrix
## if the inverse has already been calculated, the cachesolve retrieves the
## inverse from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInver()
        if(!is.null(inv)){
                message("getting cashed data")
                return(inv)
        }
        m <- x$get()
        inv <- solve(m, ...)
        x$setInver(inv)
        inv
}

