## The following two functions enable the caching of the inversion of a matrix
## once the inversion has been calcuated

## This function creates an object that will store the matrix and the cached inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {

## Only need to change x if y is different
        if( !identical(x,y)){
            x <<- y
            inv <<- NULL
        }
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function will frist check if an inverse has been cached. If it has than 
## it will be returned, if not then the inverse will be calculated, cached
## and returned.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
            return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}

