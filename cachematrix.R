## The following functions are used to cache the inverse of a matrix.

## makeCacheMatrix creates a list containing a function to...
## 0) set the value of the matrix
## 1) get the value of the matrix
## 2) set the value of inverse of the matrix
## 3) get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)
}


## cacheSolve computes the inverse unless the inverse
## has alrady been calculated, then the cacheSolve
## will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data...")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
