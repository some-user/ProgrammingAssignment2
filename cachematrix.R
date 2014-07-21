## Two functions below should be used to compute the inverse of a matrix and 
## store this inverse in the cache, so that the next time we need it, it can be 
## retrieved from the cache rather than recomputed.


## The makeCacheMatrix function is used to create a list, which contains several
## functions, namely:
## a) the one, which sets the original matrix (set);
## b) the one, which returns the original matrix (get);
## c) the one, which sets the inverse of the original matrix (setinverse);
## d) the one, which returns the inverse of the original matrix (getinverse).

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
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


## The cacheSolve function returns the inverse of the original matrix 'x'.
## In case the inverse of the original matrix has already been calculated, it 
## retrieves the inverse from the cache. Otherwise, it calculates the inverse
## and sets it in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}