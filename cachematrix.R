## We want to avoid useless long calculus using a cache.

#This function allows me to keep the matrix x and its inverse i in cache.

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


## Here, we use a fonction that checks if the inverse of the matrix x is already stored. If it is, it will return the inversed matrix
## If it is not stored, it will calculate the inverse with the solve() function and store it in cache with x$setinverse(i).

cacheinverse <- function(x, ...) {
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
