## The two functions below are used to create an object to
## store a matrix and cache its inverse.

## makeCacheMatrix function creates an object to store a 
## matrix.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
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


## The cacheSolve function calculates the inverse of the matrix
## created with the above function. However, it first checks to see if
## the inverse has been calculated. If so, 
## it gets the inverse and skips the computation. Otherwise, it calculates
## the inverse of the matrix and sets the inverse in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}
