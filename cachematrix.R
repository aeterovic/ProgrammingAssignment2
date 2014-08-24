## 
## makeCacheMatrix
## This function creates a special "matrix" object that can cache its inverse. 
## 
## cacheSolve
## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not 
## changed), then cacheSolve should retrieve the inverse from the cache.

## makeCacheMatrix
## Returns a list of 4 functions, to set or get the original matrix
## and to set or get its inverse

makeCacheMatrix <- function(x = matrix()) {
        xinverse <- NULL
        set <- function(y) {
                x <<- y
                xinverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) xinverse <<- inverse
        getinverse <- function() xinverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve
## Returns the inverse of its argument (assuming it is passed a 
## nonsingular square matrix).  
## It calculates the inverse only once.  If called again with the same
## argument it retrieves a cashed previously computed inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        xinverse <- x$getinverse()
        if(!is.null(xinverse)) {
                message("getting cached data")
                return(xinverse)
        }
        data <- x$get()
        xinverse <- solve(data, ...)
        x$setinverse(xinverse)
        xinverse
}
