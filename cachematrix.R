## Below are two functions that are used to create a special object
## that stores a matrix and caches its inverse

## makeCacheMatrix: This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() {x}
        setinver <- function(inver) {m <<- inver}
        getinver <- function() {m}
        list(set = set, get = get,
             setinver = setinver,
             getinver = getinver)
}


## cacheSolve: This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the inverse has
## already been calculated (and the matrix has not changed), then 
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinver()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinver(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
