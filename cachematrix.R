## The following pair of functions cache the inverse of a matrix.

## The first function, `makeCacheMatrix` creates a special "matrix" which is
## a list containing a function to:  set the value of the matrix, 
## get the value of the matrix, set the value of the inverse of the matrix and
## get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
        inversa <- NULL
        set <- function(y) {
                x <<- y
                inversa <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inversa <<- solve
        getinv <- function() inversa
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
}


## This second function cacheSolve takes argument x from the previous function, 
## it verifies if the inverse of the matrix has already been cached and 
## returns its value in that case. If not, the inverse is calculated and then
## caches the resulting inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inversa <- x$getinv()
        if(!is.null(inversa)) {
                message("getting cached data")
                return(inversa)
        }
        data <- x$get()
        inversa <- solve(data, ...)
        x$setinv(inversa)
        inversa
}
