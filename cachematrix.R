
## creates a special matrix that can cache its inverse
## input: invertable matrix 'x'

makeCacheMatrix <- function(x = matrix()) {
	xinv <- NULL
	set <- function(y) {
		x <<- y
		xinv <<- NULL
	}
	get <- function() x
	setinv <- function(inv) xinv <<- inv
	getinv <- function() xinv
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## calculates the inverse of a special matrix 'x'

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        xinv <- x$getinv()
        if(!is.null(xinv)) {
                message("getting cached data")
                return(xinv)
        }
        data <- x$get()
        xinv <- solve(data, ...)
        x$setinv(xinv)
        xinv
}
