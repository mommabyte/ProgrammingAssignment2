
## creates a special matrix that can cache its inverse
## input: invertable matrix 'x'
## output: list of accessors and modifiers for the matric and its inverse

makeCacheMatrix <- function(x = matrix()) {
	#initialize inverse
	xinv <- NULL	
	
	#set matrix and initialize inverse
	set <- function(y) {
		x <<- y
		xinv <<- NULL
	}
	
	#retrieve matrix
	get <- function() x
	
	#set the inverse
	setinv <- function(inv) xinv <<- inv
	
	#get the inverse
	getinv <- function() xinv
	
	#return list of accessors and modifiere
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## calculates the inverse of a special matrix 'x'
## returns cached value of inverse if it exists, else solves for the inverse
## input:  list of accessors and modifiers for a matrix and its inverse
## output: inverse of matrix
cacheSolve <- function(x, ...) {
        ## obtain the inverse
        xinv <- x$getinv()
        
        ##check if inverse is available in cache
        if(!is.null(xinv)) {
                message("getting cached data")
                return(xinv)  # return cached data and exit function
        }
        
        ## cached data not present, so solve for inverse
        data <- x$get()
        xinv <- solve(data, ...)
        
        ## set the inverse in the cache
        x$setinv(xinv)
        xinv
}
