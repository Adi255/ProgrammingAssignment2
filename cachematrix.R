## Pair of functions to first create a list containing 
## a matrix that can also store it's inverse, and a
## second function to create and then return the cached
## inverse matrix when present

## This function creates a special list to contain
## and return a matrix and it's own cached inverse
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}

	get <- function() {
		x
	}

	setInv <- function(y) {
		inv <<- y
	}
	
	getInv <- function() {
		inv
	}

	list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Return a matrix that is the inverse of that contained in 'x$get()',
## using a cached version if it is present
cacheSolve <- function(x, ...) {
	inv <- x$getInv();
	if(!is.null(inv)){
		 message("getting cached matrix inverse")
             return(inv)
	}
	else {
		message("no cached inverse present, solving the hard way")
		matrix <- x$get()
		inv <- solve(matrix,...)
		x$setInv(inv)
		inv 
	}
}
