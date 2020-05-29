# The functions below cache the value of inverse of a matrix so 
# that when we do not need to compute the inverse again when needed.

# the function makeCacheMatrix creates a special matrix (which is a list of functions)

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
    set <- function(y) 
	{
        x <<- y
        inv <<- NULL
    }
    get <- function() 
	{
		x
	}
    set_inverse <- function(inverse) 
	{
		inv <<- inverse
	}
    get_inverse <- function() 
	{
		inv
	}
	#the list of functions
    list(set = set, get = get,set_inverse = set_inverse, get_inverse = get_inverse)
}



## the function cacheSolve computes the inverse of a matrix
## if it has already been computed, it returns the cached value

cacheSolve <- function(x, ...) {
        inv <- x$get_inverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$set_inverse(inv)
        inv
}
