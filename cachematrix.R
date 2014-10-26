## The following functions (makeCacheMatrix and cacheSolve) work together to create a special object 
##that will save (or 'cache') the inverse of a given matrix to save on potentially costly computations. 

## makeChaceMatrix function creates a special object that is really a list containing a function that will:
## set the value of the matrix, retrieve that value, calculate the inverse and retrieve that.
makeCacheMatrix <- function(x = matrix()) {
# create an empty object 'm'
		m <- NULL
		set <- function(y) {
				#assigns the input matrix 'y' to 'x' in the parent environment
				x <<- y
				#assigns 'm' to be null in the parent environment
				m <<- NULL
		}
		get <- function() x
		#returns the matrix 'x'
		setinv <- function(solve) m <<- solve
		#sets the cache object 'm' to the inverse of 'x'
		getinv <- function() m
		#returns the inverse of 'x' (the object 'm')
		list(set = set, get = get,
			 setinv = setinv,
			 getinv = getinv)
}


## cacheSolve checks the special object to see if the inverse has been calculated already
#if so it returns that. If not it calculates it and saves it into the cache object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
        		message("getting cached data")
        		return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
