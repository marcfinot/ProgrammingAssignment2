## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## this function create a matrix object which can cache the inverse of the matrix in order to avoid repeat calculation.

makeCacheMatrix <- function(x = matrix()) {
		s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Write a short comment describing this function
## first use. compute the inverse of the matrix and cache it in memory.
## second use. output the cached value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
		}
		
