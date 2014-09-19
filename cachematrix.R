## cachematrix.R
## Marc Finot
## 9/18/2014
## The two following functions provide a way to calculate only 
## once the inverse of a matrix and keep the result in memory to reduce 

## this function creates a matrix object which can cache the inverse of the matrix in order to avoid repeat calculation.
## 
## How to use it. s is the matrix to be inverted
## x <- makeCacheMatrix(s)  -  create the object
## x$get() to get the matrix
## cachesolve(x) to invert the matrix and cache it in memory
## x$getsolve(): to retrieve the inverse
 
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
                message("getting cached inverse")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
		
