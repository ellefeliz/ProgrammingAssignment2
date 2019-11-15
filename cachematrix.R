## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function takes the provided matrix and uses the solve 
## function to inverse it

makeCacheMatrix <- function(x = matrix()) {
        invrs <- NULL 
        st <- function(y)
        {
                x <<- y
                invrs <<- NULL
        }
        get <- function() x
        setInvrs <- function() invrs <<- solve(x)
        getInvrs <- function() invrs
        list(st = st,
             get = get,
             setInvrs = setInvrs,
             getInvrs = getInvrs
             )        
}


## Write a short comment describing this function
## This function caches the inverse value of the matrix provided
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invrs <- x$getInvrs()
        if (!is.null(invrs)){
                message("getting cached data")
                return(invrs)
        }
        mat <- x$get()
        invrs <- solve(mat,...)
        x$setInvrs(invrs)
        invrs
}
