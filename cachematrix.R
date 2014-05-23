## Computing the inverse of a square matrix and using a cached result 
## rather than performing a costly inverse computation repeatedly in case 
## matrix has not changed.

## Creating a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) 
{
        m <- NULL
        
        ## can be set from outside environment
        set <- function(y) 
        {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        
        ## can be set from outside environment
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Computing the inverse matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        ## Checking for a non changed matrix
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
        
}
