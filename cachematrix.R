## Caching the Inverse of a Matrix
## there are 2 function to get the cache : makecachematrix and cachesolve
##1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

##makeCacheVector creates a list, which is really a list containing a function to :
##1. set the value of the vector
##2. get the value of the vector
##3. set the value of the mean
##4. get the value of the mean


makeCacheMatrix <- function(x = matrix()) {
        ##initialize matrix m with null
        m_data <- NULL
        
        set <- function(y) {
                x <<- y
                m_data <<- NULL
        }
        
        get <- function() x
        
        setinv <- function(inverse) m_data <<- inverse
        getinv <- function() m_data
        
        ##list to be returned
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Cache solve function compute the inverse of a square matrix 
## Assume X is a square invertible matrix, solve(X) returns its inverse.

cacheSolve <- function(x, ...) {
        
        m_inv <- x$getinv()
        
        ##if the inverse already in cache then return inverse
        if(!is.null(m_inv)) {
                message("getting cached inverse data")
                return(m_inv)
        }
        else
        {
                m_data <- x$get()
                ##create inverse of the matrix
                m_inv <- solve(m_data, ...)
                x$setinv(m_data)
                ##return the inverse of the matrix
                return(m_data)
        }
}
