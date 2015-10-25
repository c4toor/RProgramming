makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        
        setinv <- function(inverse) m <<- inverse
        getinv <- function() m
        
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

cacheinv <- function(x, ...) {
        m <- x$getinv()
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        
        m <- solve(data, ...)
        
        x$setinv(m)
        return(m)
}

timediff <- function(thematrix){
        ## @mat: an invertible matrix
        
        thecache = makeCacheMatrix(thematrix)
        
        start.time = Sys.time()
        cacheinv(thecache)
        duration = Sys.time() - start.time
        print(duration)
        
}
