########## Two functions that cache the inverse of a matrices rather than compute repeatedly when not necessary



##### 1 Create list of functions



makecachematrix <- function(x = numeric()) {
        m <- NULL

	set <- function(y) {                       # set matrix
                x <<- y
                m <<- NULL
        }
        
	get <- function() x                        # get matrix
        setinverse <- function(solve) m <<- solve  # set to inverted
        getinverse <- function() m                 # get inverted
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



##### 2 Output latest or new inversion


cachesolve <- function(x, ...) {
        m <- x$getinverse()			   # get current inverted matrix
        if(!is.null(m)) {			   # if inverted matrix exists then return
                message("getting cached data")
                return(m)
        }
        data <- x$get()				   # if inverted matrix does not exist, calculate & return
        m <- solve(data, ...)
        x$setinverse(m)
        m
}