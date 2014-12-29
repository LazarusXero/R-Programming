makeCacheMatrix <- function(x = matrix()) {     #first function
        invmat <- NULL                          #sets the initial inverse matrix to Null
        set <- function(y) {
                mat <<- y
                invmat <<- NULL
        }
        get <- function() mat
        setinv <- function(solve) invmat <<- solve
        getinv <- function() invmat
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
             
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invmat <- mat$getinv()
        if(!is.null(invmat)) {
                message("getting cached data")
                return(invmat)
        }
        data <- mat$get()
        invmat <- solve(data, ...)
        mat$setinv(invmat)
        invmat
        
}
