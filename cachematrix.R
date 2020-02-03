makeCachematrix <- function(x = matrix(data,nrow,ncol)) {  #This function creates a special "matrix" object that can cache its inverse, # special matrix is initialized
        i <- NULL # inverse is initialized to NULL
        set <- function(y) {
                x <<- y   # Assigns the input argument to the x object in the parent environment
                i <<- NULL   ## Assigns NULL to the i object in the parent environment, and clears any value of i that was cached before
        }
        get <- function() x    #getter of x retrieves x from parent environment
        setinverse <- function(solve) i <<- solve   # Assigns the input argument to the value of i in parent environment
        getinverse <- function() i        # get value of i
        list(set = set, get = get,        # Assigns each function as a list element
             setinverse = setinverse,
             getinverse = getinverse)
}
#
#
cacheSolve <- function(x, ...) {      # this function computes the inverse of the special matrix returned by makeCachematrix       
        i <- x$getinverse()   # retrieves inverse for input argument
        if(!is.null(i)) {     # checks to see whether result is NULL, if not i is returned
                message("getting cached data")
                return(i)
        }
        data <- x$get()    # if cached value is NULLsolve is used to compute matrix inverse
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
