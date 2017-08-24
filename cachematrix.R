## These functions cache a matrix to compute the inverse of the matrix.
## This allows for quicker computations when computing the inverse.

## makeCacheMatrix creates the cache; x is an invertible matrix

makeCacheMatrix <- function(x = matrix()) {
        ## initialize
        inverseMatrix <- NULL 
        
        ## set the value of the matrix
        set <- function(y) { 
                x <<- y
                inverseMatrix <<- NULL
        }
        
        ## get the value of the matrix
        get <- function() x 
        
        ## set the value of the inverse matrix
        setInverseMatrix <- function(settingInverse) inverseMatrix <<- 
                settingInverse
        
        ## get the value of the inverse matrix
        getInverseMatrix <- function() inverseMatrix
        
        ## create the list of functions
        list (set = set, get = get, 
              setInverseMatrix = setInverseMatrix, 
              getInverseMatrix = getInverseMatrix)
        
}


## cacheSolve calculates the inverse matrix if it has not already been cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## attempt to retreive from cache
        inverseMatrix <- x$getInverseMatrix()
        
        ## check for cached inverse and return if it exists
        if(!is.null(inverseMatrix)) {
                message("Getting cached data.")
                return(inverseMatrix)
        }
        
        ## if no cached data exists, get original matrix
        data <- x$get()
        
        ## solve for the inverse
        inverseMatrix <- solve(data, ...)
        
        ##set inverse in cache
        x$setInverseMatrix(inverseMatrix)
        
        ## return calculated inverse
        inverseMatrix
}
