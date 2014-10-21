## A pair of functions that cache the inverse of a matrix

## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {

        ## initialize the inverse property
        i <- NULL
        
        ## Method to set the matrix
        set <- function(y) {
                m <<- y
                i <<- NULL
        }
        ## Method to get the matrix
        get <- function() m
        
        ## Method to set the inverse of the matrix  
        setInverse <- function(inverse) i <<- inverse
        
        ## Method to get the inverse of the matrix
        getInverse <- function() i
        
        ## Return a list of the methods
        list(set = set, get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}

## Compute the inverse of the special matrix returned by "makeCacheMatrix" above
## If the inverse has already been calculated (and the matrix has not changed),
## then the "cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
       
        ## gets value for cached inverse if it exists
        i <- x$getInverse
        
        ## if it exists return cached inverse
        if(!is.null(i)) {
                message("getting cached inverse")
                return i
        }
        # Otherwise calculate the inverse
        ## get the matrix 
        data <- x$get()
        
        ## calculate the inverse of the matrix
        i<- solve(data) %*% data
        
        ## cache inverse
        x$setInverse(i)
        
        ## return the inverse
        i
        
}
