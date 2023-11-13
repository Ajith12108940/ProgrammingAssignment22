## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        # Initialize the inverse matrix to NULL
        inv <- NULL
        
        # Define a function 'set' to set the matrix and invalidate the cached inverse
        set <- function(y) {
                x <<- y
                inv <<- NULL  # Invalidate the cached inverse
        }
        
        # Define a function 'get' to retrieve the matrix
        get <- function() x
        
        # Define a function 'setInverse' to set the cached inverse
        setInverse <- function(inverse) inv <<- inverse
        
        # Define a function 'getInverse' to retrieve the cached inverse
        getInverse <- function() inv
        
        # Return a list of functions for manipulation and retrieval
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

# Function to compute the inverse of a matrix and cache the result
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()  # Try to retrieve the cached inverse
        
        # If the cached inverse is found, return it and display a message
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        # If the cached inverse is not found, compute the inverse
        mat <- x$get()  # Retrieve the matrix
        inv <- solve(mat, ...)  # Compute the inverse
        
        # Cache the computed inverse
        x$setInverse(inv)
        
        # Return the computed inverse
        inv
}
