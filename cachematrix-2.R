# This is to write R function(S) that will allow caching of potentially 
# time-consuming computations.


#Create a matrix that can cache its own inverse
makeCacheMatrix <- function(x = matrix()) 
{
    # Initalize inverse property
    invMatrix <- NULL
    
    # Method use to set values
    set <- function(matrix)
    {
        x <<- matrix
        invMatrix <<- NULL
    }
    
    # Method use to get valus
    get <- function()
    {
        # Return the matrix
        x
    }
    
    # Method to set inverse
    setInverse <- function(inverse)
    {
        invMatrix <<- inverse  
    }
    
    # Method to get the inverse of the matrix
    getInverse <- function()
    {
        #Return the inverse 
        invMatrix
    }
    
    # Return list of methods
    list(set = set, get = get, setInverse = setInverse, getInversse = getInverse)
    
} # End of 'makeCacheMatrix'


cacheSolve <- function(x, ...) {
    # Return the matrix that is the inverse of 'x'
    m <- x$getInverse()
    
    # Return the invervse if it is already set
    if( !is.null(m) )
    {
        message("Getting cached data")
        return(m)
    }
    
    # Get the matrix
    data <- x$get()
    
    # Calculate the inverse using matrix multiplication
    m <- solve(data) %*% data
    
    # Set the inverse of the object
    x$setInverse(m)
    
    # Return back to user
    m
    
} # End of 'cacheSolve'
