# makeCacheMatrix is a function that returns a list of functions
# Its puspose is to store a martix and a cached value of the inverse of the 
# matrix. Contains the following functions:
# * set     set the value of a matrix
# * get      get the value of a matrix
# * setinverse   get the cahced value (inverse of the matrix)
# * getinverse     get the cahced value (inverse of the matrix)

makeCacheMatrix <- function(x = numeric()) {
        
        # holds the cached value or NULL if nothing is cached
        # initially nothing is cached so set it to NULL
        cache <- NULL
        
        # store a matrix
        setMatrix <- function(newValue) {
                x <<- newValue
                # since the matrix is assigned a new value, flush the cache
                cache <<- NULL
        }

        # returns the stored matrix
        getMatrix <- function() {
                x
        }

        # cache the given argument 
        cacheInverse <- function(solve) {
                cache <<- solve
        }

        # get the cached value
        getInverse <- function() {
                cache
        }
        
        # return a list. Each named element of the list is a function
        list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}


# The following function calculates the inverse of a "special" matrix created with 
# makeCacheMatrix
cacheSolve <- function(y, ...) {
        # get the cached value
        inverse <- y$getInverse()
        # if a cached value exists return it
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        # otherwise get the matrix, caclulate the inverse and store it in
        # the cache
        data <- y$getMatrix()
        inverse <- solve(data)
        y$cacheInverse(inverse)
        
        # return the inverse
        inverse
}

## As an example:
## create a square matrix
matrix <- makeCacheMatrix( matrix(c(1,2,3,4), nrow = 2, ncol = 2) )
summary(matrix)
matrix$get()
##     [,1] [,2]
##[1,]    1    3
##[2,]    2    4

##getting cached data
cacheSolve(matrix)
## [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

