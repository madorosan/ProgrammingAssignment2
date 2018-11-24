## Put comments here that give an overall description of what your
## functions do

## This function creates a list x with elements set, get, setinv, and getinv that
## sets/gets vector and inverse values.

makeCacheMatrix <- function(x = matrix()) {
        #initialize inverse value
        inverse <- NULL
        set <- function(y){
        
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinv <- function(solveinv) inverse <<- solveinv()
        getinv <- function() inverse
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function uses the solve() function to get the inverse of the matrix 'x'

cacheSolve <- function(x, ...) {
    
        inverse <- x$getinv()
        
        #checks cached inverese value
        if(!is.null(inverse)){
                message("getting cached data")
                return(inverse)
        }
        #assignes the values of the matrix into variable data
        data <- x$get()
        
        #solve for the inverse of matrix and assign it to inverse
        inverse <- solve(data, ...)
        
        #cache the value of inverse
        x$setinv(inverse)
     
         inverse
        ## Return a matrix that is the inverse of 'x'
}
