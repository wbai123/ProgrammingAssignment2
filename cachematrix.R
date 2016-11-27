## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function:
## The makeCacheMatrix function contains four functions, set(), get(),
## setinverse() and getinverse(), and two data objects, x and Inv. 

makeCacheMatrix <- function(x = matrix()) {
        Inv <- NULL
        set <- function(y) {
                x <<- y
                Inv <<- NULL
        }
        get <- function() x
        setinverse <- function(mat.inv) Inv <<- mat.inv
        getinverse <- function() Inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
} 


## Write a short comment describing this function
## The function finds matrix' inverse and if the inverse of a specific
## matrix has been stored in cache, the function will use that 
## inverse matrix in cache instead of calculating again.

cacheSolve <- function(x, ...) {
        Inv <- x$getinverse()
        if(!is.null(Inv)) {
                message("getting cached data")
                return(Inv)
        }
        data <- x$get()
        Inv <- solve(data, ...)
        x$setinverse(Inv)
        Inv
}
