## Assignment 3 - Juan Pablo Tsou
## The makeCacheMatrix function creates a special object that can cache a matrix and its inverse.

## set: Sets the value of the matrix and resets the inverse cache.
## get: Returns the stored matrix.
## setInverse: Stores the computed inverse in the cache.
## getInverse: Returns the cached inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  
    
    set <- function(y) {
        x <<- y  
        inv <<- NULL  
    }
    
    get <- function() x  
    
    setInverse <- function(inverse) inv <<- inverse  
    
    getInverse <- function() inv  
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## NEXT FUNCTION
## This function: "cacheSolve" will use the special "matrix" object created by makeCacheMatrix.
## If the inverse has already been calculated and stored, it will retrieve it from the cache; otherwise, 
## it will compute the inverse, store the result in the cache, and then return it.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()  
    
    if(!is.null(inv)) {  
        message("getting cached data")
        return(inv)  
    }
    
    mat <- x$get()  
    inv <- solve(mat, ...)  
    x$setInverse(inv)  
    inv  
}



