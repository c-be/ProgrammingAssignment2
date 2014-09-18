## -*- coding: utf-8 -*-
## c-be
## R programming (Coursera), Assignment 2
##
## This takes a matrix arguement from the user and then caches the inverse of 
## that matrix. This is achieved by using the following functions:
##      1)   makeCacheMatrix
##      2)   cacheSolve 
##
## Code is over-commented for pedagogical reasons.


## 1. makeCacheMatrix: creates a "list object" of smaller functions that 
##    allow it to cache the inverse of the matrix that it takes as its
##    arguement.

makeCacheMatrix <- function(x = matrix()) {
    
    # Set aside the space for the cache (m).
    m <- NULL 
    
    # Reach inside the bubble to change the matrix and reset the cached value. 
    set <- function(y) {            
        x <<- y
        m <<- NULL
    }
    
    # Return the matrix currently residing in the bubble.
    get <- function() x
    
    # Within the bubble, set the value of the cache to the inverse.
    setinverse <- function(inverse) m <<- inverse
    
    # Return the inverse matrix currently residing in the bubble.
    getinverse <- function() m
    
    # Creates a list of all the forementioned functions for later re-entry into
    # the bubble.
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## 2. cacheSolve: computes the inverse of the original matrix given to 
##    makeCasheMatrix or, if the inverse has already been calculated and 
##    the matrix has not changed, then the cacheSolve retrieves the inverse 
##    from the cache.


cacheSolve <- function(x, ...) {
    
    # Peer into the bubble and extract what value exists for the inverse. 
    m <- x$getinverse()
    
    # If within the bubble there is a value for the inverse, let the user know 
    # that it is goinh to be returned.
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    # If within the bubble there is no value for the inverse, extract the
    # original matrix that resides within the bubble (x).
    data <- x$get()
    
    # Calculate the inverse of matrix x.
    m <- solve(data, ...)
    
    # Delicately place the inverse of x that was just calculated into the 
    # bubble for safe keeping.
    x$setinverse(m)
    
    # Return a matrix that is the inverse of matrix x.
    m                           

}