## The following functions are designed to catch the inverse of a matrix. 
## All process is consisted of two functions, where first is called "makeCacheMatrix" 
## and the second "cacheSolve". The reason why these functions were written is that matrix 
## inversion is usually a costly computation, so it can find some benefit to caching 
## the inverse of a matrix rather than compute it repeatedly.


## This is a first function called "makeCacheMatrix" which creates a special "matrix" 
## object that can cache its inverse as a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix


makeCacheMatrix <- function(x = matrix()) 
        {	m<- NULL
        set<- function(y) {
                x<<- y
                m<<- NULL}
        get<- function()x
        setinverse<- function(inverse)m<<- inverse
        getinverse<- function()m
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

##This is a second function called "cacheSolve" 
## and returns the inverse of the special "matrix". 
## In first step, it checks if the inverse has been computed before. 
## If yes, it will return the cached inverse without new computetion. 
## If not (the inverse was not already calculated), 
## it will compute the inverse, then cache the inverse and return it.

cacheSolve<- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data (HAPPY)")
                return(m)
        }
        data <- x$get()
        m<- solve(data, ...)
        x$setinverse(m)
        m
}
