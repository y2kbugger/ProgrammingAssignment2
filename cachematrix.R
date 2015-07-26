#!/usr/bin/env R
# These two functions work in conjunction to calculate and cache the calculated
# value of a matrix object.

# This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        # if you change the value of the matrix, make sure
        # to clear the cached inverse
        inverse <<- NULL
    }
    get <- function() x
    setInv <- function(inv) inverse <<- inv
    getInv <- function() inverse

    # return the object as a list of methods
    # calling myNewMakeCacheMatrix$set can be used to set the value of 
    # the matrix
    list(set = set,
         get = get,
         setInv = setInv,
         getInv = getInv)

}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(cacheMatrix, ...) {
    # Return a matrix that is the inverse of 'cacheMatrix'
    inverse <- cacheMatrix$getInv()
    if(!is.null(inverse)) {
            message("using cached inverse")
            return(inverse)
    }
    data <- cacheMatrix$get()
    inverse <- solve(data, ...)
    cacheMatrix$setInv(inverse)
    inverse
}


# create a random square matrix
set.seed(111)
n<-970
mat<-matrix(runif(n^2),n)

# create the special 'cacheMatrix' object
cm<-makeCacheMatrix(mat)

# matrix is loaded
print(str(cm$get))

# no inverse is solved yet
print(cm$getInv)

# takes a little while to calculate the first time
str(cacheSolve(cm))
# the subsequent calls are quick because it is using cache
str(cacheSolve(cm))
str(cacheSolve(cm))
str(cacheSolve(cm))
str(cacheSolve(cm))

# has to calculate it again when matrix is changed
# but then is cached allow for it to run quickly again
mat<-matrix(runif(n^2),n)
cm$set(mat)
str(cacheSolve(cm))
str(cacheSolve(cm))
str(cacheSolve(cm))
str(cacheSolve(cm))
str(cacheSolve(cm))
#q("no")

