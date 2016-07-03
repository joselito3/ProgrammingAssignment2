## The following two functions implement the programming assigment of week 3 of
## the Johns Hopkins Universitiy Coursera R Programming Course
##
## The goal of the assigment is to cache the inverse of a matrix, being this
## usually a costly computation, storing it together with its data
## 
## The first function will take care of the storage operations, wrapping both
## the matrix and its inverse with set and get functions
##
## The second function will take care of attempting to retrieve the cached inverse
## matrix, if available, and if not storing it for future accesses.
##
## It is an asummption of the assignment that the input matrix will always be
## invertible
##
## Example usage:
##
## > m1 <- matrix(c(1,2,3,4), 2, 2) # creates a small invertible matrix
## > mc1 <- makeCacheMatrix(m1)     # creates an object using the previous data
## > mc1$get()                      # verifies that the data is stored
##     [,1] [,2]
##[1,]    1    3
##[2,]    2    4
## > mc1$getinv()                   # no inverse is available yet
## NULL
## > cacheSolve(mc1)                # the cacheSolve function calculates the inverse
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##
## > mc1$getinv()                   # just to verify that the inv. has been stored
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
## > cacheSolve(mc1)
## getting cached data              # next calls to cacheSolve just read from the cache
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5


## The function makeCacheMatrix takes as an input parameter an invertible matrix.
## It returns an object containing:
##  - The data of the input matrix
##  - A placeholder to store the inverse matrix
## It offers methods to set and get both the data of the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {

    inv_x <- NULL
    set <- function(y) {
        x <<- y
        inv_x <<- NULL
    }
    get <- function() x
    setinv <- function(inv = matrix()) inv_x <<- inv
    getinv <- function() inv_x
    list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)

}


## The cacheSolve function takes as input an object created
## with the function makeCacheMatrix
## The funtion checks if the object contains already a cached
## inverse matrix. If so it simply returns it
## If no inverse matrix is available, it calculates it using the function
## solve and stores it in the input object for future use

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        inv_x <- x$getinv()
        if(!is.null(inv_x)) {
          message("getting cached data")
          return(inv_x)
        }
        data <- x$get()
        inv_x <- solve(data, ...)
        x$setinv(inv_x)
        inv_x
}
