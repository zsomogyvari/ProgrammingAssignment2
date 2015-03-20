## The two functions below are used to create a special object that stores a matrix and caches its inverse
## First makeCacheMatrix has to be called for the matrix variable (named my_matrix):
##      x <- makeCacheMatrix(my_matrix)
## After this cacheSolve(x) will give back the inverse of the actual my_matrix. If the inverse for the same matrix has 
## already been calculated, then it is taken from the cache, otherwise it is calculated by the function. 


## This first function creates cache variable "inv" for the inverted matrix and stores the original matrix as well (x). 
## It also gives back a list of four functions, with which the cached values can be manipulated.

makeCacheMatrix <- function(x = matrix()) {      
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)              
}


## This function first checks whether the actual my_matrix matrix differs from the stored matrix either in dimensions 
## or numerically. 
## If the two matrices are equal, then it reads the cached inverse matrix, and if its not empty it gives it back.
## If the matrices are not equal or the inverse has not yet been calculated, then the function makes the inversion, 
## gives back the inverted matrix and puts it into the cache.
## If the matrix has changed since the last calculation, then the stored matrix will also be updated.

cacheSolve <- function(x, ...) {
        if (!all(dim(my_matrix) == dim(x$get()))) {
                x$set(my_matrix)
                }
        else if (!all(x$get() == my_matrix)) {
                x$set(my_matrix)
                }
        else {
                inv <- x$getinv()
                if(!is.null(inv)) {
                        message("getting cached data")
                        return(inv)
                }
        }
        data <- x$get() 
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
