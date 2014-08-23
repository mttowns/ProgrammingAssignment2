## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix - function that takes a matrix type as input and creates a special 'matrix' 
## object that contains the original matrix type and its inverse.  It contains a list of functions to:
##
## 1. set() - set the value of the matrix
## 2. get() - get the value of the matrix
## 3. setinv() - set the value of the matrix inverse
## 4. getinv() - get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    x_inv <- NULL
    set <- function(y){
        x <<- y
        x_inv <<- NULL
    }
    get <- function() x
    setinv <- function(x_inverse) x_inv <<- x_inverse
    getinv <- function() x_inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve - checks to see if the matrix object created in makeCacheMatrix has a cached inverse. 
## If so, the function returns the inverse.  Otherwise, it gets the matrix data and calls the solve()
## function to compute the inverse, which is then added to the matrix object via the setinv() function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    x_inv <- x$getinv()
    if(!is.null(x_inv)) {
        print("getting cached inverse")
        return(x_inv)
    }
    data <- x$get()
    x_inv <- solve(data)
    x$setinv(x_inv)
    x_inv 
}


