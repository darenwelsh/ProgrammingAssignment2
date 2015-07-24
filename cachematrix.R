## These functions enable the user to calculate the inverse of a matrix
## and store the results in the cache for improved efficiency

## Usage
##
## For matrix my_matrix 
## Use "a <- makeCacheMatrix(my_matrix)" to create object "a"
## Then "cacheSolve(a)" to calculate and display the inverse matrix

## This function stores 4 functions
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    ## 1. set value of matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    ## 2. get value of matrix and display it
    get <- function() x
    ## 3. set value of inverse matrix
    setinverse <- function(solved) inv <<- solved
    ## 4. get value of inverse matrix
    getinverse <- function() inv
    ## list the functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## If a matrix was already stored with x$setinverse(inv),  
## return that value from the cache
## If no matrix has been stored with x$setinverse(inv), 
## calculate the inverse of the matrix
## store it with x$setinverse(inv) and
## return it
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        ## if inv is not null, return value
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    ## use solve() to get inverse matrix
    inv <- solve(data, ...)
    ## set the value of inverse matrix
    x$setinverse(inv)
    ## return the value of the inverse matrix
    inv
}
