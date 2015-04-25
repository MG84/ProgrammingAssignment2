## The makeCacheMatrix is a function that accepts as parameter a matrix and 
## returns a list of functions which allows the user either to store 
## a matrix and its inverse value or to retrieve them.

## The function cacheSolve accepts as parameter a makeCacheMatrix object
## (from now mcm), and returns the inverse value of the matrix stored in the mcm.
## If an inverse matrix has been already stored in the mcm
## it simply returns it (printing a message stating that the inverse was
## retrieved from the cache). 
## Otherwise it gets from the mcm the matrix, calculates its inverse and set 
## it to the mcm through the setinverse function at the very end 
## it returns the inverse matrix with a message stating that the inverse was
## calculated for the first time.

## The function makeCacheMatrix accepts a matrix objects x and returns 
## a list of functions: 
## - set(x): to store a matrix x
## - get(): to retrieve the matrix stored
## - setinverse(x): to store the inverse matrix x
## - getinverse(): to retrieve the inverse matrix stored
makeCacheMatrix <- function(x = matrix()) {
    ## initialize the inverse matrix to NULL
    i <- NULL
    ## define set function, to set a matrix
    set <- function(y) {
        ## assign y to x
        x <<- y
        ## reinitialize the inverse matrix to NULL
        i <<- NULL
    }
    ## define get function which returns the matrix x
    get <- function() x
    ## define setinverse function to store in i the inverse matrix
    setinverse <- function(solve) i <<- solve
    ## define getinverse function to retrieve the inverse matrix
    getinverse <- function() i
    ## returns a list of the functions defined before
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## The function cacheSolve accept as parameter an object x and other optional 
## arguments '...' which will be passed to the solve function.
## It returns the inverse matrix of the matrix stored in the x object.
cacheSolve <- function(x, ...) {
    ## store the inverse matrix of x in i
    i <- x$getinverse()
    ## if the inverse matrix is not null 
    ## print a message and returns the inverse matrix
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    ## get the matrix from x
    matrix <- x$get()
    ## calculate the inverse matrix applying the ... arguments
    i <- solve(matrix, ...)
    ## set the inverse matrix to the object x
    x$setinverse(i)
    ## print a message and returns the inverse matrix
    message("inverse has been calculated for the first time")
    i
}
