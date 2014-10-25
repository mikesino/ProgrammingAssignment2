## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {    ## Creates list of 4 functions 
    m <- NULL                                  ## inverse matrix is NULL
    set <- function(y) {                       ## function assignes new matrix 
        x <<- y                                ## changes "x" matrix in environment makeCacheMatrix
        m <<- NULL                             ## assigns NULL value to inverse matrix in environment makeCacheMatrix
    }
    get <- function() x                        ## outputs "x" matrix (from environment makeCacheMatrix)
    setinverse <- function(solve) m <<- solve  ## sets inverse matrix in environment makeCacheMatrix
    getinverse <- function() m                 ## outputs inverse matrix (from environment makeCacheMatrix)
    list(set = set, get = get,                 ## creates the list of above described functions
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {            ## function either solves inverse matrix or outputs cached inverse matrix calculated before
    m <- x$getinverse()                     ## calls getinverse funtion from makeCacheMatrix and assigns the inverse matrix of matrix "x" to variable "m"
    if(!is.null(m)) {                       ## if "m" is already cached, the function will output the cached value and end the function cacheSolve
        message("getting cached data")
        return(m)
    }
    data <- x$get()                         ## assigns matrix to "data" variable
    m <- solve(data, ...)                   ## calculates inverse of "data" matrix
    x$setinverse(m)                         ## caches calculated inverse matrix
    m                                       ## Return a matrix that is the inverse of 'x'
}