## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## [GM] this function first sets i (for inverse) to NULL
## [GM] then defines the set function, which will cache the matrix
## [GM] then defines get, which retreives the matrix from cache
## [GM] then defines setinverse, which will calculate the inverse
## [GM] using "solve" and then caches the result as i
## [GM] the getinverse function will retrieve i from cache
## [GM] a list is returned, with all of these functions defined

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
## [GM] This function will first try to retreive the inverse from cache
## [GM] using the getinverse function.  If available, it will load
## [GM] and display a message indicating such
## [GM] Otherwise, the function will load the matrix into the "data"
## [GM] variable, and calculate the inverse using solve
## [GM] the inverse is then saved to cache using setinverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
