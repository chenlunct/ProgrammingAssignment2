# amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
# amatrix$get()         # Returns original matrix
# cacheSolve(amatrix)   # Computes, caches, and returns    matrix inverse
# amatrix$getinverse()  # Returns matrix inverse
# cacheSolve(amatrix)   # Returns cached matrix inverse using previously computed matrix inverse

# amatrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2)) # Modify existing matrix
# cacheSolve(amatrix)   # Computes, caches, and returns new matrix inverse
# amatrix$get()         # Returns matrix
# amatrix$getinverse()  # Returns matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(val) inv <<- val
    getinverse <- function() inv
    list
    
    
}

# Computes, caches, and returns new matrix inverse if no caches
# Returns cached matrix inverse using previously computed matrix inverse if have caches
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
