## makeCacheMatrix takes in a matrix, sets the matrix, gets the matrix,
## sets the inverse of the matrix, and gets the inverse of the matrix.
## This function returns a list of the inner functions that allow this
## set of operations.
makeCacheMatrix <- function(x = matrix()) {
    temp <- NULL
    set <- function(y){
        x <<- y
        temp <<- NULL
    }
    get <- function() x
    setinv <- function(solve) {temp <<- solve}
    getinv <- function() {temp}
    list(set = set, get = get,
    setinv = setinv,
    getinv = getinv)
}
## cacheSolve takes in a matrix and returns the inverse of that matrix.
## This function first checks if the inverse of the given matrix has
## already been calculated. If so, the function retrieves the stored
## inverse matrix along with the message 'getting cached data.' If the
## inverse has not been calculated, the inverse is then calculated, set,
## and then returned without any message.
cacheSolve <- function(x, ...) {
    temp <- x$getinv()
    if(!is.null(temp)){
        message("getting cached data")
        return(temp)
    }
    data <-x$get()
    temp <- solve(data)
    x$setinv(temp)
    temp
}
