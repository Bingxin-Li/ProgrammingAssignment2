#the following two functions cache the inverse of a matrix

##create a list which contains a function

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    #set the value of the matrix
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    #get the value of the matrix
    get <- function() x
    #set the value of the inverse
    setInverse <- function(inverse) i <<- inverse
    #get the value of the inverse
    getInverse <- function() i
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}
#check if the inverse has been calculated
# if so, return the inverse that has been calculated
# if not, recalculate the inverse and set the inverse with setInverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data,...)
    x$setInverse(i)
    i
}
