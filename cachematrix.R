## Put comments here that give an overall description of what your
## functions do

## This function is to create a matrix that is inversible for the use of the next function.


makeCacheMatrix <- function(x = matrix())
{
    value <- NULL
    set <- function(y)
    {
        x <<- y
        value <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) value <<- solve
    getinverse <- function() value
    list
    (
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
    )
}


## This function is to inverse the matrix. It will inverse the matrix in cache and return it unless the matrix is already inversed already, in which case it will return the cached-inversed one,

cacheSolve <- function(x, ...)
{
    value <- x$getinverse()
    if(!is.null(value))
    {
        message("getting cached data")
        return(value)
    }
    data <- x$get()
    value <- solve(data, ...)
    x$setinverse(value)
    value
}