"makeCacheMatrix function defined below creates a special matrix object that 
returns a list containing functions to set the values of the matrix, get the 
values of the matrix, set the values of the matrix inverse and get the values of
the matrix inverse;
it uses <<- operator  to assign a value to an object in an environment that is 
different from the current environment"

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse)  inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


"cacheSolve function defined below will check to see if the inverse of the 
matrix is already computed; if so, it gets the inverse from the cache and skips 
the computation; otherwise it computes the inverse of the matrix and sets it in 
the cache via the setinv function;
function takes a non-singular square matrix as input and returns the matrix 
inverse"

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinv(m)
        m
}