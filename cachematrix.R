## A pair of functions that calculate and store the inverse of a matrix

## This function creates a special "matrix" object and stores its inverse

makeCacheMatrix <- function(x = matrix()) {
  
        inv <- NULL #initializing for later use
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        setinverse <- function(solveMatrix) inv <<- solveMatrix
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function pulls the inverse of the "matrix" object returned by makeCacheMatrix if available
## If the inverse has not been stored, then it checks if the matrix is invertible
## And if so, computes it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        
        if (!is.null(inv)) {
                message("Getting cached data")
                return(inv)
        }
        
        if (det(x$get()) == 0) {
          
                message("Matrix is singular. Inverse does not exist") ##This is unnecessary for the assignment,
                                                                      ##but felt right to do
        } else {
                data <- x$get()
                inv <- solve(data)
                x$setinverse(inv)
                inv
        }
        
}
