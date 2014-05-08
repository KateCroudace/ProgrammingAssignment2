
## Function to create a 'special' matrix which caches it's inverse. It has four functions:
## set: set the value of the matrix
## get: return the value of the matrix
## setinverse: set the value of the inverse
## getinverse: return the value of the inverse

## To test:
## x=c(1, 2 ,4, 2, 3, 6, 3, 10, 12)
## dim x = (3,3)
## b <-makeCacheMatrix(x)
## cacheSolve(b)    #will calculate and cache
## cacheSolve(b)    #will bring from cache

makeCacheMatrix <- function(x = matrix()) {
        # variable that holds inverse
        inv <- NULL
        # set matrix
        set <- function(y) {
          x <<- y
          # new matrix so clear cached value
          inv <<- NULL
        }
        # get matrix
        get <- function() x
        # set inverse
        setinverse <- function(inverse) inv <<- inverse
        # get inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Calculates inverse of passed in matrix x. It first checks to see 
## whether inverse has already been calculated and cached. If so,
## just retrieves from cache.
## x is assumed to be a square, invertible, matrix

cacheSolve <- function(x, ...) {
        # Attempt to retrieve existing inverse
        inv <- x$getinverse()
        # If exists then just return cached value
        if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
        }
        # If inverse is not already cached than calculate inverse...
        data <- x$get()
        inv <- solve(data, ...)
        #...cache...
        x$setinverse(inv)
        #...and return.
        inv
}

