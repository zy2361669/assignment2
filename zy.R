## Put comments here that give an overall description of what your
## functions do
##makeCacheMatrix:This function creates a special "matrix" object that can cache its inverse

## Write a short comment describing this function
## makeCacheMatrix creates a list containing a function to
## 1 set the value of the matrix
## 2 get the value of the matrix
## 3 set the value of inverse of the matrix
## 4 get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
            x <<- y
            inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}        
        

## Write a short comment describing this function
##cacheSolve: This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. If the inverse has already been 
##calculated (and the matrix has not changed), then the cachesolve should 
##retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
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

## TestingmakeCacheMatrix()
x = rbind(c(1, 1/2), c(1/2, 1))
m = makeCacheMatrix(x)
m$get()
##     [,1] [,2]
##[1,]  1.0  0.5
##[2,]  0.5  1.0
## Testing cacheSolve()
cacheSolve(m)
##           [,1]       [,2]
##[1,]  1.3333333 -0.6666667
##[2,] -0.6666667  1.3333333
