
## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Thus, the aim of this script is to write a pair of functions that cache the
## inverse of a matrix. These functions are :
##      1. makeCacheMatrix: this function creates a special "matrix" object that
##         can cache its inverse.
##      2. cacheSolve: this function computes the inverse of the special
##         "matrix" returned by makeCacheMatrix above. If the inverse has
##         already been calculated (and the matrix has not changed), then the
##         cachesolve should retrieve the inverse from the cache.



## The makeCacheMatrix function creates a special "matrix", which is really a
## list containing a function to :
##      1. set the value of the matrix
##      2. get the value of the matrix
##      3. set the value of the inverse
##      4. get the value of the inverse

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


## The cacheSolve function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if the
## inverse has already been calculated. If so, it gets the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of
## the matrix and sets the value of the inverse in the cache via the setinverse
## function.

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


## We test below the functions makeCacheMatrix and cacheSolve :

A<-matrix(c(0, 2, 2, 0),nrow=2,ncol=2)
A
B<-matrix(c(0, 4, 4, 0),nrow=2,ncol=2)
B
mA <- makeCacheMatrix(A)
mB <- makeCacheMatrix(B)
SumInvM <- cacheSolve(mA) + cacheSolve(mB)
SumInvM
ProdInvM <- cacheSolve(mA) %*% cacheSolve(mB)
ProdInvM

## The message "getting cached data" is not displayed when we calculate
## SumInvM because the inverse of A vs B is calculated for the first time.
## However, the message "getting cached data" is displayed twice when we
## calculate ProdInvM because the inverse of A vs B has been calculated before
## when we have calculated SumInvM.
## Conclusion : it works !
