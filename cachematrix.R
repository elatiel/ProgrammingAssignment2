## Below are two functions which are used to create a special object that stores
## a matrix and cache's its inverse.
## In this context, the <<- operator is used to assign a value to an object
## in evironment which different from the current one


## makeCacheMatrix create a list containing functions to: set the value of a matrix,
## get the value of a matrix, set the value of inverse, get the value of inverse

makeCacheMatrix <- function(x=matrix()){
        x_inv <- NULL
        set<-function(y){
                x <<-y
                x_inv <<-NULL
        }
        get<- function() x
        setinv <- function(inv) x_inv <<-inv
        getinv <- function() x_inv
        list(set=set, get=get,
             setinv=setinv, getinv=getinv)
}


## cacheSolve function calculates the inverse of the matrix 
## created in a function above. It checks if inverse has already been calculated
## and gets inverse from the cache in case of true. 
## In that way it skips extra computation.


cacheSolve <- function(x, ...) {
        x_inv <- x$getinv()
        if(!is.null(x_inv)) {
                message("getting cached data")
                return(x_inv)
        }
        data <- x$get()
        x_inv <- solve(data, ...)
        x$setinv(x_inv)
        return(x_inv)
}
