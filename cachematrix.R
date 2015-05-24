## Functions in this file create an object that stores a matrix and caches the 
## inverse of that matrix

## The first function 'makeCacheMatrix' sets up
## a list containing a function to

##      1.  set the value of the matrix
##      2.  get the value of the matrix
##      3.  set the value of the inverse
##      4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x<<-y
                s<<-NULL
        }
        get<-function() x
        setinverse <- function(solve) s<<- solve
        getinverse <- function() s
        list(set=set,get=get,
                setinverse=setinverse,
                getinverse=getinverse)
}

## The second function calculates the inverse of a matrix whose special 'vector'
## was created with the above function. However, it first checks to see if the
## inverse has already been calculated 'if(!is.null(s))'. If so, 
## it `get`s the inverse from the cache and skips the computation. Otherwise, 
## it calculates the inverse of the data and sets the value of the inverse in 
## the cache via the `setinverse` function.

cacheSolve <- function(x, ...) {
  
        s<-x$getinverse()
        if(!is.null(s)) {   
        message("getting cached data")
        return(s)
        }
        
        data<-x$get()
        s<-solve(data,...)
        x$setinverse(s)
        s
}