## Assignment2:Caching inverse of a matrix
## Following functions are used for caching inverse of a matrix

## Write a short comment describing this function
## makeCacheMatrix creates special matrix object that can cache its inverse
 ## it has variables x and inv
        ## x is matrix type which is passed while calling this function
        ## inv contains the inverse of function
 ## it has functions set, get, setinv, getinv
        ## set & get used to set and get values for variable x
        ## setinv & getinv used to set and get values for variable inv
        ## note: it does not compute inverse of matrix x
 ##list() function helps to make the matrix object special
 ##when we assign makeCacheMatrix to an object
 ##the object has all the 4 functions 

makeCacheMatrix <- function(x = matrix()) {
        inv<- NULL
        set<-function(y){
                x<<-y
                inv<<-NULL
        }
        get<-function() x
        setinv<-function(inv) inv<<-inv
        getinv<-function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


##cacheSolve computes inverse of special matrix returned by makeCacheMatrix 
##input to cacheSolve is object where makeCacheMatrix is stored
##cacheSolve verifies value inv
##if its already calculated it returns a message and value inv
##else it computes inverse in inv
## stores to cache using setinv 
## and returns value inv
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinv()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data<-x$get()
        inv<-solve(data,...)
        x$setinv(inv)
        inv
}
