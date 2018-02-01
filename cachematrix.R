## makeCacheMatrix and cacheSolve saves the inverse of a square matrix to cache
## and pulls the cached inverse when called again. The functions assume the inverse
## can be taken.

## makeCacheMatrix creates a matrix object that can cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        
        get<-function()x
        setinverse<-function(inverse) m<<- inverse
        getinverse<-function()m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## cacheSolve checks if the inverse of a matrix is already in memory
## returns inverse from memory if cached, solves inverse if not

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        g<-is.recursive(x)
        print(g)
        m<-x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return
        }
        data<-x$get()
        m<-solve(data,...)
        x$setinverse(m)
        m
}
