
makeCacheMatrix <- function(x = matrix()) {
        ## creates variable i and assigns NULL value to it
        i<-NULL
        ## creates set function that assigns values to i and matrix X in the cache.
        set <- function(y){
                x<<-y
                i<<-NULL
        }
        ## returns value of matrix X
        get<-function() x
        ## assigns value of inverse matrix i in the cache
        setInverse<-function(inverse) i<<-inverse
        ## returns value of inverse matrix i
        getInverse<- function() i
        ## creates list of all these functions and variabes that are created with the function
        list(set=set,get=get,
             setInverse=setInverse,
             getInverse=getInverse)   
}

cacheSolve <- function(x, ...) {
        ## returns inverse matrix of X
        inverse <- x$getInverse()
        ## checks if inverse matrix is defined, if yes: returns value of inverse matrix
        if(!is.null(inverse)){ return(inverse)}
        ## if inverse matrix is not defined: it gets the data from X matrix and creates inverse matrix i 
        ## and sets its value in cache.
        data<-x$get()
        i<-solve(data,...)
        x$setInverse(i)
        i
}

