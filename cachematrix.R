## makeCacheMatrix returns a list of functions called by subsetting the list with function names. set() stores an input matrix, get() retrieves it, setinv() stores the inverse, getinv() retrieves the inverse

## Once a matrix is 'set', cacheSolve checks to see if getinv() has stored a return value for the inverse other than NULL. If yes, retrieve it. Otherwise, compute the inverse, store it and display the inverse.

#default the object x to an empty matrix
makeCacheMatrix <- function (x=matrix()) {
        
        #initialize an inverse(inv) matrix as NULL
        inv <- NULL
        
        #define 'set': replace the empty x with an input matrix 
        set <- function(y) {
                x <<- y
                #re-set 'inv' to NULL whenever 'set' is called
                inv <<- NULL
        }
        
        #define get: retrieve x which is replaced by the input
        get <- function() x
        
        #define setinv: replace inv=NULL with the inverse matrix
        setinv <- function(inverse) inv <<- inverse
        
        #define getinv: retrieve 'inv' replaced by the inverse
        getinv <- function() inv
        
        #create a list of all needed functions for calling
        list (set=set, get=get, setinv=setinv, getinv=getinv)
}

# pass the return value from the above function
cacheSolve <- function (x, ...) {
        
        #retrieve the 'inv' value by calling getinv()
        inv <- x$getinv()
        
        #if 'inv' has been computed, retrieve 'inv' from 'cache' 
        if (!is.null(inv)) {
                message("getting cached data")
                return (inv)
        }
        
        #otherwise retrieve the matrix by calling get()
        data <- x$get()
        
        #compute the inverse and 'setinv' the result in 'inv'
        inv <- solve(data, ...)
        x$setinv(inv)
        
        #display the inverse
        inv
}
