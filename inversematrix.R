## The makeCachematrix function receives a matrix and creates a list of four functions that allow us to set and get 
## the values of the matrix and its inverse matrix


makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {                   #function that assign y to x
                x <<- y
                i <<- NULL
        }
        get <- function() x                    #function that returns x
        setInv <- function(solve) i <<- solve  #function that assign a matrix to the inverse of x
        getInv <- function() i                 #function that returns the inverse of x saved in cache
        list(set = set, get = get,             #creates a list with set, get, setInv and getInv functions
             setInv = setInv,
             getInv = getInv)
        
}


## The cacheSolve function gets and returns a matrix that is the inverse of x if it´s already calculated and stored in 
## cache, otherwise the function calculates the inverse using solve function and stores the result in cache


cacheSolve <- function(x, ...) {
        i <- x$getInv()                         #query the x matrix's cache 
        if(!is.null(i)) {                       #if the inverse is already calculated and stored
                message("getting cached data")
                return(i)                       #just return the cache, no computation needed
        }
        data <- x$get()                         #if i is empty
        i <- solve(data, ...)                   #we calculate the inverse
        x$setInv(i)                             #save the result back to x's cache
        i                                       #return a matrix that is the inverse of 'x'
        
}
