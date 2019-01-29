## makeCacheMatrix() creates an R object that return a list (get, set, getInv, setInv)
## cacheSolve() checks whether the inverse of the matrix has been calculated...
## if true then just prints the result
## if false then calculates the inverse and prints the result

## This function takes x as an argument and returns a list
## Once this function it's instantiated, it's possible to...
## set or get the matrix value's
## setInv assign the value of the inverse matrix
## getInv return the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    setInv <- function(solve) inv <<- solve
    getInv <- function() inv
    
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## cacheSolve first checks wether the inverse of the matrix has already been calculated...
## if true then it return the existing value of the inverse matrix
## if false then it calculates the inverse of the matrix...
## To do this, it get's the value of the matrix declared on makeCacheMatrix...
## The inverse matrix it's stored on the "inv" variable...
## And then assigns the "inv" variable to the setInv function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    if(!is.null(inv)){
        message("getting cache data")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data)
    x$setInv(inv)
    inv
}
