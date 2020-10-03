## 2020/10/03 Mehmet BORA - mehmet.bora@outlook.com
## resubmitted due to account problems
## makeCacheMatrix function assumes input is a square / invertible matrix.
## It generates a list containing functions to set and get the values of the 
## matrix and its inverse respectively.

makeCacheMatrix <- function(x = matrix()) {
    INV <- NULL
    set <- function(y){
        x <<- y
        INV <<- NULL
    }
    get <- function() x             
    # helper to pass the matrix (via caller fx)
    set_inverse <- function(solve) INV <<- solve 
    # helper to set inverse matrix
    get_inverse <- function() INV 
    # helper to get the inverse matrix
    list(set = set, get = get, 
         set_inverse = set_inverse,
         get_inverse = get_inverse)
    # List of helper functions
}


## cacheSolve function will accept makeCacheMatrix function's output as input.
## It then will check if the inverse (INV) resides in parent environment, using 
## get_inverse() helper function provided by makeCacheMatrix. If INV exists in  
## cache, it will be used. If not the matrix will be pulled by the get() helper
## function and INV will be computed using solve method. Afterwards INV will be 
## cached by the set_inverse() helper function.


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    INV <- x$get_inverse()                  # Helper to get the inverse mx
    if(!is.null(INV)){
        message("Acquiring cached inverse")
        return(INV)
    }
    mtx <- x$get()                          # Helper to get the matrix
    INV <- solve(mtx,...)                   # Solve method
    x$set_inverse(INV)                      # Helper to cache the inverse
    INV                                     # Return INV
}
