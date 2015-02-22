## Put comments here that give an overall description of what your
## Below are two functions that are used to create
## a special object that stores a matrix and cache's its inverse 


## makeCacheMatrix will create a special "vector",
## which is really a list containing a function to
##    set the value of the matrix
##    get the value of the matrix
##    set the value of the inverse matrix
##    get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {

        iM <- NULL                # inverse matrix of x
        set <- function(y) {      # set values of matrix
                x <<- y
                iM <<- NULL
        }
        get <- function() x       # get values of matrix
        setsolve <- function(solve) iM <<- solve # call solve to set value of iM
        getsolve <- function() iM # get value of iM
        list(set = set, get = get,
                setsolve = setsolve,
                getsolve = getsolve)
}




## The following function will check if the inverse matrix has already been calculated
## If yes, return the catched inverse matrix
## If no, calculate the inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## assume that 'x' is always invertible
        iM <- x$getsolve()

        ## checks to see if the iM has already been calculated
        if(!is.null(iM)) {
        message("getting cached data")
        return(iM)              # return the catched values, exit the function
        }

        ## get data and  calculate the iM
        data <- x$get()
        iM <- solve(data, ...)  # calculate the inverse matrix
        x$setsolve(iM)
        iM                      # return the calculated iM
}
