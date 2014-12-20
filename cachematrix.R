## Two functions below are to cache the inverse 
## of an invertible square matrix

## by T.M., December 2014 
## (Coursera R-Programming, Programming Assignment #2)
## using the provided prototype 
## https://github.com/rdpeng/ProgrammingAssignment2
## https://class.coursera.org/rprog-016/human_grading/...
##         view/courses/973757/assessments/3/submissions

## Additional references:
## Discussion forum on lexical scoping
## https://class.coursera.org/rprog-016/forum/list?forum_id=10012
## and available (googled) info on Object-Oriented-Programming in R
## Special thanks to Pavel Kirjanas
## https://class.coursera.org/rprog-016/forum/thread?thread_id=96

## <makeCacheMatrix>: creates an object, which is specific for a matrix,
##      to cache its inverse

##      Input argument:
##      x    a square numeric matrix assumed to be invertible

##      Output:
##      an object-specific list of four internal functions (methods) 
##              makeCacheMatrix$set: to set the value of the matrix 
##              makeCacheMatrix$get: to get the value of the matrix
##              makeCacheMatrix$setinv: to set the value of the matrix 
##              makeCacheMatrix$getinv: to get the value of the matrix
##      To set the values of the matrix and its inverse,
##      the superassignment is used to cache the inverting process and
##      thus to save computing time.

makeCacheMatrix <- function(x = matrix()) {
        
        inv.matrix <- NULL  # the variable to store the inverse matrix;
                            # set to NULL, i.e. an undefined value
 
        set <- function(y) {# sets the value of the matrix 
                x <<- y     # re-defines matrix x (declared elsewhere)
                            # and caches it for future use                 
                inv.matrix <<- NULL
                            # re-sets the inverse matrix for the
                            # newly defined matrix x and caches it
        }
        
        get <- function() x # fetches the value of x
                            # note that {} are omitted, equivalent to
                            # get <- function() {x}
        
        setinv <- function(solve) {
                            # sets the value of the inverse matrix
                            # when <makeCacheMatrix> is called for
                            # the first time for a specific matrix x
                inv.matrix <<- solve
        }
       
        getinv <- function() inv.matrix
                            # fetches the cached inverse matrix 
                            # when called for a specific matrix x 
                            # for the second and subsequent times

        # The object below is a list of the above defined functions,
        # which provides a way to access these object-specific methods 
        # from outside.
        # This object is defined "from scratch"  
        # each time <makeCacheMatrix> is called for a specific matrix x
        list(set = set, get = get, setinv = setinv, getinv = getinv)                  
        
}

## <cacheSolve>: returns a matrix that is the inverse 
##               of a square inversible matrix [stored as X.methods$get]
##      The inverse is either calculated and then stored for future use
##      (through superassignment) or its cached value is fetched

##      Input argument:
##      X.methods    an object created by the <makeCacheMatrix> function
##                   for a specific matrix
##      ...          optional arguments for the <solve> function
                     
##      Output:
##      the inverse matrix  

cacheSolve <- function(X.methods, ...) {
        
        inv.matrix <- X.methods$getinv()        
        if (!is.null(inv.matrix)) {     
                # inv.matrix is not NULL, i.e.
                # the inverse matrix has already been calculated
                # its cached value is now returned and
                # <cacheSolve> terminates here
                message("getting cached data")
                return(inv.matrix)
        } 
        # the rest of the function is only executed if the inverse 
        # has not been defined yet for the specific matrix
        data <- X.methods$get()        # gets the cached matrix
        inv.matrix <- solve(data, ...) # calculates its inverse
        X.methods$setinv(inv.matrix)   # caches the inverse for future

        inv.matrix                     # the inverse is returned  
}
