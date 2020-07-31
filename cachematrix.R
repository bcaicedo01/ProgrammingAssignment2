## The following are functions used to cache the inverse of a given matrix
## inverting a matrix can consume a considerable amount of resources so
## it may seem a good idea to keep the inverse of a given matrix already calculated
## somewhere that's handy to retrieve

## makeCacheMatrix() is a function that provides the methods to build and 
## manipulate the list that will contain the cached inverse matrix 


makeCacheMatrix <- function(x = matrix()) { 
  inversa <- NULL
  set <- function(y) {
  x <<- y
  inversa <<- NULL
}
get <- function() x
setinv <- function(invertida) inversa <<- invertida
getinv <- function() inversa
list(set = set, get = get,
     setinv = setinv,
     getinv = getinv)
  

}


## cacheSolve() is a function that receives the list initialized by makeCacheMatrix()
## and either returns the already calculated inverse matrix for the matrix stored 
## in the list through the makeCacheMatrix$set() method, if there is already 
## one calculated. Else, it will calculate the inverse of the stored matrix and
## return the result as the value of the function

# It's necessary to create the list to contain the methods and the data first
# through makeCacheMatrix() and afterwards to define the matrix whose inverse 
# is required through the makeCacheMatrix$set() method before trying to 
# call cacheSolve().  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of the matrix contained in the list X
       inversa <- x$getinv()
       if (!is.null(inversa))
       {
         message("Getting cached matrix")
         return(inversa)
       }
       ainvertir<-x$get()
       inversa<-solve(ainvertir,...)
       x$setinv(inversa)
       inversa
}
