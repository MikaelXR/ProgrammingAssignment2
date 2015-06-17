
## The following two functions are used to cache the inverse of a matrix.

## makeCacheMatrix creates a list containing a function to
## set the value of the matrix, get the value of the matrix,
## set the value of inverse of the matrix, get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get<-function() x
    setmatrix<-function(solve) m<<-solve
    getmatrix<-function() m
    list(set=set, get=get,setmatrix=setmatrix,getmatrix=getmatrix)
}


## The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# set_inverse function

cacheSolve <- function(x, ...) {
      m<-x$getmatrix()
      #Checks to see if cached matrix data is availabe.  
      if(!is.null(m)){
          message("Getting cached inverse matrix data")
          return(m)
      }
      #if cached matrix is not available, calculate inverse of matrix
      matrix<-x$get()
      m<-solve(matrix,...)
      x$setmatrix(m)
      #Return cached matrix
      m
}
