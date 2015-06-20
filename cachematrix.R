##The function, "makeCacheMatrix", is a function that returns a list of four functions.
##It is designed to create a special "matrix" object that caches its inverse.


makeCacheMatrix <- function(x = matrix()) {
 m<-NULL
  set<-function(y){      ##set is a function that uses the super assignment operater (<<-) 
    x<<-y                ## in order to substitute the value of x in the main function with the value of y 
    m<<-NULL             ## m is reset to NULL because its value is altered by the substitution of x with y.
  }
  get<-function() x      ## get() retrieves the value of function(x)
  setmatrix<-function(solve) m <<- solve     ##setmatrix is similar to set: it stores a new value in the main function
  getmatrix<-function() m                    ##getmatrix returns the new value created by setmatrix
  list(set=set, get=get,                     ##The four "internal functions" are stored in a list so that when 
       setmatrix=setmatrix,                  ## "makeCacheMatrix" is assigned to an object, the object iterates all four functions.
       getmatrix=getmatrix)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix is not changed), then cachesolve 
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
   m<-x$getmatrix()       ## m subsets the values from getmatrix assigned in the function above.
  if(!is.null(m)){        ##if m is not NULL, then the message "getting cached data" is returned and the function concludes.
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()         ##if the inverse is not calculated, get() retrieves the value of x
  m<-solve(matrix, ...)   ## the solve function calculates the inverse of the matrix
  x$setmatrix(m)          ## setmatrix sets the values of the inverse matrix and then 
  m                       ## m concludes the function by returning the values of the inverse matrix.
}
