## The following functions can be used to cache i.e.store temporarily  ## the inverse of a matrix. This is useful as
##calculating the inverse ## of matrices is a costly computation.Hence being able to cache and ## reuse the inverse
##value can make one's computations more 
## effient.Note that the following fucntions will only work for square ## invertible matrices

## The first  function will return a list of four functions. The first will can be used to set the matrix. The second the get it. The third to set the matrix inverse and the fourth the get the inverse


makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

## This function calculates the inverse of the matrix that has been set in the first function. If the inverse has
##already been calculated then it will return the cached value along with a message saying getting cached data".
## alternatively it will calculate the inverse from scratch. 


cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}