
##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

##In other words, the makeCacheMatrix creates a special "matrix" object that can cache the input matrix and its  value
##This is followed by the second function cacheSolve calls functions stored in the special "matrix" returned by makeCacheMatrix. 
##If the inverse has already been calculated , then cacheSolve retrieves the inverse from the cache. If the input is new, It will inverse the matrix.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL # sets the value of m to NULL (Default if cacheSolve has not yet been used)
  set<-function(y){
    x<<-y ## caches the inputted matrix so that cacheSolve can check whether it has changed 
    m<<-NULL # # sets the value of m to NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve #set the value of the matrix
  getmatrix<-function() m
  list(set=set, get=get, # list of the  functions
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}




        
  cacheSolve <- function(x=matrix(), ...) { # Return a matrix that is the inverse of 'x'
  m<-x$getmatrix() # if inverse already calculated this will get it
  if(!is.null(m)){ # check to see if cacheSolve has been run previously
    message("getting cache data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...) # compute the value of the inverse of the input matrix
  x$setmatrix(m) # run the setmatrix function on the input matrix to cache it
  m # return the inverse
}
