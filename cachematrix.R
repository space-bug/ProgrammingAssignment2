## this function createas a matrix that can cache it's inverse.
makeCacheMatrix<- function(x=matrix()){
  inver<- NULL
  set<-function(y){
    x<<-y
    inver <<-NULL
  }
  get<- function() (x)
  setInverse<- function(inverse) {inver<<- inverse}
  getInverse<- function() (inver)
  list(set = set, get= get, setInverse= setInverse, getInverse= getInverse)
}

## this function computes the inverse. If it has been already calclated it retrives the inverse from the cache.
cacheSolve<- function(x, ...){
  inver<- x$getInverse()
  if(!is.null(inver)){
    message("getting data")
    return(inver)
  }
  mat<- x$get()
  inver<-solve(mat,...)
  x$setInverse(inver)
  inver
}
