## makeCacheMatrix 
## It defines the getter and the setter of a matrix (x) and its inverse (i)

makeCacheMatrix <- function(x = matrix()) {

  i<-NULL
 
  set<-function(y){
    x<<-y
    i<-NULL
  }
  get<-function(){
    x
  }
  
  setInverse<-function(solve){
    i<<-solve
  }
  
  getInverse<-function(){
    i
  }
  
  list(set =set, get=get, setInverse=setInverse,getInverse=getInverse)
  
}

##cacheSolve
## Cache the result of solve (ie the inverse of matrix x) into its internal variable i

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i<-x$getInverse()
  if(!is.null(i)){
    message("Retreiving Cached Data")
    return(i)
  }
  
  data<-x$get()
  i<-solve(data,...)
  x$setInverse(i)
  return(i)
  
}
