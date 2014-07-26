## makeCacheMatrix returns a list of functions that can be used to set and retrieve a matrix,
## set and get the inverse of a matrix. cacheSolve takes a matrix, and either retrieves its inverse from the cache or
## calculates the inverse 

## functions to cache a matrix and its inverse


makeCacheMatrix <-function(Amatrix=matrix()){
  inverse <- NULL
  
  set<-function(y){
    Amatrix<<-y
    inverse<<-NULL
  }
  get<-function() Amatrix
  
  setInverse<- function(inverseMatrix) inverse<<-inverseMatrix     
  getInverse <- function() inverse 
  
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
  
}
 # calculate or retrieve inverse

cacheSolve<-function(Amatrix,...){
  inverse<-Amatrix$getInverse()
  if(!is.null(inverse)){
    message("Getting from cache")
    return(inverse)
  }
  inverse<-solve(Amatrix$get(),...)
  Amatrix$setInverse(inverse)
  inverse
  
  
  
}