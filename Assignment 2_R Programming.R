#This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix<-function(x=matrix()){
  inv=NULL
  set<-function(matrice){
    mat<<-matrice
    inv<<-NULL}
  get<-function()mat
  set.inverse<-function(setinv) inv<<-setinv
  get.inverse<-function()inv
  list(set= set, get=get,
       set.inverse=set.inverse,
       get.inverse=get.inverse)
}



#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
cacheSolve<-function(cached.mat,...){
  inverso<-cached.mat$get.inverse()    ##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
  if(!is.null(inverso)){
    print("get cache inverse")
    return(inverso)    
  }
  altra_mat<-cached.mat$get()
  i<-solve(altra_mat,...)
  cached.mat$set.inverse(i)
  i
}
