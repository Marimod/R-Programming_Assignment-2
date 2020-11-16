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




cacheSolve<-function(cached.mat,...){
  inverso<-cached.mat$get.inverse()
  if(!is.null(inverso)){
    print("get cache inverse")
    return(inverso)    
  }
  altra_mat<-cached.mat$get()
  i<-solve(altra_mat,...)
  cached.mat$set.inverse(i)
  i
}
