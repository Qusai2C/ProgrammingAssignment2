# makeCacheMatrix as follows

# Create the new special "matrix" object by returning a list() to assign each of these functions as an element within a list ()and returns it to the parent environment. Here we're naming thelist elements, which allows to use the $ form of the extract operator to access the functions by name rather than by [[

makeCacheMatrix <- function(x = matrix()) {
inv<-NULL
set<-function(y){
  x<<-y
  inv<<-NULL
}
get<- function(){x}
setInverse<-function(inverse){inv<<-inverse}
getInverse<-function(){inv}
list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


# cacheSolve requires an argument that is returned by makeCacheMatrix() in order to retrieve the inverse from the cached value that is stored in the makeCacheMatrix() object's environment. cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed),then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv<-x$getInverse()
        if(!is.null(inv)){
          message("geting cached data")
          return(inv)
        }
        mat<-x$get()
        inv<-solve(mat,...)
        x$setInverse(inv)
        inv
}
#And thats it!