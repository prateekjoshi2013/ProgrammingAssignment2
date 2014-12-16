
## makeCacheMatrix function creates a special "matrix" object that can cache its inverse




## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix<-function(x=matrix()){
	inv<-NULL
	set<- function(y){
	x<<-y
	inv<<-NULL
	}

	get<-function()x
	setinv<-function(solve) inv<<-solve
	getinv<-function()inv
	list(set = set, get = get,setinv = setinv,getinv=getinv)
}




## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## if the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## retrieves the inverse from the cache.


 

cacheSolve <- function(x, ...) {
## m is the variable which stores inverted matrix
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting that data from cache")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinv(m)
        m
}
