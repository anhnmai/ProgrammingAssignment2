## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	m<-NULL
	set<-function(y){
		x<<-y
		inverseVal<<-NULL
	}
	get<-function() x
	setVal<-function(solve) inverseVal<<-solve
	getVal<-function() inverseVal
	list(set=set,get=get,setVal=setVal,getVal=getVal)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        #inverseVal<<-NULL
        inverseVal<-x$getVal()
        if(!is.null(inverseVal)){
        	message("getting cached data")
        	return(inverseVal)
        }
        data<-x$get()
        inverseVal<-solve(data,...)
        x$setVal(inverseVal)
        inverseVal
}
