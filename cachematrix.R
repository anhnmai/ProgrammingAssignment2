## Put comments here that give an overall description of what your
## functions do
## This is the function to create a matrix object which can cache its inverse
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	#Set the inverse matrix to zero
	inverseVal<-NULL
	#create function to set the value of the given matrix
	# and cache them, here given matrix x and its inverse are cached
	set<-function(y){
		x<<-y
		inverseVal<<-NULL
	}
	#get the given matrix from cache
	get<-function() x
	# search for existing of inverse matrix and redefine it in cache
	setVal<-function(solve) inverseVal<<-solve
	#get the inverse matrix from the cache 
	getVal<-function() inverseVal
	list(set=set,get=get,setVal=setVal,getVal=getVal)
}


## Write a short comment describing this function
##The function returns the inverse matrix of given matrix which special made or returned from function makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        #Get the inverse matrix from the cache if it is there
        inverseVal<-x$getVal()
        if(!is.null(inverseVal)){
        	message("getting cached data")
        	return(inverseVal)
        }
        #Else do this
        #Get the stored matrix x from the cache
        data<-x$get()
        #Use solve to get the inverse matrix 
        #as it has not been computed  
        inverseVal<-solve(data)
        #cache the result
        x$setVal(inverseVal)
        #return the inverse matrix as the result
        inverseVal
}
