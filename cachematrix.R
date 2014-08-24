##ProgrammingAssignment2
## The function makeCacheMatrix() creates a special "matrix" object that can cache its inverse 


makeCacheMatrix <- function(x = matrix()) {
i<-NULL 
set<-function(matrix){ 
	x<<-matrix 
 	i<<-NULL 
 } 
get<-function()x 
setInverse<-function(inverse){ 
	i<<-inverse 
 } 
 getInverse<-function()i 
 list(set=set,get=get, 
 	setInverse=setInverse, 
 	getInverse=getInverse) 
 } 


## The function cacheSolve() computes the inverse of the special "matrix" after searching for the result in cache 


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

i<-x$getInverse() 
         if(!is.null(i)){ 
         	message("getting cached result") 
        	return(i) 
         } 
         data<-x$get() 
         i<-solve(data) 
         x$setInverse(i) 
         i 
 } 