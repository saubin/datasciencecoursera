##Program for Coursera R Programming Assignment #2
## Function will create inversed matrix object in cache

makeCacheMatrix <- function (x= matrix()) {
	inv<- NULL
	set <- function (y) {
		x<<-y
		inv<<-NULL
	}
	get<-function() x
	setInverse<-function (inverse) inv<<-inverse
	getInverse<-function() inv
	list (set = set,
		  get=get,
		  setInverse = setInverse
		  getInverse = getInverse)
}

##Computing the data set of the inversed matrix created above

cacheSolve<-function (x, ...) {
	##Return a matrix that is the inverse of 'x'
	inv<-x$getInverse()
	if (!is.null(inv)){
		Message("getting cached data")
		return(inv)
	}
	mat<-x$get()
	inv<-solve(mat,...)
	x$setInverse(inv)
	inv
}
