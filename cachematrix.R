## You create a matrix by: a<-makeCacheMatrix(cbind( c(1, 3), c(3, 1)))
## The result of makeCacheMatrix function is a list. Tou can check it by: class(a)
## Then you can get the matrix by: a$get()
## If you want to change the matrix, you can do it by: a$set(cbind( c(1, 3), c(3, 1)))
## You may set the inverse of a matrix by a$setinverse(cbind(c(2, 4), c(4, 2)))
## and get it by a$getinverse().
## This is not an inverse of the actual matrix, that we set by a$set(cbind( c(...,...), c(...,...)))
## it is only a cache memory, that we can use to store some inverse.

makeCacheMatrix <- function(x = matrix()) {
	i<-NULL
	set<-function(y) {
		x<<-y
		i<<-NULL
	}
	get<-function() x
	setinverse<-function(inverse) i<<-inverse
	getinverse<-function() i
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## You use the function cacheSolve by: cacheSolve(a)
## When you will use it for the first time, it will return you the value stored in cache, wchich most likely be
## cbind(c(2, 4), c(4, 2)) matrix, together with a comment: "getting cached data",
## you probably set the inverse value of a matrix by a$setinverse(cbind(c(2, 4), c(4, 2)))
## Than you can set a new matrix, that you want to inverse. You do it by: a$set(cbind( c(1, 9), c(9, 1)))
## You will receive the inverse by: cacheSolve(a)
##If you will write cacheSolve(a) again, you will receive the same value, but with a comment "getting cached data".

cacheSolve <- function(x, ...) {
	i<-x$getinverse()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data<-x$get()
	i<-solve(data, ...)
	x$setinverse(i)
	i
}
