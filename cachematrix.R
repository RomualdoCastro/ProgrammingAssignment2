

# 
#          $Workfile:$
#         
#          $Revision: 1.0 $
#         
#          Author: Romualdo Castro
# 
#         Description: This library includes 2 functions 
#               makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#               cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
# 
#        


#
# 
#       function: makeCacheMatrix constructor.
#       param: matrix()
#       return: It initializes a cache matrix which is used by cacheSolve function, 
#       this function is used to cache the inverse of the matrix
#
#       Author:Romualdo Castro       

makeCacheMatrix <- function(x = matrix()) {
        
         ### initilizes the values and creates the function to be used by CacheSolve
         matrizInversa <- NULL
         
          set <- function(matrizY = matrix()) {
                  x <<- matrizY
                  matrizInversa <<- NULL
          }
         # print(paste0("matrizX:",x,"--matrizInversa:",matrizInversa ))
         get <- function() x
         
         setInverseMatrix <- function(solve) matrizInversa <<- solve
         
         getInverseMatrix <- function() matrizInversa
         
         
         list(set = set, get = get,
              setInverseMatrix = setInverseMatrix,
              getInverseMatrix = getInverseMatrix)
        
        

}


#
# 
#       function: cacheSolve constructor.
#       param: x is the value returned by makeCacheMatrix function.
#       return: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
#
#       Author:Romualdo Castro  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
         
        
        inverseMatrix <- x$getInverseMatrix()
        
        # if the inverse has already compute and cached then it returns the cahed inverse matrix
        
        if(!is.null(inverseMatrix))
        {
                message("getting cached data")
                return(inverseMatrix)
        }
        # if is the first time, the function set the value, calculate the inverse and its cached
        else
        {
                data<- x$get() 
                inverseMatrix <- solve(data,...)
                x$setInverseMatrix(inverseMatrix)
                return(inverseMatrix)
                
                
                
        }
        
        
}

## previamente asignar el directorio local:setwd("C:/Users/romualdo/Documents/Romu/personal/desarrolloprof/Data Scientist/Coursera/02- R Programming/ProgrammingAssignment2/ProgrammingAssignment2")
## después usar los siguientes comandos para probar el correcto funcionamiento

# source("cachematrix.R")
# 
# mat<-matrix(31:34,2,2)
# myMCM<-makeCacheMatrix(mat)
# minv<-cacheSolve(myMCM)
# minv<-cacheSolve(myMCM)
# mt3<-matrix(c(13,22,53,66,72,8,114,132,413), 3,3)
# myMC3<-makeCacheMatrix(mt3)
# minv3<-cacheSolve(myMC3)
# minv3<-cacheSolve(myMC3)

