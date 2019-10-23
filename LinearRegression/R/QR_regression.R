#' QR Decomposition Calculation 
QR_decomp <- setRefClass( "QR_Ridge", 
             fields = list (
               var_name= "character",
               beta_coef= "matrix"
               
             ), 
             methods =list(
               initialize = function (formula, data) {
                 X <- model.matrix(formula, data) 
                 var_name <<- colnames(X)
                 
                 # extarcting the independent variable
                 t1 <- all.vars(formula)
                 # to change the class of the data from a data frame to a list
                 f1 <- which(names(data) == t1[1])
                 y1 <- data[f1]
                 Y <- as.matrix(y1)
                 
                 # decomposition of matrix X
                 D<- qr(X)
                 UpQ <- qr.Q(D)
                 LoQ<- qr.R(D)
                 
                 # the transpose of the matrix R is
                 TR <- solve(LoQ)
                 beta_coef <<- TR %*% t(UpQ) %*% Y 
                 
               }
             )
  )