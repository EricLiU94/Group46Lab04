#' QR Decomposition Calculation 
#' 
#' @references: 
QR_decomp <- setRefClass( "QR_Ridge", 
             fields = list (
               var_name= "character",
               beta_coef1= "matrix",
               beta_coef_ridge= "matrix"
             ), 
             methods =list(
               initialize = function (formula, data, lambda) {
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
                 beta_coef1 <<- TR %*% t(UpQ) %*% Y  # beta coefficient, in accordance with QX decomposition method 
                 
                 # secondly, it is important to implement the ridged regression methods 
                 l<- lambda 
                 t1<- nrow(X)
                 t2<- ncol(X) 
                 tempx <- t(X) %*% X + diag(t1*l, t2, t2)
                 QR <- qr(tempx)  
                 
               }
             )
  )