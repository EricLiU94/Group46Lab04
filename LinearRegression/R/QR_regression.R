#' QR Decomposition Calculation 
#' 
#' @export QR_decomp , an object, which is used to calculate the beta coefficient
#' @field var_name the names of all field variables 
#' @field beta_coef1 the calculated beta coefficient, by QR composition method 
#' @field fitted_y, the estimated y value, using the beta coefficient
#' @field beta_coef_ridge, the estimated y value, using the ridged QR decomposition method 
#' @param formula 
#' @param data 
#' @param lambda the penalty value, which could varies between 0 to 1, for instance 
#' @return a ridged QR regression object containing relevant paramters
#' @references \url{https://machinelearningmastery.com/solve-linear-regression-using-linear-algebra/}
QR_decomp <- setRefClass( "QR_Ridge", 
             fields = list (
               var_name= "character",
               beta_coef1= "matrix",
               fitted_y = "matrix",
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
                 beta_coef_ridge<<- qr.coef(QR, t(X)%*%Y)
                 
                 fitted_y <<- X %*% beta_coef_ridge
               }
             )
  )