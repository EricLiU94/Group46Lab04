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
#' 
QR_decomp <- setRefClass("QR_Ridge", 
             fields = list (
               var_name= "character",
               input = "character",
               beta_coef1= "matrix",
               fitted_y = "matrix",
               beta_coef_ridge= "matrix",
               standard_resvec = "matrix",
               sq_standard_resvec = "matrix",
               regression_var = "matrix",
               degrees_of_freedom = "numeric",
               res_var = "numeric",
               p_value = "numeric",
               t_values = "numeric"
             ), 
             
             methods = list(
               initialize = function(formula, data, lambda) {
                 X <- model.matrix(formula, data) 
                 degrees_of_freedom <<- dim(X)[1]-dim(X)[2]
                 
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
                 
                 res_value <- Y -fitted_y # the residue vector 
                 
                 standard_resvec<<- (res_value -mean(res_value))/sd(res_value)
                 
                 sq_standard_resvec<<- sqrt(abs(standard_resvec)) 
                 
                 e<- res_value   # the estimated residue 
                 
                 res_var <<- as.numeric(t(e) %*% e)/(degrees_of_freedom) 
                 
                 sigma2 <- res_var
                 regression_var <<- solve(t(X) %*% X)*sigma2
                 
                 tempt1 <- sapply(diag(regression_var), sqrt) 
                 temptt2 <- c(1:length(tempt1))
                 temptt3<- as.numeric(beta_coef_ridge)
                 temptt2<- temptt3 /tempt1
                 
                 t_values <<-  temptt2 
                 p_value <<- 2*pt(-abs(t_values) , degrees_of_freedom)
                 
                 input <<- paste(" QR_decomp(formula = ", deparse(formula), ", data = ", deparse(substitute(data)), ", lambda = ", deparse(substitute(lambda)), ")", sep = "") 
               },
               print = function () {
                 'This is a print out function, which outlines the regression coefficients of the given system'
                 cat("Call:\n")
                 cat(noquote(input), "\n\n")
                 cat("Coefficients:\n")
                 base::print(t(beta_coef_ridge), row.names = FALSE)
               },
               predict = function(){
                 ' Retuns the fitted value y, from task 1.1.3'
                  colnames(fitted_y) <<- NULL
                 return(t(as.matrix(fitted_y))) 
               },
               coef = function(){ 
                 'Returns the regression coefficient, as a function of formula, data, also the penalty coefficient, from task 1.1.3 '
                 return(beta_coef_ridge)
               }
             )
  )