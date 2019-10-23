#' QR Decomposition Calculation 
QR_decomp <- setRefClass( "QR_Ridge", 
             fields = list (
               var_name= "character" 
               
             ), 
             methods =list(
               initialize = function (formula, data) {
                 X <- model.matrix(formula, data) 
                 var_name <<- colnames(X)
                 # decomposition of matrix X
                 D<- qr(X)
                 UpQ <- qr.Q(D)
                 LoQ<- qr.R(D)
                 
               }
             )
  )