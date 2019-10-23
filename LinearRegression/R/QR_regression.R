#' QR Decomposition Calculation 
QR_decomp <- setRefClass( "QR_Ridge", 
             fields = list (
               
             ), 
             methods =list(
               initialize = function (formula, data) {
                 X <- model.matrix(formula, data)
               }
             )
  )