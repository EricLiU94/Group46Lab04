linreg <- setRefClass( "QRdecomposition", 
                       fields= list(
                         q1="matrix", 
                         q2="matrix"
                       ),
                       methods = list( 
                         initialize <- function(formula, data){ 
                           X <- model.matrix(formula, data) 
                           
                           }
                           
                         )
  )