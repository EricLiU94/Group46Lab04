library(ggplot2)
#' Linear Regression package
#' 
#' @export linreg
#' @param formula a relation between variables
#' @param datathe data set containing variable values
#' @return a linear regression object containing relevant paramters
linreg <- setRefClass("linreg",
                      fields = list(
                        input = "character",
                        regres_coef = "matrix",
                        fitted_y = "matrix",
                        res_value = "matrix",
                        degrees_of_freedom = "numeric",
                        res_var = "numeric",
                        regression_var = "matrix",
                        t_values = "numeric",
                        p_value = "numeric"),
                      methods = list(
                        initialize = function(formula, data) {
                          input <<- paste("linreg( formula =", deparse(formula), ", data =", deparse(substitute(data)), ")")
                          # the dependent variable
                          X <- model.matrix(formula, data)
                          
                          # extarcting the independent variable
                          t1 <- all.vars(formula)
                          
                          # to change the class of the data from a data frame to a list
                          f1 <- which(names(data) == t1[1])
                          y1 <- data[f1]
                          Y <- as.matrix(y1)
                          
                          regres_coef <<- (solve(t(X) %*% X)) %*% ( t(X) %*% Y)
                          
                          fitted_y <<- X %*% regres_coef
                          
                          res_value <<- Y -fitted_y
                          
                          f <- dim(X)
                          p<-f[2]  # two parameters, beta zero and beta one
                          degrees_of_freedom <<- f[1]-f[2]
                          
                          e<- res_value   # the estimated residue 
                          
                          res_var <<- as.numeric(t(e) %*% e)/(degrees_of_freedom) 
                          
                          sigma2 <- res_var
                          regression_var <<- solve(t(X) %*% X)*sigma2
                          
                          tempt1 <- sapply(diag(regression_var), sqrt) 
                          temptt2 <- c(1:length(tempt1))
                          temptt3<- as.numeric(regres_coef)
                          temptt2<- temptt3 /tempt1
                          
                          t_values <<-  temptt2 
                          df<- degrees_of_freedom
                          p_value <<- 2*pt(-abs(t_values) , df) 
                        }, 
                        
                        resid = function() {
                          print(paste("The vector of residuals is")) 
                          return(res_value)
                        }, 
                        
                        pred= function(){
                          return(fitted_y)
                        }, 
                        
                        coef= function(){ 
                          beta<-as.vector(regres_coef )
                          names(beta)<-rownames(regres_coef)
                          return(beta)
                        }   
                      )
)
#' A function to print relevant regression output
#'
#' @param x a linreg object
#' @exportMethod print
setMethod("print", "linreg", function(x) {
  cat("Call:\n")
  cat(noquote(x$input), "\n\n")
  cat("Coefficients:\n")
  print(t(x$regres_coef), row.names = FALSE)
  }
)

#' A function to plot the regression output
#' 
#' @param x a linreg object
#' @exportMethod plot
setMethod("plot", "linreg", function(x) {
  df <- data.frame(x$res_value, x$fitted_y)
  colnames(df) <- c("ResidualValues", "FittedValues")
  df$Difference <- abs(df$ResidualValues)
  MeanValues <- df[,c(2,1)]
  MeanValues <- aggregate(df[,1], list(MeanValues$FittedValues), mean)
  colnames(MeanValues) <- c("FittedValues", "Value")
  df <- merge(df, MeanValues, by = "FittedValues", all.x = TRUE)
  df <- df[order(df$Difference, decreasing = TRUE),]
  ggplot(df, aes(x = FittedValues, y = ResidualValues, label = rownames(df))) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = 12, hjust = 0.5),
          plot.caption = element_text(size = 12, hjust = 0.5),
          axis.title = element_text(size = 12)) +
    labs(title = "Residuals vs Fitted", caption = x$input, xlab = "Fitted values", ylab = "Residuals") +
    geom_point(shape = 1, size = 4, stroke = 2) +
    geom_text(label = ifelse(df$ResidualValues == df$ResidualValues[1], rownames(df), 
                             ifelse(df$ResidualValues == df$ResidualValues[2], rownames(df),
                             ifelse(df$ResidualValues == df$ResidualValues[3], rownames(df),""))), hjust = 1.5, check_overlap = TRUE) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "grey") +
    geom_line(aes(y = Value), color = "red")
  
  df <- data.frame(sqrt(abs(x$res_value)), x$fitted_y)
  colnames(df) <- c("ResidualValues", "FittedValues")
  df$Difference <- abs(df$ResidualValues)
  MeanValues <- df[,c(2,1)]
  MeanValues <- aggregate(df[,1], list(MeanValues$FittedValues), mean)
  colnames(MeanValues) <- c("FittedValues", "Value")
  df <- merge(df, MeanValues, by = "FittedValues", all.x = TRUE)
  df <- df[order(df$Difference, decreasing = TRUE),]
  ggplot(df, aes(x = FittedValues, y = ResidualValues, label = rownames(df))) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = 12, hjust = 0.5),
          plot.caption = element_text(size = 12, hjust = 0.5),
          axis.title = element_text(size = 12)) +
    labs(title = "Scale-Location", caption = x$input, xlab = "Fitted values", ylab = "Residuals") +
    geom_point(shape = 1, size = 4, stroke = 2) +
    geom_text(label = ifelse(df$ResidualValues == df$ResidualValues[1], rownames(df), 
                             ifelse(df$ResidualValues == df$ResidualValues[2], rownames(df),
                                    ifelse(df$ResidualValues == df$ResidualValues[3], rownames(df),""))), hjust = 1.5, check_overlap = TRUE) +
    geom_line(aes(y = Value), color = "red")
  }
)
