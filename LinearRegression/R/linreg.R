library(ggplot2)
#' Linear Regression package
#' 
#' @export linreg
#' @field input, the input variables, formula and data
#' @field regres_coef contains the estimated beta value. If the scrutinised system contains one dependent variable, the number of element in regres_coef is two, beta_zero and beta one. On the other hand, if two dependent variables are included, the number of beta elements would be three. 
#' @field fitted_y, is the estimated value, using the regression coefficients
#' @field res_value, is the residuals, the difference between the sampling points and the estimated value
#' @field degrees_of_freedom, the degree of freedom describe the degress of freedom of the scrutinised system 
#' @field res_value, the residual variance is the variance of the error, between the estimated value and the sampling value 
#' @field standard_resvec, the standard residuals gives a measure of the strength of the difference between observed and expected values
#' @field res_var, the variance of the regression coefficients
#' @field t_values, the t-values for each coefficient
#' @field p_value, the p-values for each coefficient
#' @field sq_standard_resvec gives the square root of the standard residual vector value
#' @param formula a relation between variables
#' @param data the data set containing variable values
#' @return a linear regression object containing relevant paramters
#' @references \url{https://towardsdatascience.com/linear-regression-detailed-view-ea73175f6e86}
linreg <- setRefClass("linreg",
                      fields = list(
                        input = "character",
                        var_name = "character",
                        regres_coef = "matrix",
                        fitted_y = "matrix",
                        res_value = "matrix",
                        standard_resvec = "matrix",
                        sq_standard_resvec = "matrix",
                        degrees_of_freedom = "numeric",
                        res_var = "numeric",
                        regression_var = "matrix",
                        t_values = "numeric",
                        p_value = "numeric"),
                      methods = list(
                        initialize = function(formula, data) {
                          input <<- paste("linreg(formula = ", deparse(formula), ", data = ", deparse(substitute(data)), ")", sep = "")
                          # the dependent variable
                          X <- model.matrix(formula, data)
                          var_name <<- colnames(X)
                          
                          # extarcting the independent variable
                          t1 <- all.vars(formula)
                          
                          # to change the class of the data from a data frame to a list
                          f1 <- which(names(data) == t1[1])
                          y1 <- data[f1]
                          Y <- as.matrix(y1)
                          
                          regres_coef <<- (solve(t(X) %*% X)) %*% ( t(X) %*% Y)
                          
                          fitted_y <<- X %*% regres_coef
                          
                          res_value <<- Y -fitted_y
                        
                          
                          standard_resvec<<- (res_value -mean(res_value))/sd(res_value)
                          
                          sq_standard_resvec<<- sqrt(abs(standard_resvec)) 
                            
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
                        
                        print = function() {
                          'This is a print out function, which outlines the regression coefficients of the given system'
                          cat("Call:\n")
                          cat(noquote(input), "\n\n")
                          cat("Coefficients:\n")
                          base::print(t(regres_coef), row.names = FALSE)
                        },
                        
                        plot = function() {
                          'This is a plotting function, which outlines the residuals and the fitted values'  
                          df <- data.frame(res_value, fitted_y)
                          colnames(df) <- c("ResidualValues", "FittedValues")
                          df$Difference <- abs(df$ResidualValues)
                          MeanValues <- df[,c(2,1)]
                          MeanValues <- aggregate(df[,1], list(MeanValues$FittedValues), mean)
                          colnames(MeanValues) <- c("FittedValues", "Value")
                          df <- merge(df, MeanValues, by = "FittedValues", all.x = TRUE)
                          df <- df[order(df$Difference, decreasing = TRUE),]
                          plot1 <- ggplot2::ggplot(df, ggplot2::aes(x = FittedValues, y = ResidualValues, label = rownames(df))) +
                            ggplot2::theme_bw() +
                            ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                                           panel.grid.minor = ggplot2::element_blank(),
                                           plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                                           plot.caption = ggplot2::element_text(size = 12, hjust = 0.5),
                                           axis.title = ggplot2::element_text(size = 12)) +
                            ggplot2::labs(title = "Residuals vs Fitted", caption = input, x = "Fitted values", y = "Residuals") +
                            ggplot2::geom_point(shape = 1, size = 4, stroke = 1) +
                            ggplot2::geom_text(label = ifelse(df$ResidualValues == df$ResidualValues[1], rownames(df), 
                                                              ifelse(df$ResidualValues == df$ResidualValues[2], rownames(df),
                                                                     ifelse(df$ResidualValues == df$ResidualValues[3], rownames(df),""))),
                                               hjust = 1.5, check_overlap = TRUE) +
                            ggplot2::geom_hline(yintercept = 0, linetype = "dotted", color = "grey") +
                            ggplot2::geom_line(ggplot2::aes(y = Value), color = "red")
                          
                          df <- data.frame(sq_standard_resvec, fitted_y)
                          colnames(df) <- c("StResidualValues", "FittedValues")
                          df$Difference <- abs(df$StResidualValues)
                          MeanValues <- df[,c(2,1)]
                          MeanValues <- aggregate(df[,1], list(MeanValues$FittedValues), mean)
                          colnames(MeanValues) <- c("FittedValues", "Value")
                          df <- merge(df, MeanValues, by = "FittedValues", all.x = TRUE)
                          df <- df[order(df$Difference, decreasing = TRUE),]
                          plot2 <- ggplot2::ggplot(df, ggplot2::aes(x = FittedValues, y = StResidualValues, label = rownames(df))) +
                            ggplot2::theme_bw() +
                            ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                                           panel.grid.minor = ggplot2::element_blank(),
                                           plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                                           plot.caption = ggplot2::element_text(size = 12, hjust = 0.5),
                                           axis.title = ggplot2::element_text(size = 12)) +
                            ggplot2::labs(title = "Scale-Location", caption = input, x = "Fitted values", 
                                          y = expression(sqrt("|Standardized residuals|"))) +
                            ggplot2::geom_point(shape = 1, size = 4, stroke = 1) +
                            ggplot2::geom_text(label = ifelse(df$StResidualValues == df$StResidualValues[1], rownames(df), 
                                                              ifelse(df$StResidualValues == df$StResidualValues[2], rownames(df),
                                                                     ifelse(df$StResidualValues == df$StResidualValues[3], rownames(df),""))),
                                               hjust = 1.5, check_overlap = TRUE) +
                            ggplot2::geom_line(ggplot2::aes(y = Value), color = "red")
                          list(plot1, plot2)
                        },
                        
                        resid = function() {
                          'This method returns a vector of, which contains the estimated residuals'   
                          return(as.vector(res_value))
                        }, 
                        
                        pred = function(){
                          'This method returns the estimated fitted value, as a vector'  
                          return(fitted_y)
                        }, 
                        
                        coef = function(){ 
                          'This method returns the regression coefficients as a named vector'  
                          beta <- as.vector(regres_coef )
                          names(beta)<-rownames(regres_coef)
                          return(beta)
                        },
                        
                        summary =function() { 
                          'This method summarises all included coefficients in the regression computation.'  
                          l1<-length(p_value)
                          l2<-1:l1
                          cat("The Included Regression Coefficients are:\n")
                          cat("           ", "coeffi.", "s. error", "t-value", "p-value", "\n")
                          for (i in l2){
                            cat(var_name[i], format(regres_coef[i], digits = 5),format(sqrt(regression_var[i,i]), digits = 5),
                                format(t_values[i], digits = 5), format(p_value[i], digits = 5), "****", sep = " ")
                            cat("\n")
                          }
                          cat("\n")
                          cat("Residual standard error:", round(sqrt(res_var ), digits = 5), "on", degrees_of_freedom, "degrees of freedom", sep = " ")
                        }
                      )
)
