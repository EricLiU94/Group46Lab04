#The object
linreg <- setRefClass("linreg",
                      fields = list(
                        regres_coef = "matrix",
                        fitted_y = "matrix",
                        res_value = "matrix",
                        degrees_of_freedom = "numeric",
                        res_var = "numeric",
                        regression_var = "numeric",
                        t_values = "matrix",
                        p_value = "matrix")
)