# OUTPUT CV matrix
install.packages("vcd")
library(vcd)

# Simulate some data or paste your own
df <- data.frame(x1 = sample(letters[1:5], 20, replace = TRUE), 
                 x2 = sample(letters[1:5], 20, replace = TRUE), 
                 x3 = sample(letters[1:5], 20, replace = TRUE))

# Initialize empty matrix to store coefficients
empty_m <- matrix(ncol = length(df),
            nrow = length(df),
            dimnames = list(names(df), 
                            names(df)))
# Function that accepts matrix for coefficients and data and returns a correlation matrix
calculate_cramer <- function(m, df) {
 for (r in seq(nrow(m))){
   for (c in seq(ncol(m))){
     m[[r, c]] <- assocstats(table(df[[r]], df[[c]]))$cramer
   }
 }
    return(m)
}

cor_matrix <- calculate_cramer(empty_m ,data)

corrplot(cor_matrix)