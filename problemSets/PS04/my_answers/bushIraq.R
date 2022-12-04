load("../../../datasets/anes.Rdata " )
# subset data to relevant var iables

anes <- anes[complete.cases(anes$caseid), ]

reg_DF <- anes[, c("white" , "female" , "age" , "partyid" , "bushiraq" ) ]

inputDF <- reg_DF
covariates <- c("white", "female", "age", "partyid")
outcome <- "bushiraq"
#reg_results <- lm_by_hand(reg_DF, c("white", "female", "age", "partyid"), "bushiraq" )

# estimate regression by hand
#lm_by_hand <- function(inputDF , covariates , outcome ) {
# create matrices
X <- as.matrix(cbind(rep(1 , dim(inputDF )[ 1 ] ) , inputDF[ ,covariates ] ) )
Y <- inputDF[ , outcome ]
# cal culate betas
betas <- solve((t(X )%*%X ) ) %*%(t(X )%*%Y )
rownames(betas )[ 1 ] <- "Intercept"
n <- dim(inputDF )[ 1 ]
k <- ncol(X )
# cal culate SEs for betas
# estimate of sigma =squared
sigma_squared <- sum((Y - X%*%betas )^2) /(nrow(X )-ncol(X ) )
# create variance = covariance matrix for betas
var_covar_mat <- sigma_squared* solve(t(X )%*%X )
# standard er ror s for c o e f f i c i e n t estimates
SEs <- sqrt(diag(var_covar_mat) )
  
# get t = s t at and p= values
TS <-(betas -0) /SEs
p_values <- 2 * pt(abs(TS) , n-k , lower.tail= F )
TS
# check to see i f we did i t cor r e c t l y
auto_results <- lm(bushiraq ~ ., data= reg_DF )
n-k
k
Rsq <- 0.51541887478640
F.test <- (Rsq/( k - 1 ) ) / ( ( 1 -  Rsq ) / ( n - k ) )
df1 <- k - 1
df2 <- 378

F.pvalue <-df(F.test, df1, df2)
summary(auto_results)
