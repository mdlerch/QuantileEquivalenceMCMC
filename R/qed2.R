qed2 <- function(X, quant, epsilon = 0.01, alpha = 0.05)
{
    # get prob and pass to qed()
    prob <- sum(X < quant) / length(X)

    qed(X, prob, epsilon, alpha)
}
