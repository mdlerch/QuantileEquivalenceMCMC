eq_test_one_param <- function(z, epsilon, alpha = 0.05)
{
    # rejection region boundary
    C_alpha <- sqrt(qchisq(alpha, 1, ncp = epsilon^2))

    # return result of test
    return(abs(z) < C_alpha)
}
