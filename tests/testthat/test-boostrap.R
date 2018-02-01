context("test bootstrap_readability")

test_that("bootstrap_readability works as expected with two measures", {
    corp <- c("The cat in the hat ate hamburgers.",
              "One two six seven eleven.")
    set.seed(1)
    bsr1 <- bootstrap_readability(corp, n = 10, measure = c("Flesch", "Flesch.Kincaid"))
    class(bsr1$original) <- "data.frame"
    rownames(bsr1$original) <- NULL
    expect_equal(
        bsr1,
        list(original = data.frame(Flesch = c(90.96, 66.4), 
                                   Flesch.Kincaid = c(2.31, 5.24)),
             bs_mean = matrix(c(90.96, 66.4, 2.31, 5.24), ncol = 2,
                              dimnames = list(1:2, c("Flesch", "Flesch.Kincaid"))),
             bs_sd = matrix(c(0, 0, 0, 0), ncol = 2,
                            dimnames = list(1:2, c("Flesch", "Flesch.Kincaid"))) ),
        tol = .01
    )
})
    
test_that("bootstrap_readability works as expected with one measure", {
    set.seed(1)
    corp <- c("The cat in the hat ate hamburgers.",
              "One two six seven eleven.")
    bsr2 <- bootstrap_readability(corp, n = 10, measure = c("Flesch"))
    class(bsr2$original) <- "data.frame"
    rownames(bsr2$original) <- NULL
    expect_equal(
        bsr2,
        list(original = data.frame(Flesch = c(90.96, 66.4)),
             bs_mean = matrix(c(90.96, 66.4), ncol = 1,
                              dimnames = list(1:2, c("Flesch"))),
             bs_sd = matrix(c(0, 0), ncol = 1,
                            dimnames = list(1:2, c("Flesch"))) ),
        tol = .01
    )
})
