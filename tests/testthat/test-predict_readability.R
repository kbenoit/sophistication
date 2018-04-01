context("test prediction")

test_that("vectorized prediction works", {
    skip_on_travis()
    skip_on_appveyor()
    txt <- c("the quick brown fox jumped",
             "the quick brown fox jumped",
             "ok computer hedgehog",
             "ok computer hedgehog")
    pred <- predict_readability(data_BTm_bms, newdata = txt, 
                                baseline_year = c(2000, 1500, 1800, 1900))
    expect_false( pred[1, "prob"] == pred[2, "prob"] )
    expect_false( pred[3, "prob"] == pred[4, "prob"] )
})

test_that("scalar prediction works", {
    skip_on_travis()
    skip_on_appveyor()
    txt <- c("the quick brown fox jumped",
             "the quick brown fox jumped",
             "ok computer hedgehog",
             "ok computer hedgehog")
    pred <- predict_readability(data_BTm_bms,
                                newdata = txt, 
                                baseline_year = 1900)
    expect_equal( pred[1, "prob"], pred[2, "prob"] )
    expect_equal( pred[3, "prob"], pred[4, "prob"] )
    
    pred2000 <- predict_readability(BT_best, newdata = txt, 
                                    baseline_year = 2000)
    expect_false( pred[1, "prob"] == pred2000[1, "prob"] )
})
