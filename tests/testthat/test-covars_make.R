context("test covariate creation")

test_that("covars_make works as expected", {
    skip_if_not_installed("quanteda")
    require(quanteda)
    #                                          6                           4   words
    #        1    2 3   4  5   6   7   8  9   10     1  2   3     4  5  6  7   syllables
    #                     3 + 5 + 6 + 3 + 3 + 11  +            3 + 6 + 5 + 8   characters
    txt <- "The silly ginger cat ate gingerbread.  The gopher drank lemonade."
    result <- covars_make(txt)

    expect_equal(result$meanSentenceLength, (6 + 4)/2)
    expect_equal(result$meanWordSyllables, 17 / (6 + 4))
    expect_equal(result$meanWordChars, sum(nchar(as.character(tokens(txt, remove_punct = TRUE)))) / (6 + 4))
    expect_equal(result$meanSentenceChars, sum(nchar(as.character(tokens(txt, remove_punct = TRUE)))) / 2)
    expect_equal(result$meanSentenceSyllables, (10 + 7) / 2)
    expect_equal(result$W3Sy, 2 / 10)
    expect_equal(result$W2Sy, 5 / 10)
    expect_equal(result$W2Sy, 5 / 10)
    expect_equal(result$W6C, 4 / 10)
    expect_equal(result$Wlt3Sy, 8 / 10)

    # note: Dale-Chall is the proportion of words NOT in the Dale-Chall list
    expect_equal(
        result$W_wl.Dale.Chall,
        length(which(! char_tolower(as.character(tokens(txt, remove_punct = TRUE))) %in% quanteda::data_char_wordlists$dalechall)) / 10
    )
})

txt <- c(test1 = "One two cat.  One two cat.  Always eat apples.")

frompredict <- as.data.frame(sophistication:::get_covars_from_newdata.character(txt))

test_that("meanSentenceChars computed the same in predict v component function", {
    # should be: (9 + 9 + 15) / 3 = 11
    expect_equal(covars_make(txt)[, c("meanSentenceChars")], 11)
    expect_equal(frompredict[, "meanSentenceChars"], 11)
})

test_that("google_min_2000 computed the same in predict v component function", {
    expect_equal(
        covars_make_baselines(txt)[, c("google_min_2000")],
        frompredict[, "google_min"]
    )
})

test_that("meanWordChars computed the same in predict v component function", {
    # should be: ((3 + 3 + 3) * 2 + 6 + 3 + 6) / 9 = 3.6667
    expect_equal(covars_make(txt)[, c("meanWordChars")], 3.6667, tol = .0001)
    expect_equal(frompredict[, "meanWordChars"], 3.6667, tol = .0001)
})

test_that("pr_noun computed the same in predict v component function", {
    # should be: (1 + 1 + 1) / 9 = 0.33333
    # doc_id sentence_id token_id  token  lemma   pos     entity
    # 1   test1           1        1    One    one   NUM CARDINAL_B
    # 2   test1           1        2    two    two   NUM CARDINAL_B
    # 3   test1           1        3    cat    cat  NOUN           
    # 4   test1           1        4      .      . PUNCT           
    # 5   test1           1        5               SPACE           
    # 6   test1           2        1    One    one   NUM CARDINAL_B
    # 7   test1           2        2    two    two   NUM CARDINAL_B
    # 8   test1           2        3    cat    cat  NOUN           
    # 9   test1           2        4      .      . PUNCT           
    # 10  test1           2        5               SPACE           
    # 11  test1           3        1 Always always   ADV           
    # 12  test1           3        2    eat    eat  VERB           
    # 13  test1           3        3 apples  apple  NOUN           
    # 14  test1           3        4      .      . PUNCT  
    
    expect_equal(covars_make_pos(txt)[, c("pr_noun")], 0.333, tol = .001)
    expect_equal(frompredict[, "pr_noun"], 0.333, tol = .001)    # 0.214
})

test_that("paper example texts are correctly computed", {
    txt_clinton <- "If we do these things---end social promotion; turn around failing schools; build modern ones; support qualified teachers; promote innovation, competition and discipline - then we will begin to meet our generation's historic responsibility to create 21st century schools.  Now, we also have to do more to support the millions of parents who give their all every day at home and at work."
    txt_bush <- "And the victory of freedom in Iraq will strengthen a new ally in the war on  terror, inspire democratic reformers from Damascus to Tehran, bring more hope  and progress to a troubled region, and thereby lift a terrible threat from the  lives of our children and grandchildren.  We will succeed because the Iraqi  people value their own liberty---as they showed the world last Sunday."
    corp_example <- corpus(c(Clinton_1999 = txt_clinton, Bush_2005 = txt_bush))
    
    frompredict <- as.data.frame(sophistication:::get_covars_from_newdata.corpus(corp_example))
    row.names(frompredict) <- docnames(corp_example)

    expect_identical(
        covars_make(corp_example)["meanSentenceChars"],
        frompredict["meanSentenceChars"]
    )
    
    expect_equivalent(
        covars_make_baselines(corp_example)["google_min_2000"],
        frompredict["google_min"]
    )
    
    expect_equal(
        covars_make(corp_example)["meanWordChars"],
        frompredict["meanWordChars"],
        tol = .016
    )

    expect_equivalent(
        covars_make_pos(corp_example)[, "pr_noun", drop = FALSE],
        frompredict["pr_noun"]
    )
    
})



