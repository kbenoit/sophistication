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
