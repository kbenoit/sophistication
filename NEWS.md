# **sophistication** changes and history

## Added 0.6.x

* `covars_make_baselines()` and `predict_readability()` now have a vectorized `baseline_years` arguments, for fitting Google frequencies to specific decades.
* Added the `data_BTm_bms` data object containing the fitted values from Benoit, Munger and Spirling (2018).
* Fixed bug causing `covars_make_pos()` to include spaces and punctuation characters in the count of tokens, affecting the proportions (e.g. `pr_noun`). (#9)

## Added 0.5.x

* Added `predict_readability()`, to fit readability scores on any text or corpus, given a fitted `BTm` model.
* Added (*fast*) bootstrapping at the sentence level, for `predict_readability()`.
* Added status messages to `predict_readability()` through `verbose` option.
* Removed some UTF-8 characters from `data_corpus_SOTU` that were causing `spacy_parse()` to crash on Windows.

## Added 0.3.x

*  corpus methods implemented for `make_covars_*`  
*  `make_covars_baseline()` now allows a reference year, and a reference word, to be set in the calling function.  
*  Computation of mean baseline usage for tokens in a text from Google 1-gram corpus, along with Google 1-gram data object.  
*  `normalize = TRUE` argument added to `covars_make()` and `covars_make_pos()`  
*  Added support for computing part-of-speech quantities, using the **spacyr** package.  
*  Major changes to the API, see the description in the root of the GitHub repository.  
*  Package now moved to a subfolder of the GitHub repo, `R_package/`.

## Added 0.1.5:

*  Added `browsePairs()` to examine pairs (that are not gold).
*  Added an exteme value limit (`readability.limits`) to cleanSnippets.  
*  Added a random sampling argument `n.pairs` to `pairSnippets()`.  

## Added 0.1.3:

*  Added `browseGold()` function to make it easier to examine gold snippets.

## Added 0.1.3:

*  Added `makeCFdata()` to produce an output dataset for CF upload.  
*  For `makeGold(x, screeners = TRUE)` the sampling is now random rather than from the most different pairs.  This prevents having the same pairs as both regular gold and screener gold.  
*  Added `seed` argument to `makeGold()`.  
*  Added `addSnippetVariables()` function to add additional variables about each text snippet.  Includes all readability, summary functions for now.  Soon: To add parts of speech summaries, and LIWC categories.  
*  Added `presDebateCorpus` data object.


## Added 0.1.2:

*  Enhancements to `makeGold()`, including new arguments `screener` and `min.diff.quantile` and `min.diff.absolute`.
*  Improvements to status messages.

