<!--
![Downloads](http://cranlogs.r-pkg.org/badges/sophistication)
-->
[![CRAN
Version](http://www.r-pkg.org/badges/version/sophistication)](http://cran.r-project.org/package=sophistication)
[![Travis-CI Build
Status](https://travis-ci.org/kbenoit/sophistication.svg?branch=master)](https://travis-ci.org/kbenoit/sophistication)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/kbenoit/sophistication?branch=master&svg=true)](https://ci.appveyor.com/project/kbenoit/sophistication)
[![Coverage
Status](https://img.shields.io/codecov/c/github/kbenoit/sophistication/master.svg)](https://codecov.io/github/kbenoit/sophistication?branch=master)

Code for use in measuring the sophistication of political text
--------------------------------------------------------------

“Measuring and Explaining Political Sophistication Through Textual
Complexity” by Kenneth Benoit, Kevin Munger, and Arthur Spirling. This
package is built on [**quanteda**](http://quanteda.io).

### How to install

Using the **devtools** package:

``` r
devtools::install_github("kbenoit/sophistication")
```

### Included Data

| new name                     | original name      | description                           |
|------------------------------|--------------------|---------------------------------------|
| `data_corpus_fifthgrade`     | `fifthCorpus`      | Fifth-grade reading texts             |
| `data_corpus_crimson`        | `crimsonCorpus`    | Editorials from the Harvard *Crimson* |
| `data_corpus_partybroadcast` | `partybcastCorpus` | UK political party broadcasts         |
| `data_corpus_presdebates`    | `presDebateCorpus` | US presidential debates 2016          |
| `data_corpus_SOTU`           | `SOTUCorpus`       | US State of the Union corpus          |

### How to use

``` r
library("sophistication")
## Loading required package: quanteda
## quanteda version 1.0.0
## Using 7 of 8 threads for parallel computing
## 
## Attaching package: 'quanteda'
## The following object is masked from 'package:utils':
## 
##     View
## sophistication version 0.58
## Dropbox folder location set to: ~/Dropbox/Papers/Benoit_Spirling_Readability/

# make the snipepts of one sentence, between 100-350 chars in length
snippetData <- snippets_make(data_corpus_SOTU, nsentence = 1, minchar = 150, maxchar = 250)
# clean up the snippets
snippetData <- snippets_clean(snippetData)
## Cleaning 17,051 snippets...
##    removed 990 snippets containing numbers of at least 1,000
##    removed 133 snippets containing ALL CAPS titles
##    ...finished.

# randomly sample three snippets
set.seed(10)
testData <- snippetData[sample(1:nrow(snippetData), 5), ]

# generate pairs for a minimum spanning tree
(snippetPairsMST <- pairs_regular_make(testData))
##                                                                                                                                                                                                                         text1
## 1                               This purpose can probably best be accomplished by an international agreement to regard all private property at sea as exempt from capture or destruction by the forces of belligerent powers.
## 2                                        The sincere desire for peace by which I am animated led me to approve the proposal, already made, to submit the question which had thus arisen between the countries to arbitration.
## 3                           This course has borne rich fruit in the past, and it is our duty as a nation to preserve the heritage of good repute which a century of right dealing with foreign governments has secured to us.
## 4 The attitude which our Government took and maintained toward an adjustment of European reparations, by pointing out that it wits not a political but a business problem, has demonstrated its wisdom by its actual results.
##          docID1 snippetID1 completeSet1
## 1 McKinley-1898   11000463         TRUE
## 2  Johnson-1865    7700232         TRUE
## 3 Harrison-1889   10100007         TRUE
## 4 Coolidge-1924   13600217         TRUE
##                                                                                                                                                                                                                                      text2
## 1                                                     The sincere desire for peace by which I am animated led me to approve the proposal, already made, to submit the question which had thus arisen between the countries to arbitration.
## 2                                        This course has borne rich fruit in the past, and it is our duty as a nation to preserve the heritage of good repute which a century of right dealing with foreign governments has secured to us.
## 3              The attitude which our Government took and maintained toward an adjustment of European reparations, by pointing out that it wits not a political but a business problem, has demonstrated its wisdom by its actual results.
## 4 Renewed authority has been asked for by the representative of Austria, and in the mean time the rapidly increasing trade and navigation between the two countries have been placed upon the most liberal footing of our navigation acts.
##          docID2 snippetID2 completeSet2
## 1  Johnson-1865    7700232         TRUE
## 2 Harrison-1889   10100007         TRUE
## 3 Coolidge-1924   13600217         TRUE
## 4  Jackson-1830    4200087         TRUE
```

We can also use the package function to generate “gold” questions based
on readability differences:

``` r
# make a lot of candidate pairs
snippetPairsAll <- pairs_regular_make(snippetData[sample(1:nrow(snippetData), 1000), ])
# make 10 gold from these
pairs_gold_make(snippetPairsAll, n.pairs = 10)
## Starting the creation of gold questions...
##    computing Flesch readability measure
##    selecting top different 10 pairs
##    applying min.diff.quantile thresholds of 2.91, 33.92
##    creating gold_reason text
##    ...finished.
##                                                                                                                                                                                                                                     text1
## 1                On this whole proposition, including the appropriation of money with the acquisition of territory, does not the expediency amount to absolute necessity-that without which the Government itself can not be perpetuated?
## 2         The second object of a proper immigration law ought to be to secure by a careful and not merely perfunctory educational test some intelligent capacity to appreciate American institutions and act sanely as American citizens.
## 3           Duties were levied on certain commodities which are included in the reciprocity treaty now existing between the United States and the Kingdom of Hawaii, without indicating the necessary exception in favor of that Kingdom.
## 4  The envoy extraordinary and minister plenipotentiary of the United States to Mexico has been received and accredited, and a diplomatic representative from Mexico of similar rank has been received and accredited by this Government.
## 5                                                                        But again and again in the past our little Regular Army has rendered service literally vital to the country, and it may at any time have to do so in the future.
## 6                                                                               Right now, because of loopholes and shelters in the tax code, a quarter of all millionaires pay lower tax rates than millions of middle-class households.
## 7                   This propinquity, community of language and occupation, and similarity of political and social institutions indicate the practicability and obvious wisdom of maintaining mutually beneficial and friendly relations.
## 8                                                                          The present rulers of the Soviet Union have shown that they are willing to use this power to destroy the free nations and win domination over the whole world.
## 9                                                            While uncertain of the course of things, the time may be advantageously employed in obtaining the powers necessary for a system of improvement, should that be thought best.
## 10                                                           And if with less money, or money more easily paid, we can preserve the benefits of the Union by this means than we can by the war alone, is it not also economical to do it?
##            docID1 snippetID1 completeSet1
## 1    Lincoln-1861    7300140         TRUE
## 2  Roosevelt-1901   11300174         TRUE
## 3   Harrison-1890   10200059         TRUE
## 4       Polk-1848    6000025         TRUE
## 5  Roosevelt-1907   11900651         TRUE
## 6      Obama-2012   22600292         TRUE
## 7  Cleveland-1886    9800027         TRUE
## 8     Truman-1951   16200040         TRUE
## 9  Jefferson-1808    2000063         TRUE
## 10   Lincoln-1862    7400198         TRUE
##                                                                                                                                                                                                                                                text2
## 1                                                                                       And we are discussing with a number of our area friends further ways we can help to improve their security and ours, both for the short and the longer term.
## 2                                                         This liberal enlargement of the general law should suggest a more careful scrutiny of bills for special relief, both as to the cases where relief is granted and as to the amount allowed.
## 3                                                                                Working by these guide lines I believe with all my heart that America can be as sure of the strength and efficiency of her armed forces as she is of their loyalty.
## 4                                                                          I believe that the scheme would be of economic advantage, for the robe of the buffalo is of high market value, and the same is true of the robe of the crossbred animals.
## 5  We are supporting the Pan American efforts that are being made toward the codification of international law, and looking with sympathy oil the investigations conducted under philanthropic auspices of the proposal to agreements outlawing war.
## 6                               Americans also want a good material standard of living--not simply to accumulate possessions, but to fulfill a legitimate aspiration for an environment in which their families may live meaningful and happy lives.
## 7                                        The field designated is unquestionably one of interest and one capable of large development of commercial interests-advantageous to the peoples reached and to those who may establish relations with them.
## 8                                                                         I also renew my recommendation in favor of legislation affording just copyright protection to foreign authors on a footing of reciprocal advantage for our authors abroad.
## 9                                                      We restored the vital center, replacing outmoded ideologies with a new vision anchored in basic, enduring values: opportunity for all, responsibility from all, a community of all Americans.
## 10                                                               In a desire to eliminate all possibilities of injustice due to difficulties in establishing service connection of disabilities, these principles have been to some degree extended.
##             docID2 snippetID2 completeSet2      read1     read2  readdiff
## 1      Carter-1981   19400975         TRUE  -0.132500 55.125000 -55.25750
## 2    Harrison-1890   10200177         TRUE  -5.141429 45.158182 -50.29961
## 3  Eisenhower-1959   17100068         TRUE   8.101471 60.673750 -52.57228
## 4   Roosevelt-1905   11700632         TRUE  -6.827941 55.412727 -62.24067
## 5    Coolidge-1925   13700157         TRUE  47.455000 -2.724286  50.17929
## 6  Eisenhower-1955   16700222         TRUE  63.306538 11.830909  51.47563
## 7       Grant-1874    8600170         TRUE -36.770000 25.273226 -62.04323
## 8    Harrison-1890   10200312         TRUE  54.875862  7.991154  46.88471
## 9     Clinton-2000   21400014         TRUE  45.472143 -2.870714  48.34286
## 10     Hoover-1929   14100333         TRUE  52.795000  1.483462  51.31154
##    _golden easier_gold
## 1     TRUE           2
## 2     TRUE           2
## 3     TRUE           2
## 4     TRUE           2
## 5     TRUE           1
## 6     TRUE           1
## 7     TRUE           2
## 8     TRUE           1
## 9     TRUE           1
## 10    TRUE           1
##                                                                                                                                                                                                      easier_gold_reason
## 1  Text B is "easier" to read because it contains some combination of shorter sentences, more commonly used and more easily understood terms, and is generally less complicated and easier to read and grasp its point.
## 2  Text B is "easier" to read because it contains some combination of shorter sentences, more commonly used and more easily understood terms, and is generally less complicated and easier to read and grasp its point.
## 3  Text B is "easier" to read because it contains some combination of shorter sentences, more commonly used and more easily understood terms, and is generally less complicated and easier to read and grasp its point.
## 4  Text B is "easier" to read because it contains some combination of shorter sentences, more commonly used and more easily understood terms, and is generally less complicated and easier to read and grasp its point.
## 5  Text A is "easier" to read because it contains some combination of shorter sentences, more commonly used and more easily understood terms, and is generally less complicated and easier to read and grasp its point.
## 6  Text A is "easier" to read because it contains some combination of shorter sentences, more commonly used and more easily understood terms, and is generally less complicated and easier to read and grasp its point.
## 7  Text B is "easier" to read because it contains some combination of shorter sentences, more commonly used and more easily understood terms, and is generally less complicated and easier to read and grasp its point.
## 8  Text A is "easier" to read because it contains some combination of shorter sentences, more commonly used and more easily understood terms, and is generally less complicated and easier to read and grasp its point.
## 9  Text A is "easier" to read because it contains some combination of shorter sentences, more commonly used and more easily understood terms, and is generally less complicated and easier to read and grasp its point.
## 10 Text A is "easier" to read because it contains some combination of shorter sentences, more commonly used and more easily understood terms, and is generally less complicated and easier to read and grasp its point.
```

There is a lot more than this, of course. Our documentation will improve
as we develop the package with an aim to eventual CRAN release.
