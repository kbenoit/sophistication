<!--
[![CRAN Version](http://www.r-pkg.org/badges/version/sophistication)](http://cran.r-project.org/package=sophistication)
![Downloads](http://cranlogs.r-pkg.org/badges/sophistication)
[![Travis-CI Build Status](https://travis-ci.org/kbenoit/sophistication.svg?branch=master)](https://travis-ci.org/kbenoit/sophistication)
[![codecov.io](https://codecov.io/github/kbenoit/sophistication/coverage.svg?branch=master)][1]
-->
Code for use in measuring the sophistication of political text
--------------------------------------------------------------

"Measuring and Explaining Political Sophistication Through Textual Complexity" by Kenneth Benoit, Kevin Munger, and Arthur Spirling. This package is built on [**quanteda**](https://github.com/kbenoit/quanteda), whose namespace it imports.

### How to install

Two methods:

1.  Install using **devtools** directly from GitHub, you will need to generate a personal access token (PAT) in <https://github.com/settings/tokens> and supply it as below: (you should, preferably, use your own!)

    ``` r
    devtools::install_github("kbenoit/sophistication", subdir = "R_package",
                             auth_token = "309171976db5586eab402a922604229cd5190c81")
    ```

2.  Pull the repo into your project, and choose "**Build & Reload**" from the **Build** tab of the Build pane in RStudio.

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
require(sophistication)
```

    ## Loading required package: sophistication

``` r
# make the snipepts of one sentence, between 100-350 chars in length
snippetData <- snippets_make(data_corpus_SOTU, nsentence = 1, minchar = 150, maxchar = 250)
# clean up the snippets
snippetData <- snippets_clean(snippetData)
```

    ## Cleaning 17,050 snippets...

    ##    removed 990 snippets containing numbers of at least 1,000

    ##    removed 133 snippets containing ALL CAPS titles

    ##    ...finished.

``` r
# randomly sample three snippets
set.seed(10)
testData <- snippetData[sample(1:nrow(snippetData), 5), ]

# generate pairs for a minimum spanning tree
(snippetPairsMST <- pairs_regular_make(testData))
```

    ##                                                                                                                                                                                                                                text1
    ## 1 It is true we may suffer in such cases less than other communities, but all nations are damaged more or less by the state of uneasiness and apprehension into which an outbreak of hostilities throws the entire commercial world.
    ## 2                                               The sincere desire for peace by which I am animated led me to approve the proposal, already made, to submit the question which had thus arisen between the countries to arbitration.
    ## 3                                  This course has borne rich fruit in the past, and it is our duty as a nation to preserve the heritage of good repute which a century of right dealing with foreign governments has secured to us.
    ## 4                                                              It will ever be recognized that their welfare is of the first concern and always entitled to the most solicitous consideration oil the part of their fellow citizens.
    ##          docID1 snippetID1
    ## 1 McKinley-1898   11000461
    ## 2  Johnson-1865    7700232
    ## 3 Harrison-1889   10100007
    ## 4 Coolidge-1924   13600210
    ##                                                                                                                                                                                                                                      text2
    ## 1                                                     The sincere desire for peace by which I am animated led me to approve the proposal, already made, to submit the question which had thus arisen between the countries to arbitration.
    ## 2                                        This course has borne rich fruit in the past, and it is our duty as a nation to preserve the heritage of good repute which a century of right dealing with foreign governments has secured to us.
    ## 3                                                                    It will ever be recognized that their welfare is of the first concern and always entitled to the most solicitous consideration oil the part of their fellow citizens.
    ## 4 Renewed authority has been asked for by the representative of Austria, and in the mean time the rapidly increasing trade and navigation between the two countries have been placed upon the most liberal footing of our navigation acts.
    ##          docID2 snippetID2
    ## 1  Johnson-1865    7700232
    ## 2 Harrison-1889   10100007
    ## 3 Coolidge-1924   13600210
    ## 4  Jackson-1830    4200087

We can also use the package function to generate "gold" questions based on readability differences:

``` r
# make a lot of candidate pairs
snippetPairsAll <- pairs_regular_make(snippetData[sample(1:nrow(snippetData), 1000), ])
# make 10 gold from these
pairs_gold_make(snippetPairsAll, n.pairs = 10)
```

    ## Starting the creation of gold questions...

    ##    computing Flesch readability measure

    ##    selecting top different 10 pairs

    ##    applying min.diff.quantile thresholds of 2.23, 33.29

    ##    creating gold_reason text

    ##    ...finished.

    ##                                                                                                                                                                                                                                     text1
    ## 1                              And considerations equally cogent require a more convenient organization of the subordinate tribunals, which may be accomplished without an objectionable increase of the number or expense of the judges.
    ## 2                                                                          The work is so far advanced now that the health of all those employed in canal work is as well guarded as it is on similar work in this country and elsewhere.
    ## 3                                                                                The killing fields of Iraq, where hundreds of thousands of men and women and children vanished into the sands, would still be known only to the killers.
    ## 4  The envoy extraordinary and minister plenipotentiary of the United States to Mexico has been received and accredited, and a diplomatic representative from Mexico of similar rank has been received and accredited by this Government.
    ## 5         The second object of a proper immigration law ought to be to secure by a careful and not merely perfunctory educational test some intelligent capacity to appreciate American institutions and act sanely as American citizens.
    ## 6              There are many administrative subjects, such as departmental reorganization, extension of the civil service, readjustment of the postal rates, etc., which at some appropriate time require the attention of the Congress.
    ## 7                                                                              Among the payments those made in discharge of the principal and interest of the national debt will shew that the public faith has been exactly maintained.
    ## 8                        With a supply of finished guns of large caliber already on hand, to which additions should now rapidly follow, the wisdom of providing carriages and emplacements for their mount can not be too strongly urged.
    ## 9                                                      These facts were known in Paris prior to the 28th of March, 1835, when the committee to whom the bill of indemnification had been referred reported it to the Chamber of Deputies.
    ## 10                The extensive complications involved by the requirement of dealing with three other governments engaged in occupation and with the governments of liberated countries require intensive work and energetic cooperation.
    ##            docID1 snippetID1
    ## 1    Madison-1816    2800049
    ## 2  Roosevelt-1905   11700771
    ## 3       Bush-2004   21800099
    ## 4       Polk-1848    6000025
    ## 5  Roosevelt-1901   11300174
    ## 6     Hoover-1930   14200149
    ## 7  Jefferson-1801    1300045
    ## 8  Cleveland-1894   10600170
    ## 9    Jackson-1835    4700112
    ## 10    Truman-1946   15700258
    ##                                                                                                                                                                                                                                                        text2
    ## 1                                                                                   The cavalry is much more difficult to form than infantry, and it should be kept up to the maximum both in efficiency and in strength, for it can not be made in a hurry.
    ## 2  Such information points out the way to a prudent foresight in the selection and cultivation of crops and to a release from the bondage of unreasoning monotony of production, a glutted and depressed market, and constantly recurring unprofitable toil.
    ## 3                                                The customs receipts have decreased owing to disturbed political and economic conditions and to a very natural curtailment of imports in view of the anticipated revision of the Dominican tariff schedule.
    ## 4                                                                                  I believe that the scheme would be of economic advantage, for the robe of the buffalo is of high market value, and the same is true of the robe of the crossbred animals.
    ## 5                                                                 This liberal enlargement of the general law should suggest a more careful scrutiny of bills for special relief, both as to the cases where relief is granted and as to the amount allowed.
    ## 6                                                         We must face accomplished facts, and the adjustment of factory conditions must be made, but surely it can be made with less friction and less harmful effects on family life than is now the case.
    ## 7                                                        As the Shuttle development phases down, however, there will be added flexibility to consider new space applications, space science and new space exploration activities.  - Technology Development.
    ## 8                     The envoy extraordinary and minister plenipotentiary of the United States to Mexico has been received and accredited, and a diplomatic representative from Mexico of similar rank has been received and accredited by this Government.
    ## 9             Reluctant to credit the reports in general circulation as to the quantity of gold, the officer commanding our forces in California visited the mineral district in July last for the purpose of obtaining accurate information on the subject.
    ## 10                                                                                        The rules adopted to improve the civil service of the Government have been adhered to as closely as has been practicable with the opposition with which they meet.
    ##            docID2 snippetID2      read1     read2  readdiff _golden
    ## 1  Roosevelt-1908   12000583  2.3655172 57.866176 -55.50066    TRUE
    ## 2  Cleveland-1896   10800331 68.6050000 17.573077  51.03192    TRUE
    ## 3       Taft-1909   12100090 63.4966667  8.899032  54.59763    TRUE
    ## 4  Roosevelt-1905   11700632 -6.8279412 55.412727 -62.24067    TRUE
    ## 5   Harrison-1890   10200177 -5.1414286 45.158182 -50.29961    TRUE
    ## 6  Roosevelt-1905   11700164  0.7119355 52.870000 -52.15806    TRUE
    ## 7     Carter-1981   19400523 56.7988462 -1.770385  58.56923    TRUE
    ## 8       Polk-1848    6000025 40.7842857 -6.827941  47.61223    TRUE
    ## 9       Polk-1848    6000110 55.3862500  5.743947  49.64230    TRUE
    ## 10     Grant-1874    8600247 -3.4689655 48.493571 -51.96254    TRUE
    ##    easier_gold
    ## 1            2
    ## 2            1
    ## 3            1
    ## 4            2
    ## 5            2
    ## 6            2
    ## 7            1
    ## 8            1
    ## 9            1
    ## 10           2
    ##                                                                                                                                                                                                      easier_gold_reason
    ## 1  Text B is "easier" to read because it contains some combination of shorter sentences, more commonly used and more easily understood terms, and is generally less complicated and easier to read and grasp its point.
    ## 2  Text A is "easier" to read because it contains some combination of shorter sentences, more commonly used and more easily understood terms, and is generally less complicated and easier to read and grasp its point.
    ## 3  Text A is "easier" to read because it contains some combination of shorter sentences, more commonly used and more easily understood terms, and is generally less complicated and easier to read and grasp its point.
    ## 4  Text B is "easier" to read because it contains some combination of shorter sentences, more commonly used and more easily understood terms, and is generally less complicated and easier to read and grasp its point.
    ## 5  Text B is "easier" to read because it contains some combination of shorter sentences, more commonly used and more easily understood terms, and is generally less complicated and easier to read and grasp its point.
    ## 6  Text B is "easier" to read because it contains some combination of shorter sentences, more commonly used and more easily understood terms, and is generally less complicated and easier to read and grasp its point.
    ## 7  Text A is "easier" to read because it contains some combination of shorter sentences, more commonly used and more easily understood terms, and is generally less complicated and easier to read and grasp its point.
    ## 8  Text A is "easier" to read because it contains some combination of shorter sentences, more commonly used and more easily understood terms, and is generally less complicated and easier to read and grasp its point.
    ## 9  Text A is "easier" to read because it contains some combination of shorter sentences, more commonly used and more easily understood terms, and is generally less complicated and easier to read and grasp its point.
    ## 10 Text B is "easier" to read because it contains some combination of shorter sentences, more commonly used and more easily understood terms, and is generally less complicated and easier to read and grasp its point.
