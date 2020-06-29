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

If you have a trouble with your **sophistication** installation using **devtools**, check that you have pre-installed **conda** or **mini-conda** and are using the correct version of **spacyr**. Try installing **sophistication** with the following steps:

``` r
devtools::install_github("quanteda/spacyr", build_vignettes = FALSE)
library("spacyr")
spacy_install()
spacy_initialize()
devtools::install_github("kbenoit/sophistication")
```

For more information please see the **spacyr** documentation here: https://cran.r-project.org/web/packages/spacyr/readme/README.html .

### Included Data

| new name                     | original name      | description                           |
|------------------------------|--------------------|---------------------------------------|
| `data_corpus_fifthgrade`     | `fifthCorpus`      | Fifth-grade reading texts             |
| `data_corpus_crimson`        | `crimsonCorpus`    | Editorials from the Harvard *Crimson* |
| `data_corpus_partybroadcast` | `partybcastCorpus` | UK political party broadcasts         |
| `data_corpus_presdebates`    | `presDebateCorpus` | US presidential debates 2016          |

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
data(data_corpus_sotu, package = "quanteda.corpora")
snippetData <- snippets_make(data_corpus_sotu, nsentence = 1, minchar = 150, maxchar = 250)
# clean up the snippets
snippetData <- snippets_clean(snippetData)
## Cleaning 17,138 snippets...
##    removed 992 snippets containing numbers of at least 1,000
##    removed 133 snippets containing ALL CAPS titles
##    ...finished.

# randomly sample three snippets
set.seed(10)
testData <- snippetData[sample(1:nrow(snippetData), 5), ]

# generate pairs for a minimum spanning tree
(snippetPairsMST <- pairs_regular_make(testData))
##                                                                                                                                                                                                                         text1
## 1                                                     In its earlier history the National Banking Act seemed to prove a reasonable avenue through which needful additions to the circulation could from time to time be made.
## 2                   It would have accorded with the great principle enunciated in the Declaration of American Independence that no people ought to bear the burden of taxation and yet be denied the right of representation.
## 3 They have a direct relation to home production, to work, to wages, and to the commercial independence of our country, and the wise and patriotic legislator should enlarge the field of his vision to include all of these.
## 4                                We shall do far better service to ourselves and to others if we admit this and discharge our duties voluntarily, than if we deny it and are forced to meet the same obligations unwillingly.
##          docID1 snippetID1 completeSet1
## 1 McKinley-1899   11100030         TRUE
## 2  Johnson-1866    7800024         TRUE
## 3 Harrison-1889   10100116         TRUE
## 4 Coolidge-1925   13700200         TRUE
##                                                                                                                                                                                                                         text2
## 1                   It would have accorded with the great principle enunciated in the Declaration of American Independence that no people ought to bear the burden of taxation and yet be denied the right of representation.
## 2 They have a direct relation to home production, to work, to wages, and to the commercial independence of our country, and the wise and patriotic legislator should enlarge the field of his vision to include all of these.
## 3                                We shall do far better service to ourselves and to others if we admit this and discharge our duties voluntarily, than if we deny it and are forced to meet the same obligations unwillingly.
## 4                                                It is indisputable that whatever gives facility and security to navigation cheapens imports and all who consume them are alike interested in what ever produces this effect.
##          docID2 snippetID2 completeSet2
## 1  Johnson-1866    7800024         TRUE
## 2 Harrison-1889   10100116         TRUE
## 3 Coolidge-1925   13700200         TRUE
## 4  Jackson-1830    4200105         TRUE
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
##    applying min.diff.quantile thresholds of 2.7, 35.14
##    creating gold_reason text
##    ...finished.
##                                                                                                                                                                                                                                                    text1
## 1                               The Italian Government has raised a question as to the propriety of recognizing in his dual capacity the representative of this country recently accredited both as secretary of legation and as consul-general at Rome.
## 2                                                                                               I would emphasize that that is why an important link between Russia and the United States is in our common interest, in arms control and in disarmament.
## 3                                                                                               Generally the support which this reform receives is from those who give it their support only to find fault when the rules are apparently departed from.
## 4                                                            The convention for the resurvey of the boundary from the Rio Grande to the Pacific having been ratified and exchanged, the preliminary reconnoissance therein stipulated has been effected.
## 5                                                                            The Secretary of State visited Mexico City in November, and, along with the Mexican Secretary of Foreign Relations, reviewed the performance of the Consultative Mechanism.
## 6                                                                    To the Congress of the United States:  I appear before the Congress today to report on the State of the Union and the relationships of the Union to the other nations of the world.
## 7                                               Every step in this direction is welcomed with public approval, and every interruption of steady and uniform progress to the desired consummation awakens general uneasiness and widespread condemnation.
## 8                                                          Neither the President nor the Congress nor the conscience of this Nation can permit money which comes from all the people to be used in a way which discriminates against some of the people.
## 9                                          The new legislation and the coordinator's office will bring common sense and consolidation to our Nation's previously fragmented, inconsistent, and in many ways, outdated, refugee and immigration policies.
## 10 You will be called upon to consider the expediency of making special provision by law for the temporary admission of some Chinese artisans and laborers in connection with the exhibit of Chinese industries at the approaching Columbian Exposition.
##             docID1 snippetID1 completeSet1
## 1      Arthur-1882    9400026         TRUE
## 2     Johnson-1967   18000253         TRUE
## 3       Grant-1874    8600250         TRUE
## 4      Arthur-1883    9500051         TRUE
## 5      Carter-1981   19401072         TRUE
## 6  Eisenhower-1957   16900001         TRUE
## 7       Hayes-1878    9000020         TRUE
## 8       Nixon-1971   18400096         TRUE
## 9      Carter-1981   19400470         TRUE
## 10   Harrison-1891   10300088         TRUE
##                                                                                                                                                                                                                                                         text2
## 1                                                                                     We have fought four wars in this century, but our power has never been used to break the peace, only to keep it; never been used to destroy freedom, only to defend it.
## 2                                                                                                   Severe commercial revulsions abroad have always heretofore operated to depress and often to affect disastrously almost every branch of American industry.
## 3                                       The work of the commission of inquiry and conciliation between Bolivia and Paraguay, in which a representative of this Government participated, has successfully terminated an incident which seemed to threaten war.
## 4                                                                                     It can help us build a nation and a world where all people are free to seek the truth and to add to human understanding, so that all of us may live our lives in peace.
## 5                                                               Neither the President nor the Congress nor the conscience of this Nation can permit money which comes from all the people to be used in a way which discriminates against some of the people.
## 6               Consumers and businesses need reliable supplies of energy to make our economy run, so I urge you to pass legislation to modernize our electricity system, promote conservation, and make America less dependent on foreign sources of energy.
## 7                                                                         To the Congress of the United States:  I appear before the Congress today to report on the State of the Union and the relationships of the Union to the other nations of the world.
## 8  The questions between Great Britain and the United States relating to the rights of American fishermen, under treaty and international comity, in the territorial waters of Canada and Newfoundland, I regret to say, are not yet satisfactorily adjusted.
## 9                                                                       It could not but happen, however, that a return to this state of things from that which had followed an execution of the arrangement by the United States would involve difficulties.
## 10                                                                  At that time I informed the Congress of the approaching completion of nine 12-inch, twenty 10-inch, and thirty-four 8-inch high-power steel guns and seventy-five 12-inch rifled mortars.
##             docID2 snippetID2 completeSet2     read1      read2  readdiff
## 1       Nixon-1972   18500060         TRUE  6.944286 60.5400000 -53.59571
## 2        Polk-1848    6000273         TRUE 47.830000 -3.8228571  51.65286
## 3      Hoover-1929   14100027         TRUE 53.545000  0.7119355  52.83306
## 4      Carter-1979   19200160         TRUE 12.236429 69.2450000 -57.00857
## 5       Nixon-1971   18400096         TRUE  2.108000 52.8897059 -50.78171
## 6        Bush-2004   21800170         TRUE 50.401471 -2.2064865  52.60796
## 7  Eisenhower-1957   16900001         TRUE -5.892143 50.4014706 -56.29361
## 8   Cleveland-1888   10000086         TRUE 52.889706  3.5176316  49.37207
## 9     Madison-1809    2100006         TRUE -2.870714 47.1054839 -49.97620
## 10  Cleveland-1896   10800166         TRUE  3.517632 55.3779412 -51.86031
##    _golden easier_gold
## 1     TRUE           2
## 2     TRUE           1
## 3     TRUE           1
## 4     TRUE           2
## 5     TRUE           2
## 6     TRUE           1
## 7     TRUE           2
## 8     TRUE           1
## 9     TRUE           2
## 10    TRUE           2
##                                                                                                                                                                                                      easier_gold_reason
## 1  Text B is "easier" to read because it contains some combination of shorter sentences, more commonly used and more easily understood terms, and is generally less complicated and easier to read and grasp its point.
## 2  Text A is "easier" to read because it contains some combination of shorter sentences, more commonly used and more easily understood terms, and is generally less complicated and easier to read and grasp its point.
## 3  Text A is "easier" to read because it contains some combination of shorter sentences, more commonly used and more easily understood terms, and is generally less complicated and easier to read and grasp its point.
## 4  Text B is "easier" to read because it contains some combination of shorter sentences, more commonly used and more easily understood terms, and is generally less complicated and easier to read and grasp its point.
## 5  Text B is "easier" to read because it contains some combination of shorter sentences, more commonly used and more easily understood terms, and is generally less complicated and easier to read and grasp its point.
## 6  Text A is "easier" to read because it contains some combination of shorter sentences, more commonly used and more easily understood terms, and is generally less complicated and easier to read and grasp its point.
## 7  Text B is "easier" to read because it contains some combination of shorter sentences, more commonly used and more easily understood terms, and is generally less complicated and easier to read and grasp its point.
## 8  Text A is "easier" to read because it contains some combination of shorter sentences, more commonly used and more easily understood terms, and is generally less complicated and easier to read and grasp its point.
## 9  Text B is "easier" to read because it contains some combination of shorter sentences, more commonly used and more easily understood terms, and is generally less complicated and easier to read and grasp its point.
## 10 Text B is "easier" to read because it contains some combination of shorter sentences, more commonly used and more easily understood terms, and is generally less complicated and easier to read and grasp its point.
```

There is a lot more than this, of course. Our documentation will improve
as we develop the package with an aim to eventual CRAN release.
