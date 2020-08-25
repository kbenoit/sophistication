<!--
![Downloads](http://cranlogs.r-pkg.org/badges/sophistication)
-->
<!-- badges: start -->

[![CRAN
Version](http://www.r-pkg.org/badges/version/sophistication)](http://cran.r-project.org/package=sophistication)
[![R build
status](https://github.com/kbenoit/sophistication/workflows/R-CMD-check/badge.svg)](https://github.com/kbenoit/sophistication/actions)
[![Coverage
Status](https://img.shields.io/codecov/c/github/kbenoit/sophistication/master.svg)](https://codecov.io/github/kbenoit/sophistication?branch=master)
<!-- badges: end -->

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

### How to use

``` r
library("sophistication")
## Loading required package: quanteda
## Package version: 2.1.9000
## Parallel computing: 2 of 12 threads used.
## See https://quanteda.io for tutorials and examples.
## 
## Attaching package: 'quanteda'
## The following object is masked from 'package:utils':
## 
##     View
## spacy python option is already set, spacyr will use:
##  condaenv = "spacy_condaenv"
## successfully initialized (spaCy Version: 2.3.2, language model: en_core_web_sm)
## (python options: type = "condaenv", value = "spacy_condaenv")

# make the snipepts of one sentence, between 100-350 chars in length
data(data_corpus_sotu, package = "quanteda.corpora")
snippetData <- snippets_make(data_corpus_sotu, nsentence = 1, minchar = 150, maxchar = 250)
# clean up the snippets
snippetData <- snippets_clean(snippetData)
## Cleaning 20,662 snippets...
##    removed 1,166 snippets containing numbers of at least 1,000
##    removed 273 snippets containing ALL CAPS titles
##    ...finished.

# randomly sample three snippets
set.seed(10)
testData <- snippetData[sample(1:nrow(snippetData), 5), ]

# generate pairs for a minimum spanning tree
(snippetPairsMST <- pairs_regular_make(testData))
##           docID1 snippetID1
## 1   Madison-1813    2500042
## 2 Roosevelt-1938   14900134
## 3     Grant-1872    8400141
## 4   Johnson-1966   18100222
##                                                                                                                                                                                                                                                    text1
## 1                                                                         The minister plenipotentiary of the United States at Paris had not been enabled by proper opportunities to press the objects of his mission as prescribed by his instructions.
## 2 We have but to talk with hundreds of small bankers throughout the United States to realize that irrespective of local conditions, they are compelled in practice to accept the policies laid down by a small number of the larger banks in the Nation.
## 3                        Ten additional stations have been established in the United States, and arrangements have been made for an exchange of reports with Canada, and a similar exchange of observations is contemplated with the West India Islands.
## 4                                                                                    We will respond if others reduce their use of force, and we will withdraw our soldiers once South Vietnam is securely guaranteed the right to shape its own future.
##           docID2 snippetID2
## 1 Roosevelt-1938   14900134
## 2     Grant-1872    8400141
## 3   Johnson-1966   18100222
## 4   Clinton-1998   21900127
##                                                                                                                                                                                                                                                    text2
## 1 We have but to talk with hundreds of small bankers throughout the United States to realize that irrespective of local conditions, they are compelled in practice to accept the policies laid down by a small number of the larger banks in the Nation.
## 2                        Ten additional stations have been established in the United States, and arrangements have been made for an exchange of reports with Canada, and a similar exchange of observations is contemplated with the West India Islands.
## 3                                                                                    We will respond if others reduce their use of force, and we will withdraw our soldiers once South Vietnam is securely guaranteed the right to shape its own future.
## 4                                      And I think we should say to all the people we're trying to represent here that preparing for a far-off storm that may reach our shores is far wiser than ignoring the thunder till the clouds are just overhead.
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
##    applying min.diff.quantile thresholds of 2.89, 34.57
##    creating gold_reason text
##    ...finished.
##              docID1 snippetID1
## 1         Taft-1910   12200029
## 2        Grant-1872    8400202
## 3         Polk-1846    5800321
## 4        Obama-2010   23100392
## 5       Monroe-1818    3000043
## 6       Hoover-1929   14100290
## 7  Eisenhower-1953b   16600327
## 8      Carter-1979b   19800517
## 9       Arthur-1884    9600167
## 10       Nixon-1971   18600034
##                                                                                                                                                                                                                                              text1
## 1                                                            In completion of this work, the regulations agreed upon require congressional legislation to make them effective and for their enforcement in fulfillment of the treaty stipulations.
## 2                                                                          The work which in some of them for some years has been in arrears has been brought down to a recent date, and in all the current business is being promptly dispatched.
## 3                                                        The reasons which induced me to recommend the measure at that time still exist, and I again submit the subject for your consideration and suggest the importance of early action upon it.
## 4                                                                 And it lives on in all the Americans who've dropped everything to go some place they've never been and pull people they've never known from rubble, prompting chants of "U.S.A.!
## 5                                                                 Even if the territory had been exclusively that of Spain and her power complete over it, we had a right by the law of nations to follow the enemy on it and to subdue him there.
## 6                                                                           Any other attitude by the Federal Government will undermine one of the most precious possessions of the American people; that is, local and individual responsibility.
## 7  I shall shortly send you specific recommendations for establishing such an appropriate commission, together with a reorganization plan defining new administrative status for all Federal activities in health, education, and social security.
## 8                         I recently announced my intention to submit legislation to Congress protecting the rights of the press, and others preparing materials for publication, from searches and seizures undertaken without judicial approval.
## 9                       The Secretary of War submits the report of the Chief of Engineers as to the practicability of protecting our important cities on the seaboard by fortifications and other defenses able to repel modern methods of attack.
## 10                                                                                     Over the next 2 weeks, I will call upon Congress to take action on more than 35 pieces of proposed legislation on which action was not completed last year.
##             docID2 snippetID2
## 1   Cleveland-1888   10000309
## 2    Coolidge-1927   13900428
## 3  Eisenhower-1960   17400074
## 4        Taft-1912   12400227
## 5     Carter-1978b   19600273
## 6       Obama-2016   23700349
## 7       Grant-1870    8200062
## 8   Roosevelt-1936   14700068
## 9     Lincoln-1861    7300176
## 10    Carter-1980b   20000194
##                                                                                                                                                                                                                                               text2
## 1                                                                                         It remains to make the most of it, and when that shall be done the curse will be lifted, the Indian race saved, and the sin of their oppression redeemed.
## 2                                                                                 Stimson, former Secretary of War, was sent there to cooperate with our diplomatic and military officers in effecting a settlement between the contending parties.
## 3                                                              These qualities of determination are particularly essential because of the fact that the process of improvement will necessarily be gradual and laborious rather than revolutionary.
## 4  The good offices which the commissioners were able to exercise were instrumental in bringing the contending parties together and in furnishing a basis of adjustment which it is hoped will result in permanent benefit to the Dominican people.
## 5                                                                                         This year we will continue our deregulatory efforts in the legislative and administrative areas in order to reduce anti-competitive practices and abuses.
## 6                               I see it in the elderly woman who will wait in line to cast her vote as long as she has to, the new citizen who casts his vote for the first time, the volunteers at the polls who believe every vote should count.
## 7                                                                                   Its possession by us will in a few years build up a coastwise commerce of immense magnitude, which will go far toward restoring to us our lost merchant marine.
## 8                                                                  In March, 1933, I appealed to the Congress of the United States and to the people of the United States in a new effort to restore power to those to whom it rightfully belonged.
## 9                                                             In a storm at sea no one on board can wish the ship to sink, and yet not unfrequently all go down together because too many will direct and no single mind can be allowed to control.
## 10                                                              If unemployment should dramatically increase, I will be prepared to consider actions to counter that increase, consistent with our overriding concern about accelerating inflation.
##        read1     read2  readdiff _golden easier_gold
## 1   14.49885 72.045000 -57.54615    TRUE           2
## 2   71.24875  9.750000  61.49875    TRUE           1
## 3   39.52375 -8.044000  47.56775    TRUE           1
## 4   60.76500 14.649211  46.11579    TRUE           1
## 5   50.44500 -4.101304  54.54630    TRUE           1
## 6    5.49200 58.347727 -52.85573    TRUE           2
## 7  -18.63875 51.958621 -70.59737    TRUE           2
## 8    4.36500 55.377941 -51.01294    TRUE           2
## 9   12.84500 59.528649 -46.68365    TRUE           2
## 10  57.79310  2.700000  55.09310    TRUE           1
##                                                                                                                                                                                                      easier_gold_reason
## 1  Text B is "easier" to read because it contains some combination of shorter sentences, more commonly used and more easily understood terms, and is generally less complicated and easier to read and grasp its point.
## 2  Text A is "easier" to read because it contains some combination of shorter sentences, more commonly used and more easily understood terms, and is generally less complicated and easier to read and grasp its point.
## 3  Text A is "easier" to read because it contains some combination of shorter sentences, more commonly used and more easily understood terms, and is generally less complicated and easier to read and grasp its point.
## 4  Text A is "easier" to read because it contains some combination of shorter sentences, more commonly used and more easily understood terms, and is generally less complicated and easier to read and grasp its point.
## 5  Text A is "easier" to read because it contains some combination of shorter sentences, more commonly used and more easily understood terms, and is generally less complicated and easier to read and grasp its point.
## 6  Text B is "easier" to read because it contains some combination of shorter sentences, more commonly used and more easily understood terms, and is generally less complicated and easier to read and grasp its point.
## 7  Text B is "easier" to read because it contains some combination of shorter sentences, more commonly used and more easily understood terms, and is generally less complicated and easier to read and grasp its point.
## 8  Text B is "easier" to read because it contains some combination of shorter sentences, more commonly used and more easily understood terms, and is generally less complicated and easier to read and grasp its point.
## 9  Text B is "easier" to read because it contains some combination of shorter sentences, more commonly used and more easily understood terms, and is generally less complicated and easier to read and grasp its point.
## 10 Text A is "easier" to read because it contains some combination of shorter sentences, more commonly used and more easily understood terms, and is generally less complicated and easier to read and grasp its point.
```

There is a lot more than this, of course. Our documentation will improve
as we develop the package with an aim to eventual CRAN release.
