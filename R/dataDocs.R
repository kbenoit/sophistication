#' @name data_corpus_presdebates
#' @title Corpus of segemented US presidential candidate debates
#' @docType data
#' @description A corpus object US presidential candidate debates, currently
#'   just 2016, but we could easily add more. The corpus includes the following
#'   document variables: \describe{ \item{speaker}{Last name of the speaker}
#'   \item{party}{Party of the candidate (Democratic or Republican)}
#'   \item{date}{Date of the debate.} \item{city}{City where the debate took
#'   place} \item{speakertype}{e.g. \code{"candidate"}, \code{"moderator"},
#'   etc.} }
#' @source The American Presidency Project,
#'   \url{http://www.presidency.ucsb.edu}.
NULL

#' @name data_corpus_partybroadcasts
#' @title Corpus of UK political party broadcasts
#' @docType data
#' @description A corpus object UK political party broadcasts, 1961-2004, for the Labour
#' and Conservative parties.  The corpus includes the following
#'   document variables: \describe{ \item{party}{factor variable, \code{Lab} or \code{Con}}
#'   \item{date}{Date of the broadcast (formatted as date)} }
#' @source
#'   \url{http://www.politicsresources.net/area/uk/peb.htm}
NULL

#' @name data_corpus_presdebates2016
#' @title Corpus of primary debate transcripts from the US Presidential election
#'   of 2016
#' @docType data
#' @description A corpus of primary debate transcripts from the US Presidential
#'   election of 2016, segmented by speaker, with additional information as docvars.
#' @source The American Presidency Project,
#'   \url{http://www.presidency.ucsb.edu}.
NULL

#' @name data_corpus_Crimson
#' @title Corpus of archives of articles in the Harvard Crimson
#' @docType data
#' @description A corpus object of articles retrieved from the online archives of the Harvard
#'   Crimson, 1873-2016.  The corpus includes the following
#'   document variable: \describe{\item{date}{Date of the article (formatted as date)} }
#' @source
#'   \url{https://www.thecrimson.com/sitemap/}
NULL

#' @name data_corpus_fifthgrade
#' @title Corpus of fifth grade texts
#' @docType data
#' @description A corpus object of articles taken from a repository of fifth-grade texts
#' from The Friday Institute for Educational Innovation.
#' @source
#'   \url{https://www.ncsu.edu/project/lancet/fifth.htm}
NULL

#' Corpus of U.S. Presidential Proclamations
#'
#' @format Includes one docvar, \code{Year}.
#' @note To clean the texts for sentence counting, it is recommended to
#'   pre-process it using the code from the example, e.g.
#'
#'   \code{example(data_corpus_pp)}
#' @examples
#' \donttest{
#' data(data_corpus_pp)
#' patts2remove <- "^Judges|^For the \\w+ Ward|By the President|^[\\p{Lu}\\s]+$"
#' data_corpus_pp <-
#'     corpus_trimsentences(data_corpus_pp, min_length = 5, exclude_pattern = patts2remove)
#'
#' ## inspect the sentences
#' # corpus_sentences_browse(data_corpus_pp)
#' }
#' @source The American Presidency Project, \url{http://www.presidency.ucsb.edu/sou.php}.
"data_corpus_pp"

#' @name data_matrix_google1grams
#' @title Matrix of counts of unigrams from Google 1-gram corpus
#' @docType data
#' @description Matrix of counts by decade of English language words from Google 1-gram corpus.
#' @source
#'   \url{http://storage.googleapis.com/books/ngrams/books/datasetsv2.html}
NULL


#' @name data_integer_brownfreq
#' @title Matrix of counts of unigrams from Brown corpus
#' @docType data
#' @description Matrix of counts of term frequencies from the Brown corpus, taken from
#' the \pkg{zipfR} package.
NULL

#' @name data_corpus_SCOTUS
#' @title Corpus of Supreme Court decisions 1790-2012
#' @description All decisions written by SCOTUS justices, 1790 to 2012
#' @source \url{https://supreme.justia.com/}, scraped by Tom Clark.
#' @docType data
#' @examples
#' \dontrun{
#' # to load this dataset
#' data_dropbox(data_corpus_SCOTUS)
#' }
NULL

#' Corpus of Executive orders
#'
#' Executive orders from 1826 to 2017
#' @source \url{http://www.presidency.ucsb.edu/executive_orders.php}
#' @format includes integer \code{Year}
"data_corpus_eo"
