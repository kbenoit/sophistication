.onAttach <- function(...) {
    # set user options
    user <- Sys.info()["user"]
    options(ROOT_DROPBOX =
            switch(user,
                   kbenoit = "~/Dropbox/Papers/Benoit_Spirling_Readability/",
                   as9934 = "c:/users/as9934/dropbox/Benoit_Spirling_Readability/",
                   kevin = "C:/Users/kevin/Dropbox/Benoit_Spirling_Readability/")
    )

    options(PYTHON_EXECUTABLE =
                switch(user,
                       kbenoit = "/Library/Frameworks/Python.framework/Versions/3.6/bin/python3",
                       as9934 = NULL,
                       kevin = NULL)
    )

    # startup message
    packageStartupMessage("sophistication version ", as.character(utils::packageVersion("sophistication")),
                          "\nDropbox folder location set to: ", getOption("ROOT_DROPBOX"))
}
