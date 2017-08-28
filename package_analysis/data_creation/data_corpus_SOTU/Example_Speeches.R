
wilson <- "I trust that the Congress will give its immediate consideration to the problem of future taxation. Simplification of the income and profits taxes has become an immediate necessity. These taxes performed indispensable service during the war. They must, however, be simplified, not only to save the taxpayer inconvenience and expense, but in order that his liability may be made certain and definite."
FDR <- "We are justified in our present confidence. Restoration of national income, which shows continuing gains for the third successive year, supports the normal and logical policies under which agriculture and industry are returning to full activity. Under these policies we approach a balance of the national budget. National income increases; tax receipts, based on that income, increase without the levying of new taxes. That is why I am able to say to this, the Second Session of the 74th Congress, that it is my belief based on existing laws that no new taxes, over and above the present taxes, are either advisable or necessary."
bush <- "Some say my tax plan is too big. Others say it's too small. I respectfully disagree. This plan is just right. I didn't throw darts at a board to come up with a number for tax relief. I didn't take a poll or develop an arbitrary formula that might sound good. I looked at problems in the Tax Code and calculated the cost to fix them."
bush2 <- "Good jobs depend on sound tax policy. Last year, some in this Hall thought my tax relief plan was too small; some thought it was too big. But when the checks arrived in the mail, most Americans thought tax relief was just about right. Congress listened to the people and responded by reducing tax rates, doubling the child credit, and ending the death tax. For the sake of long-term growth and to help Americans plan for the future, let's make these tax cuts permanent."

require(quanteda)
readability(c(wilson = wilson, FDR = FDR, bush1 = bush, bush2 = bush2), "Flesch.Kincaid")


filen <- ifelse(Sys.info()["user"] == "kbenoit", 
                "~/Dropbox/Papers/Benoit_Spirling_Readability/data/sou_speeches/*.txt", 
                "c:/users/as9934/dropbox/Benoit_Spirling_Readability/data/sou_speeches/*.txt")
mytext <- texts(textfile(filen, encodingFrom = "UTF-8-BOM"))
summary(mytext)