df <- read.csv("./alteryx/data/TrustPilot.csv",header = TRUE)
some_txt = df$Review.Content
some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
some_txt = gsub("@\\w+", "", some_txt)
some_txt = gsub("[[:punct:]]", "", some_txt)
some_txt = gsub("[[:digit:]]", "", some_txt)
some_txt = gsub("http\\w+", "", some_txt)
some_txt = gsub("[ \t]{2,}", "", some_txt)
some_txt = gsub("^\\s+|\\s+$", "", some_txt)

try.error = function(x)
  {
     # create missing value
     y = NA
    # tryCatch error
    try_error = tryCatch(tolower(x), error=function(e) e)
    # if not an error
    if (!inherits(try_error, "error"))
    y = tolower(x)
    # result
    return(y)
 }

some_txt = sapply(some_txt, try.error)
some_txt = some_txt[!is.na(some_txt)]
names(some_txt) = NULL

library(sentiment)

library(tm)
library(NLP)
library(Rstem)
class_emo = classify_emotion(some_txt, algorithm="bayes", prior=1.0)
emotion = class_emo[,7]
emotion[is.na(emotion)] = "unknown"
class_pol = classify_polarity(some_txt, algorithm="bayes")
polarity = class_pol[,4]
sent_df = data.frame(text=some_txt, emotion=emotion,polarity=polarity, stringsAsFactors=FALSE)
sent_df = within(sent_df,emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
library(xlsx)
df$emotion <- sent_df$emotion
df$polarity <- sent_df$emotion
write.xlsx(df, "./TextMining/Trustdata.xlsx")
