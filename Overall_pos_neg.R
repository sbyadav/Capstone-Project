#Code to get the number of overall positive and negative words from a comment.
#Comment eg. Salesperson was friendly and helpful but delivery guy was rude.
#Positive words - friendly , helpful (2); Negative words - rude1(1)
#Overall count - 2-1 = 1

#positive-words.txt - file with 2000 positive words
#negative-words.txt - file with 4800 negative words

pos_words <- scan('./positive-words.txt', what='character', comment.char=';')
neg_words <- scan('./negative-words.txt', what='character', comment.char=';')


score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
    require(plyr)
    require(stringr)
     
    # a simple array ("a") of scores back
    scores = laply(sentences, function(sentence, pos.words, neg.words) {
         
        # clean the individual comments
        sentence = gsub('[[:punct:]]', '', sentence)
        sentence = gsub('[[:cntrl:]]', '', sentence)
        sentence = gsub('\\d+', '', sentence)
        # convert to lower case:
        sentence = tolower(sentence)
 
        # split into words
        word.list = str_split(sentence, '\\s+')
        # sometimes a list() is one level of hierarchy too much
        words = unlist(word.list)
 
        # compare the words to the positive & negative terms
        pos.matches = match(words, pos.words)
        neg.matches = match(words, neg.words)
     
        # match() returns the position of the matched term or NA
        # TRUE/FALSE
        pos.matches = !is.na(pos.matches)
        neg.matches = !is.na(neg.matches)
 
        score = sum(pos.matches) - sum(neg.matches)
 
        return(score)
    }, pos.words, neg.words, .progress=.progress )
 
    scores.df = data.frame(score=scores, text=sentences)
    return(scores.df)
}

#test = c("very good delivery but salesman was very rude")
df = read.csv(".\\TrustPilot.csv")
test = df$Review.Content

result = score.sentiment(test,pos_words,neg_words)

df$result = result$score
library(xlsx)
write.xlsx(df, "./Overall.xlsx")

#print(head(result))
#print(result)
