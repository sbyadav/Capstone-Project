#Code to find the frequency of each distinct word from the overall customer comments.

library(xlsx)
df = read.xlsx("./nafile.xlsx",1)
n = nrow(df)
#print(n)
#stop()
#names(df)
df1 = ""
for(i in 1:n)
{
df2 <- paste(df$Review.Content[i],df$Review.Title[i])
#df2 <- df$Review.Content[i]
df1 = paste(df1,df2, sep = ' ')
}
df1 = tolower(df1)
print(df1)
#stop()
df1 = as.character(df1)
df1 = gsub("[[:punct:]]|[[:space:]]|[[:digit:]]", " ", iconv(df1, to = "ASCII//TRANSLIT"))
#df1<-gsub("\\.","",df1)
#df1<-gsub("\\,","",df1)
#df1<-gsub("\\!","",df1)
words <- strsplit(df1," ")



words.freq<-table(words)
trust = sort(words.freq, decreasing = T)
#print(trust[1])
trust1 = as.data.frame(trust)
#print(trust1)

write.xlsx(trust1, "./NaWordsFrequency.xlsx")
