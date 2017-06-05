library(xlsx)
df = read.xlsx("./Trustdata.xlsx",1)
m = nrow(df)

mydata <- matrix(, nrow = m, ncol = 5)


#names(df)
findwords <- c("delivery", "salesperson", "price", "store")
n = length(findwords)

for(j in 1:m)
{
#df1 = paste(df$Reviews[j],df$Title[j])
df1 = paste(df$Review.Content[j],df$Review.Title[j])
df1 = as.character(df1)

df1 = tolower(df1)
df1 = gsub("[[:punct:]]|[[:space:]]|[[:digit:]]", " ", iconv(df1, to = "ASCII//TRANSLIT"))

df1 = gsub('\\bdeliv\\w+|\\bmove\\w+', 'delivery', df1)
df1 = gsub('\\bpric\\w+', 'price', df1)
df1 = gsub('\\bsale\\w+|\\bstaf\\w+|\\bsale\\b', 'salesperson', df1)
df1 = gsub('\\bstore\\w+', 'store', df1)

	
for(i in 1:n)
{
if(grepl(findwords[i],df1) == TRUE) 
{
mydata[j,i] = 1
}
else
{
mydata[j,i] = 0
}
}
if(mydata[j,1] == 0 && mydata[j,2] == 0 && mydata[j,3] == 0 && mydata[j,4] == 0)
{
mydata[j,5] = 1
}
else 
{
mydata[j,5] = 0
}
}

#print(mydata)
df$Delivery = mydata[,1]
df$Salesperson = mydata[,2]
df$Price = mydata[,3]
df$Store = mydata[,4]
df$Overall_Experience = mydata[,5]
write.xlsx(df, "./Trustdata_final.xlsx")

stop()



#if(grepl(findwords,df1) == TRUE) print ("thank")
#a = sapply(findwords, grepl, df1)
#a = as.data.frame(a)
#print(a)

words <- strsplit(df1," ")
#print(head(words))
#stop()
words.freq<-table(words)
trust = sort(words.freq, decreasing = T)
#print(trust[1])
trust1 = as.data.frame(trust)
#print(trust1)

#findwords <- c("good", "experience", "you")
#a = sapply(findwords, grepl, trust1$words)
print(trust1$words)

#print(grep("delivery",trust1$words))




#write.xlsx(trust1, "./WordsFrequency.xlsx")