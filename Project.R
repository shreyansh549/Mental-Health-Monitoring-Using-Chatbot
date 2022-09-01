#https://api.telegram.org/bot2022741682:AAEL3HuqJkU6gK-kG07aYZlM7hfUonid-Ik/getUpdates
library(telegram.bot)
library(stringr)
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("syuzhet")
library("ggplot2")

bot_token <- "2022741682:AAEL3HuqJkU6gK-kG07aYZlM7hfUonid-Ik"
target_chat_id <- 909650045

bot <- Bot(token = bot_token)

bot_sendmessage <-
  function(text_to_sent) {
    bot$sendMessage(chat_id = target_chat_id,
                    text_to_sent,
                    parse_mode = NULL)
  }

Hour = "08"
Minute = "32"
remtime = paste(Hour, Minute, "00", sep ="")

yestext <- readLines("E:\\academics\\3rdyrfall\\data analytics\\Project\\dataset\\yestext.txt")
lasttext = yestext
greet = FALSE
clean = FALSE
donetoday = FALSE

while (!donetoday) {
  if(format(Sys.time(), "%H%M%S") == remtime){
  bot$clean_updates()
  clean = TRUE
  }
  if(greet){
    history <- bot$getUpdates()
    if (length(history) != 0) {
      lastmessage <- history[[length(history)]]
      
      lasttext <- lastmessage$message$text
      lastuser <- lastmessage$message$from$first_name
      
      print(lastuser)
      print(lasttext)
    }
  }
    if(lasttext != yestext){
      yestext <- lasttext
      writeLines(yestext, "E:\\academics\\3rdyrfall\\data analytics\\Project\\dataset\\yestext.txt")
      text <- lasttext
      TextDoc <- Corpus(VectorSource(text))
      toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
      TextDoc <- tm_map(TextDoc, toSpace, "/")
      TextDoc <- tm_map(TextDoc, toSpace, "@")
      TextDoc <- tm_map(TextDoc, toSpace, "\\|")
      # Convert the text to lower case
      TextDoc <- tm_map(TextDoc, content_transformer(tolower))
      # Remove numbers
      TextDoc <- tm_map(TextDoc, removeNumbers)
      # Remove english common stopwords
      TextDoc <- tm_map(TextDoc, removeWords, stopwords("english"))
      # Remove your own stop word
      # specify your custom stopwords as a character vector
      TextDoc <- tm_map(TextDoc, removeWords, c("s", "company", "team")) 
      # Remove punctuations
      TextDoc <- tm_map(TextDoc, removePunctuation)
      # Eliminate extra white spaces
      TextDoc <- tm_map(TextDoc, stripWhitespace)
      # Text stemming - which reduces words to their root form
      TextDoc <- tm_map(TextDoc, stemDocument)
      # Build a term-document matrix
      TextDoc_dtm <- TermDocumentMatrix(TextDoc)
      dtm_m <- as.matrix(TextDoc_dtm)
      # Sort by descearing value of frequency
      dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
      dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)
      # Display the top 5 most frequent words
      head(dtm_d, 5)
      # Plot the most frequent words
      png(filename="C:\\Users\\Tejaswanth Maram\\Downloads\\bplot.png")
      barplot(dtm_d[1:5,]$freq, las = 2, names.arg = dtm_d[1:5,]$word,
              col ="lightgreen", main ="Top 5 most frequent words",
              ylab = "Word frequencies")
      dev.off()
      bot$sendPhoto(
        chat_id = 909650045,
        photo = "C:\\Users\\Tejaswanth Maram\\Downloads\\bplot.png",
        caption = "Top 5 frequent words in ur diary"
      )
      #generate word cloud
      png(filename="C:\\Users\\Tejaswanth Maram\\Downloads\\wcloud.png")
      set.seed(1234)
      par(mar = c(3,3,3,3))
      wordcloud(words = dtm_d$word, freq = dtm_d$freq, min.freq = 5,
                max.words=100, random.order=FALSE, rot.per=0.40, 
                colors=brewer.pal(8, "Dark2"))
      dev.off()
      bot$sendPhoto(
        chat_id = 909650045,
        photo = "C:\\Users\\Tejaswanth Maram\\Downloads\\wcloud.png",
        caption = "word cloud of your diary"
      )
      
      #associations 
      findAssocs(TextDoc_dtm, terms = c("good","work","health"), corlimit = 0.25)			
      findAssocs(TextDoc_dtm, terms = findFreqTerms(TextDoc_dtm, lowfreq = 50), corlimit = 0.25)
      
      #syuzhet
      syuzhet_vector <- get_sentiment(text, method="syuzhet")
      print(head(syuzhet_vector))
      print(summary(syuzhet_vector))
      
      # bing
      bing_vector <- get_sentiment(text, method="bing")
      print(head(bing_vector))
      print(summary(bing_vector))
      
      #affin
      afinn_vector <- get_sentiment(text, method="afinn")
      print(head(afinn_vector))
      print(summary(afinn_vector))
      
      #compare the first row of each vector using sign function
      rbind(
        sign(head(syuzhet_vector)),
        sign(head(bing_vector)),
        sign(head(afinn_vector))
      )
  
      d<-get_nrc_sentiment(text)
      print(head (d,10))
      
      td<-data.frame(t(d))
      td_new <- data.frame(rowSums(td))
      names(td_new)[1] <- "count"
      td_new <- cbind("sentiment" = rownames(td_new), td_new)
      rownames(td_new) <- NULL
      td_new2<-td_new[1:8,]
      
      jpeg(filename="C:\\Users\\Tejaswanth Maram\\Downloads\\nrcplot.jpg")
      quickplot(sentiment, data=td_new2, weight=count, geom="bar", fill=sentiment, ylab="count")+ggtitle("Survey sentiments")
      dev.off()
      bot$sendPhoto(
        chat_id = 909650045,
        photo = "C:\\Users\\Tejaswanth Maram\\Downloads\\nrcplot1.jpg",
        caption = "Your Sentiment Analysis Report of the Day"
      )
      
      jpeg(filename="C:\\Users\\Tejaswanth Maram\\Downloads\\emoplot.jpg")
      barplot(
        sort(colSums(prop.table(d[, 1:8]))), 
        horiz = TRUE, 
        cex.names = 0.7, 
        las = 1, 
        main = "Emotions in Text", xlab="Percentage"
      )
      dev.off()
      bot$sendPhoto(
        chat_id = 909650045,
        photo = "C:\\Users\\Tejaswanth Maram\\Downloads\\emoplot.jpg",
        caption = "Your Sentiment Analysis Report of the Day"
      )
      
      bot_sendmessage("Wish you a great day tommorow, good night!")
      donetoday = TRUE
    }
    if (clean && !donetoday && !greet) {
      bot_sendmessage("Hey! how's ur day?")
      greet = TRUE
    }
}
