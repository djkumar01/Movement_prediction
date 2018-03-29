library(tm)
#input
feb16 <- readLines(file.choose())
apr16 <- readLines(file.choose())
jun16 <- readLines(file.choose())
aug16 <- readLines(file.choose())
oct16 <- readLines(file.choose())
dec16 <- readLines(file.choose())


library(tidytext)
library(dplyr)
library(tibble)
feb16_df <- data_frame(line = 1:33, text = feb16)
apr16_df <- data_frame(line = 1:1371, text = apr16)
jun16_df <- data_frame(line = 1:34, text = jun16)
aug16_df <- data_frame(line = 1:34, text = aug16)
oct16_df <- data_frame(line = 1:19, text = oct16)
dec16_df <- data_frame(line = 1:31, text = dec16)
#sapply(feb16_df, class)

feb16_df <- cbind(feb16_df, report = "")
apr16_df <- cbind(apr16_df, report = "")
jun16_df <- cbind(jun16_df, report = "")
aug16_df <- cbind(aug16_df, report = "")
oct16_df <- cbind(oct16_df, report = "")
dec16_df <- cbind(dec16_df, report = "")

feb16_df$report <- paste(feb16_df$report, "Feb16", sep = "")
apr16_df$report <- paste(apr16_df$report, "Apr16", sep = "")
jun16_df$report <- paste(jun16_df$report, "Jun16", sep = "")
aug16_df$report <- paste(aug16_df$report, "Aug16", sep = "")
oct16_df$report <- paste(oct16_df$report, "Oct16", sep = "")
dec16_df$report <- paste(dec16_df$report, "Dec16", sep = "")

data_df <- rbind(feb16_df, apr16_df, jun16_df, aug16_df, oct16_df, dec16_df)
data_df <- as_tibble(data_df)

data_df <-  data_df %>% unnest_tokens(word, text)
class(data_df)
data_df

data_sentiment <- data_df %>% group_by(report) %>%
  inner_join(get_sentiments("loughran"), by= "word") %>% count(sentiment)
data_sentiment

library(ggplot2)
ggplot(data_sentiment, aes(x= sentiment,y = n, fill = report)) + 
  geom_col(show.legend = F) + facet_wrap(~report, scales = 'free_x') + 
  labs(title = "Sentimental Analysis of Bi-Monthly Reports of RBI") + 
  theme(plot.title = element_text(hjust = 0.5)) + coord_flip()
