################################
##### The tidy text format #####
################################
text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")

library(dplyr)
library(tidytext)
library(janeaustenr)
library(stringr)
library(ggplot2)
library(gutenbergr)

text_df <- data_frame(line = 1:4, text = text)

text_df %>%
  unnest_tokens(word, text) # separate by word and displays line

original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup()

tidy_books <- original_books %>%
  unnest_tokens(word, text)

data(stop_words)

tidy_books <- tidy_books %>%
  anti_join(stop_words)

tidy_books %>%
  count(word, sort = TRUE)

tidy_books %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

gutenberg_metadata %>%
  filter(title == "Wuthering Heights")

gutenberg_works(author == "Austen, Jane")

gutenberg_works(str_detect(author, "Austen"))

wuthering_heights <- gutenberg_download(768)

books <- gutenberg_download(c(768, 1260), meta_fields = "title")

books %>%
  count(title)

gutenberg_subjects %>%
  filter(grepl("Holmes, Sherlock", subject))

words <- books %>%
  unnest_tokens(word, text)

word_counts <- words %>%
  anti_join(stop_words, by = "word") %>%
  count(title, word, sort = TRUE)