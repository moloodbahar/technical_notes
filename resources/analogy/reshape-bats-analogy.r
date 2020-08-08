

library(tidyverse)

#######################33
# build analogy function --
build_questions <- function (x) { 
  
  lapply(unique(x$category), function(i) {
    comcaps <-subset(x, category == i)
    combos <- data.frame(t(combn(paste(comcaps$a, comcaps$b, 
                                       sep = ' '), m = 2))) 
    a <- paste(combos$X1, combos$X2, sep = ' ')
    b <- paste(combos$X2, combos$X1, sep = ' ')
    combos3 <- data.frame(full = c(a, b))
    combos3$category <- i
    combos3
    })
}


setwd("/home/jtimm/jt_work/GitHub/x_wv_model_eval/BATS_3.0")
ans <- list.files(path="/home/jtimm/jt_work/GitHub/x_wv_model_eval/BATS_3.0", 
                  recursive=TRUE,
                  pattern = ".txt")

fans <- lapply(ans, read.csv, sep = '\t', header = F)
names(fans) <- ans

fansf <- fans %>% bind_rows(.id = 'category') %>%
  filter(!grepl('/', V2)) %>% # kills multiple responses -- 
  rename(a = V1, b = V2)

fansf2 <- build_questions(fansf) %>%
  bind_rows() 

fansf3 <- fansf2 %>%
  group_by(category) %>%
  do(add_row(., .before=0)) %>%
  ungroup() %>%
  fill(category, .direction = c("up")) %>%
  mutate(category = paste0(': ', gsub(' |\\.txt$', '', category)),
         full = ifelse(is.na(full), category, full)) %>%
  select(full)


setwd("/home/jtimm/jt_work/GitHub/x_wv_model_eval/")
writeLines(fansf3$full, 'bats-questions-words.txt')

