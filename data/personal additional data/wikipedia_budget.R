# create a new data from original data

budget_validation <- df %>% filter(is.na(budget) | budget < 500) %>%
  select(imdb_id, title, budget, revenue) %>%
  mutate(benefit = revenue/budget,
         title_2 = str_replace_all(title," ","_")) %>%
  mutate(wiki = str_c("https://en.wikipedia.org/wiki/",title_2, sep = ""))

head(budget_validation)

# write a function to extract data from Wikipedia

wiki_extract <- function(link){
  h <- read_html(link)
  
  a <- h %>%
    html_node("table") %>%
    html_table(fill = TRUE)
  
  names(a) <- c("c1","c2")
  a$c2[which(a$c1 == "Budget")]
}

# extract data from wiki pedia

links <- budget_validation$wiki

budget_wiki <- sapply(links, function(link){
  
  print(which(links == link)/2070)
  
  return(tryCatch(wiki_extract(link), error=function(e) NULL))
})

names(budget_wiki) <- budget_validation$imdb_id

new_budgets <- data.frame(matrix(unlist(budget_wiki), nrow = length(budget_wiki), byrow = T))
new_budgets <- data.frame(imdb_id = budget_validation$imdb_id, 
                          wiki_budget = new_budgets$matrix.unlist.budget_wiki...nrow...length.budget_wiki...byrow...T.)
head(new_budgets,50)

new_budgets$wiki_budget[which(str_detect(new_budgets$wiki_budget,"million\\s?\\[?\\d?\\]?\\)?"))]

# mining budget
new_budgets <- new_budgets %>%
  mutate(new_budget = str_replace_all(wiki_budget,"\\[.*\\]",""),
         currency_unit = ifelse(str_detect(wiki_budget,"(\\$)|(USD)|(US)"),1,
                         ifelse(str_detect(wiki_budget,"\\¥"),0.0094,
                         ifelse(str_detect(wiki_budget,"\\s+crore.*"),156595,
                         ifelse(str_detect(wiki_budget,"(\\£)|(\\£)"),1.22,
                         ifelse(str_detect(wiki_budget,"\\€"),1.11,
                         ifelse(str_detect(wiki_budget,"\\₹"),0.014,
                         ifelse(str_detect(wiki_budget,"\\₱"),0.019,0)))))))) %>%	
  mutate(new_budget = str_replace_all(new_budget, 
                          "(\\s?million.*)|(.*\\$)|(USD)|(US)|(US\\$)|(\\s+Million)|
                          (\\s+billion.*)|(\\¥)|(\\s+crore.*)|(\\-.*)|(\\(\\w+\\))|
                          (\\£)|(\\€)|(\\₹)|(\\,)|(\\s+)","")) %>%
  mutate(new_budget = str_replace_all(new_budget,"(\\s?\\£)|(\\–\\d+)","")) %>%
  mutate(final_budget = ifelse(str_detect(wiki_budget,"million"),as.numeric(new_budget)*currency_unit*1000000,
                               as.numeric(new_budget)*currency_unit))

mean(is.na(new_budgets$final_budget))

write.csv(new_budgets, file = "wikipedia_budget.csv", row.names = F)
