# filter_taxa
filter_taxa <- function(tbl,
                        rate){
tbl %>% 
  checklevels("Variable") %>% 
  .[1] -> factor

tbl %>% 
  filter(Variable == factor) %>% 
  nrow -> n

tbl %>% 
  filter(Value > 0) %>% 
  group_by(Variable) %>% 
  dplyr::summarise(Count = n()) %>% 
  filter(Count > n * (rate)) %>% 
  checklevels("Variable") -> include_variable

tbl %>% 
  filter(Variable %in% include_variable)}



# filter_taxa
filter_metabolite <- function(tbl,
                        threshold,
                        rate){
  tbl %>% 
    checklevels("Variable") %>% 
    .[1] -> factor
  
  tbl %>% 
    filter(Variable == factor) %>% 
    nrow -> n
  
  tbl %>% 
    filter(Value > threshold) %>% 
    group_by(Variable) %>% 
    dplyr::summarise(Count = n()) %>% 
    filter(Count > n * (rate)) %>% 
    checklevels("Variable") -> include_variable
  
  tbl %>% 
    filter(Variable %in% include_variable)}



lefse_add_taxonomy <- function(x){
  data.frame(Taxonomy = as.factor(c("Phylum", "Class", "Order", 
                                    "Family", "Genus", "Species")),
             Levels = seq(1:6)) %>% 
    tbl_df -> taxonomy_tbl
  
  x %>% 
    mutate(Levels = gsub("[a-zA-Z]|[0-9]|_|\\.","", Variable)) %>%
    mutate(Levels = nchar(Levels)) %>%
    mutate(Variable = gsub("_", " ", Variable)) %>%
    inner_join(., taxonomy_tbl) %>%
    mutate(Taxonomy = fct_relevel(Taxonomy,
                                  "Phylum", "Class", "Order", 
                                  "Family", "Genus", "Species")) %>% 
    dplyr::select(-Levels)
}



%>% 
  filter(Taxonomy == "Genus") %>% 
  mutate(Variable = gsub(".*\\|.*\\|.*\\|.*\\|", "", Variable)) %>% 
  dplyr::select(-Levels, -Taxonomy) %>% 
  mutate(adj.LDA = as.numeric(as.character(adj.LDA))) %>% 
  mutate(Variable = fct_reorder(Variable, adj.LDA)) %>% 
  mutate(Age = fct_relevel_age(Age)) -> Genus_RES_Delivery_tbl