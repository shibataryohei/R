# data #####
check_levels <-
  function(data, x){levels(droplevels(as.factor(data[[x]])))}

table2png <- 
  function(data, file){
    tg = gridExtra::tableGrob(data, rows=NULL)
    h = grid::convertHeight(sum(tg$heights), "in", TRUE)
    w = grid::convertWidth(sum(tg$widths), "in", TRUE)
    ggplot2::ggsave(paste0(file),
                    tg, width = w, height = h, dpi = 300)}

c2r <- function(data, x){as.data.frame(data) %>% 
    tibble::column_to_rownames(., x)}

r2c <- function(data, x){as.data.frame(data) %>% 
    tibble::rownames_to_column(., x) }

c2f <- function(data){data %>% 
    mutate_if(is.character, funs(as.factor(.)))}

# satatistics #####
qvalue_r <- function(x){
  qvalue::qvalue(x,
                 lambd = seq(0, 0.95, 0.05),
                 pi0 = 1,
                 pi0.method = "bootstrap")$qvalue}

add_sd <-
  function(Value){
    ifelse(Value < 0.001, paste("***"),
           ifelse(Value < 0.01, paste("**"),
                  ifelse(Value < 0.05, paste("*"),
                         paste(""))))}

# format #####
format_pvalue <- function(Value){
  if_else(Value < 0.001, paste("<0.001"),
          if_else(Value < 0.1, sprintf(Value, fmt = "%.3f"),
                  sprintf(Value, fmt = "%.2f")))}

format_rstatix <- function(df){
  df %>% 
    tbl_df %>% 
    dplyr::select(Variable, p) %>% 
    dplyr::rename(P.value = p) %>% 
    mutate(Q.value = q.value(P.value)) %>% 
    arrange(P.value)}

format_digit <- 
  function(x, i){sprintf(paste0("%.", i, "f"), x)}

# pheatmap #####
associate_r <- function(df1, df2){
  intersect(rownames(df1),
            rownames(df2)) -> id
  microbiome::associate(df1[id, , drop = FALSE],
                        df2[id, , drop = FALSE],
                        method = "spearman") %>% 
    dplyr::rename(Q.value = p.adj)} 

make_list_pheatmap <- function(tbw, X1, X2){
  tbw %>% 
    dplyr::select(-Q.value) %>% 
    spread(X2, Correlation) %>% 
    c2r("X1") -> correlation_df
  
  tbw %>% 
    dplyr::select(-Correlation) %>% 
    mutate(Q.value = addsd(Q.value)) %>% 
    spread(X2, Q.value) %>% 
    c2r("X1") -> sd_df
  
  max(abs(correlation_df)) -> range
  
  list(cor = correlation_df,
       sd = sd_df,
       range = range)}

# Epidemiology #####
tidy_table <- function(tidy){
  tidy %>% 
    mutate(p.value = arrange.pvalue(p.value)) %>% 
    rename(Variable = term,
                  `p-value` = p.value) %>% 
    mutate_if(is.numeric, funs(sprintf("%.2f", .))) %>% 
    mutate(Estimate = glue::glue("{estimate} ({conf.low}–{conf.high})")) %>% 
    select(-estimate, -std.error, -statistic,
                  -conf.low, -conf.high)}