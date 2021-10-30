digit <- 
  function(x, i){sprintf(paste0("%.", i, "f"), x)}

checklevels <-
  function(data, x){levels(droplevels(as.factor(data[[x]])))}

tablepng <- 
  function(data, file){
    tg = gridExtra::tableGrob(data, rows=NULL)
    h = grid::convertHeight(sum(tg$heights), "in", TRUE)
    w = grid::convertWidth(sum(tg$widths), "in", TRUE)
    ggplot2::ggsave(paste(file,".png",sep=""),
                    tg, width=w, height=h, dpi=300)}

tablepng2 <- 
  function(data, file){
    tg = gridExtra::tableGrob(data, rows=NULL)
    h = grid::convertHeight(sum(tg$heights), "in", TRUE)
    w = grid::convertWidth(sum(tg$widths), "in", TRUE)
    ggplot2::ggsave(paste0(file),
                    tg, width=w, height=h, dpi=300)}

c2r <- function(data, x){as.data.frame(data) %>% 
    tibble::column_to_rownames(., x)}

r2c <- function(data, x){as.data.frame(data) %>% 
    tibble::rownames_to_column(., x) }

c2f <- function(data){data %>% 
    mutate_if(is.character, funs(as.factor(.)))}

library("htmltools")
library("webshot") 
export_formattable <- function(f, file, width,height,
                               background = "white", delay = 0.2)
{
  w <- as.htmlwidget(f, width = width, height = height)
  path <- html_print(w, background = background, viewer = NULL)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot(url,
          file = file,
          selector = ".formattable_widget",
          delay = delay,
          zoom = 10)}

# Test #####
fisher <- function(a,b,c,d){
  data <- matrix(c(a,b,c,d),ncol=2)
  c(p = fisher.test(data)$p.value,  # simulate.p.value=TRUE, B=1e4, workspace = 100000
    OR = fisher.test(data)$estimate,
    CI = fisher.test(data)$conf.int)}

q.value <- function(x){
  qvalue::qvalue(x,
                 lambd = seq(0, 0.95, 0.05),
                 pi0 = 1,
                 pi0.method = "bootstrap")$qvalue}

addsd <-
  function(Value){
    ifelse(Value < 0.001, paste("***"),
           ifelse(Value < 0.01,paste("**"),
                  ifelse(Value < 0.05,paste("*"),
                         paste(""))))}

arrange.pvalue <- function(Value){
  if_else(Value < 0.001, paste("<0.001"),
          if_else(Value < 0.1, sprintf(Value, fmt = "%.3f"),
                  sprintf(Value, fmt = "%.2f")))}

# Clinical #####
# Pediatric eGFR
# age = 0.1
# ht = 0.8
# cre = 1
# sex = "M"

refcre <- function(ht, sex){
  ifelse(sex %in% c("M","Male","male"),
         -1.259*ht^5+7.815*ht^4-18.57*ht^3+21.39*ht^2-11.71*ht+2.628,
         -4.536*ht^5+27.16*ht^4-63.47*ht^3+72.43*ht^2-40.06*ht+8.778)}

percre <- function(cre, ht, sex){
  (ifelse(sex %in% c("M","Male","male"),
         -1.259*ht^5+7.815*ht^4-18.57*ht^3+21.39*ht^2-11.71*ht+2.628,
         -4.536*ht^5+27.16*ht^4-63.47*ht^3+72.43*ht^2-40.06*ht+8.778))/cre*100}

pegfr <- function(cre, age, ht, sex){
  R = ifelse(age<(3/12), NA,
             ifelse(0.107*log(age*12)+0.656, 1))
  r_cre = ifelse(sex %in% c("M","Male","male"),
                  -1.259*ht^5+7.815*ht^4-18.57*ht^3+21.39*ht^2-11.71*ht+2.628,
                  -4.536*ht^5+27.16*ht^4-63.47*ht^3+72.43*ht^2-40.06*ht+8.778)
  110.2*R*r_cre/cre+2.93 }

# Utility #####
ntf <- function(x){mailR::send.mail(from = "shibataryohei@gmail.com",
                                    to = "shibataryohei@gmail.com",
                                    subject = "RStudio Notification",
                                    body = "Your R script completed",
                                    encoding = "utf-8",
                                    smtp = list(host.name = "smtp.gmail.com",
                                                port = 587,
                                                user.name = "shibataryohei@gmail.com",
                                                passwd = x,
                                                ssl = T),
                                    authenticate = TRUE,
                                    send = TRUE)}



# Correlation analysis #####

associate2 <- function(df1, df2){
  intersect(rownames(df1),
            rownames(df2)) -> id
  microbiome::associate(df1[id, , drop = FALSE],
                        df2[id, , drop = FALSE],
                        method = "spearman") %>% 
    dplyr::rename(Q.value = p.adj)} 

df.pheatmap <- function(tbw, X1, X2){
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

# Omics

omit_rare <- function(df, var, id, ratio){
  names(df)[names(df) == var] <- "variable"
  names(df)[names(df) == id] <- "id"
  
  df %>% 
    group_by(id) %>% 
    dplyr::summarise() %>% 
    nrow -> id_number
  
  df %>% 
    filter(Value == 0) %>% 
    group_by(variable) %>% 
    dplyr::summarise(Count= n()) %>% 
    filter(Count > id_number*(1-ratio)) %>% 
    .$variable -> variable_omit
  
  df %>% 
    ungroup %>% 
    filter(!variable %in% variable_omit) -> df2
  
  names(df2)[names(df2) == "variable"] <- var
  names(df2)[names(df2) == "id"] <- id
  
  df2}

arrange_rstatix <- function(df){
  df %>% 
    tbl_df %>% 
    dplyr::select(Variable, p) %>% 
    dplyr::rename(P.value = p) %>% 
    mutate(Q.value = q.value(P.value)) %>% 
    arrange(P.value)
}