install.packages("omu")
library(omu)


 data("c57_nos2KO_mouse_countDF") # 668 metabolites
 data("c57_nos2KO_mouse_metadata")
 
 c57_nos2KO_mouse_metadata
 
 c57_nos2KO_mouse_countDF 
   
# Assiging Hierarchical Class Data
assign_hierarchy(count_data = c57_nos2KO_mouse_countDF,
                 keep_unknowns = TRUE,
                 identifier = "KEGG") -> DF # KEGGがわかっていればClass/Subclassを取得できる
 
 
 DF <- assign_hierarchy(count_data = c57_nos2KO_mouse_countDF, keep_unknowns = TRUE, identifier = "KEGG")
 
 
# Modeling with Univariate Statistics
 omu_summary(count_data = DF,
             metadata = c57_nos2KO_mouse_metadata,
             numerator = "Strep",
             denominator = "Mock",
             response_variable = "Metabolite",
             Factor = "Treatment",
             log_transform = TRUE,
             p_adjust = "BH") -> DF_stats
 
omu_anova(count_data = c57_nos2KO_mouse_countDF,
          metadata = c57_nos2KO_mouse_metadata,
          response_variable = "Metabolite",
          var1 = "Background",
          var2 = "Treatment",
          interaction = TRUE,
          log_transform = TRUE,
          p_adjust = "BH") -> DF_anova

factor(paste0(c57_nos2KO_mouse_metadata$Background,
              c57_nos2KO_mouse_metadata$Treatment)) -> c57_nos2KO_mouse_metadata$Grouped

omu_summary(count_data = c57_nos2KO_mouse_countDF,
            metadata = c57_nos2KO_mouse_metadata,
            numerator = "WTStrep",
            denominator = "WTMock",
            response_variable = "Metabolite",
            Factor = "Grouped",
            log_transform = TRUE,
            p_adjust = "BH") -> DF_stats_grouped

# Gathering Functional Orthology and Gene Data
# dplyrはclassが変わってしまうため使えない
DF_stats_sub <- subset(DF_stats, Class=="Organic acids")
DF_stats_sub <- DF_stats_sub[which(DF_stats_sub[,"padj"] <= 0.05),]

DF_stats_sub_KO <- KEGG_gather(DF_stats_sub)

DF_stats_sub[1:10, 1:5]
DF_stats_sub %>% names

DF_stats_sub_KO[1:10, 1:5]
assign_hierarchy(count_data = DF_stats_sub_KO,
                 keep_unknowns = TRUE,
                 identifier = "KO_Number") -> DF_stats_sub_KO
