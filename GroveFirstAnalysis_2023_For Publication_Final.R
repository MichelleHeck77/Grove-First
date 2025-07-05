#Heck et al. R Code for Phase 1 data analysis
# Load libraries ----
library(readxl)
library(dplyr)
library(tibble)
library(ggplot2)
library(ggpubr)

###Need to set working directory

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Import and aggregate data ----
## Fruit sorter data (n = 34,903) ----
fruit.sorter <- read_excel("Fruit Sorter Data 2024.xlsx")

# aggregate data at the tree level
fruit.sorter.aggregated <- fruit.sorter %>%
  filter(!is.na(MoleculeID)) %>%
  filter(MoleculeID != "Not Part of Trial") %>%
  group_by(Row, TreeID, MoleculeID, TreeRating) %>%
  summarise(AvgWeight = mean(Weight), NumFruit = n()) 
# n = 263
# note: Row 43, Tree ID 4316, M7 was lost by the sorting computer
# add that Tree manually
fruit.sorter.aggregated <- fruit.sorter.aggregated %>%
  ungroup() %>%
  add_row(TreeID = 4316, Row = 43, MoleculeID = "M7", TreeRating = "Beta",
          AvgWeight = NA, NumFruit = NA)


## Fruit yield quality data (n = 264) ----
fruit.yield.quality <- read_excel("Fruit Yield Quality 2024.xlsx")

fruit.yield.quality <- fruit.yield.quality %>%
  dplyr::select(-Tree, -PlotID)


## Fruit drop data (n = 264) ----
fruit.drop <- read_excel("Fruit Drop 2024.xlsx")


## Tree canopy density data (n = 264) ----
canopy.density <- read_excel("Canopy Density 2024.xlsx")

canopy.density <- canopy.density %>%
  dplyr::select(Row, TreeID, MoleculeID, Set, starts_with("Canopy_Density"))


## Tree canopy volume data (n = 264) ----
canopy.volume <- read_excel("Pre Harvest Canopy Volume 2024.xlsx")

canopy.volume <- canopy.volume %>%
  dplyr::select(Row, TreeID, MoleculeID, Set, TrunkDiameter_PreHarvest_mm, CanopyHeight_PreHarvest_m,
         CanopyVolume_PreHarvest_m3)

# merge canopy density & volume data
canopy.data <- merge(canopy.volume, canopy.density, 
                     by = c("Row", "TreeID", "MoleculeID", "Set"))


## Plant health index data (n = 264) ----
plant.health <- read_excel("Plant Health Index 2024 New.xlsx")

plant.health <- plant.health %>%
  dplyr::select(Set, Row, TreeID, MoleculeID, starts_with("East"), starts_with("West"))

# convert scores of 10 to 5 - 10 rating indicated tree almost dead so score as 5
plant.health <- plant.health %>%
  mutate(across(East_Q1_450:West_Q4_0, ~ ifelse(. == 10, 5, .)))

# calculate changes in health
plant.health <- plant.health %>%
  # deltas at quad level
  mutate(East_Q1_delta = East_Q1_0 - East_Q1_90, East_Q2_delta = East_Q2_0 - East_Q2_90,
         East_Q3_delta = East_Q3_0 - East_Q3_90, East_Q4_delta = East_Q4_0 - East_Q4_90,
         West_Q1_delta = West_Q1_0 - West_Q1_90, West_Q2_delta = West_Q2_0 - West_Q2_90,
         West_Q3_delta = West_Q3_0 - West_Q3_90, West_Q4_delta = West_Q4_0 - West_Q4_90,
         
         East_Q1_delta_180 = East_Q1_0 - East_Q1_180, East_Q2_delta_180 = East_Q2_0 - East_Q2_180,
         East_Q3_delta_180 = East_Q3_0 - East_Q3_180, East_Q4_delta_180 = East_Q4_0 - East_Q4_180,
         West_Q1_delta_180 = West_Q1_0 - West_Q1_180, West_Q2_delta_180 = West_Q2_0 - West_Q2_180,
         West_Q3_delta_180 = West_Q3_0 - West_Q3_180, West_Q4_delta_180 = West_Q4_0 - West_Q4_180,
         
         East_Q1_delta_270 = East_Q1_0 - East_Q1_270, East_Q2_delta_270 = East_Q2_0 - East_Q2_270,
         East_Q3_delta_270 = East_Q3_0 - East_Q3_270, East_Q4_delta_270 = East_Q4_0 - East_Q4_270,
         West_Q1_delta_270 = West_Q1_0 - West_Q1_270, West_Q2_delta_270 = West_Q2_0 - West_Q2_270,
         West_Q3_delta_270 = West_Q3_0 - West_Q3_270, West_Q4_delta_270 = West_Q4_0 - West_Q4_270,
         
         East_Q1_delta_450 = East_Q1_0 - East_Q1_450, East_Q2_delta_450 = East_Q2_0 - East_Q2_450,
         East_Q3_delta_450 = East_Q3_0 - East_Q3_450, East_Q4_delta_450 = East_Q4_0 - East_Q4_450,
         West_Q1_delta_450 = West_Q1_0 - West_Q1_450, West_Q2_delta_450 = West_Q2_0 - West_Q2_450,
         West_Q3_delta_450 = West_Q3_0 - West_Q3_450, West_Q4_delta_450 = West_Q4_0 - West_Q4_450) %>%
  # side total score
  mutate(East_0 = East_Q1_0 + East_Q2_0 + East_Q3_0 + East_Q4_0,
         West_0 = West_Q1_0 + West_Q2_0 + West_Q3_0 + West_Q4_0,
         East_90 = East_Q1_90 + East_Q2_90 + East_Q3_90 + East_Q4_90,
         West_90 = West_Q1_90 + West_Q2_90 + West_Q3_90 + West_Q4_90,
         East_180 = East_Q1_180 + East_Q2_180 + East_Q3_180 + East_Q4_180,
         West_180 = West_Q1_180 + West_Q2_180 + West_Q3_180 + West_Q4_180,
         East_270 = East_Q1_270 + East_Q2_270 + East_Q3_270 + East_Q4_270,
         West_270 = West_Q1_270 + West_Q2_270 + West_Q3_270 + West_Q4_270,
         East_450 = East_Q1_450 + East_Q2_450 + East_Q3_450 + East_Q4_450,
         West_450 = West_Q1_450 + West_Q2_450 + West_Q3_450 + West_Q4_450) %>%
  # tree total score
  mutate(Tree_0  = East_0 + West_0,
         Tree_90 = East_90 + West_90,
         Tree_180 = East_180 + West_180,
         Tree_270 = East_270 + West_270,
         Tree_450 = East_450 + West_450) %>%
  # deltas at side level
  mutate(East_delta_90 = East_0 - East_90,
         West_delta_90 = West_0 - West_90,
         East_delta_180 = East_0 - East_180,
         West_delta_180 = West_0 - West_180,
         East_delta_270 = East_0 - East_270,
         West_delta_270 = West_0 - West_270,
         East_delta_450 = East_0 - East_450,
         West_delta_450 = West_0 - West_450) %>%
  # deltas at tree level
  mutate(Tree_delta_90 = Tree_0 - Tree_90,
         Tree_delta_180 = Tree_0 - Tree_180,
         Tree_delta_270 = Tree_0 - Tree_270,
         Tree_delta_450 = Tree_0 - Tree_450)


## Merge files ----
merge1 <- merge(fruit.sorter.aggregated, fruit.yield.quality, by = c("Row", "TreeID", "MoleculeID"), 
                  all = TRUE)
merge2 <- merge(merge1, fruit.drop, by = c("Row", "TreeID", "MoleculeID", "Set"))
merge3 <- merge(merge2, canopy.data, by = c("Row", "TreeID", "MoleculeID", "Set"))
fulldata <- merge(merge3, plant.health, by = c("Row", "TreeID", "MoleculeID", "Set"))
# n = 264


# Main Figures ----
## Figures 2&3 ----

### Figs 2&3 Panel a create treatment indicator OTC usable right away ----
# create a dataframe with average phenotype variables at the treatment level (for all figure panels in 2&3)
phenotype.data <- fulldata %>%
  group_by(MoleculeID) %>%
  summarise(Brix = mean(AvgBrix, na.rm = T),
            TSS_TA_Ratio = mean(TSS_TA_Ratio, na.rm = T),
            JuiceVolumeCorrected_mL = mean(JuiceVolumeCorrected_mL, na.rm = T),
            Tree_delta_180 = mean(Tree_delta_180, na.rm = T),
            Solid_perBox = mean(Solid_perBox, na.rm = T), 
            TotalFruitDrop = mean(TotalFruitDrop, na.rm = T))


### Fig 2a Tree health, Fruit drop and Solids per box ----
phenotype.data <- phenotype.data %>%
  mutate(trmt.ind.single.a = "1" ) %>%
  mutate(trmt.ind.single.a = ifelse(MoleculeID %in% c("M53", "M10", "M1", "M55", "M58", "M68", "M66", "M65", "M63", "M62"), "2", trmt.ind.single.a)) %>%
  mutate(trmt.ind.separate.a = ifelse(MoleculeID %in% c("M53", "M10", "M1", "M55", "M58", "M68", "M66", "M65", "M63", "M62"), MoleculeID, trmt.ind.single.a))

ggplot(phenotype.data, aes(Tree_delta_180, TotalFruitDrop, 
                           size = Solid_perBox, color = trmt.ind.separate.a)) +
  geom_point(aes(alpha = trmt.ind.single.a)) +
  guides(size = guide_legend(title = "Solids per box (lbs)")) +
  guides(color = guide_legend(title = "Treatment")) +
  theme_bw() +
  scale_alpha_discrete(range = c(0.2, 0.8))

#### Fig 3a Brix, Acidity & Juice Vol plots ----
ggplot(phenotype.data, aes(Brix, TSS_TA_Ratio, size = JuiceVolumeCorrected_mL,
                           color = trmt.ind.separate.a)) +
  geom_point(aes(alpha = trmt.ind.single.a)) +
  guides(size = guide_legend(title = "Juice Volume (mL)")) +
  guides(color = guide_legend(title = "Treatment")) +
  theme_bw() +
  scale_alpha_discrete(range = c(0.2, 0.8))


### Figs 2&3 Panel b create treatment indicator all other antibiotics ----
phenotype.data <- phenotype.data %>%
  mutate(trmt.ind.single.b = "1" ) %>%
  mutate(trmt.ind.single.b = ifelse(MoleculeID %in% c("M15", "M12", "M20", "M16", "M29", "M34", "M36", "M41"), "2", trmt.ind.single.b)) %>%
  mutate(trmt.ind.separate.b = ifelse(MoleculeID %in% c("M15", "M12", "M20", "M16", "M29","M34", "M36", "M41"), MoleculeID, trmt.ind.single.b))

#### Fig 2b Tree health, Fruit drop and Solids per box ----
ggplot(phenotype.data, aes(Tree_delta_180, TotalFruitDrop, 
                           size = Solid_perBox, color = trmt.ind.separate.b)) +
  geom_point(aes(alpha = trmt.ind.single.b)) +
  guides(size = guide_legend(title = "Fruit Weight (kg)")) +
  guides(color = guide_legend(title = "Treatment")) +
  theme_bw() +
  scale_alpha_discrete(range = c(0.2, 0.8))

#### Fig 3b Brix, Acidity & Juice Vol plots ----
ggplot(phenotype.data, aes(Brix, TSS_TA_Ratio, size = JuiceVolumeCorrected_mL,
                           color = trmt.ind.separate.b)) +
  geom_point(aes(alpha = trmt.ind.single.b)) +
  guides(size = guide_legend(title = "Juice Volume (mL)")) +
  guides(color = guide_legend(title = "Treatment")) +
  theme_bw() +
  scale_alpha_discrete(range = c(0.2, 0.8))


### Figs 2&3 Panel c create treatment indicator Essential Oils ----
phenotype.data <- phenotype.data %>%
  mutate(trmt.ind.single.c = "1" ) %>%
  mutate(trmt.ind.single.c = ifelse(MoleculeID %in% c("M49", "M43", "M48", "M47", "M52", "M51", "M57", "M59", "M54", "M78"), "2", trmt.ind.single.c)) %>%
  mutate(trmt.ind.separate.c = ifelse(MoleculeID %in% c("M49", "M43", "M48", "M47", "M52", "M51", "M57", "M59", "M54", "M78"), MoleculeID, trmt.ind.single.c))

#### Fig 2c Tree health, Fruit drop and Solids per box ----
ggplot(phenotype.data, aes(Tree_delta_180, TotalFruitDrop, 
                           size = Solid_perBox, color = trmt.ind.separate.c)) +
  geom_point(aes(alpha = trmt.ind.single.c)) +
  guides(size = guide_legend(title = "Fruit Weight (kg)")) +
  guides(color = guide_legend(title = "Treatment")) +
  theme_bw() +
  scale_alpha_discrete(range = c(0.2, 0.8))

#### Fig 3c Brix, Acidity & Juice Vol plots ----
ggplot(phenotype.data, aes(Brix, TSS_TA_Ratio, size = JuiceVolumeCorrected_mL,
                           color = trmt.ind.separate.c)) +
  geom_point(aes(alpha = trmt.ind.single.c)) +
  guides(size = guide_legend(title = "Juice Volume (mL)")) +
  guides(color = guide_legend(title = "Treatment")) +
  theme_bw() +
  scale_alpha_discrete(range = c(0.2, 0.8))


### Figs 2&3 Panel d create treatment indicator Fertilizers ----
phenotype.data <- phenotype.data %>%
  mutate(trmt.ind.single.d = "1" ) %>%
  mutate(trmt.ind.single.d = ifelse(MoleculeID %in% c("M13", "M14", "M42", "M46", "M56", "M67", "M70", "M61", "M71", "M83"), "2", trmt.ind.single.d)) %>%
  mutate(trmt.ind.separate.d = ifelse(MoleculeID %in% c("M13", "M14", "M42", "M46", "M56", "M67", "M70", "M61", "M71", "M83"), MoleculeID, trmt.ind.single.d))

#### Fig 2d Tree health, Fruit drop and Solids per box ----
ggplot(phenotype.data, aes(Tree_delta_180, TotalFruitDrop, 
                           size = Solid_perBox, color = trmt.ind.separate.d)) +
  geom_point(aes(alpha = trmt.ind.single.d)) +
  guides(size = guide_legend(title = "Fruit Weight (kg)")) +
  guides(color = guide_legend(title = "Treatment")) +
  theme_bw() +
  scale_alpha_discrete(range = c(0.2, 0.8))

#### Fig 3d Brix, Acidity & Juice Vol plots ----
ggplot(phenotype.data, aes(Brix, TSS_TA_Ratio, size = JuiceVolumeCorrected_mL,
                           color = trmt.ind.separate.d)) +
  geom_point(aes(alpha = trmt.ind.single.d)) +
  guides(size = guide_legend(title = "Juice Volume (mL)")) +
  guides(color = guide_legend(title = "Treatment")) +
  theme_bw() +
  scale_alpha_discrete(range = c(0.2, 0.8))


### Figs 2&3 Panel e create treatment indicator for Phase 2 Treatments ----
phenotype.data <- phenotype.data %>%
  mutate(trmt.ind.single.e = "1" ) %>%
  mutate(trmt.ind.single.e = ifelse(MoleculeID %in% c("M53", "M10", "M20", "M42", "M54", "M47", "M75", "M73", "M49"), "2", trmt.ind.single.e)) %>%
  mutate(trmt.ind.separate.e = ifelse(MoleculeID %in% c("M53", "M10", "M20", "M42", "M54", "M47", "M75", "M73", "M49"), MoleculeID, trmt.ind.single.e))

#### Fig 2e Tree health, Fruit drop and Solids per box ----
ggplot(phenotype.data, aes(Tree_delta_180, TotalFruitDrop, 
                           size = Solid_perBox, color = trmt.ind.separate.e)) +
  geom_point(aes(alpha = trmt.ind.single.e)) +
  guides(size = guide_legend(title = "Fruit Weight (kg)")) +
  guides(color = guide_legend(title = "Treatment")) +
  theme_bw() +
  scale_alpha_discrete(range = c(0.2, 0.8))

#### Fig 3e Brix, Acidity & Juice Vol plots ----
ggplot(phenotype.data, aes(Brix, TSS_TA_Ratio, size = JuiceVolumeCorrected_mL,
                           color = trmt.ind.separate.e)) +
  geom_point(aes(alpha = trmt.ind.single.e)) +
  guides(size = guide_legend(title = "Juice Volume (mL)")) +
  guides(color = guide_legend(title = "Treatment")) +
  theme_bw() +
  scale_alpha_discrete(range = c(0.2, 0.8))


## Figure 4 ----
# filter data for fruit weight > 200g and remove "Not Part of Trial"
filtered_data <- fruit.sorter %>%
  filter(Weight > 200, MoleculeID != "Not Part of Trial") %>%
  group_by(MoleculeID) %>%
  summarise(NumFruit = n(), .groups = 'drop')

# compute total number of fruit per Molecule ID for ordering
molecule_order <- filtered_data %>%
  arrange(desc(NumFruit)) %>%
  pull(MoleculeID)

# reorder the Molecule IDs based on total fruit count
filtered_data$MoleculeID <- factor(filtered_data$MoleculeID, levels = molecule_order)

# define colors: Highlight M10, M53, M57, and M20; others black
highlight_molecules <- c("M10", "M53", "M57", "M20")
filtered_data$Color <- ifelse(filtered_data$MoleculeID %in% highlight_molecules, 
                              filtered_data$MoleculeID, "Other")
color_palette <- c("M10" = "red", "M53" = "blue", "M57" = "green", "M20" = "purple", "Other" = "black")


### Fig 4a - Pareto chart for fruit >200g ----
# Create the stacked bar graph with custom colors
ggplot(filtered_data, aes(x = MoleculeID, y = NumFruit, fill = Color)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = color_palette) + 
  labs(x = "Molecule ID", y = "Number of Fruit > 200g") +
  theme_minimal() +
  theme(legend.position = "none",  
    axis.text.x = element_text(size = 8, angle = 90, hjust = 1, vjust = 0.5),  
    axis.title.x = element_text(margin = margin(t = 10))) + 
  coord_cartesian(clip = "off")  

### Fig 4b Box plot comparing Fruit Average Weight values for selected ----
ggplot(fulldata %>% filter(MoleculeID %in% c("M10","M20","M57","M53")), aes(x = MoleculeID, y = AvgWeight, fill = MoleculeID)) +
  geom_boxplot() +
  labs(title = "Average Weight", x = "Treatment (MoleculeID)", y = "Average Weight") +
  theme_minimal()

### Fig 4c Box plot comparing boxes per acre values for selected ----
ggplot(fulldata %>% filter(MoleculeID %in% c("M10","M20","M57","M53")), aes(x = MoleculeID, y = Box_perAcre, fill = MoleculeID)) +
  geom_boxplot() +
  labs(title = "Boxes per Acre", x = "Treatment (MoleculeID)", y = "Boxes per Acre") +
  theme_minimal()

### Fig 4d Box plot comparing pounds solid per box for selected ----
ggplot(fulldata %>% filter(MoleculeID %in% c("M10","M20","M57","M53")), aes(x = MoleculeID, y = Solid_perBox, fill = MoleculeID)) +
  geom_boxplot() +
  labs(title = "Pounds Solid Per Box", x = "Treatment (MoleculeID)", y = "Lbs Solid") +
  theme_minimal()

### Fig 4e Box plot comparing Fruit Brix values for selected ----
ggplot(fulldata %>% filter(MoleculeID %in% c("M10","M20","M57","M53")), aes(x = MoleculeID, y = AvgBrix, fill = MoleculeID)) +
  geom_boxplot() +
  labs(title = "Average Brix", x = "Treatment (MoleculeID)", y = "Average Brix") +
  theme_minimal()

### Fig 4f Box plot comparing TSS:TA ratio values for selected ----
ggplot(fulldata %>% filter(MoleculeID %in% c("M10","M20","M57","M53")), aes(x = MoleculeID, y = TSS_TA_Ratio, fill = MoleculeID)) +
  geom_boxplot() +
  labs(title = "TSS:TA Ratio", x = "Treatment (MoleculeID)", y = "Ratio") +
  theme_minimal()

### Fig 4g Box plot comparing % juice for selected ----
ggplot(fulldata %>% filter(MoleculeID %in% c("M10","M20","M57","M53")), aes(x = MoleculeID, y = PctJuice, fill = MoleculeID)) +
  geom_boxplot() +
  labs(title = "% Juice", x = "Treatment (MoleculeID)", y = "% Juice") +
  theme_minimal()


## Figure 5 ----
# Hierarchical clustering at the molecule level with selected phenotypes
MID.variables <- fulldata %>%
  group_by(MoleculeID) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE) %>%
  dplyr::select(MoleculeID, AvgWeight, NumFruit, AvgBrix, Tree_delta_180, Tree_delta_90, TotalFruitDrop,
         CanopyVolume_PreHarvest_m3, CanopyHeight_PreHarvest_m, Solid_perBox, PctJuice) %>%
  column_to_rownames(var = "MoleculeID") 

MID.variables.r <- cor(as.matrix(MID.variables), use = "pairwise.complete.obs")

d2 <- dist(scale(MID.variables), method = "euclidean")
hc2 <- hclust(d2, method = "complete")
plot(hc2, cex = 0.6, main = "Complete Linkage", sub = "", xlab = "", hang = -1)
rect.hclust(hc2, k = 10, border = 2:5)
cutree(hc2, k = 10)


# Supplementary Figures ----
## Fig S1a Tree rating by row ----
rating.data <- as.data.frame(prop.table(table(Row = fulldata$Row, Rating = fulldata$TreeRating), m = 1))

ggplot(rating.data, aes(Row, Freq, fill = Rating)) +
  theme_bw() +
  geom_bar(stat = "identity") +
  xlab("Row") + ylab("Percent")

## Fig S1b Number of fruit per row by Tree Rating w legend ----
ggplot(fulldata, aes(as.factor(Row), NumFruit)) +
  geom_boxplot(outlier.shape = NA) +
  theme_bw() +
  geom_jitter(aes(color = TreeRating), alpha = 0.6, width = 0.2) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8)) +
  xlab("Row") + ylab("Number of Fruit") 

## Fig S1c Avg fruit weight per row ----
ggplot(fulldata, aes(as.factor(Row), AvgWeight)) +
  geom_boxplot(outlier.shape = NA) +
  theme_bw() +
  geom_jitter(aes(color = TreeRating), alpha = 0.6, width = 0.2) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8)) +
  xlab("Row") + ylab("Average Fruit Weight") 

### Fig S5a Plant health index Day 0 to 90----
fulldata <- fulldata %>%
  mutate(TreatmentID = substring(MoleculeID, 2, nchar(MoleculeID)))

ggplot(fulldata, aes(reorder(TreatmentID, Tree_delta_90, median, na.rm = T), Tree_delta_90, fill = Set)) +
  geom_abline(slope = 0, intercept = 0, color = "red", size = 1.2) +
  geom_boxplot() +
  geom_point(alpha = 0.5) +
  #  geom_abline(slope = 0, intercept = 0, color = "red", size = 1.2) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8)) +
  xlab("Molecule ID") + ylab("Change from 0 - 90") +
  coord_cartesian(ylim = c(-15, 15))

## Fig S5b Plant health index Day 0 to 180 ----
ggplot(fulldata, aes(reorder(TreatmentID, Tree_delta_180, median, na.rm = T), Tree_delta_180, fill = Set)) +
  geom_abline(slope = 0, intercept = 0, color = "red", size = 1.2) +
  geom_boxplot() +
  geom_point(alpha = 0.5) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8)) +
  xlab("Molecule ID") + ylab("Change from 0 - 180") +
  coord_cartesian(ylim = c(-15, 15))

## Fig S5c Plant health index Day 0 to 270 ----
ggplot(fulldata, aes(reorder(TreatmentID, Tree_delta_270, median, na.rm = T), Tree_delta_270, fill = Set)) +
  geom_abline(slope = 0, intercept = 0, color = "red", size = 1.2) +
  geom_boxplot() +
  geom_point(alpha = 0.5) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8)) +
  xlab("Molecule ID") + ylab("Change from 0 - 270") +
  coord_cartesian(ylim = c(-15, 15))


## Fig S7a Plant health vs Tree rating Day 90 ----
ggplot(fulldata, aes(TreeRating, Tree_delta_90)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.6, size = 2, width = 0.2) +
  xlab("Tree Rating") + ylab("Change from 0 - 90") +
  coord_cartesian(ylim = c(-15, 15))

## Fig S7b Plant health vs Tree rating Day 180 ----
ggplot(fulldata, aes(TreeRating, Tree_delta_180)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.6, size = 2, width = 0.2) +
  xlab("Tree Rating") + ylab("Change from 0 - 180") +
  coord_cartesian(ylim = c(-15, 15))

## Fig S7c Plant health vs Tree rating Day 270 ----
ggplot(fulldata, aes(TreeRating, Tree_delta_270)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.6, size = 2, width = 0.2) +
  xlab("Tree Rating") + ylab("Change from 0 - 270") +
  coord_cartesian(ylim = c(-15, 15))


## Fig S8a Percent Fruit Drop by MID colored by TreeRating ----
ggplot(fulldata, aes(reorder(MoleculeID, PctFruitDrop, median, na.rm = T), PctFruitDrop, fill = TreeRating)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8)) +
  xlab("Molecule ID") + ylab("Percent Fruit Drop")

## Fig S8b Percent Fruit Drop by MID colored by TreeRating sorted by SD ----
ggplot(fulldata, aes(reorder(TreatmentID, PctFruitDrop, sd, na.rm = T), PctFruitDrop, fill = TreeRating)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8)) +
  xlab("Molecule ID") + ylab("Percent Fruit Drop")

## Fig S9a Day 90 Number of Total fruit (sorted only) vs change in plant health index ----
ggplot(fulldata, aes(Tree_delta_90, TotalSorterFruit)) +
  theme_bw() +
  geom_smooth(method = "lm", se = F, color = "darkgrey", linewidth = 0.8) + 
  geom_jitter(aes(color = "Black"), width = 0.2, alpha = 0.6, size = 2) +
  xlab("Change from 0 - 90") + ylab("Number of Fruit") +
  scale_x_continuous(limits = c(-15, 10)) +
  stat_cor(label.x = -15, label.y = 450, p.accuracy = 0.001)

## Fig S9a Day 180 Number of Total fruit (sorted only) vs change in plant health index ----
ggplot(fulldata, aes(Tree_delta_180, TotalSorterFruit)) +
  theme_bw() +
  geom_smooth(method = "lm", se = F, color = "darkgrey", linewidth = 0.8) + 
  geom_jitter(aes(color = "Black"), width = 0.2, alpha = 0.6, size = 2) +
  xlab("Change from 0 - 180") + ylab("Number of Fruit") +
  scale_x_continuous(limits = c(-15, 10)) +
  stat_cor(label.x = -15, label.y = 450, p.accuracy = 0.001)

## Fig S9a Day 270 Number of Total fruit (sorted only) vs change in plant health index ----
ggplot(fulldata, aes(Tree_delta_270, TotalSorterFruit)) +
  geom_smooth(method = "lm", se = F, color = "darkgrey", linewidth = 0.8) + 
  theme_bw() +
  geom_jitter(aes(color = "Black"), width = 0.2, alpha = 0.6, size = 2) +
  xlab("Change from 0 - 270") + ylab("Number of Fruit") +
  scale_x_continuous(limits = c(-15, 10)) +
  stat_cor(label.x = -15, label.y = 450, p.accuracy = 0.001)

## Fig S9b Day 0 to 90 Fruit Weight x Plant Health Index ----
ggplot(fulldata, aes(Tree_delta_90, AvgWeight)) +
  theme_bw() +
  geom_smooth(method = "lm", se = F, color = "darkgrey", linewidth = 0.8) +
  geom_jitter(aes(color = "Black"), width = 0.2, alpha = 0.6, size = 2) +
  xlab("Change from 0 - 90") + ylab("Average Fruit Weight") +
  scale_x_continuous(limits = c(-15, 10)) +
  stat_cor(label.x = -15, label.y = 180, p.accuracy = 0.001)

## Fig S9b Day 0 to 180 Fruit Weight x Plant Health Index ----
ggplot(fulldata, aes(Tree_delta_180, AvgWeight)) +
  theme_bw() +
  geom_smooth(method = "lm", se = F, color = "darkgrey", linewidth = 0.8) +
  geom_jitter(aes(color = "Black"), width = 0.2, alpha = 0.6, size = 2) +
  xlab("Change from 0 - 180") + ylab("Average Fruit Weight") +
  scale_x_continuous(limits = c(-15, 10)) +
  stat_cor(label.x = -15, label.y = 180, p.accuracy = 0.001)

## Fig S9b Day 0 to 270 Fruit Weight x Plant Health Index ----
ggplot(fulldata, aes(Tree_delta_270, AvgWeight)) +
  theme_bw() +
  geom_smooth(method = "lm", se = F, color = "darkgrey", linewidth = 0.8) +
  geom_jitter(aes(color = "Black"), width = 0.2, alpha = 0.6, size = 2) +
  xlab("Change from 0 - 270") + ylab("Average Fruit Weight") +
  scale_x_continuous(limits = c(-15, 10)) +
  stat_cor(label.x = -15, label.y = 180, p.accuracy = 0.001)


## Fig S10a Number of fruit by MID (includes sorted, dropped and skirted fruit) ----
## trees were skirted and # fruit on skirted branches were counted and referred to ask skirted fruit
ggplot(fulldata, aes(reorder(TreatmentID, TotalFruit, median, na.rm = T), TotalFruit)) +
  geom_boxplot() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8)) +
  xlab("Molecule ID") + ylab("Number of Fruit")

## Fig S10b Avg fruit weight vs number of fruit ----
ggplot(fulldata, aes(reorder(TreatmentID, TotalFruit, median, na.rm = T), AvgWeight)) +
  geom_boxplot() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8)) +
  xlab("Molecule ID") + ylab("Avg Fruit Weight (on Tree)")

## Fig S10c Avg fruit weight vs number of fruit without color ----
ggplot(fulldata, aes(TotalFruit, AvgWeight)) +
  geom_point(alpha = 0.4) +
  theme_bw() +
  theme(legend.position = "none") +
  geom_smooth(method = "lm", linewidth = 0.8, color = "black") +
  xlab("Number of Fruit") + ylab("Average Fruit Weight") +
  stat_cor(label.x = 300, label.y = 175, p.accuracy = 0.001)


## Fig S11a Average Brix ----
ggplot(fulldata, aes(reorder(TreatmentID, TSS_TA_Ratio, median, na.rm = T), AvgBrix)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8)) +
  xlab("Molecule ID") + ylab("Average Brix")

## Fig S11b TSS_TA_Ratio ----
ggplot(fulldata, aes(reorder(TreatmentID, TSS_TA_Ratio, median, na.rm = T), TSS_TA_Ratio)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8)) +
  xlab("Molecule ID") + ylab("TSS TA Ratio")


# Tables S2-6 ----
# Note: M53 - OTC, pH 2 standard treatment

## Table S2 Pairwise t-tests for fruit drop between each treatment and M53 ----
# Welch's ANOVA (doesn't assume equal variances)
oneway.test(TotalFruitDrop ~ MoleculeID, var.equal = F, data = fulldata)

# Fruit drop values for M53
FruitDrop_M53 <- fulldata %>%
  filter(MoleculeID == "M53") %>%
  dplyr::select(TotalFruitDrop)

# t-tests vs M53
FruitDrop_tests <- fulldata %>%
  filter(MoleculeID != "M53") %>%
  group_by(MoleculeID) %>% 
  summarise(avg_FruitDrop = round(mean(TotalFruitDrop), 2),
            p_value = round(t.test(FruitDrop_M53, TotalFruitDrop)$p.value, 4)) %>%
  mutate(avg_FruitDrop_M53 = round(mean(FruitDrop_M53$TotalFruitDrop),2)) %>%
  mutate(delta = avg_FruitDrop - avg_FruitDrop_M53) %>%
  dplyr::select(MoleculeID, avg_FruitDrop, avg_FruitDrop_M53, delta, p_value) %>%
  arrange(p_value) %>%
  View()

## Table S3 Pairwise t-tests for yield between treatments and M53 ----
# Welch's ANOVA (doesn't assume equal variances)
oneway.test(TotalFruit ~ MoleculeID, var.equal = F, data = fulldata)

# Yield values for M53
TotalFruit_M53 <- fulldata %>%
  filter(MoleculeID == "M53") %>%
  dplyr::select(TotalFruit)

# t-tests vs M53
TotalFruit_tests <- fulldata %>%
  filter(MoleculeID != "M53") %>%
  group_by(MoleculeID) %>% 
  summarise(avg_TotalFruit = round(mean(TotalFruit), 2),
            p_value = round(t.test(TotalFruit_M53, TotalFruitDrop)$p.value, 4)) %>%
  mutate(avg_TotalFruit_M53 = round(mean(TotalFruit_M53$TotalFruit),2)) %>%
  mutate(delta = avg_TotalFruit - avg_TotalFruit_M53) %>%
  dplyr::select(MoleculeID, avg_TotalFruit, avg_TotalFruit_M53, delta, p_value) %>%
  arrange(p_value) %>%
  View()

## Table S4 Pairwise t-tests for fruit weight between each treatment and M53 ----
# Welch's ANOVA (doesn't assume equal variances)
oneway.test(AvgWeight ~ MoleculeID, var.equal = F, data = fulldata)

# Fruit weight for M53
FruitWeight_M53 <- fulldata %>%
  filter(MoleculeID == "M53") %>%
  dplyr::select(AvgWeight)

# t-tests vs M53
FruitWeight_tests <- fulldata %>%
  filter(MoleculeID != "M53") %>%
  group_by(MoleculeID) %>% 
  summarise(avg_FruitWeight = round(mean(AvgWeight), 2),
            p_value = round(t.test(FruitWeight_M53, AvgWeight)$p.value, 4)) %>%
  mutate(avg_FruitWeight_M53 = round(mean(FruitWeight_M53$AvgWeight),2)) %>%
  mutate(delta = avg_FruitWeight - avg_FruitWeight_M53) %>%
  dplyr::select(MoleculeID, avg_FruitWeight, avg_FruitWeight_M53, delta, p_value) %>%
  arrange(p_value) %>%
  View()

## Table S5 Pairwise t-tests for Brix between each Treatment and M53 ----
# Welch's ANOVA (doesn't assume equal variances)
oneway.test(AvgBrix ~ MoleculeID, var.equal = F, data = fulldata)

# Brix values for M53
Brix_M53 <- fulldata %>%
  filter(MoleculeID == "M53") %>%
  dplyr::select(AvgBrix)

# t-tests vs M53
Brix_tests <- fulldata %>%
  filter(MoleculeID != "M53") %>%
  group_by(MoleculeID) %>% 
  summarise(avg_Brix = round(mean(AvgBrix), 2),
            p_value = round(t.test(Brix_M53, AvgBrix)$p.value, 4)) %>%
  mutate(avg_Brix_M53 = round(mean(Brix_M53$AvgBrix),2)) %>%
  mutate(delta = avg_Brix - avg_Brix_M53) %>%
  dplyr::select(MoleculeID, avg_Brix, avg_Brix_M53, delta, p_value) %>%
  arrange(p_value) %>%
  View()

## Table S6 Pairwise t-test comparison for Solids per box between each treatment and M53 ----
# Welch's ANOVA (doesn't assume equal variances)
oneway.test(Solid_perBox ~ MoleculeID, var.equal = F, data = fulldata)

Solid_perBox_M53 <- fulldata %>%
  filter(MoleculeID == "M53") %>%
  dplyr::select(Solid_perBox)

# t-tests vs M53
Solid_perBox_tests <- fulldata %>%
  filter(MoleculeID != "M53") %>%
  group_by(MoleculeID) %>% 
  summarise(avg_Solid_perBox = round(mean(Solid_perBox), 2),
            p_value = round(t.test(Solid_perBox_M53, Solid_perBox)$p.value, 4)) %>%
  mutate(avg_Solid_perBox_M53 = round(mean(Solid_perBox_M53$Solid_perBox),2)) %>%
  mutate(delta = avg_Solid_perBox - avg_Solid_perBox_M53) %>%
  dplyr::select(MoleculeID, avg_Solid_perBox, avg_Solid_perBox_M53, delta, p_value) %>%
  arrange(p_value) %>%
  View()

