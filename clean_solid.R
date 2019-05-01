library(readxl)

solid_xlsx = "ProXe_NIBRX_models_annotations_20190329_sk_xl2011.xlsx"

## read in solid metadata as solid_meta

solid_meta <- readxl::read_excel(
  file.path(data_outside_app_dir, solid_xlsx),
  sheet = "Header_Data"
)

solid <- readxl::read_excel(
  file.path(data_outside_app_dir, solid_xlsx),
  sheet = 1
)

match_vector = c("MODELID" = "PDX_Name",  
  "HISTOLOGY"="COSMIC_Type",
  "PRIMARY_SITE"="COSMIC_Primary_Site", 
  "HIST_SUBTYPE1"="COSMIC_Subtype")

# swap solid_meta$Internal_Column_Header names and colnames(solid)

for (name in names(match_vector)) {
  if (name %in% colnames(solid)){
    colnames(solid)[colnames(solid) == name] <- match_vector[name]
  }
  if (name %in% solid_meta$Internal_Column_Header) {
    solid_meta$Internal_Column_Header[solid_meta$Internal_Column_Header == name] <- match_vector[name]
  }
}

# change sample prefix
solid$PDX_Name <- gsub("^", "NIBR-", solid$PDX_Name)


# add availability column manually
solid$Distribution_Permissions <- "academic, industry-sponsored academic, and industry"


##### ---- RNA-seq data manipulation for plotting  ------ #####


# TODO: manipulate this, create a graph out of it,
# and also fold it in (link too?) with 'solid' df above through a merge.
gao_rna <- readxl::read_excel(
  file.path(data_outside_app_dir, "Gao_et_al_PDXs_suppl_table_Nat_Med_2015.xlsx"),
  sheet = "RNAseq_fpkm"
)
gao_rna <- as.data.frame(gao_rna)
rownames(gao_rna) <- gao_rna$Sample
gao_rna$Sample <- NULL
library(pheatmap)
# change sample prefix
colnames(gao_rna) <- gsub("X-", "NIBR-", colnames(gao_rna))

# remove genes that are all zero
rows_to_keep <- apply(gao_rna, 1, function(row) {
  sum(row) > 0
})
gao_rna <- gao_rna[rows_to_keep, ]

# # keep only genes with rpkm > 5 in > 20 samples
# rows_to_keep <- apply(gao_rna,1,function(row){
#   sum(row > 5) > 20
# })
# gr_filt <- gao_rna[rows_to_keep,]
#
# # calculate variance/mean ratio
# if(anyNA(gr_filt)) stop("NAs in gao_rna filtered")
# var_mean_ratio_sort <- sort(
#   apply(gr_filt,1,function(row){
#     var(row) / mean(row)
#   })
# , decreasing = FALSE)

# create a log2-scaled gr
# grfl <- log2(gr_filt + 0.01)

# create z-scores = (value - mean) / sqrt(variance)
# grfz <- t(scale(t(gr_filt)))

# take top X by var/mean ratio
# X = 500
# grfz_topX <- grfz[names(var_mean_ratio_sort)[1:X],]

# # filter for one type
# grfz_topX_type = grfz_topX[,
#   solid[na.omit(solid$COSMIC_Primary_Site=="pancreas" & solid$COSMIC_Type=="carcinoma"),]$PDX_Name
# ]

# plot
# breaksList = seq(floor(min(grfz)), -floor(min(grfz)),length.out=length(my_palette))
# pheatmap::pheatmap(grfz_topX_type,
#   color = my_palette,
#   breaks = breaksList
# )

## TODO:
# add heatmap plot to app.
# plot metadata on the side.
# maybe make interactive



# look at stats
# sums are different, thus these are probably FPKM/RPKM
# range(colSums(gao_rna)) # 260k - 905k
# compute a mean of all samples for comparison.
# min(gao_rna[gao_rna > 0]) # 0.01 across all samples
# gao_rna_pc <- gao_rna + 1
# gao_meds <- apply(gao_rna_pc,1,median)
# gao_fc <- sweep(gao_rna_pc,MARGIN = 1,STATS = gao_meds, FUN = "/")
# grp_lfc <- log2(gao_fc)
#   # determine number of genes >2^6-fold up
# up <- apply(grp_lfc,2,function(col){
#   sum(col > 6)
# })
# grp <- data.frame( Sample = names(up), num_genes_64x_up = up, stringsAsFactors = F)
#   # which genes
# grp$genes_64x_up = apply(grp_lfc,2,function(col){
#   paste0(names(col[col>6]),collapse=" | ")
# })
#   # determine number of genes <2^5-fold down
# grp$num_genes_16x_dn <- apply(grp_lfc,2,function(col){
#   sum(col < -4)
# })
# # which genes
# grp$genes_16x_dn = apply(grp_lfc,2,function(col){
#   paste0(names(col[col < -4]),collapse=" | ")
# })

# Ideas for more columns
# 1. take top and bottom 10 genes, show
# 2. take genes above threshold that cross-reference with oncogene list (already in app)
# 3. Compute most commonly-changed genes
#

# Ideas for plots:
# 1. PCA
# 2. correlation heatmaps.

if (F) {
  # try PCA
  # ok to use raw rpkm values?
  gpca <- t(gao_rna)
  # remove constant cols
  gpca <- gpca[, apply(gpca, 2, var) != 0]
  gpca_prcomp <- prcomp(gpca, scale = TRUE) # note sure whether I'm supposed to be transposing. Think this is right.
  plot(gpca_prcomp)
  summary(gpca_prcomp)
  biplot(gpca_prcomp)
  #   gpca_princomp <- princomp(t(gpca)) # just playing, could be wrong. failed.
  #   biplot(gpca_princomp)
}



####### --------- mutation data ---------- #############



## read in mutation data ##
gao_mut <- readxl::read_excel(
  file.path(data_outside_app_dir, "Gao_et_al_PDXs_suppl_table_Nat_Med_2015.xlsx"),
  sheet = "pdxe_mut_and_cn2"
)

# change sample prefix
gao_mut$Sample <- gsub("X-", "NIBR-", gao_mut$Sample)
gao_mut <- gao_mut[grepl("Mut", x = gao_mut$Category), ]
gao_mut$Alteration <- gsub(",.*", "", gao_mut$Details)
gao_mut$Pct <- as.character(paste0(as.numeric(gsub(".*,", "", gao_mut$Details)) * 100, "%"))
gao_mut$Details2 <- paste(gao_mut$Gene, gao_mut$Alteration, "in", gao_mut$Pct, "of reads")

# Separate Known/Likely vs VUS
gao_pos <- gao_mut[gao_mut$Category %in% c("MutKnownFunctional", "MutLikelyFunctional"), ]
gao_vus <- gao_mut[gao_mut$Category == "MutNovel", ]

# For each, create summary df
unique_samples <- levels(as.factor(gao_pos$Sample))
outdf <- data.frame(Sample = unique_samples, Genes = rep(NA, length(unique_samples)), Details = rep(NA, length(unique_samples)))
for (sam in unique_samples) {
  gao_sam <- gao_pos[gao_pos$Sample == sam, ]
  outdf[outdf$Sample == sam, "Genes"] <- paste(gao_sam$Gene, collapse = " | ")
  outdf[outdf$Sample == sam, "Details"] <- paste(gao_sam$Details2, collapse = " | ")
  outdf[outdf$Sample == sam, "Number"] <- nrow(gao_sam)
}
names(outdf) <- c("Sample", "PDX_Mutations_Positive", "PDX_Mutations_Details", "PDX_Mutations_Count")

unique_samples <- levels(as.factor(gao_vus$Sample))
outdf2 <- data.frame(Sample = unique_samples, Genes = rep(NA, length(unique_samples)), Details = rep(NA, length(unique_samples)))
for (sam in unique_samples) {
  gao_sam <- gao_vus[gao_vus$Sample == sam, ]
  outdf2[outdf2$Sample == sam, "Genes"] <- paste(gao_sam$Gene, collapse = " | ")
  outdf2[outdf2$Sample == sam, "Details"] <- paste(gao_sam$Details2, collapse = " | ")
}
names(outdf2) <- c("Sample", "PDX_VUS", "PDX_VUS_Details")
gao_muts <- merge(outdf, outdf2, by = "Sample")
rm(list = c("gao_mut", "gao_pos", "gao_sam", "gao_vus", "outdf", "outdf2"))
names(gao_muts)[1] <- "PDX_Name"

# merge new columns back to 'solid' to show in app
# solid <- merge(solid,grp,by="Sample")
solid <- merge(solid, gao_muts, by = "PDX_Name")
rm(gao_muts)


##-- Switch to PRoXe_Column_Header -- ##

# convert column names from PRIMAGRAFTS name to desired PRoXe name
# order 'meta' by 'meta$Interal_Column_Header' matching names(df)
solid_meta <- solid_meta[match(names(solid), solid_meta$Internal_Column_Header), ]
if (!all(names(solid) == solid_meta$Internal_Column_Header)) stop("ordering incorrect")
names(solid) <- solid_meta$PRoXe_Column_Header


## -- choose which columns should be visible, invisible, etc. -- ##

# levels(as.factor(meta$Visible_Invisible)) # [1] "cond_vis" "delete"   "ob_invis" "ob_vis"
# 1. get rid of 'delete'
to_delete <- which(solid_meta$Visible_Invisible == "delete")
if (length(to_delete) > 0) {
  solid <- solid[, -to_delete]
  solid_meta2 <- solid_meta[-to_delete, ]
} else {
  solid_meta2 <- solid_meta
}
# 2. sort all columns by categories
solid_meta2$Visible_Invisible_int <- rep(NA_integer_, nrow(solid_meta2))
solid_meta2[solid_meta2$Visible_Invisible == "ob_vis", ]$Visible_Invisible_int <- 1
solid_meta2[solid_meta2$Visible_Invisible == "cond_vis", ]$Visible_Invisible_int <- 2
if (nrow(solid_meta2[solid_meta2$Visible_Invisible == "ob_invis", ]) > 0) {
  solid_meta2[solid_meta2$Visible_Invisible == "ob_invis", ]$Visible_Invisible_int <- 3
}
library(plyr)
solid_meta2 <- arrange(solid_meta2, Visible_Invisible_int)
solid <- solid[, solid_meta2$PRoXe_Column_Header]
# 3. store values for demarcation
categ_count <- table(solid_meta2$Visible_Invisible)
condVis_ind_solid <- unname(categ_count["ob_vis"] + 1) # marks beginning of cond vis
obInvisRet_ind_solid <- unname(condVis_ind + categ_count["cond_vis"]) # marks beginning of invis but retained

## change appropriate columns to type ##
solid <- as.data.frame(solid)
solid <- convert.magic(solid, solid_meta2$Data_Type)

