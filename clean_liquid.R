# This R script is sourced at the top of ui.R and server.R
# It is useful for cleaning data.
# TODO: determine whether it needs to be sourced in both UI and SERVER or just one.

###############################################################################
### --- Outline of this document --- ###
# 1. Read in libraries.
# 2. Import glossary, metadata, line data.
# 3. Clean data.
# 4. Convert all
#
#
#


###############################################################################
### --- Commented code for dev/debugging work --- ###

# setwd("/Users/scott/Dropbox/PRoXe/PRoXe_app")
# deployApp(appDir = "/Users/scott/Dropbox/PRoXe/PRoXe_app",
#           appName = "PRoXe", account = "proxe")
# capture.output(shinyapps::showLogs(appName="PRoXe_alpha",account="proxe",entries=5000),file="~/logs5000.txt")

library(shiny)
library(readxl)
library(xlsx)
library(stringr) # for parsing PDX_Name below.
source("functions.R")
library(plyr)
library(dplyr)

###############################################################################
### --- Import data and metadata --- ###

# new SQL start.
if (F) {
  # connect to SQL database
  library(RMySQL)
  mydb <- dbConnect(RMySQL::MySQL(), user = "scott", password = "proxe123", dbname = "proxe_test", host = "127.0.0.1")

  # read in 'columns' metadata
  q1 <- dbSendQuery(mydb, "SELECT * FROM columns")
  columns <- dbFetch(q1, n = -1)
  dbClearResult(q1)

  # read in all tables.

  # dbListTables(mydb)
  # q2 <- dbSendQuery(mydb,
  #   "SELECT * FROM admin
  #   JOIN clinical ON admin.pdx_id=clinical.pdx_id"
  # )
  # adminical <- dbFetch(q2, n = -1)
  # dbClearResult(q2)

  # join all tables for primagrafts.
  q3 <- dbSendQuery(
    mydb,
    "SELECT admin.*, clinical.*, tumor.*, pdx.*
    FROM admin
    JOIN clinical
    ON clinical.pdx_id = admin.pdx_id
    JOIN tumor
    ON tumor.pdx_id = clinical.pdx_id
    JOIN pdx
    ON pdx.pdx_id = admin.pdx_id"
  )
  four <- dbFetch(q3, n = -1)
  ### seems to work but dates don't look good.
  ## TODO next: subset by 'columns' dataframe and/or read in other tables.


  # join tables here? or in SQL
}

# read in master (backup) glossary
gloss.filename <- dir("./data/", pattern = glob2rx("Master_Glossary*xlsx"))
if (length(gloss.filename) != 1) stop("too few or too many Master_Glossary sheets in dropbox")
meta_gloss <- read_excel(paste0("./data/", gloss.filename), sheet = 1)
meta_gloss <- as.data.frame(meta_gloss)

# read in metadata
prima.filename <- dir(data_outside_app_dir, pattern = glob2rx("PRIMAGRAFTS*xlsx"))
if (length(prima.filename) != 1) stop("too few or too many PRIMAGRAFTS sheets in dropbox")
meta <- read_excel(file.path(data_outside_app_dir, prima.filename), sheet = "Header_Data")
meta <- as.data.frame(meta)

# add meta into meta_gloss
tmp <- meta
tmp$In_PRIMAGRAFTS <- 1
meta_gloss <- rbind(meta_gloss, tmp)
# reformat
meta_gloss$PRoXe_Column_Header <- gsub("_", " ", meta_gloss$PRoXe_Column_Header)

# warn if all rows in 'meta' don't exist in meta_gloss
if (length(setdiff(meta$Internal_Column_Header, meta_gloss$Internal_Column_Header) != 0)) {
  warning(paste(
    "The following fields are in PRIMAGRAFTS but not Master_Glossary:\n",
    setdiff(meta$Internal_Column_Header, meta_gloss$Internal_Column_Header)
  ))
}

# convert column in 'meta' to specify type as "blank", "numeric", "date" or "text" for read_excel()
# original types: "character" "date" "factor" "logical" "numeric"
stopifnot(all(names(levels(meta$read_excel_type)) %in% c("character", "factor", "logical", "numeric")))
meta$read_excel_type[meta$read_excel_type %in% c("character", "factor")] <- "text"
meta$read_excel_type[meta$read_excel_type %in% c("logical", "numeric")] <- "numeric"

# TODO here: consider writing a few lines of code that read in df naively, then compare col names with 'meta' and throw a detailed bidirectional setdiff() error if they don't match.

# read in data, returning difference with meta if error.
# try(expr={ # TODO: implement try-else-print-debugging
df <- read_excel(file.path(data_outside_app_dir, prima.filename),
  sheet = "Injected",
  col_types = rep("text", nrow(meta))
) # meta$read_excel_type)
# })
df <- as.data.frame(df) # added because the default class of read_excel output is ‘tbl_df’, ‘tbl’ and 'data.frame' which is incompatible with FUN of convert.magic() 8/2016

# convert column names from PRIMAGRAFTS name to desired PRoXe name
# order 'meta' by 'meta$Interal_Column_Header' matching names(df)
meta <- meta[match(names(df), meta$Internal_Column_Header), ]
if (!all(names(df) == meta$Internal_Column_Header)) stop("ordering incorrect")
names(df) <- meta$PRoXe_Column_Header

# convert numeric columns in meta$read_excel_type to numeric
df[, which(meta$read_excel_type == "numeric")] <- as.data.frame(lapply(df[, which(meta$read_excel_type == "numeric")], as.numeric))


###############################################################################
### --- Clean data --- ###

# replace all NA with 'NSG' in Mouse_Strain
df$Mouse_Strain[is.na(df$Mouse_Strain)] <- "NSG"

# remove PDXs that have not yet been banked, reset rownames index
df <- df[!is.na(df$Latest_Passage_Banked), ]
df <- df[df$Latest_Passage_Banked != "PX", ]
df <- df[!grepl("VX$", df$PDX_Name), ]
rownames(df) <- NULL

# encode a particular age instead of "pediatric"
df$Age <- gsub(pattern = "pediatric", replacement = 9.111, x = df$Age)
df$Age <- gsub(pattern = "10.00-17.99", replacement = 15.555, x = df$Age)
df$Age <- round(as.numeric(df$Age), 3)

# remove pediatric data for "Brief Clinical Summary" and "Prior Treatment"
df[df$Age < 18 & !is.na(df$Age),c("Brief_Clinical_Summary","Prior_Treatment")] <- NA

# remove 80+ ages because they are PHI. Changing all to 81.
df$Age[which(df$Age >= 80)] <- 81

# bin and scramble young ages
df$Age[which(between(df$Age, 0, 9))] <- round(runif(sum(between(df$Age, 0, 9), na.rm = T), 0, 9), 2)
df$Age[which(between(df$Age, 9.0000001, 18))] <- round(runif(sum(between(df$Age, 9, 18), na.rm = T), 9, 18), 2)

# remove ">95" and ">90" -- simply convert to integer.
df$Percent_Tissue_Involvement <- gsub(">", "", df$Percent_Tissue_Involvement)
# explicitly convert "Unclear" to NA
df$Percent_Tissue_Involvement <- gsub("Unclear", NA, df$Percent_Tissue_Involvement)
# explicitly convert "NA" to NA. TODO: ask Mark what kind of meaning "NA" carries here, and perhaps convert.
df$Percent_Tissue_Involvement <- gsub("NA", NA, df$Percent_Tissue_Involvement)
df$Percent_Tissue_Involvement <- as.integer(df$Percent_Tissue_Involvement)

# convert appropriate chars to numeric after removing "unknown", "uncertain", etc.
df$Presenting_WBC[grep(">1000", df$Presenting_WBC, ignore.case = TRUE)] <- 1111
df$Presenting_WBC[grep("un", df$Presenting_WBC, ignore.case = TRUE)] <- NA
df$Presenting_WBC[grep(">50000", df$Presenting_WBC, ignore.case = TRUE)] <- 55555
if (any(grepl(">", df$Presenting_WBC))) {
  warning("No specific rule for df$Presenting_WBC entry, replacing '>' with ''")
  df$Presenting_WBC <- gsub(">", "", df$Presenting_WBC)
}

###############################################################################
### -- Create new Distribution_Permissions column from Consent columns -- ###

# equality function that returns FALSE if left argument is NA.
narmEqual <- function(x, equalTo) {
  if (!is.na(x)) {
    if (x == equalTo) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    return(FALSE)
  }
}

# Implementing new rules 7/22/16 from Mark:
tmp_dist <- df$Distribution_Permissions # for testing below.
warn_BODFI <- FALSE
for (i in 1:nrow(df)) {
  # first look at institution of origin;
  inst_ori <- stringr::str_sub(df$PDX_Name[i], 1, 2)
  # if non-DFCI/BWH/CHB,
  if (!(inst_ori %in% c("CB", "DF", "BW"))) { # note none currently (7-2016) are "BW", but that will change.
    # Permissions are as per BODFI spreadsheet -- which currently are none for any institution.
    warn_BODFI <- TRUE

    # sample code:
    # if(inst_ori %in% c("MD","WC","MU")) { # MD = MDACC, WC = MSK (Weill-Cornell), MU = Munich are all prohibited in BODFI spreadsheet.
    # }

    # exception case: MD Anderson now allowed to be distributed to academic labs (code 3)
    if (inst_ori %in% c("MD")) {
      df$Distribution_Permissions[i] <- 3
    } else {
      # no distribution for all the rest of the non-DFCI/BWH/CHB:
      df$Distribution_Permissions[i] <- 0
    }
  } else {
    ## If institution of origin is DFCI/BWH/CHB,

    # Calculate as per tissue banking consents with our current rules.
    # Numbered-Consent-based Rules:
    # (1) If "1" in 01-206, 11-104, 06-078, or 13-563, then OK for academic, industry-sponsored academic, and industry
    # [of note these consents override limitations imposed by other tissue banking or study protocols];
    # (2) If "1" in 05-001 or 11-001 and nothing in 01-206, 11-104, 06-078, and 13-563, then OK for academic only for now.
    # For all other samples, the vast majority of which will have been contributed from external collaborators,
    # the permission level will be dictated by the BODFI spreadsheet; in absence of any specific information,
    # default will be "not available"

    # calculate Consent-logic permission
    if (stringr::str_sub(df$PDX_Name[i], 1, 10) == "DFBL-39435") {
      df$Distribution_Permissions[i] <- 4
    } else if (narmEqual(df$`01-206_Consent`[i], "1") | narmEqual(df$`11-104_Consent`[i], "1") |
      narmEqual(df$`06-078_Consent`[i], "1") | narmEqual(df$`13-563_Consent`[i], "1")) {
      df$Distribution_Permissions[i] <- 1
    } else if ((narmEqual(df$`05-001_Consent`[i], "1") | narmEqual(df$`11-001_Consent`[i], "1"))) {
      df$Distribution_Permissions[i] <- 3
    } else {
      df$Distribution_Permissions[i] <- 0
    }
  }
}
if (warn_BODFI == TRUE) warning("BODFI 3-30-16 distribution permissions are hardcoded. (No distribution at all of non-DF/CB/BW-origin samples.) Exception: hardcoded change Nov-16 that allows academic distribution of MDACC.")

# testing results -- this shows which have changed from PRIMAGRAFTS Distribution_Permissions.
tmp_oldnew <- as.data.frame(cbind(tmp_dist, df$Distribution_Permissions, df$PDX_Name))
colnames(tmp_oldnew) <- c("prima", "new", "PDX_Name")
print("The following lines have changed permissions from Primagrafts:")
print(tmp_oldnew[!complete.cases(tmp_oldnew), ])
print(tmp_oldnew[complete.cases(tmp_oldnew) & tmp_oldnew$prima != tmp_oldnew$new, ])

# change Distribution_Permissions to text from 0/1/2/3
# NOTE 2 does not apply to any lines right now (7/21/16)
df$Distribution_Permissions <- factor(df$Distribution_Permissions,
  levels = 0:4,
  labels = c(
    "none currently",
    "academic, industry-sponsored academic, and industry",
    "academic, industry-sponsored academic",
    "academic only",
    "Dana-Farber only"
  )
)

meta[meta$PRoXe_Column_Header == "Distribution_Permissions", "Column_Description"] <- "Indicates to whom relevant materials transfer agreements permit distribution."
warning("Note edited Distribution_Permissions description in app, not PRIMAGRAFTS. Temporary fix.")

###############################################################################
### --- convert all columns to meta$Data_Type --- ###
df <- convert.magic(df, meta$Data_Type)

###############################################################################
### --- drop and hide specified columns --- ###

# drop incompletely characterized samples
df <- df[-which(df$Incompletely_Characterized == 1), ]

## -- choose which columns should be visible, invisible, etc. -- ##

# levels(as.factor(meta$Visible_Invisible)) # [1] "cond_vis" "delete"   "ob_invis" "ob_vis"
# 1. get rid of 'delete'
to_delete <- which(meta$Visible_Invisible == "delete")
df <- df[, -to_delete]
meta2 <- meta[-to_delete, ]
# 2. sort all columns by categories
meta2$Visible_Invisible_int <- rep(NA_integer_, nrow(meta2))
meta2[meta2$Visible_Invisible == "ob_vis", ]$Visible_Invisible_int <- 1
meta2[meta2$Visible_Invisible == "cond_vis", ]$Visible_Invisible_int <- 2
meta2[meta2$Visible_Invisible == "ob_invis", ]$Visible_Invisible_int <- 3

meta2 <- arrange(meta2, Visible_Invisible_int)
df <- df[, meta2$PRoXe_Column_Header]
# 3. store values for demarcation
categ_count <- table(meta2$Visible_Invisible)
condVis_ind <- unname(categ_count["ob_vis"] + 1) # marks beginning of cond vis
obInvisRet_ind <- unname(condVis_ind + categ_count["cond_vis"]) # marks beginning of invis but retained

###############################################################################
### --- TODO: function: read in all ____ files in www/___ and add link as column to data frame --- ###
# this would generalize what I've done below with the three file sets.

###############################################################################
### --- read in all Flow_Cytometry files and add link as column to data frame --- ###

fc <- data.frame(filenames = dir("www/Flow_Cytometry/", pattern = "_fc.pdf$"), stringsAsFactors = F)
# parse filename into parts
fc$short <- gsub("_\\d+_fc.pdf$", "", fc$filenames, perl = TRUE)
fc$date <- gsub("_fc.pdf$", "", fc$filenames, perl = TRUE)
fc$date <- gsub("^.+_", "", fc$date, perl = TRUE)
fc$date <- as.Date(fc$date, "%Y%m%d")

# create column for merging on pdx_id
fc$namenum <- stringr::str_sub(fc$short, 1, 10)
# create column in df same as fc$namenum
df$namenum <- stringr::str_sub(df$PDX_Name, 1, 10)

# leave only newest of duplicated samples
dups <- fc$namenum[duplicated(fc$namenum)]
for (dup in dups) {
  tempfc <- fc[fc$namenum == dup, ]
  tempfc <- tempfc[order(tempfc$date, decreasing = T), ]
  filenames_to_remove <- tempfc[2:nrow(tempfc), ]$filenames
  fc <- fc[!(fc$filenames %in% filenames_to_remove), ]
  rm(list = (c("tempfc", "filenames_to_remove")))
}
createLinks <- function(filename_vector, column_heading) {
  unlist(lapply(filename_vector, function(filename) {
    as.character(a("click for PDF", target = "_blank", href = paste0(column_heading, "/", filename)))
  }))
}
fc$filenames <- createLinks(fc$filenames, "Flow_Cytometry")

# drop columns, merge with dataframe using namenum/pdx_id
fc$short <- NULL
fc$date <- NULL
names(fc)[names(fc) == "filenames"] <- "Flow_Cytometry_PDF"
df <- merge(df, fc, by = "namenum", all.x = T)

# move namenum to end of df
df <- df[, c(2:ncol(df), 1)]

# move inserted columns around, change indices of which columns to show
new_col_inds <- (ncol(df) - (ncol(fc) - 1)):(ncol(df) - 1)
new_col_order <- c(
  1:(obInvisRet_ind - 1),
  new_col_inds,
  (obInvisRet_ind):(ncol(df) - 2),
  ncol(df)
)
df <- df[, new_col_order]
obInvisRet_ind <- obInvisRet_ind + length(new_col_inds)

###############################################################################
### ---  read in all IHC and link as column to data frame --- ###

ihc <- data.frame(filenames = dir("www/IHC/", pattern = "_IHC.pdf$"), stringsAsFactors = F)
# parse filename into parts
ihc$namenum <- gsub("-[RV][0-4].*_IHC.pdf", "", ihc$filenames, perl = TRUE) # TODO: doesn't quite work because of typos, I think.
# TODO: continue here, perhaps after fixing IHC filenames manually (Alex?)

ihc$filenames <- createLinks(ihc$filenames, "IHC")

# merge with dataframe, probably using PDX Name #TODO: confirm this worked ok.
names(ihc)[names(ihc) == "filenames"] <- "IHC_PDF"
df <- merge(df, ihc, by = "namenum", all.x = T)


# move namenum to end of df
df <- df[, c(2:ncol(df), 1)]
# move IHC_PDF column to visible section, change indices of which columns to show
new_col_inds <- which(names(df) == "IHC_PDF")
new_col_order <- c(
  1:(obInvisRet_ind - 1),
  new_col_inds,
  (obInvisRet_ind):(ncol(df) - length(new_col_inds) - 1),
  ncol(df)
)
df <- df[, new_col_order]
obInvisRet_ind <- obInvisRet_ind + length(new_col_inds)

###############################################################################
### ---  read in all Pathology_Reports and link as column to data frame --- ###

pr <- data.frame(filenames = dir("www/Pathology_Reports/", pattern = "_path.pdf$"), stringsAsFactors = F)
# parse filename into parts
pr$namenum <- gsub("-[RV][0-4].*_path.pdf", "", pr$filenames, perl = TRUE)
pr$filenames <- createLinks(pr$filenames, "Pathology_Reports")

# merge with dataframe, probably using PDX Name #TODO: confirm this worked ok.
names(pr)[names(pr) == "filenames"] <- "Path_Report_PDF"
df <- merge(df, pr, by = "namenum", all.x = T)

# move namenum to end of df
df <- df[, c(2:(ncol(df)), 1)]
# move Path_Report_PDF column to visible section, change indices of which columns to show
# df <- df[,c(1:(ncol(df)-2),(ncol(df)-1))]
new_col_inds <- which(names(df) == "Path_Report_PDF")
# insert_ind <- (which(names(df) == "P0_Injected")) - 1
new_col_order <- c(
  1:(obInvisRet_ind - 1),
  new_col_inds,
  (obInvisRet_ind):(ncol(df) - length(new_col_inds) - 1), ncol(df)
)
df <- df[, new_col_order]
obInvisRet_ind <- obInvisRet_ind + length(new_col_inds)

###############################################################################
### --- Include inventory information --- ###

# confirm inventory files
inv.filename.pdx <- dir(data_outside_app_dir, pattern = glob2rx("^PDX_Inventory_*.xlsx"))
if (length(inv.filename.pdx) != 1) stop("too few or too many Inventory sheets in Dropbox/PRoXe/data_outside_app/")

# TODO: Ask Mark whether also to show BM and Tumor vials. -- no for now, later yes to Tumor for solid (-Amanda)
# Read in and sum number of spleen vials left from both adult and pediatric PDXs.
# inv <- read_excel("data/Inventory_Tracking/2015-9-2_Adult_Inventory.xlsx",1)
inv <- read.xlsx2(file = file.path(data_outside_app_dir, inv.filename.pdx), sheetName = "Banked", stringsAsFactors = FALSE)

# save inventory-last-updated date from inventory
inv$Date <- as.Date("1900-01-01") + as.numeric(inv$Date) - 2
stopifnot(length(which(inv$Date > Sys.Date())) == 0) # stops if dates are in the future
inv_upDate <- max(inv$Date, na.rm = T)

# convert all Modification columns that are " " to ""
inv$Modification <- gsub(" ", "", inv$Modification)

# subset columns, rename:
# for future use if grouping by P,Route -- Amanda said grouping by route might be helpful
if (F) {
  inv <- inv[, c("New.PDX.ID", "P.", "Route", "Spleen....vials.")]
  names(inv) <- c("PDX_Name", "P", "Route", "Spleen_Vials_Left")
}
# current approach: sum all spleen vials per line, of any Route type
if (T) {
  inv <- inv[, c("New.PDX.ID", "Passage", "Route", "Modification", "Strain", "Spleen....vials.", "BM....vials.", "Tumor.cells....vials.", "Tumor.seeds....vials.")]
  names(inv) <- c("PDX_Name", "P", "Route", "Mod", "Strain", "Spleen_Vials_Left", "BM_Vials_Left", "DTC_Vials_Left", "Seed_Vials_Left")
}

# TODO: remove after fixing dependent code that uses inv$Spleen_Vials_Left below.
# # Convert "/"-separated vials counts to total number. #TODO: vectorize, perhaps.
# for (i in 1:length(inv$Spleen_Vials)){
#   inv$Spleen_Vials_Left[i] <- sum(as.numeric(unlist(strsplit(inv$Spleen_Vials[i],"/"))))
# }

# remove rows missing name
inv <- inv[!is.na(inv$PDX_Name), ]
inv <- inv[inv$PDX_Name != "", ]

# create primary key
if (F) {
  inv$pdx_full <- paste0(inv$PDX_Name, "-", inv$Route, inv$P, ifelse(inv$Mod == "", "", paste0("-", inv$Mod)))
  inv$pdx_pkey <- paste0(inv$pdx_full, "_", inv$Strain)
}

# convert all cols-to-sum to numeric
cols_ind <- grepl("Left", names(inv))
inv[, cols_ind] <- as.data.frame(
  lapply(
    inv[, cols_ind],
    function(col) as.integer(round(as.numeric(col), 0))
  )
)

# sum vials by many features for line report (_lr)
inv_lr <- aggregate(cbind(Spleen_Vials_Left, BM_Vials_Left, DTC_Vials_Left, Seed_Vials_Left) ~ PDX_Name + P + Route + Mod + Strain, data = inv, FUN = sum)
# sort, rearrange
inv_lr <- arrange(inv_lr, Route, P, Mod, Strain)
names(inv_lr) <- c("PDX_id", "Passage", "Route", "Modification", "Strain", "Spleen_Vials", "BM_Vials", "DTC_Vials", "Seed_Vials")
inv_lr <- inv_lr[c("PDX_id", "Route", "Passage", "Modification", "Strain", "Spleen_Vials", "BM_Vials", "DTC_Vials", "Seed_Vials")]
names(inv_lr) <- c("PDX_id", "Engraftment Route", "Passage Number", "Modification", "Host Strain", "Splenocyte Vials", "Bone Marrow Vials", "Dissociated Tumor Cell Vials", "Tumor Seed Vials")
row.names(inv_lr) <- NULL
# for later use if want to concatenate, also need to edit:
if (F) {
  inv_lr2 <- inv_lr
  inv_lr2$RoutePModStrain <- paste0(inv_lr2$Route, inv_lr2$P, ifelse(inv_lr2$Mod == "", "", paste0("-", inv_lr2$Mod)), ifelse(inv_lr2$Strain == "", "", paste0("_", inv_lr2$Strain)))
  names(inv_lr2) <- c("PDX_Name", "P", "Route", "Mod", "Strain", "Spleen", "BM", "DTC", "Seed", "RoutePModStrain")
  inv_lr2 <- arrange(inv_lr2, RoutePModStrain)
  inv_lr3 <- inv_lr2[c("PDX_Name", "Spleen", "BM", "DTC", "Seed", "RoutePModStrain")]
}

# sum vials for samples with multiple rows
inv <- aggregate(cbind(Spleen_Vials_Left, BM_Vials_Left, DTC_Vials_Left, Seed_Vials_Left) ~ PDX_Name, data = inv, FUN = sum)

# remove any duplicates still in inventory -- should never run now because of 'aggregage' above
if (anyDuplicated(inv$PDX_Name)) {
  warning("Some inventory PDX_Name occur on multiple rows. Decide what to do; currently removing latter.")
  inv_dups <- inv[duplicated(inv$PDX_Name), ]$PDX_Name
  print("Duplicated, removing:")
  print(inv[inv$PDX_Name %in% inv_dups, ])
  inv <- inv[-which(duplicated(inv$PDX_Name)), ]
}

# prepare and merge selected, processed inventory columns with main dataset.
inv$At_Least_6_Spleen_Vials_Left <- as.factor(inv$Spleen_Vials_Left >= 6)
levels(inv$At_Least_6_Spleen_Vials_Left) <- c("No", "Yes")

# drop unwanted columns
inv <- inv[, c("PDX_Name", "At_Least_6_Spleen_Vials_Left")]

# add a temporary 'pdx_id' column to df for matching
df$PDX_id <- stringr::str_sub(df$PDX_Name, start = 1, end = 10)
names(inv)[names(inv) == "PDX_Name"] <- "PDX_id"

# merge with df
df <- merge(df, inv, by = "PDX_id", all.x = TRUE)
df$PDX_id <- NULL

# move new columns to visible section, change indices of which columns to show
new_col_names <- names(inv)[names(inv) %in% names(df)]
new_col_inds <- which(names(df) %in% new_col_names)
new_col_order <- c(
  1:(obInvisRet_ind - 1),
  new_col_inds,
  (obInvisRet_ind):(ncol(df) - length(new_col_inds))
)

df <- df[, new_col_order]
obInvisRet_ind <- obInvisRet_ind + length(new_col_inds)

###############################################################################
### --- Include HLA typing info --- ###

# Mark M.: Someone from the Wu lab applied a program that infers class I HLA type from RNA-Seq data.
# HLA type at six alleles for 116 of our samples, according to the abbreviated RNA-Seq name
# (e.g., AML01, AML02, etc.)
# I would label the columns as follows:
# > A1 = HLA-A allele 1
# > A2 = HLA-A allele 2
# > B1 = HLA-B allele 1
# > B2 = HLA-B allele 2
# > C1 = HLA-C allele 1
# > C2 = HLA-C allele 2

hla <- read_excel("data/hla_results_010816.xlsx", sheet = 1, col_names = TRUE)
hla <- as.data.frame(hla) # added because the default class of read_excel output is ‘tbl_df’, ‘tbl’ and 'data.frame, which is incompatible with FUN in convert.magic() 8/2016
hla$A <- paste(hla$A1, hla$A2)
hla$B <- paste(hla$B1, hla$B2)
hla$C <- paste(hla$C1, hla$C2)
hla <- hla[, c("Sample", "A", "B", "C")]
colnames(hla) <- c("PDX_RNA-Seq_Name", "HLA-A Alleles", "HLA-B Alleles", "HLA-C Alleles")
hla <- convert.magic(hla, rep("factor", 7))
hla$`PDX_RNA-Seq_Name` <- as.character(hla$`PDX_RNA-Seq_Name`)
df <- merge(df, hla, by = "PDX_RNA-Seq_Name", all.x = TRUE)

# move new columns to visible section, change indices of which columns to show
new_col_names <- names(hla)[-which(names(hla) == "PDX_RNA-Seq_Name")]
new_col_inds <- which(names(df) %in% new_col_names)
new_col_order <- c(
  1:(obInvisRet_ind - 1), # TODO: add parens around all these and see what happens
  new_col_inds,
  (obInvisRet_ind):(ncol(df) - length(new_col_inds))
)

df <- df[, new_col_order]
obInvisRet_ind <- obInvisRet_ind + length(new_col_inds)

# add HLA glossary data to 'meta2'
# hla_meta <- read_excel("data/hla_results_010816.xlsx",sheet = "hla_header_data",col_names = TRUE)
# hla_meta$Visible_Invisible_int <- rep(NA_integer_,nrow(hla_meta))
# meta_gloss <- rbind(meta_gloss,hla_meta)
# meta2[meta2$Visible_Invisible == "ob_vis",]$Visible_Invisible_int <- 1
# meta2[meta2$Visible_Invisible == "cond_vis",]$Visible_Invisible_int <- 2
# meta2[meta2$Visible_Invisible == "ob_invis",]$Visible_Invisible_int <- 3

# Optional line for making WHO_Classification a factor for contingency table purposes.
df$WHO_Classification <- as.factor(df$WHO_Classification)

###############################################################################
### --- Final aesthetic modifications --- ###

# TODO: recode some boolean/factor columns as explanatory text (i.e. T/F to Y/N)?

# remove all underscores from colnames and make consistent with rest of code.
names(df) <- gsub(pattern = "_", replacement = " ", x = names(df))


# randomize row ordering of df, but deterministically
set.seed(12)
df <- df[sample(1:nrow(df), size = nrow(df), replace = FALSE), ]
# consider summing non-empty cells to put most complete cells near top

print("didcomehere7")

# remove all unused levels from factor variables
# note this is not ideal if we want to show that some factor has a value of
# zero for some factor level.
df <- droplevels(df)
# TODO, perhaps: order factor levels specifically for barplots

# save.image("mark_shiny.RData")

# todo
# filters - only show subset available
