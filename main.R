###############################################################################################
library(data.table)
# cdisc variables
cdisc_variables <- "cdisc_domains_150301.txt"
#bring in the cdisc variables
cdisc <- read.table(cdisc_variables,sep="\t",header=TRUE,colClasses="character",fill=TRUE)

## LOOP THROUGH THE METADATA FILE
for (mRow in 1:dim(metaData)[1]) {
  # Extract information for a particular row from the metaData file
  workingDirectory <- metaData$Working_directory[mRow]
  
  data_file <- paste(workingDirectory,metaData$Annotation_table[mRow],sep = "/")
  mapper_file <- paste(workingDirectory,metaData$Mapper_file[mRow],sep = "/")
  domainToConsider <- metaData$Domains[mRow]
  
  # read in the data
  #setwd(workingDirectory)
  data <- read.table(data_file,fill=TRUE,header=TRUE,sep="\t",comment.char = "",row.names=NULL,quote="\"",colClasses="character",check.names=FALSE)
  mapper <- read.table(mapper_file,sep="\t",header=TRUE,quote="\"",colClasses="character")
  
  # subset the mapper file based on information in the meta file
  domains <- unlist(strsplit(domainToConsider,";"))
  #AP <- paste0("AP",domains)
  SUPP <- paste0("SUPP",domains)
  FA <- paste0("FA",domains)
  #domains <- c(domains,AP,SUPP,FA)
  domains <- c(domains,SUPP,FA)
  
  # some of these domains may also have SUPP or AP or FA domains, generate corresponding AP and SUPP and FA
  identifierRows <- which(mapper$Mapping.function %in% c("STUDYID;","USUBJID;","SPDEVID;","SUBJID;"))
  mapper <- mapper[c(identifierRows,which(mapper$Domain %in% domains)),]
  
  #Preprocessing
  #first the data file
  #colnames(data) <- gsub("-",".",colnames(data))
  #colnames(data) <- gsub("\\s+",".",colnames(data))
  colnames(data) <- gsub("\\(|\\)|\\s+|-|\\||\\/|:\\=|\\?|\\[|\\]",".",colnames(data))
  #mapper$original.attribute <- gsub("-",".",mapper$original.attribute)
  #mapper$original.attribute <- gsub("\\s+",".",mapper$original.attribute)
  mapper$original.attribute <- gsub("\\(|\\)|\\s+|-|\\||\\/|:\\=|\\?|\\[|\\]",".",mapper$original.attribute)
  # End Preprocessing
  # handle multiple RelTos
  
  listWithMapperandDataFiles <- handle_multiple_RelTo(mapper,data)
  data <- listWithMapperandDataFiles$myData
  mapper <- listWithMapperandDataFiles$myMapper
  

  
  # Identify study attributes that don't match with those mentioned in the mapper file
  study_attributes_that_dont_match <- mapper[!mapper[,1] %in% colnames(data),1]
  study_attributes_that_dont_match <- mapper[which(mapper[,1] %in% study_attributes_that_dont_match),c(1,2)]
  study_attributes_that_dont_match <- study_attributes_that_dont_match[which(study_attributes_that_dont_match[,2]!=""),1]
  if (length(study_attributes_that_dont_match) > 0) { 
    print(study_attributes_that_dont_match)
    stop("The above file(s) are present in mapper but not the data file!!!")
  }
  
  #bring in the cdisc variables
  #cdisc <- read.table(cdisc_variables,sep="\t",header=TRUE,colClasses="character",fill=TRUE)
  
  my.list <- list() # list of tables that will hold all the cdisc domains
  
  for (i in 1:dim(cdisc)[2]) {
    temp <- data.frame(matrix(NA,nrow=0,ncol=length(cdisc[!cdisc[,i]=="",i])),stringsAsFactors=FALSE)
    colnames(temp) <- cdisc[!cdisc[,i]=="",i]
    my.list[length(my.list)+1] <- list(temp)
    
  }
  names(my.list) <- colnames(cdisc)
  
  # create the relevant AP domain. NOTE: This should be created immediately after the my.list has been created
  if (!length(grep("^AP.*$",mapper$Domain))==0) {
    my.list <- create_ap_domains(mapper,my.list)
  }
  ################################
  associatedColumns_df <- determine_associated_columns(mapper)
  #Preprocessing
  #associatedColumns_df$X2 <- gsub("-",".",associatedColumns_df$X2)
  #associatedColumns_df$X2 <- gsub("\\s+",".",associatedColumns_df$X2)
  associatedColumns_df$X2 <- gsub("\\(|\\)|\\s+|-|\\||\\/|:\\=|\\?|\\[|\\]",".",associatedColumns_df$X2)
  
  # End Preprocessing
  
  ############# Identify SUPP tables
  suppList <- identify_SUPP_add_cols_to_domain(mapper,my.list)
  my.list <- suppList$myList
  suppMetaTable <- suppList$suppTable
  ##########################
  faList <- identify_FA_add_cols_to_domain(mapper,my.list)
  my.list <- faList$myList
  faMetaTable <- faList$faTable
  ####################################################
  coList <- identify_CO_add_cols_to_domain(mapper,my.list,associatedColumns_df)
  my.list <- coList$myList
  mapper <- coList$myMapper
  associatedColumns_df <- coList$associatedColumns
  coMetaTable <- coList$coTable
  
  for (row in 1:dim(mapper)[1]) {
    
    if (!mapper[row,2]=="") {
      domainIndc <- which(names(my.list)==mapper[row,2])
      col_header_for_subjid <- mapper[grep("^USUBJID",mapper[,3]),1]
      elements_in_notes <- unlist(strsplit(mapper[row,3],";"))
      ## see if the current study attribute is present in the associated columns data frame, if it is present in the second column, skip this row
      if (!length(which(associatedColumns_df[,2]==mapper[row,1]))==0) { # end the for loop if the variable is present in the second column of the associatedColumns df
        next;
      }
      #print(row)
      associatedColumns <- ""
      if(!length(which(associatedColumns_df[,1]==mapper[row,1]))==0) {
        associatedColumns <- associatedColumns_df[which(associatedColumns_df[,1]==mapper[row,1]),2]
      }
      
      df <- rename_and_insert_with_associated_columns(data,mapper,associatedColumns,row,my.list)
      my.list[[domainIndc]]  <- insert_into_sdtm_table(df,my.list,mapper,row)
      
    }
  }
  
  my.list <- remove_empty_or_null_and_duplicate_rows(my.list)
  my.list <- generate_apid(my.list)
  my.list <- create_seq(my.list)
  my.list <- insert_domain_name(my.list)
  my.list <- generate_supplementary_tables(suppMetaTable,my.list)
  my.list <- split_semicolon_values_recreate_dataframe(my.list)
  my.list <- generate_fa_tables(faMetaTable,my.list)
  my.list <- generate_co_tables(coMetaTable,my.list)
  my.list <- remove_NA_values(my.list)
  my.list <- insert_subjID(data,mapper,my.list,domains)
 # my.list <- populate_decod_for_condition(my.list,domains)
  outputFile <- paste0(workingDirectory,"/Tables")
  write_tables_to_folder(my.list,folder=outputFile) 
  
  
}
#######################################################################################################################