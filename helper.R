library(data.table)

rename_and_insert_with_associated_columns <- function (dfdata,mapper_file,associated,row_number,MyList,drop=TRUE) {
  data_copy <- dfdata
  mapper <- mapper_file
  Study.ID <- mapper[grep("^STUDYID",mapper[,3]),1]
  name_of_domain <- mapper[row_number,2]
  
  ## The subject id or the identifier for the domain needs to be captured and fixed
  if (name_of_domain %in% c("DI","DO","DT")) {
    Unique.ID <- mapper[grep("^SPDEVID",mapper[,3]),1]
    Unique.ID.name <- "SPDEVID"
  } else if (name_of_domain=="TS") {
    Unique.ID<- mapper[grep("^TSPARMCD",mapper[,3]),1]
    Unique.ID.name <- "TSPARMCD"
  } else if (!length(grep("^AP.*$",name_of_domain))==0) {
    Unique.ID<- mapper[grep("^USUBJID",mapper[,3]),1]
    Unique.ID.name <- "RSUBJID"
  } else {
    Unique.ID<- mapper[grep("^USUBJID",mapper[,3]),1]
    Unique.ID.name <- "USUBJID"
  }
  
  #colNames <- colnames(df)
  main_pivot_variable_name <- mapper[row_number,1]
  # study_attribute <- mapper[row_number,1]
  insert_col_vals <- mapper[row_number,3]
  #################################################################
  # These will contain the list of column headers and the list of values under them
  my_list_column_headers <- list()
  my_list_values <- list()
  #start by inserting subject id in the header list and the values in the value list
  ## Make a decision here as to what you want to be the fixed column, just USUBJID | STUDYID,USUBJID | STUDYID
  ## Some domains do not have USUBJID, for these domains the fixedColumn* should not contain USUBJID
  ## Also, when STUDYID is not provided as a column in the annotation table, fixedColumn* should not be used
  ## BEWARE, if fixedColumn* is empty do the needful
  
  domains_withOut_Identifier <- c("TV","TD","TI","PB")
  
  if (name_of_domain %in% domains_withOut_Identifier) {
    fixedColumnNames <- c("STUDYID")
    fixedColumnValues <- c(Study.ID) ## fixedColumnNames and fixedColumnValues should match
  } else {
    fixedColumnNames <- c("STUDYID",Unique.ID.name)
    fixedColumnValues <- c(Study.ID,Unique.ID) ## fixedColumnNames and fixedColumnValues should match
  }
  
  my_list_column_headers[c(1:length(fixedColumnNames))] <- fixedColumnNames
  ## just to insure that it is read in a proper order read in the values just as in the same order as fixedColumnValues
  
  for (i in 1:length(fixedColumnValues)) {
    my_list_values[i] <- as.data.frame(data_copy[,colnames(data_copy) %in% fixedColumnValues[i]],stringsAsFactors=FALSE)
  }
  
  ######## Operations for the main column ###################################################################################
  
  elements_in_notes <- unlist(strsplit(insert_col_vals,";"))
  rename_header_to <- gsub("Rename:(.*)$", "\\1", elements_in_notes[grep("Rename",elements_in_notes)], perl=T)
  
  insertion_header <-   gsub("^Insert:(.*?),.*$", "\\1", elements_in_notes[grep("Insert",elements_in_notes)], perl=T)
  insertion_value <-   gsub("^Insert:.*,Val=(.*)$", "\\1", elements_in_notes[grep("Insert",elements_in_notes)], perl=T)
  
  #if (length(rename_header_to)==0) {rename_header_to <- study_attribute } #if rename has not been mentioned, use the study attribute
  if (length(rename_header_to)==0) {rename_header_to <- main_pivot_variable_name } #if rename has not been mentioned, use the study attribute
  
  my_list_column_headers[length(my_list_column_headers)+1] <- rename_header_to
  main_pivot_variable_value <- as.data.frame(data_copy[,colnames(data_copy) %in% c(main_pivot_variable_name)],stringsAsFactors=FALSE)
  my_list_values[length(my_list_values)+1] <- main_pivot_variable_value
  
  # identify elements that are empty or null in the main_pivot_variable
  indc_na_or_empty  <- which(is.na(data_copy[,colnames(data_copy) %in% main_pivot_variable_name]) | data_copy[,colnames(data_copy) %in% main_pivot_variable_name] == "")
  
  if (!length(insertion_header)==0) {
    for (i in 1:length(insertion_header)) {
      my_list_column_headers[length(my_list_column_headers)+1] <- insertion_header[i]
    }
    
    for (i in 1:length(insertion_header)){
      insertion_column <- as.data.frame(as.character(rep(insertion_value[i],dim(data_copy)[1])),stringsAsFactors=FALSE)
      
      if (!length(indc_na_or_empty)==0) {
        insertion_column[indc_na_or_empty,] <- main_pivot_variable_value[indc_na_or_empty,]
      }
      
      
      my_list_values[length(my_list_values)+1] <- insertion_column
    }
  }
  
  ######## Operations for the associated (related) columns
  if (!associated[1]=="") {
    ## In case the associated column is a supplemental section, we need to populate the following data frame and then associate with the main
    colNamesOfSUPP <- c("RDOMAIN","IDVAR","IDVARVAL","QNAM","QLABEL","QVAL","QORIG","QEVAL")
    myListSUPP <- setNames(vector("list", length(colNamesOfSUPP)), colNamesOfSUPP)
    ## In case the associated column is a FA domain, we need to populate the following data frame and then associate with the main
    testList <- MyList # obtain column names from the definition in the main list
    colNamesOfFA <- colnames(testList[[which(names(testList)=="FA")]])
    colNamesOfFA <- c(colNamesOfFA,"FAIDVAR") # add IDVAR since IDVAR is not a FA domain variable
    colNamesOfFA <- colNamesOfFA[-which(colNamesOfFA %in% c("STUDYID","DOMAIN","USUBJID"))]
    myListFA <- setNames(vector("list", length(colNamesOfFA)), colNamesOfFA)
    ## In case the associated column is a CO domain, we need to populate the following data frame and then associate with the main
    colNamesOfCO <- colnames(testList[[which(names(testList)=="CO")]])
    colNamesOfCO <- colNamesOfCO[-which(colNamesOfCO %in% c("CO_STUDYID","CO_DOMAIN","CO_USUBJID"))]
    myListCO <- setNames(vector("list", length(colNamesOfCO)), colNamesOfCO)
    
    for (i in 1:length(associated)) {
      elements_in_notes_AC <- unlist(strsplit(mapper[which(mapper[1]==associated[i]),3],";"))
      isDomainSUPP <- mapper[which(mapper[1]==associated[i]),2]
      
      if (!length(grep("^SUPP.*$",isDomainSUPP))==0) {
        myListSUPP <- suppTableForAssociated(myListSUPP,elements_in_notes_AC,data_copy,name_of_domain,main_pivot_variable_name,associated[i])
        
        next;
      } else if (!length(grep("^FA..$",isDomainSUPP))==0) {
        myListFA <- faTableForAssociated(myListFA,elements_in_notes_AC,data_copy,name_of_domain,main_pivot_variable_name,associated[i])
        
        next;
      } else if (!length(grep("^CO_..$",isDomainSUPP))==0) {
        myListCO <- coTableForAssociated(myListCO,elements_in_notes_AC,data_copy,name_of_domain,main_pivot_variable_name,associated[i])
        
        next;
      }
      
      rename_header_to_AC <- gsub("Rename:(.*)$", "\\1", elements_in_notes_AC[grep("Rename",elements_in_notes_AC)], perl=T)
      if (length(rename_header_to_AC)==0) {rename_header_to_AC <- associated[i] } #if rename has not been mentioned, use the study attribute
      
      my_list_column_headers[length(my_list_column_headers)+1] <- rename_header_to_AC ## This is the pivot in the associated
      pivot_attribute_value <- as.data.frame(data_copy[,colnames(data_copy) %in% associated[i]],stringsAsFactors=FALSE)
      if (!length(indc_na_or_empty)==0 & !name_of_domain=="DM") {
        pivot_attribute_value[indc_na_or_empty,] <- main_pivot_variable_value[indc_na_or_empty,]
      }
      
      my_list_values[length(my_list_values)+1] <- pivot_attribute_value
      
      
      
      insertion_header_AC <-   gsub("^Insert:(.*),.*$", "\\1", elements_in_notes_AC[grep("Insert",elements_in_notes_AC)], perl=T)
      insertion_value_AC <-   gsub("^Insert:.*,Val=(.*)$", "\\1", elements_in_notes_AC[grep("Insert",elements_in_notes_AC)], perl=T)
      
      if (!length(insertion_header_AC)==0) {
        for (j in 1:length(insertion_header_AC)) {
          my_list_column_headers[length(my_list_column_headers)+1] <- insertion_header_AC[j]
        }
        for (j in 1:length(insertion_header_AC)){
          insertion_column_AC <- as.data.frame(as.character(rep(insertion_value_AC[j],dim(data_copy)[1])),stringsAsFactors=FALSE)
          
          if (!length(indc_na_or_empty)==0 & !name_of_domain=="DM") { #only if the main variable has a value the associated columns will have a value
            
            insertion_column_AC[indc_na_or_empty,] <- pivot_attribute_value[indc_na_or_empty,]
          }
          
          my_list_values[length(my_list_values)+1] <- insertion_column_AC
        }
        
      }
      
    } ## add the myListSUPP here 
    # First ascertain if the supplemental list is populated or not
    # But first, IDVAR is mandatory in the mapper file, and for SUPPDM it has no value, therefore it takes the value of ";", which is unnecessary, we need to make it NULL in this case (again this is relevant for SUPPDM)
    
    
    for (l in 1:length(myListSUPP)) {
      #if (length(unique(myListSUPP[[l]]==";"))==1 && unique(myListSUPP[[l]]==";")==TRUE) {
      if (length(grep("^;+$",myListSUPP[[l]]))==length(myListSUPP[[l]])) {   
        myListSUPP[l] <- list(NULL)
      }
    }
    ## Remove values that contain just ";" or remove the semi colon from something like "Not caucasian;"
    myListSUPP <- lapply(myListSUPP,function(x) {x <- gsub("^;+$","",x);return(x)})
    
    ascertainSUPPisPopulated <- vector(mode="character",length=length(myListSUPP)) # this will contain TRUE/FALSE values, TRUE if it has value, FALSE if it is empty
    for (l in 1:length(myListSUPP)) {ascertainSUPPisPopulated[l] <- !length(myListSUPP[[l]])==0}
    if (length(grep("TRUE",ascertainSUPPisPopulated))>0) { # if any of the elements within the list is populated go ahead and insert the values along with the parent domain
      
      for (k in 1:length(myListSUPP)) {
        if (ascertainSUPPisPopulated[k] ==TRUE) {
          my_list_column_headers[length(my_list_column_headers)+1] <- names(myListSUPP)[k]
          my_list_values[length(my_list_values)+1] <- as.data.frame(myListSUPP[[k]],stringsAsFactors=FALSE)
        }
      }
      
    }
    ## Take care of the FA table
    ascertainFAisPopulated <- vector(mode="character",length=length(myListFA)) # this will contain TRUE/FALSE values, TRUE if it has value, FALSE if it is empty
    for (l in 1:length(myListFA)) {ascertainFAisPopulated[l] <- !length(myListFA[[l]])==0}
    if (length(grep("TRUE",ascertainFAisPopulated))>0) { # if any of the elements within the list is populated go ahead and insert the values along with the parent domain
      
      for (k in 1:length(myListFA)) {
        if (ascertainFAisPopulated[k] ==TRUE) {
          my_list_column_headers[length(my_list_column_headers)+1] <- names(myListFA)[k]
          my_list_values[length(my_list_values)+1] <- as.data.frame(myListFA[[k]],stringsAsFactors=FALSE)
        }
      }
      
    }
    ## Take care of the CO table
    ascertainCOisPopulated <- vector(mode="character",length=length(myListCO)) # this will contain TRUE/FALSE values, TRUE if it has value, FALSE if it is empty
    for (l in 1:length(myListCO)) {ascertainCOisPopulated[l] <- !length(myListCO[[l]])==0}
    if (length(grep("TRUE",ascertainCOisPopulated))>0) { # if any of the elements within the list is populated go ahead and insert the values along with the parent domain
      
      for (k in 1:length(myListCO)) {
        if (ascertainCOisPopulated[k] ==TRUE) {
          my_list_column_headers[length(my_list_column_headers)+1] <- names(myListCO)[k]
          my_list_values[length(my_list_values)+1] <- as.data.frame(myListCO[[k]],stringsAsFactors=FALSE)
        }
      }
      
    }
    
  }
  ############# the final data frame that has the renames, insertions and all the related study attributes for the given study attribute
  df_temp <- as.data.frame(my_list_values,stringsAsFactors=FALSE)
  colnames(df_temp) <- my_list_column_headers
  return(df_temp)
  
}
##################################################################################################################################################
determine_associated_columns <- function(mapperFile) {
  mapper <- mapperFile
  associatedColumns <- data.frame(matrix(NA,nrow=0,ncol=2),stringsAsFactors=FALSE)
  # row=15
  # row=18
  for (row in 1:dim(mapper)[1]) {
    if (!mapper[row,2]=="") {
      elements_in_notes <- unlist(strsplit(mapper[row,3],";"))
      if (mapper[row,2]=="SUPPDM") { # This is to associate SUPPDM with a variable from the DM domain
        rowAC <- dim(associatedColumns)[1]+1
        associatedColumns[rowAC,1] <- mapper[min(which(mapper[2]=="DM")),1] # just the first DM domain variable from the mapper file 
        associatedColumns[rowAC,2] <- mapper[row,1]
      } else if (!length(grep(".*RelTo.*",elements_in_notes))==0) {
        rowAC <- dim(associatedColumns)[1]+1
        associatedColumns[rowAC,1] <-  gsub("RelTo:(.*)", "\\1", elements_in_notes[grep("RelTo",elements_in_notes)], perl=T)    
        associatedColumns[rowAC,2] <- mapper[row,1]
      }
      
    }
  }
  return(associatedColumns)
}
######################################################################################################
## Insert transformed data into SDTM table
insert_into_sdtm_table <- function (df_temp,MYList,mapper_file,row_number,drop=TRUE) {
  data_copy <- df_temp
  mapper <- mapper_file
  name_of_domain <- mapper[row_number,2]
  testList <- MYList
  domainIndc <- which(names(testList)==name_of_domain)
  ## temporary sdtm table
  df_sdtm_temp <- data.frame(matrix(NA,nrow=dim(data_copy)[1],ncol=dim(testList[[domainIndc]])[2]),stringsAsFactors=FALSE)
  colnames(df_sdtm_temp) <- colnames(testList[[domainIndc]])
  df_sdtm_temp[colnames(data_copy)] <- data_copy
  t1 <- testList[[domainIndc]] # t1 contains the domain table, could be empty/populated already

  if (length(unique(colnames(data_copy) %in% colnames(t1))) > 1) { ## this is a boolean test, if there is both TRUE and FALSE, the length will be greater than 1 which is not right
    test1 <-  colnames(data_copy)[!colnames(data_copy) %in% colnames(t1)]
    print(test1)
    stop("ERROR: VARIABLE NAME NOT DEFINED IN CDISC!!!")
  }
  ascertain_if_columns_are_na <- as.data.frame(unique(is.na(t1[colnames(data_copy)])))
  ascertain_if_columns_are_na <- ascertain_if_columns_are_na[!c(colnames(ascertain_if_columns_are_na) %in% c("STUDYID","USUBJID"))] # if the column has already been populated this will have a dim[1] of 1 and a value of TRUE
  if (dim(t1)[1]==0) {
    testList[[domainIndc]] <- df_sdtm_temp
  }
  else if (length(unique(as.character(ascertain_if_columns_are_na[1,])))==1  && unique(as.character(ascertain_if_columns_are_na[1,]))==TRUE) {
    t1[colnames(data_copy)] <- data_copy
    testList[[domainIndc]] <- t1
  }
  else {testList[[domainIndc]] <- rbind(testList[[domainIndc]],df_sdtm_temp)}
  return(testList[[domainIndc]])
}
##########################################
remove_empty_or_null_and_duplicate_rows <- function(populatedList) {
  testList <- populatedList
  testList <- lapply(testList,function(df) {
    if (!length(which(duplicated(df)))==0) {
      df <- df[-which(duplicated(df)),] #first remove the redundancy
    }
    t2 <- as.data.frame(df[,-which(colnames(df) %in% c("STUDYID","USUBJID","SPDEVID","RSUBJID"))],)
    if (length(which(apply(t2,1,function(x) all(is.na(x) | x=="")))) >0) {
      df <-df[-which(apply(t2,1,function(x) all(is.na(x) | x==""))),]
    } else {df <- df}
    # Remove potential redundance in parent domain due to empty rows in supp variables
    suppColumns <- c("RDOMAIN","IDVAR","IDVARVAL","QNAM","QLABEL","QVAL","QORIG","QEVAL")
    if (!length(which(colnames(df) %in% suppColumns))==0) {
      parentColumns_df <- as.data.frame(df[,-which(colnames(df) %in% suppColumns)])
      duplicated_rows_in_parent_domain <- as.numeric(which(duplicated(parentColumns_df)))
      onlySuppColumns_df <- as.data.frame(df[, which(colnames(df) %in% suppColumns)])
      emptyRowsInSupp <- as.numeric(which(apply(onlySuppColumns_df,1,function(x) all(is.na(x) | x=="")))) # identify all the rows that are empty in SUPP columns
      # remove the rows where the parent domain is duplicated and the supp columns are empty
      # this happens when the parent domain row is repeated (multiple SEQ values because of non-redundancy) in anticipation of SUPP values
      rowsToBeRemoved <- as.numeric(intersect(duplicated_rows_in_parent_domain,emptyRowsInSupp))
      if (!length(rowsToBeRemoved)==0) {
        df <- df[-rowsToBeRemoved,]
      } else {df <- df}
      
    } else {df <- df}
    
  })
}
#####################################################################
create_seq <- function (populatedList) {
  testList <- populatedList
  for (j in 1:length(testList)) { 
    domainName <- names(testList[j])
    if (!dim(testList[[j]])[1]==0 & !domainName %in% c("DM","TV","TD","TI","RELSUB")) { # These domains do not contain SEQ (for future work store these somewhere and read it in)
      t1 <- testList[[j]]
      ## to build the SEQ the USUBJID has to be ordered first, some domains that do not have USUBJID have to be ordered based on another variable
      ## these are provided below
      if (domainName %in% c("DI","DO","DT")) {
        t1 <- t1[with(t1, order(SPDEVID)), ]
        colNumberForIdentifier <- which(colnames(t1)=="SPDEVID")
      } else if (domainName=="TS") {
        t1 <- t1[with(t1, order(TSPARMCD)), ]
        colNumberForIdentifier <- which(colnames(t1)=="TSPARMCD")
      }else if (!length(grep("^AP..$",domainName))==0) {
        t1 <- t1[with(t1, order(APID)), ]
        colNumberForIdentifier <- which(colnames(t1)=="APID")
      }else {
        t1 <- t1[with(t1, order(USUBJID)), ]
        colNumberForIdentifier <- which(colnames(t1)=="USUBJID")
      }
      row.names(t1) <- NULL
      
      # if t1 is an AP domain, we need to consider the parent domain for SEQ purposes, for example, the domainName should be MH for APMH
      if (!length(grep("^AP..$",domainName))==0) {
        domainName <- gsub("AP(..)$","\\1",domainName)
      }
      colName_astSeq <- paste(domainName,"SEQ",sep="") # For example LB + SEQ = LBSEQ
      for(i in unique(t1[,colNumberForIdentifier])) {
        t1[which(t1[,colNumberForIdentifier]==i),which(colnames(t1)==colName_astSeq)] <- seq(1:length(which(t1[,colNumberForIdentifier]==i))) 
      }
      testList[[j]] <- t1
    }
  }
  return(testList) 
}
########################################################################
insert_domain_name <- function (populatedList) {
  testList <- populatedList
  for (j in 1:length(testList)) { 
    names <- names(testList[j])
    if (!names=="RELSUB") {
      if (!dim(testList[[j]])[1]==0) {
        t1 <- testList[[j]]
        t1[is.na(t1[,colnames(t1) %in% "DOMAIN"]),colnames(t1) %in% "DOMAIN"] <- names
      } else {next}
      testList[[j]] <- t1
    }
  }
  
  return(testList) 
}
#################################################################################
write_tables_to_folder <- function (populatedList,folder){
  testList <- populatedList
  
  for (names in names(testList)) {
    
    temp_file <- testList[[which(names(testList)==names)]] 
    if (!dim(temp_file)[1]==0) {
      #temp_file <- temp_file[,colSums(temp_file=="")<nrow(temp_file)] # removes empty columns in a data.frame
      dir <- paste(folder,"/",names,".txt",sep="")
      write.table(temp_file,dir,sep="\t",row.names=FALSE,col.names=TRUE,qmethod="double",quote=FALSE)
    }
    
  }
  
}
###############################################################################################
remove_NA_values <- function (populatedList) {
  testList <- populatedList
  for (j in 1:length(testList)) { 
    
    if (!dim(testList[[j]])[1]==0) {
      t1 <- testList[[j]]
      t1[is.na(t1)] <- ""
    } else {next}
    testList[[j]] <- t1
  }
  return(testList) 
}
############################################################################
identify_SUPP_add_cols_to_domain <- function(mapper_file,MyList) {
  testList <- MyList
  x <- mapper_file[grep("^SUPP.*$",mapper_file$Domain),c(2,3)]
  
  duplicateRowsToBeRemoved <- which(duplicated(x[1])) # if the mapper file contains the SUPP-- domain multiple times, remove it, we don't need it here
  if (dim(x)[1]==0) {
    df_empty <- data.frame(matrix("",nrow=0,ncol=2),stringsAsFactors=FALSE)
    return (list("myList"=testList,"suppTable"= df_empty))
  }else if (!dim(x)[1]==0 & !length(duplicateRowsToBeRemoved)==0) {
    x <- x[-c( duplicateRowsToBeRemoved),]
  }
  
  IDVAR <- gsub("^.*Insert:IDVAR,Val=(.*?);(.*$)", "\\1", as.character(x[,2]))
  Domain <- gsub("^SUPP","",x[,1])
  suppTable <- data.frame(Domain,IDVAR,stringsAsFactors=FALSE)
  #return(suppTable)
  
  
  for (i in 1:dim(suppTable)[1]) {
    name_of_domain <- suppTable[i,1]
    # domainIndc <- grep(name_of_domain,names(testList) ) which(names(testList)==name_of_domain)
    domainIndc <- which(names(testList)==name_of_domain)
    t1 <- testList[[domainIndc]] 
    colNamesOfParentDomain <- colnames(t1)
    colNamesOfSUPP <- c("RDOMAIN","IDVAR","IDVARVAL","QNAM","QLABEL","QVAL","QORIG","QEVAL")
    colNamesCombined <- c(colNamesOfParentDomain,colNamesOfSUPP)
    
    temp <- data.frame(matrix(NA,nrow=0,ncol=length(colNamesCombined)),stringsAsFactors=FALSE)
    colnames(temp) <- colNamesCombined 
    
    testList[[domainIndc]] <- temp
  }
  
  return(list("myList"=testList,"suppTable"=suppTable))
}
################################################################################################
generate_supplementary_tables <- function(MetaTable,MyList) {
  testList <- MyList
  if (dim(MetaTable)[1]==0) {
    return(testList)
  } else {
    for (i in 1:dim(MetaTable)[1]) {
      name_of_domain <- MetaTable[i,1]
      name_of_idvar <- MetaTable[i,2]
      ## Read in the domain 
      domainIndc <- which(names(testList)==name_of_domain)
      temp <- testList[[domainIndc]]
      if (!name_of_idvar=="") { ## name_of_idvar will be equal to "" for the DM domain
        temp$IDVARVAL <- temp[,colnames(temp)==name_of_idvar]
      }
      
      
      ## To insure that the supp table columns are in the right order
      sidNr <- which(colnames(temp)=="STUDYID")
      rdNr <- which(colnames(temp)=="RDOMAIN")
      if (length(grep("^AP..$",name_of_domain))==0) {
        usidNR <- which(colnames(temp)=="USUBJID")
      } else {usidNR <- which(colnames(temp)=="APID")}
      
      idvarNr <- which(colnames(temp)=="IDVAR")
      idvarvalNr <- which(colnames(temp)=="IDVARVAL")
      qnamNr <- which(colnames(temp)=="QNAM")
      qlabelNr <- which(colnames(temp)=="QLABEL")
      qvalNr <- which(colnames(temp)=="QVAL")
      qorigNr <- which(colnames(temp)=="QORIG")
      qevalNr <- which(colnames(temp)=="QEVAL")
      
      supp_table_temp <- temp[c(sidNr,rdNr,usidNR,idvarNr,idvarvalNr,qnamNr,qlabelNr,qvalNr,qorigNr,qevalNr)]
      ## remove the supp columns from the parent domain
      parentDomain <- temp[-c(rdNr,idvarNr,idvarvalNr,qnamNr,qlabelNr,qvalNr,qorigNr,qevalNr)]
      
      
      ## Populate the supp table in the list of domains and name it appropriately
      names_testList <- names(testList)
      domainIndcSupp <- length(testList)+1 
      name_of_supp_domain <- paste("SUPP",name_of_domain,sep="")
      
      
      ## remove empty rows
      if (!dim(supp_table_temp[-which((supp_table_temp$IDVAR=="" | is.na(supp_table_temp$IDVAR)) & (supp_table_temp$QNAM=="" | is.na(supp_table_temp$QNAM))),])[1]==0) {
        supp_table_temp <- supp_table_temp[-which((supp_table_temp$IDVAR=="" | is.na(supp_table_temp$IDVAR)) & (supp_table_temp$QNAM=="" | is.na(supp_table_temp$QNAM))),]
      }
      
      testList[[domainIndcSupp]] <- supp_table_temp
      names(testList)[domainIndcSupp] <-  name_of_supp_domain
      ## re-insert the changed parent domain
      testList[[domainIndc]] <- parentDomain
    }
    
    return(testList)
  }
  
}
#########################################################################3
suppTableForAssociated <- function(ListSUPP,elements_in_notes,data,domainName,mainVariableName,variableName) {
  
  # These are the column name and the values for the pivot variable (pivot: variable that will be present in column A of the mapper file, and may have a bunch of "Inserts" in the column C of the mapper file)
  rename_header_to_AC <- gsub("Rename:(.*)$", "\\1", elements_in_notes[grep("Rename",elements_in_notes)], perl=T)
  if (length(rename_header_to_AC)==0) {rename_header_to_AC <-variableName } #if rename has not been mentioned, use the study attribute
  
  # Here also bring in the main_pivot_variable
  indc_na_or_empty  <- which(is.na(data[,colnames(data) %in% mainVariableName]) | data[,colnames(data) %in% mainVariableName] == "")
  pivot_attribute_value <- data[,colnames(data) %in% c(variableName)]
  if (!domainName=="DM") {
    pivot_attribute_value[indc_na_or_empty] <- data[indc_na_or_empty,colnames(data) %in% mainVariableName]
  }
  if (length(ListSUPP[[which(names(ListSUPP)==rename_header_to_AC )]])==0) {
    ListSUPP[[which(names(ListSUPP)==rename_header_to_AC )]] <- pivot_attribute_value
  } else {
    ListSUPP[[which(names(ListSUPP)==rename_header_to_AC )]] <-  paste(ListSUPP[[which(names(ListSUPP)==rename_header_to_AC )]],pivot_attribute_value,sep=";")
  }
  
  ## Now the Inserts
  insertion_header_AC <-   gsub("^Insert:(.*),Val.*$", "\\1", elements_in_notes[grep("Insert",elements_in_notes)], perl=T)
  insertion_value_AC <-   gsub("^Insert:.*,Val=(.*)$", "\\1", elements_in_notes[grep("Insert",elements_in_notes)], perl=T)

  if (!length(insertion_header_AC)==0) {
    for (j in 1:length(insertion_header_AC)) {
      
      #insertion_column_AC <- as.character(rep(insertion_value_AC[j],dim(data)[1]))
      insertion_column_AC <- as.character(rep(insertion_value_AC[j],dim(data)[1]))
      indc_na_or_empty_pivot  <- which(is.na(pivot_attribute_value) | pivot_attribute_value == "")
      if (!length(indc_na_or_empty_pivot)==0) {
        if (domainName=="DM" & insertion_header_AC[j]=="IDVAR") {
          # for the DM domain we want IDVAR to be untouched, hence this arrangement
        } else {insertion_column_AC[indc_na_or_empty_pivot] <- pivot_attribute_value[indc_na_or_empty_pivot]}
        #insertion_column_AC[indc_na_or_empty] <- pivot_attribute_value[indc_na_or_empty]
        
      }
      if (length(ListSUPP[[which(names(ListSUPP)==insertion_header_AC[j])]])==0) {
        ListSUPP[[which(names(ListSUPP)==insertion_header_AC[j] )]] <- insertion_column_AC
      } else {
        ListSUPP[[which(names(ListSUPP)==insertion_header_AC[j] )]] <- paste(ListSUPP[[which(names(ListSUPP)==insertion_header_AC[j])]],insertion_column_AC,sep=";")
        
      }
    }
  }
  return(ListSUPP)
}
#############################################################################################
split_semicolon_values_recreate_dataframe <- function(MyList) {
  testList <- MyList
  suppDomainIndc <- grep("^SUPP",names(testList))
  
  if (length(suppDomainIndc)==0) {
    return(testList)
  } else if (!length(suppDomainIndc)==0) {
    for (j in 1:length(suppDomainIndc)) {
      suppDM <- testList[[suppDomainIndc[j]]]
      
      
      ## Identify rows in the data frame that has semicolons
      rowsWithSemiColon_temp_a <- apply(suppDM,1,function(x) {x <- length(grep(";",x)); return(x)})
      rowsWithSemiColon <- as.numeric(which(!rowsWithSemiColon_temp_a=="0"))
      if (length(rowsWithSemiColon)==0) {
        if (!length(which(duplicated(suppDM)))==0) {
          suppDM <- suppDM[-which(duplicated(suppDM)),]
        }
        testList[[suppDomainIndc[j]]] <- suppDM
        next;
      } else {
        suppDM_rowsWithSemiColon <- suppDM[rowsWithSemiColon,]
        
        ## Create an empty data frame that will contain the reshaped rows with values that have semicolon in them
        supp_SemiColon_temp <- data.frame(matrix("",nrow=0,ncol=dim(suppDM)[2]),stringsAsFactors=FALSE)
        
        ############
        ## populate the data frame which will have the reshaped data for rows with semi colons
        for (i in 1:dim(suppDM_rowsWithSemiColon)[1]) {
          currentRow <- suppDM_rowsWithSemiColon[i,]
          supp_SemiColon_temp <- split_semicolon_values_recreate_dataframe_subfunctionA(currentRow,supp_SemiColon_temp)
        }
        
        ## remove rows where all the columns "to be considered" are empty
        
        colNamesToBeConsidered <- c("qnamNr","qlabelNr","qvalNr","qorigNr","qevalNr")
        numberOfEmptyElements <- vector(mode="numeric",length=dim(supp_SemiColon_temp)[1])
        for (tempRow in 1:dim(supp_SemiColon_temp)[1]) {
          numberOfEmptyElements[tempRow] <- length(which(supp_SemiColon_temp[tempRow,which(colnames(supp_SemiColon_temp) %in%colNamesToBeConsidered)]=="NA" | supp_SemiColon_temp[tempRow,which(colnames(supp_SemiColon_temp) %in%colNamesToBeConsidered)]=="") )
        }
        
        # Now remove the empty rows
        if (!length(which(numberOfEmptyElements==length(colNamesToBeConsidered)))==0) {
          supp_SemiColon_temp <- supp_SemiColon_temp[-which(numberOfEmptyElements==length(colNamesToBeConsidered)),]
        }
        ## Now bring together the two data frames (rows without semicolons and reshaped rows with semicolons)
        suppDM_rowsWithOUTSemiColon <- suppDM[-rowsWithSemiColon,]
        colnames(supp_SemiColon_temp) <- colnames(suppDM_rowsWithOUTSemiColon)
        suppDM_Final <- rbind(suppDM_rowsWithOUTSemiColon,supp_SemiColon_temp)
        suppDM_Final <- suppDM_Final[with(suppDM_Final, order(USUBJID,IDVARVAL)), ]
        
        # remove duplicate values
        if (!length(which(duplicated(suppDM_Final)))==0) {
          suppDM_Final <- suppDM_Final[-which(duplicated(suppDM_Final)),]
        }
        testList[[suppDomainIndc[j]]] <- suppDM_Final
      }
      
    }
    
  }
  return(testList)
}
###########################################################################################
split_semicolon_values_recreate_dataframe_subfunctionA <- function(currentRow,supp_SemiColon_temp) {
  
  # identify the maximum number of rows that we may have to create
  MAX <- max(apply(currentRow[,grep(";",currentRow)],2,function(x) {
    as.numeric(length(unlist(strsplit(x,";"))))
  }))
  
  ## Initialize
  sidNr <- vector(mode="character",length=MAX)
  rdNr <- vector(mode="character",length=MAX)
  usidNR <- vector(mode="character",length=MAX)
  idvarNr <-vector(mode="character",length=MAX)
  idvarvalNr <-vector(mode="character",length=MAX)
  qnamNr <- vector(mode="character",length=MAX)
  qlabelNr <- vector(mode="character",length=MAX)
  qvalNr <- vector(mode="character",length=MAX)
  qorigNr <- vector(mode="character",length=MAX)
  qevalNr <- vector(mode="character",length=MAX)
  
  ## Populate for the fixed columns
  sidNr <- rep(as.character(currentRow[which(colnames(currentRow)=="STUDYID")]),length=MAX)
  usidNR <- rep(as.character(currentRow[which(colnames(currentRow)=="USUBJID")]),length=MAX)
  idvarvalNr <- rep(as.character(currentRow[which(colnames(currentRow)=="IDVARVAL")]),length=MAX)
  idvarvalNr[which(idvarvalNr=="NA")] <- NA # For DM domain whre IDVARVAL will be equal to "NA" , change the character NA to proper NA
  ## Populate the NON Fixed columns
  if (!is.na(currentRow[,colnames(currentRow)=="RDOMAIN"])) { # in a way RDOMAIN is fixed, however we want curators to mention this explicitly
    rdNr[1:length(unlist(strsplit(currentRow[,colnames(currentRow)=="RDOMAIN"],";")))] <- unlist(strsplit(currentRow[,colnames(currentRow)=="RDOMAIN"],";"))
  }
  if (!is.na(currentRow[,colnames(currentRow)=="IDVAR"])) { 
    idvarNr[1:length(unlist(strsplit(currentRow[,colnames(currentRow)=="IDVAR"],";")))] <- unlist(strsplit(currentRow[,colnames(currentRow)=="IDVAR"],";"))
  }
  if (!is.na(currentRow[,colnames(currentRow)=="QNAM"])) {
    qnamNr[1:length(unlist(strsplit(currentRow[,colnames(currentRow)=="QNAM"],";")))] <- unlist(strsplit(currentRow[,colnames(currentRow)=="QNAM"],";"))
  }
  if (!is.na(currentRow[,colnames(currentRow)=="QLABEL"])) {
    qlabelNr[1:length(unlist(strsplit(currentRow[,colnames(currentRow)=="QLABEL"],";")))] <- unlist(strsplit(currentRow[,colnames(currentRow)=="QLABEL"],";"))
  }
  if (!is.na(currentRow[,colnames(currentRow)=="QVAL"])) {
    qvalNr[1:length(unlist(strsplit(currentRow[,colnames(currentRow)=="QVAL"],";")))] <- unlist(strsplit(currentRow[,colnames(currentRow)=="QVAL"],";"))
  }
  if (!is.na(currentRow[,colnames(currentRow)=="QORIG"])) {
    qorigNr[1:length(unlist(strsplit(currentRow[,colnames(currentRow)=="QORIG"],";")))] <- unlist(strsplit(currentRow[,colnames(currentRow)=="QORIG"],";"))
  }
  if (!is.na(currentRow[,colnames(currentRow)=="QEVAL"])) {
    qevalNr[1:length(unlist(strsplit(currentRow[,colnames(currentRow)=="QEVAL"],";")))] <- unlist(strsplit(currentRow[,colnames(currentRow)=="QEVAL"],";"))
  }
  x <- data.frame(sidNr,rdNr,usidNR,idvarNr,idvarvalNr,qnamNr,qlabelNr,qvalNr,qorigNr,qevalNr,stringsAsFactors=FALSE)
  supp_SemiColon_temp <- rbind(supp_SemiColon_temp,x)  
  return(supp_SemiColon_temp)
}
#####################################################################################################################################
identify_FA_add_cols_to_domain <- function(mapper_file,MyList) {
  testList <- MyList
  x <- mapper_file[grep("^FA..$",mapper_file$Domain),c(2,3)]
  
  duplicateRowsToBeRemoved <- which(duplicated(x[1])) # if the mapper file contains the SUPP-- domain multiple times, remove it, we don't need it here
  if (dim(x)[1]==0) {
    df_empty <- data.frame(matrix("",nrow=0,ncol=2),stringsAsFactors=FALSE)
    return (list("myList"=testList,"faTable"= df_empty))
  }else if (!dim(x)[1]==0 & !length(duplicateRowsToBeRemoved)==0) {
    x <- x[-c( duplicateRowsToBeRemoved),]
  }
  
  #IDVAR <- gsub("^.*Insert:IDVAR,Val=(.*?);(.*$)", "\\1", as.character(x[,2]))
  IDVAR <- rep("SEQ",dim(x)[1])
  Domain <- gsub("^FA","",x[,1])
  IDVAR <- paste(Domain,IDVAR,sep="")
  faTable <- data.frame(Domain,IDVAR,stringsAsFactors=FALSE)
  #return(suppTable)
  
  
  for (i in 1:dim(faTable)[1]) {
    name_of_domain <- faTable[i,1]
    domainIndc <- which(names(testList)==name_of_domain)
    t1 <- testList[[domainIndc]] 
    colNamesOfParentDomain <- colnames(t1)
    colNamesOfFA <- colnames(testList[[which(names(testList)=="FA")]])
    colNamesOfFA <- colNamesOfFA[-which(colNamesOfFA %in% c("STUDYID","DOMAIN","USUBJID"))]
    colNamesCombined <- c(colNamesOfParentDomain,colNamesOfFA)
    
    temp <- data.frame(matrix(NA,nrow=0,ncol=length(colNamesCombined)),stringsAsFactors=FALSE)
    colnames(temp) <- colNamesCombined 
    
    testList[[domainIndc]] <- temp
  }
  
  return(list("myList"=testList,"faTable"=faTable))
}
#####################################################################################################################################
faTableForAssociated <- function(ListFA,elements_in_notes,data,domainName,mainVariableName,variableName) {
  
  # These are the column name and the values for the pivot variable (pivot: variable that will be present in column A of the mapper file, and may have a bunch of "Inserts" in the column C of the mapper file)
  rename_header_to_AC <- gsub("Rename:(.*)$", "\\1", elements_in_notes[grep("Rename",elements_in_notes)], perl=T)
  if (length(rename_header_to_AC)==0) {rename_header_to_AC <-variableName } #if rename has not been mentioned, use the study attribute
  
  # Here also bring in the main_pivot_variable
  indc_na_or_empty  <- which(is.na(data[,colnames(data) %in% mainVariableName]) | data[,colnames(data) %in% mainVariableName] == "")
  pivot_attribute_value <- data[,colnames(data) %in% c(variableName)]
  if (!domainName=="DM" & !length(indc_na_or_empty)==0) { # DM is a case where we do not want to depend on the pivot value, I guess we should follow it for FA as well, but we will think about it later
    pivot_attribute_value[indc_na_or_empty] <- data[indc_na_or_empty,colnames(data) %in% mainVariableName]
  }
  
  
  if (length(ListFA[[which(names(ListFA)==rename_header_to_AC )]])==0) { #if the variable has not been populated
    ListFA[[which(names(ListFA)==rename_header_to_AC )]] <- pivot_attribute_value
  } else {
    ListFA[[which(names(ListFA)==rename_header_to_AC )]] <-  paste(ListFA[[which(names(ListFA)==rename_header_to_AC )]],pivot_attribute_value,sep=";")
  }
  
  ## Now the Inserts
  insertion_header_AC <-   gsub("^Insert:(.*),.*$", "\\1", elements_in_notes[grep("Insert",elements_in_notes)], perl=T)
  insertion_value_AC <-   gsub("^Insert:.*,Val=(.*)$", "\\1", elements_in_notes[grep("Insert",elements_in_notes)], perl=T)
  
  
  
  if (!length(insertion_header_AC)==0) {
    for (j in 1:length(insertion_header_AC)) {
      
      #insertion_column_AC <- as.character(rep(insertion_value_AC[j],dim(data)[1]))
      insertion_column_AC <- as.character(rep(insertion_value_AC[j],dim(data)[1]))
      indc_na_or_empty_pivot  <- which(is.na(pivot_attribute_value) | pivot_attribute_value == "")
      if (!length(indc_na_or_empty_pivot)==0) {
        
        #insertion_column_AC[indc_na_or_empty] <- pivot_attribute_value[indc_na_or_empty]
        insertion_column_AC[indc_na_or_empty_pivot] <- pivot_attribute_value[indc_na_or_empty_pivot]
      }
      if (length(ListFA[[which(names(ListFA)==insertion_header_AC[j])]])==0) {
        ListFA[[which(names(ListFA)==insertion_header_AC[j] )]] <- insertion_column_AC
      } else {
        ListFA[[which(names(ListFA)==insertion_header_AC[j] )]] <- paste(ListFA[[which(names(ListFA)==insertion_header_AC[j])]],insertion_column_AC,sep=";")
        
      }
    }
  }
  return(ListFA)
}
###############################################################################################################################
generate_fa_tables <- function(MetaTable,MyList) {
  testList <- MyList
  if (dim(MetaTable)[1]==0) {
    return(testList)
  } else {
    for (i in 1:dim(MetaTable)[1]) {
      name_of_domain <- MetaTable[i,1]
      name_of_idvar <- MetaTable[i,2]
      ## Read in the domain 
      domainIndc <- which(names(testList)==name_of_domain)
      temp <- testList[[domainIndc]]
      if (!name_of_idvar=="") { ## name_of_idvar will be equal to "" for the DM domain
        temp$FASEQ <- temp[,colnames(temp)==name_of_idvar]
      }
      
      
      # Grab the variables of the FA domain, other than the keys including SEQ, for a particular row if all variables (of FA domain) are empty remove the row
      colnamesOfFA <- colnames(testList[[which(names(testList)=="FA")]]) # column names of the FA domain
      faVariablesForTesting_colNames <- colnamesOfFA[-c(1:4)] # col names that need to be tested for emptiness, remove the key variables including FASEQ (because we have already populated it above)
      faVariablesForTesting_df <- temp[,colnames(temp) %in% faVariablesForTesting_colNames] # df of relevant columns
      len <- apply(faVariablesForTesting_df,1,function(x) {len <- length(c(which(x==""),which(is.na(x))));return(len)}) # len will contain the number of columns that are either empty or na
      if (!length(which(len==dim(faVariablesForTesting_df)[2]))==0) {
        temp_fa <-  temp[-which(len==dim(faVariablesForTesting_df)[2]),] # remove the empty rows, create a FA temptemp_fa <-  temp[-which(len==dim(faVariablesForTesting_df)[2]),] # remove the empty rows, create a FA temp
      } else {temp_fa <- temp}
      
      # To subset the columns relevant to the FA domain
      # Issues: Parent domain and FA domain may have identical variables
      fa_table <- temp_fa[,c(which(colnames(temp_fa) %in% c("STUDYID","DOMAIN","USUBJID")),min(grep("^FA.*$",colnames(temp_fa))):dim(temp_fa)[2])]
      
      ## remove the fa domain columns from the parent domain (STUDYID,DOMAIN,USUBJID are common to both domains, don't remove them)
      #colNamesUniqueToFA <- colnamesOfFA[-which(colnamesOfFA %in% c("STUDYID","DOMAIN","USUBJID"))]
      parentDomain <- temp[,c(1:(min(grep("^FA.*$",colnames(temp)))-1))] # grab every variable upto the first FA variable (basically FASEQ)
      
      
      ## Populate the supp table in the list of domains and name it appropriately
      #names_testList <- names(testList)
      domainIndcFA <- length(testList)+1 
      name_of_FA_domain <- paste("FA",name_of_domain,sep="")
      
      
      # insert the fa domain table
      testList[[domainIndcFA]] <- fa_table
      names(testList)[domainIndcFA] <-  name_of_FA_domain
      ## re-insert the changed parent domain
      testList[[domainIndc]] <- parentDomain
      
      ## create relrec assuming one to relationship
      # first read in the RELREC table from testList
      rr_colnames <- colnames(testList[[which(names(testList)=="RELREC")]])
      
      all_study_ids_in_fa_table <- unique(fa_table$STUDYID)
      ## create vectors based on the above
      sid <- rep(all_study_ids_in_fa_table,each=2)
      rdomain <- rep(c(name_of_domain,"FA"),(length(sid)/2))
      idvar <- rep(c(paste(name_of_domain,"SEQ",sep=""),"FASEQ"),(length(sid)/2))
      reltype <- rep("ONE",length(sid))
      
      rr_temp <- as.data.frame(matrix("",nrow=length(sid),ncol=length(rr_colnames)),stringsAsFactors=FALSE)
      colnames(rr_temp) <- rr_colnames
      
      rr_temp$STUDYID <- sid
      rr_temp$RDOMAIN <- rdomain
      rr_temp$IDVAR <- idvar
      rr_temp$RELTYPE <- reltype
      
      ## name the relrec as name of domain+FA and insert into testList
      name_of_relrec_domain <- paste("RELREC","_FA",name_of_domain,sep="")
      domainIndcRELREC <- length(testList)+1 
      testList[[domainIndcRELREC]]  <- rr_temp
      names(testList)[domainIndcRELREC] <-  name_of_relrec_domain
      
    }
    
    return(testList)
    
  }
  
}
############################################################
create_ap_domains <- function(mapper_file,MyList) {
  testList <- MyList
  ap_domains <- unique(mapper_file[grep("^AP..$",mapper_file$Domain),c(2)])
  
  for (i in 1:length(ap_domains)) {
    domain <-  gsub("^AP(.*)$","\\1",ap_domains[i])
    columnNames <- colnames(testList[[which(names(testList)==domain)]])
    columnNames <- columnNames[-which(columnNames %in% c("STUDYID","DOMAIN","USUBJID"))] # remove the idenitifiers from the original domain
    # insure a proper order of variables within domain
    if (!length(grep("^..SEQ$",columnNames))==0) {
      columnNames <- c("STUDYID","DOMAIN","APID",columnNames[grep("^..SEQ$",columnNames)],"RSUBJID","RDEVID","SREL",columnNames[-grep("^..SEQ$",columnNames)])
    } else if (domain=="DM"){
      columnNames <- columnNames[-which(columnNames %in% c("SUBJID","RFSTDTC","RFENDTC","RFXSTDTC","RFXENDTC","RFICDTC","RFPENDTC","ARMCD","ARM","ACTARMCD","ACTARM"))]
      columnNames <- c("STUDYID","DOMAIN","APID","RSUBJID","RDEVID","SREL",columnNames)
    }
    
    temp <- data.frame(matrix(NA,nrow=0,ncol=length(columnNames)),stringsAsFactors=FALSE)
    colnames(temp) <- columnNames
    testList[length(testList)+1] <- list(temp)
    names(testList)[length(testList)] <- ap_domains[i]
  }
  
  return(testList)
}
######################################################################
generate_apid <- function(MyList) {
  testList <- MyList
  
  df_rsubjid_srel <- data.frame(matrix("",nrow=0,ncol=2),stringsAsFactors=FALSE) # this is to store all subjid and srels from all ap domains
  if (!length(grep("^AP.*$",names(testList)))==0) {
    indc_for_ap_domains <- grep("^AP.*$",names(testList))
    # if the apid is already populated
    ap_df_temp <- testList[[indc_for_ap_domains[1]]]
    if (!length(which(is.na(ap_df_temp$APID)==TRUE))==dim(ap_df_temp)[1]) {  # return if the apid is already populated
      return(testList)
    } 
    
    for (i in 1:length(indc_for_ap_domains)) {
      ap_df_temp <- testList[[indc_for_ap_domains[i]]]
      df_rsubjid_srel <- rbind(df_rsubjid_srel,ap_df_temp[,which(colnames(ap_df_temp) %in% c("RSUBJID","SREL"))])
    }
    # Remove redundancies in df_rsubjid_srel
    if (!length(which(duplicated(df_rsubjid_srel)))==0) {
      df_rsubjid_srel <- df_rsubjid_srel[-which(duplicated(df_rsubjid_srel)),]
    }
    
    # generate the random numbers
    set.seed(17)
    if (dim(df_rsubjid_srel)[1] > 1000) {
      randomNumber <- sample(1001:9999,dim(df_rsubjid_srel)[1],replace=F)
    } else { randomNumber <- sample(100:999,dim(df_rsubjid_srel)[1],replace=F)}
    
    APID <- paste("AP",randomNumber,sep="-")
    df_rsubjid_srel <- data.frame(df_rsubjid_srel,APID,stringsAsFactors=FALSE)
    
    ## Now that we have the APIDs, we need to insert them in the AP domains 
    ## iterate over the AP domains
    concatenate_rsubjid_srel <- paste(df_rsubjid_srel$RSUBJID,df_rsubjid_srel$SREL,sep=":")
    df_rsubjid_srel <- data.frame(df_rsubjid_srel,concatenate_rsubjid_srel,stringsAsFactors=FALSE)
    for (i in 1:length(indc_for_ap_domains)) {
      ap_df_temp <- testList[[indc_for_ap_domains[i]]]
      for (j in 1:dim(ap_df_temp)[1]) {
        apid_temp <- df_rsubjid_srel[which(df_rsubjid_srel$concatenate_rsubjid_srel==paste(ap_df_temp$RSUBJID[j],ap_df_temp$SREL[j],sep=":")),3]
        ap_df_temp[j,which(colnames(ap_df_temp)=="APID")] <- apid_temp
      }
      # apid is inserted, put the temp df back into testList
      testList[[indc_for_ap_domains[i]]] <- ap_df_temp
      
    }
  }
  return(testList)
  
}
#################################################################################################################
handle_multiple_RelTo <- function(mapper_file,data_copy) {
  data <- data_copy
  mapper <- mapper_file
  
  ##
  newAttributesToBeCreated <- data.frame(matrix(NA,nrow=0,ncol=2),stringsAsFactors=FALSE)
  colnames(newAttributesToBeCreated) <- c("newAttribute","copyValuesFrom")
  
  appendToMapperFile <- data.frame(matrix(NA,nrow=0,ncol=3),stringsAsFactors=FALSE)
  colnames(appendToMapperFile) <- c("original.attribute","Domain","Mapping.function")
  
  mapperRowsToDelete <- c()
  # row=103
  for (row in 1:dim(mapper)[1]) {
    if (!mapper[row,2]=="") {
      elements_in_notes <- unlist(strsplit(mapper[row,3],";"))
      if (length(grep("RelTo",elements_in_notes))>1) {
        mapperRowsToDelete <- c(mapperRowsToDelete,row)
        origAttribute <- mapper[row,1]
        RelTo_Elements <- elements_in_notes[grep("RelTo",elements_in_notes)]
        
        for (i in 1:length(RelTo_Elements)) {
          appendAtRow <- dim(newAttributesToBeCreated)[1]+1
          newAttributesToBeCreated[appendAtRow,c(1,2)] <- c(paste(origAttribute,unlist(strsplit(RelTo_Elements[i],":"))[2],sep="_"),origAttribute)
          
          ## Running mapper file, to be appended at the end
          appendAtRow_mapper <- dim(appendToMapperFile)[1]+1
          # all the mapping functions other than RelTo have to be inserted back for the new variable
          # if there is nothing other than RelTo's in elements_in_notes, then we need to insert Rename:[origAttribute]
          if (paste(elements_in_notes[-grep("RelTo",elements_in_notes)],collapse=";")=="") {
            mappingElements <- paste(paste0("Rename:",origAttribute),paste(RelTo_Elements[i],";",sep=""),sep=";")
          } else {
            mappingElements <- paste(paste(elements_in_notes[-grep("RelTo",elements_in_notes)],collapse=";"),paste(RelTo_Elements[i],";",sep=""),sep=";")
          }
          
          appendToMapperFile[appendAtRow_mapper,c(1,2,3)] <- c(paste(origAttribute,unlist(strsplit(RelTo_Elements[i],":"))[2],sep="_"),mapper[row,2],mappingElements)
        }
        
        
      }
    }
  }
  
  ## if we have any multiple RelTo's in the mapper file, we have to split up these into multiple rows in the mapper file and add the new attributes in the data file
  
  if (!length(mapperRowsToDelete)==0) {
    # first change the mapper file
    mapper <- mapper[-mapperRowsToDelete,]
    mapper <- rbind(mapper,appendToMapperFile)
    # now change the data file
    dataToBeAppended <- data.frame(matrix("",nrow=dim(data)[1],ncol=dim(newAttributesToBeCreated)[1]),stringsAsFactors=FALSE)
    colnames(dataToBeAppended) <- newAttributesToBeCreated[,1]
    for (i in 1:dim(newAttributesToBeCreated)[1]) {
      dataToBeAppended[,i] <- data[,which(colnames(data)==newAttributesToBeCreated[i,2])]
    }
    
    data <- data.frame(data,dataToBeAppended,stringsAsFactors=FALSE,check.names=FALSE)
    
  }
  return (list("myData"=data,"myMapper"= mapper))
}
#########################################################################################################
identify_CO_add_cols_to_domain <- function(mapper_file,MyList,associatedColumn_df_fn) {
  testList <- MyList
  
  # for the CO domain, change the mapper file so that the CO domain specific variables are prefixed with CO_, this way they will not clash with SUPP domain/FA domain
  for (i in 1:dim(mapper_file)[1]) {
    if (mapper_file$Domain[i]=="CO") {
      #change renames and Inserts with the prefix CO_ so that it does not clash with supp domain
      elements_in_notes <- unlist(strsplit(mapper_file[i,3],";"))
      elements_in_notes <- gsub("Insert:","Insert:CO_",elements_in_notes)
      elements_in_notes <- gsub("Rename:","Rename:CO_",elements_in_notes)
      mapper_file[i,3] <- paste(elements_in_notes,collapse=";")
      mapper_file[i,3] <- paste(mapper_file[i,3],";",sep="")
      # change the domain name as well, example from CO to CO_SV
      commentRelatedTo_studyVariableName <-  gsub("^.*RelTo:(.*?);.*$", "\\1", mapper_file[i,3])
      commentRelatedTo_domainName <- mapper_file[which(mapper_file[1]==commentRelatedTo_studyVariableName),2]
      # In the case of comments for the DM domain, curators will populate the mapper file as: Insert:IDVAR,Val=; for these cases I must insist that they mention 
      commentRelatedTo_domainName_RDOMAIN <-  gsub("^.*Insert:CO_RDOMAIN,Val=(.*?);.*$", "\\1", mapper_file[i,3])
      if (commentRelatedTo_domainName_RDOMAIN=="DM") {
        commentRelatedTo_domainName <- "DM"
        ## Also, in the case of DM, there is no RelTo that the curator defines, so the program needs to take the initiative to populate the associatedColumns_df
        ## this is normally handled by the function: determine_associated_columns, however in the order of running the functions we have already passed the latter, hence need to take care of this one here
        associatedColumn_df_fn[(dim(associatedColumn_df_fn)[1]+1),c(1,2)] <-  c(mapper_file[min(which(mapper_file[2]=="DM")),1],mapper_file[i,1])
      }
      mapper_file[i,2] <- paste(mapper_file[i,2],commentRelatedTo_domainName,sep="_")
      
    }
    
  }
  # now change the names of the variables in the CO domain by prefixing them with CO_
  colnames(testList[[which(names(testList)=="CO")]]) <- paste("CO_",colnames(testList[[which(names(testList)=="CO")]]),sep="")
  
  ## now proceed as usual 
  x <- mapper_file[grep("^CO.*$",mapper_file$Domain),c(2,3)]
  
  duplicateRowsToBeRemoved <- which(duplicated(x[1])) # if the mapper file contains the SUPP-- domain multiple times, remove it, we don't need it here
  if (dim(x)[1]==0) {
    df_empty <- data.frame(matrix("",nrow=0,ncol=2),stringsAsFactors=FALSE)
    return (list("myList"=testList,"myMapper"=mapper_file,"coTable"= df_empty,"associatedColumns"=associatedColumn_df_fn))
  }else if (!dim(x)[1]==0 & !length(duplicateRowsToBeRemoved)==0) {
    x <- x[-c( duplicateRowsToBeRemoved),]
  }
  
  IDVAR <- gsub("^.*Insert:CO_IDVAR,Val=(.*?);.*$", "\\1", as.character(x[,2]))
  Domain <- gsub("^CO_","",x[,1])
  coTable <- data.frame(Domain,IDVAR,stringsAsFactors=FALSE)
  #return(suppTable)
  
  
  for (i in 1:dim(coTable)[1]) {
    name_of_domain <- coTable[i,1]
    # domainIndc <- grep(name_of_domain,names(testList) ) which(names(testList)==name_of_domain)
    domainIndc <- which(names(testList)==name_of_domain)
    t1 <- testList[[domainIndc]] 
    colNamesOfParentDomain <- colnames(t1)
    colNamesOfCO <- colnames(testList[[which(names(testList)=="CO")]])
    colNamesCombined <- c(colNamesOfParentDomain,colNamesOfCO)
    
    temp <- data.frame(matrix(NA,nrow=0,ncol=length(colNamesCombined)),stringsAsFactors=FALSE)
    colnames(temp) <- colNamesCombined 
    
    testList[[domainIndc]] <- temp
  }
  
  return(list("myList"=testList,"myMapper"=mapper_file,"coTable"= coTable,"associatedColumns"=associatedColumn_df_fn))
}
##############################################################################################################################
coTableForAssociated <- function(ListCO,elements_in_notes,data,domainName,mainVariableName,variableName) {
  
  # These are the column name and the values for the pivot variable (pivot: variable that will be present in column A of the mapper file, and may have a bunch of "Inserts" in the column C of the mapper file)
  rename_header_to_AC <- gsub("Rename:(.*)$", "\\1", elements_in_notes[grep("Rename",elements_in_notes)], perl=T)
  if (length(rename_header_to_AC)==0) {rename_header_to_AC <-variableName } #if rename has not been mentioned, use the study attribute
  
  # Here also bring in the main_pivot_variable
  indc_na_or_empty  <- which(is.na(data[,colnames(data) %in% mainVariableName]) | data[,colnames(data) %in% mainVariableName] == "")
  pivot_attribute_value <- data[,colnames(data) %in% c(variableName)]
  if (!domainName=="DM" & !length(indc_na_or_empty)==0) { # DM is a case where we do not want to depend on the pivot value, I guess we should follow it for FA as well, but we will think about it later
    pivot_attribute_value[indc_na_or_empty] <- data[indc_na_or_empty,colnames(data) %in% mainVariableName]
  }
  
  
  if (length(ListCO[[which(names(ListCO)==rename_header_to_AC )]])==0) { #if the variable has not been populated
    ListCO[[which(names(ListCO)==rename_header_to_AC )]] <- pivot_attribute_value
  } else {
    ListCO[[which(names(ListCO)==rename_header_to_AC )]] <-  paste(ListCO[[which(names(ListCO)==rename_header_to_AC )]],pivot_attribute_value,sep=";")
  }
  
  ## Now the Inserts
  insertion_header_AC <-   gsub("^Insert:(.*),.*$", "\\1", elements_in_notes[grep("Insert",elements_in_notes)], perl=T)
  insertion_value_AC <-   gsub("^Insert:.*,Val=(.*)$", "\\1", elements_in_notes[grep("Insert",elements_in_notes)], perl=T)
  
  
  
  if (!length(insertion_header_AC)==0) {
    for (j in 1:length(insertion_header_AC)) {
      
      #insertion_column_AC <- as.character(rep(insertion_value_AC[j],dim(data)[1]))
      insertion_column_AC <- as.character(rep(insertion_value_AC[j],dim(data)[1]))
      indc_na_or_empty_pivot  <- which(is.na(pivot_attribute_value) | pivot_attribute_value == "")
      if (!length(indc_na_or_empty_pivot)==0) {
        
        #insertion_column_AC[indc_na_or_empty] <- pivot_attribute_value[indc_na_or_empty]
        insertion_column_AC[indc_na_or_empty_pivot] <- pivot_attribute_value[indc_na_or_empty_pivot]
      }
      if (length(ListCO[[which(names(ListCO)==insertion_header_AC[j])]])==0) {
        ListCO[[which(names(ListCO)==insertion_header_AC[j] )]] <- insertion_column_AC
      } else {
        ListCO[[which(names(ListCO)==insertion_header_AC[j] )]] <- paste(ListCO[[which(names(ListCO)==insertion_header_AC[j])]],insertion_column_AC,sep=";")
        
      }
    }
  }
  return(ListCO)
}
##############################################################################################################################################################
generate_co_tables <- function(MetaTable,MyList) {
  testList <- MyList
  if (dim(MetaTable)[1]==0) {
    return(testList)
  } else {
    for (i in 1:dim(MetaTable)[1]) {
      name_of_domain <- MetaTable[i,1]
      name_of_idvar <- MetaTable[i,2]
      ## Read in the domain 
      domainIndc <- which(names(testList)==name_of_domain)
      temp <- testList[[domainIndc]]
      if (!name_of_idvar=="") { ## name_of_idvar will be equal to "" for the DM domain
        temp$CO_IDVARVAL <- temp[,colnames(temp)==name_of_idvar]
        temp$CO_IDVAR <- rep(name_of_idvar,length=dim(temp)[1])
      }
      # Populate STUDYID, DOMAIN, RDOMAIN
      temp$CO_STUDYID <- temp$STUDYID
      temp$CO_RDOMAIN <- temp$DOMAIN
      temp$CO_USUBJID <- temp$USUBJID
      temp$CO_DOMAIN <- rep("CO",length=dim(temp)[1])
      
      # Grab the variables of the CO domain, other than the keys including SEQ, for a particular row if all variables (of CO domain) are empty remove the row
      colnamesOfCO <- colnames(testList[[which(names(testList)=="CO")]]) # column names of the CO domain
      coVariablesForTesting_colNames <- colnamesOfCO[-which(colnamesOfCO %in% c("CO_STUDYID","CO_DOMAIN","CO_RDOMAIN","CO_USUBJID","CO_IDVAR","CO_IDVARVAL"))] # col names that need to be tested for emptiness, remove the key variables including FASEQ (because we have already populated it above)
      coVariablesForTesting_df <- temp[,colnames(temp) %in% coVariablesForTesting_colNames] # df of relevant columns
      
      len <- apply(coVariablesForTesting_df,1,function(x) {len <- length(c(which(x==""),which(is.na(x))));return(len)}) # len will contain the number of columns that are either empty or na
      
      temp_co <-  temp[-which(len==dim(coVariablesForTesting_df)[2]),] # remove the empty rows, create a CO temp
      
      # Subset the comment domain
      co_table <- temp_co[,grep("^CO_.*$",colnames(temp_co))] 
      # Now remove the CO_ from the variable names
      colnames(co_table) <- gsub("^CO_(.*)$","\\1",colnames(co_table))
      ## remove the fa domain columns from the parent domain (STUDYID,DOMAIN,USUBJID are common to both domains, don't remove them)
      #colNamesUniqueToFA <- colnamesOfFA[-which(colnamesOfFA %in% c("STUDYID","DOMAIN","USUBJID"))]
      parentDomain <- temp[,-grep("^CO_.*$",colnames(temp_co))] # grab all variables that do not begin with CO_
      
      # identify the index where the current comment domain will be inserted and the name of of the domain, for example CO_SV      
      domainIndcCO <- length(testList)+1 
      name_of_CO_domain <- paste("CO",name_of_domain,sep="_")
      
      # insert the  domain table
      testList[[domainIndcCO]] <- co_table
      names(testList)[domainIndcCO] <-  name_of_CO_domain
      ## re-insert the changed parent domain
      testList[[domainIndc]] <- parentDomain
      
    } # After going through the metaTable, we need to merge all the CO_ tables into one CO table, that takes place below
    # do.call calls the list of COMMENT data frames rbinds them, this needs to be transformed before it is put back in the COMMENTS domain
    
    commentsDomain_temp <- data.frame(t(do.call(rbind,testList[[grep("CO_",names(testList))]])),stringsAsFactors=FALSE)
    # create seq
    commentsDomain_temp <- commentsDomain_temp[with(commentsDomain_temp, order(USUBJID)), ]
    for(i in unique(commentsDomain_temp[,which(colnames(commentsDomain_temp)=="USUBJID")])) {
      commentsDomain_temp[which(commentsDomain_temp[,which(colnames(commentsDomain_temp)=="USUBJID")]==i),which(colnames(commentsDomain_temp)=="COSEQ")] <- seq(1:length(which(commentsDomain_temp[,which(colnames(commentsDomain_temp)=="USUBJID")]==i))) 
    }
    
    # Before we return the testList, it is a good idea to remove the CO_ tables (they are present in CO table now)
    testList <- testList[-grep("^CO_.*$",names(testList))]
    testList[[which(names(testList)=="CO")]] <- commentsDomain_temp
    
    # Now return the testList
    return(testList)
    
  }
  
}
########################################################################################################################
#library(data.table)
#x <- data.table(read.table("C:/Users/HDHARURI/Desktop/SVN_Harish/CDISC_conversion/ToConvert/metaData.txt",fill=TRUE,header=TRUE,sep="\t",comment.char = "",row.names=NULL,quote="\"",colClasses="character",check.names=TRUE))

metaDataNATOR <- function(x) {
  metaData_raw <- x
  
  for (metaRow in 1:dim(metaData_raw)[1]) {
    if (!exists("dfFinal",inherits=FALSE)) {
      dfFinal <- data.frame(matrix("",nrow=0,ncol=4),stringsAsFactors=FALSE)
      colnames(dfFinal) <- c("Working_directory","Annotation_table","Mapper_file","Domains")
    }
    
    workingDirectory <- as.character(metaData_raw[metaRow,Working_directory])
    annoTables <- unlist(strsplit(as.character(metaData_raw[metaRow,Annotation_table]),";"))
    annoTables <- paste(workingDirectory,annoTables,sep="/")
    # annoTables will contain the name(s) of the annotation tables (present in a cell seperated by semi-colons) 
    annoAttributes <- lapply(annoTables,function(read) { # will contain the original attributes for all of the annotation tables
      dat <- read.table(read,fill=TRUE,header=TRUE,sep="\t",comment.char = "",row.names=NULL,quote="\"",colClasses="character",check.names=FALSE)
      colnames(dat)
    })
    
    ## Pre-processing of annoAttributes so that they are in harmony with column names of annotation table
    annoAttributes <- lapply(annoAttributes, function(x) {
      #x <- gsub("\\(|\\)|\\s+|-",".",x)
      x <- gsub("\\(|\\)|\\s+|-|\\||\\/|:\\=|\\?|\\[|\\]",".",x)
      x
    })
    
    
    mappingFileName <- paste(workingDirectory,as.character(metaData_raw[metaRow,Mapper_file]),sep="/")
    mappingFile <- data.table(read.table(mappingFileName,fill=TRUE,header=TRUE,sep="\t",comment.char = "",row.names=NULL,quote="\"",colClasses="character",check.names=TRUE))
    
    
    #mappingFile <- mappingFile[,domainHarmo:=gsub("^AP|SUPP|FA|SUPPAP","",Domain)] #domainHarmo will contain the domain domain without their SUPP/AP prefixes
    mappingFile <- mappingFile[,domainHarmo:=gsub("^SUPP|FA|SUPPAP","",Domain)] #domainHarmo will contain the domain domain without their SUPP/AP prefixes
    allDomains <- unique(mappingFile[,domainHarmo]) # unique domains present in the mapping file
    #allDomains <- unique(mappingFile$Domain)
    allDomains <- allDomains[-which(allDomains=="")]
    
    ## create two vectors that will contain domain and corresponding annotation table file name
    ## after this loop ends we will transform the vectors in such a way that for each annotation table we 
    ## will have a corresponding cell with all domain names
    domainNames <- vector(mode="character")
    annoTableNames <- vector(mode="character")
    
    # loop through all the domains
    # domain_index <-5
    for (domain_index in 1:length(allDomains)) {
      original.attributes.per.domain <- mappingFile[Domain==allDomains[domain_index],original.attribute]
      ## Pre-processing of original.attributes.per.domain so that they are in harmony with column names of annotation table
      #original.attributes.per.domain <- gsub("\\(|\\)|\\s+|-",".",original.attributes.per.domain)
      original.attributes.per.domain <- gsub("\\(|\\)|\\s+|-|\\||\\/|:\\=|\\?|\\[|\\]",".",original.attributes.per.domain)
      # create a list that will hold the file names of the annotation tables for the domain in question
      ###############################################################################################################################################    
      annoTable_fileNames <- vector(mode="list",length=length(original.attributes.per.domain))
      # loop through all the original attributes assigned to a domain and identify the annotation table they are present in
      #attribute_index <- 1
      for (attribute_index in 1:length(original.attributes.per.domain)) {
        
        #annoTable_fileNames[[attribute_index]] <- annoTables[grep(paste0("\\b",original.attributes.per.domain[attribute_index],"\\b"),annoAttributes)]
        annoTable_fileNames[[attribute_index]] <- annoTables[grep(original.attributes.per.domain[attribute_index],annoAttributes)]
      }
      
      # Attributes might be present in multiple annotation tables; for example there may be 8 attributes under the EX domain and all of them may be present 
      # in an annotation table called EX.txt, however one of the attributes might also be present in patient.txt. Clearly, EX.txt needs to be considered in this case.
      # The following will go through the list and find the common element across the members, in other words EX.txt will be common across all members and therefore will be chosen as the annotation table
      if (length(annoTable_fileNames) > 1) {
        inAll <- intersect(annoTable_fileNames[[1]],annoTable_fileNames[[2]])
        if (length(annoTable_fileNames) > 2) {
          for (i in 3:length(annoTable_fileNames)) {
            inAll <- intersect(inAll,annoTable_fileNames[[i]])
          }
        }
        
      } else {inAll <- unlist(annoTable_fileNames)}
      
      
      # If the domain is scattered across multiple files or is not present in the annotation table; generate an error message
      
      if (!length(inAll)==1) {
        message <- paste0("The attributes for ",allDomains[domain_index]," are not present in a single annotation table, Here are the attributes:  ",paste(original.attributes.per.domain,collapse=";")," and the annotation tables: ",paste(unique(unlist(annoTable_fileNames)),collapse=";"))
        stop(message)
      }
      
      # create a data.frame that will contain all the unique domains and the annotation table file names
      # later this dataframe should be transformed to insure that (if) multiple domains are associated with a single annotation table
      # then they will be captured in the same cell seperated by semi-colons
      
      annoTableNames <- c(annoTableNames,inAll) # annoTableNames will have a one-to-one correspondence with domainNames
      domainNames <- c(domainNames,allDomains[domain_index])
      
    }
    
    ## Transform the vectors in such a way that for every annotation table file name, we will have all the domains that are associated with it
    annoTableNames_uniq <- unique(annoTableNames)
    for (i in 1:length(annoTableNames_uniq)) {
      domainNames_correspondingTo_annoTable <- paste(domainNames[which(annoTableNames==annoTableNames_uniq[i])],collapse=";")
      dfTemp <- data.frame(matrix("",nrow=1,ncol=4),stringsAsFactors=FALSE)
      colnames(dfTemp) <- c("Working_directory","Annotation_table","Mapper_file","Domains")
      dfTemp[1,1] <- workingDirectory
      dfTemp[1,2] <- gsub("^.*/","",annoTableNames_uniq[i])
      dfTemp[1,3] <- as.character(metaData_raw[metaRow,Mapper_file])
      dfTemp[1,4] <- domainNames_correspondingTo_annoTable
      dfFinal <- rbind(dfFinal,dfTemp)    
    }
    
  }
  return(dfFinal)
}
####################################################################################################################################
insert_subjID <- function(data,mapper,testList,domains) {
   if ("DM" %in% domains) {
    dm <- testList[[which(names(testList)=="DM")]]
    # Identify the variable that contains the STUDYID,USUBJID,SUBJID
    nbAttribute_STUDYID <- mapper[grep("^STUDYID;?$",mapper$Mapping.function),1]
    nbAttribute_USUBJID <- mapper[grep("^USUBJID;?$",mapper$Mapping.function),1]
    nbAttribute_SUBJID <- mapper[grep("^SUBJID;?$",mapper$Mapping.function),1]
    
    # if there is no SUBJID then return
    if (!length(nbAttribute_SUBJID)==0) {
      # Generate a key by concatentating STUDYID and USUBJID
      keyData <- paste(data[,which(colnames(data)==nbAttribute_STUDYID)],data[,which(colnames(data)==nbAttribute_USUBJID)],sep=":")
      
      if (!length(which(duplicated(keyData)))==0) {
        data <- data[-which(duplicated(keyData)),]
        keyData <- keyData[-which(duplicated(keyData))]
      }
      
      # iterate through the DM domain 
      for (i in 1:dim(dm)[1]) {
        keyDM <- paste(dm[i,1],dm[i,3],sep=":")
        
        dm[i,4] <- data[which(keyData==keyDM),which(colnames(data)==nbAttribute_SUBJID )]
      }
      testList[[which(names(testList)=="DM")]] <- dm
      
    }
  }
  
  return(testList)
}
#######################################################################################################################################
populate_decod_for_condition <- function(testList,domains) {
  domainsForReplace <- domains[grep("^MH.*$|^CE.*$|^AE.*$",domains)]
  
  if (length(domainsForReplace)==0) {
    return(testList)
  } else {
    #dictionary <- data.table(read.table("C:/Users/HDHARURI/Desktop/SVN_Harish2/CDISC_conversion/ToConvert/ForValidation/conditionDictionary_mapped.txt",fill=TRUE,header=TRUE,sep="\t",comment.char = "",row.names=NULL,quote="\"",colClasses="character",check.names=FALSE))
    dictionary <- data.table(read.table("/Users/harishdharuri/Documents/SDTM-Scripts/conditionDictionary_mapped.txt",fill=TRUE,header=TRUE,sep="\t",comment.char = "",row.names=NULL,quote="\"",colClasses="character",check.names=FALSE))
    
    for (dmns in 1:length(domainsForReplace)) {
      dt <- data.table(testList[[which(names(testList)==domainsForReplace[dmns])]])
      
      
      columnsWithOriginalValues <- c("MHTERM","CETERM","AETERM")
      columnsWhereValuesToBeInserted <- c("MHDECOD","CEDECOD","AEDECOD")
      
      uniqueValues <- unique(dt[[which(colnames(dt) %in% columnsWithOriginalValues)]])
      
      ## replace values
      term <- columnsWithOriginalValues[grep(paste0("^",domainsForReplace[dmns],".*$"),columnsWithOriginalValues)]
      decod <- columnsWhereValuesToBeInserted[grep(paste0("^",domainsForReplace[dmns],".*$"),columnsWhereValuesToBeInserted)]
      setkey(dictionary,Term)
      
      for (i in 1:length(uniqueValues)) {
        replaceValue <- dictionary[uniqueValues[i],Decode]
        dt[get(term)==uniqueValues[i],(decod):= replaceValue]
      }
      #class(dt) <- "data.frame" # I am forcing it back to data.frame because write function is giving an error; will look into this later but for now this should work
      testList[[which(names(testList)==domainsForReplace[dmns])]] <- as.data.frame(dt)
    }
    
  }
  return(testList)
  
}
