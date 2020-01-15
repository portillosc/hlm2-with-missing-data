# function that reads in the file
return_data <- function(name, type){
  df = NULL
  if(type == "comma delimited"){
    df <- read.csv(name, header = TRUE)
  } else if(type == "spss"){
    df <- read.spss(name, to.data.frame = TRUE)
  } else if(type == "sas"){
    df <- data.frame(read_sas(name))
  } else if(type == "whitespace delimited"){
    df <- read.table(name, sep="", header = TRUE)
  } else if(type == "stata"){
    df <- read.dta(name)
  }
  return(df)
}
# function to make a customized checkbox element for L1 random effects
L1_random_effects_checkbox <- function(variable, label, choices, selected, colors){
  choices_names <- choices
  if(length(names(choices))>0) my_names <- names(choices)
  div(id=variable,class="form-group shiny-input-checkboxgroup shiny-input-container shiny-bound-input",
      HTML(paste0('<label class="control-label" for="',variable,'">',label,'</label>')),
      div( class="shiny-options-group",
           HTML(paste0('<div class="checkbox" style="color:', colors,'">',
                       '<label>',
                       '<input type="checkbox" name="', variable, 
                       '" value="', choices, 
                       '"', ifelse(choices %in% selected, 'checked="checked"', ''), 
                       '/>',
                       '<span>', choices_names,'</span>',
                       '</label>',
                       '</div>', collapse = " "))
      )
  )
}
# function that tells you if a vector is empty
vector_is_empty <- function(x) {
  return(length(x) ==0 )
}
# function that returns which values are null in selected L1 and L2 files
return_L1_L2_nulls <- function(df,df2){
  value <- is.na(df)
  cmeans <- colMeans(value)
  bool_cmeans <- cmeans == 0
  is_comp <- cmeans[bool_cmeans]
  has_null <- cmeans[!bool_cmeans]
  
  comp_data <- df[names(is_comp)]
  null_data <- df[names(has_null)]
  
  tester <<- df2
  
  if(is.null(ncol(df2))){
    temp_df2 <- as.data.frame(df2)
    value2 <- is.na(temp_df2)
  }else{
    value2 <- is.na(df2)
  }
  cmeans2 <- colMeans(value2)
  bool_cmeans2 <- cmeans2 == 0
  is_comp2 <- cmeans2[bool_cmeans2]
  has_null2 <- cmeans2[!bool_cmeans2]
  
  comp_data_2 <- df2[names(is_comp2)]
  null_data_2 <- df2[names(has_null2)]
  
  return(list('L1_completely_observed' = comp_data,
              'L1_missing' = null_data,
              'L2_completely_observed' = comp_data_2,
              'L2_missing' = null_data_2))
}
# function that returns the string representation of the L1 formula
return_L1_equation <- function(response, L1_covariates){
  eq <- paste0(response, tags$sub("ij"), " = ", HTML("&beta;"), tags$sub("0j"))
  count <- 1
  # create a for loop to append new parts of the equation
  while(count <= length(L1_covariates)){
    eq <- paste0(eq, " + ", HTML("&beta;"), tags$sub(count), tags$sub("j"), L1_covariates[count], tags$sub("ij"))
    count <- count + 1
  }
  eq <- paste0(tags$h3("L1 Model"), eq, " + e", tags$sub("ij"), tags$hr())
  return(eq)
}
# function that returns the string representation of the hierarchical formula
return_hierarchical_equation <- function(resp, x, covs){
  ###############################################
  # start the one from 8/9
  ###############################################
  if(!(nrow(x) == 0)){
    output = ""
    diff_model = paste0(resp, tags$sub("ij"), " = ")
    # counter to keep track of what row we're on
    row_counter = 0
    ran_counter = 0
    diff_ran_counter = 0
    for(i in 1:nrow(x)){
      inner_counter = 0
      L2vec <- x[i, "Equation"]
      corrected_string <- substr(L2vec,0,nchar(L2vec)-1)
      split_string <- strsplit(corrected_string, ",")
      
      # convert the list to vector
      split_string <- unlist(split_string)
      
      # don't append a "+" on the first row
      if(row_counter == 0){
        output = paste0(output,
                        HTML("&beta;"),
                        tags$sub(i - 1), 
                        tags$sub("j"), 
                        " = ", 
                        HTML("&gamma;"),
                        tags$sub(i - 1),
                        tags$sub(inner_counter))
        diff_model = paste0(diff_model,
                            HTML("&gamma;"),
                            tags$sub(i - 1),
                            tags$sub(inner_counter))
      }
      else{
        if(length(split_string) > 0 | x[i, "random_effect"] != ""){
          output = paste0(output,
                          HTML("&beta;"),
                          tags$sub(i - 1), 
                          tags$sub("j"), 
                          " = ", 
                          HTML("&gamma;"),
                          tags$sub(i - 1),
                          tags$sub(inner_counter))
        }
        else{
          output = paste0(output,
                          HTML("&beta;"),
                          tags$sub(i - 1), 
                          tags$sub("j"), 
                          " = ", 
                          HTML("&gamma;"),
                          tags$sub(i - 1),
                          tags$sub(inner_counter))
        }
        diff_model = paste0(diff_model,
                            " + ",
                            HTML("&gamma;"),
                            tags$sub(i - 1),
                            tags$sub(inner_counter),
                            covs[row_counter],
                            tags$sub("i"),
                            tags$sub("j"))
      }
      
      inner_output <- ""
      # add the L2 covariates
      if(length(split_string) > 0){
        for(L2var in split_string){
          inner_counter = inner_counter + 1
          inner_output = paste0(inner_output, " + ",
                                HTML("&gamma;"),
                                tags$sub(i - 1),
                                tags$sub(inner_counter),
                                L2var,
                                tags$sub("j"))
          
          if(length(covs[row_counter]) != 0){
            diff_model = paste0(diff_model, " + ",
                                HTML("&gamma;"),
                                tags$sub(i - 1),
                                tags$sub(inner_counter),
                                L2var,
                                tags$sub("j"),
                                covs[row_counter],
                                tags$sub("i"),
                                tags$sub("j"))
          }
          else{
            diff_model = paste0(diff_model, " + ",
                                HTML("&gamma;"),
                                tags$sub(i - 1),
                                tags$sub(inner_counter),
                                L2var,
                                tags$sub("j"))
          }
        }
        inner_output = paste(inner_output, collapse = "")
      }
      if(i == 1){
        output = paste0(output,
                        inner_output,
                        " + u",
                        tags$sub(i - 1),
                        tags$sub("j"))
        ran_counter <- ran_counter + 1
      } else if(x[i, "random_effect"] != ""){
        output = paste0(output,
                        inner_output,
                        " + u",
                        tags$sub(i - 1),
                        tags$sub("j"))
        ran_counter <- ran_counter + 1
      } else if(length(split_string) > 0){
        output = paste0(output,
                        inner_output)
      }
      else{
        output = paste0(output,
                        inner_output)
      }
      # if(length(covs) > 0 & row_counter >= 1){
      #   output = paste0(output,
      #                   covs[row_counter],
      #                   tags$sub("i"),
      #                   tags$sub("j"))
      # }
      row_counter = row_counter + 1
      output = paste0(output, HTML("<br>"))
    }
    
    c <- 0
    for(i in 1:nrow(x)){
      if(i == 1){
        diff_model = paste0(diff_model,
                            " + ",
                            "u",
                            tags$sub(i - 1),
                            tags$sub("j"))
        diff_ran_counter <- diff_ran_counter + 1
      }
      else if(x[i, "random_effect"] != ""){
        if(is.null(covs[c])){
          diff_model = paste0(diff_model,
                              " + ",
                              "u",
                              tags$sub(i - 1),
                              tags$sub("j"))
          diff_ran_counter <- diff_ran_counter + 1
        }
        else{
          if(length(covs[c]) == 0){
            diff_model = paste0(diff_model,
                                " + ",
                                "u",
                                tags$sub(i - 1),
                                tags$sub("j"))
            diff_ran_counter <- diff_ran_counter + 1
          }
          else{
            diff_model = paste0(diff_model,
                                " + ",
                                "u",
                                tags$sub(i - 1),
                                tags$sub("j"),
                                covs[c],
                                tags$sub("i"),
                                tags$sub("j"))
            diff_ran_counter <- diff_ran_counter + 1
          }
        }
      }
      c <- c + 1
    }
    diff_model = paste0(diff_model, " + e", tags$sub("ij"))
    
    output = paste0(tags$h3("L2 Model"), output, tags$hr(), tags$h3("Mixed Model"), diff_model, tags$hr())
  }
  else{
    output = ""
  }
  ###############################################
  # end the one from 8/9
  ###############################################
  
  return(HTML(output))
}
# function that trims the missing values from L1 and L2 datasets
trim_missing_L1_L2_data <- function(L1_data,L2_data,parameters){
  l1<-L1_data
  l2<-L2_data
  # case 1
  if(parameters[1]==0 & parameters[3]==1){
    names_temp <- names(l1)
    l1_na_data <- data.frame(cbind(l1[,1],!is.na(l1[,2:ncol(l1)])))
    names(l1_na_data) <- names_temp
    
    summed_rows <- aggregate(l1_na_data[,2], by=list(id=l1_na_data[,1]), FUN=sum)
    final_data <- cbind(summed_rows[,1],summed_rows[,2:ncol(summed_rows)])
    ids_missing <- final_data[which(final_data[,2]==0),1]
    
    l1_trimmed <- l1[!l1[,1] %in% ids_missing,]
    l2_trimmed <- l2[!l2[,1] %in% ids_missing,]
    # case2
  }else if(parameters[1]==0 & parameters[3]>1){ #should work but might break; didnt test it
    names_temp <- names(l1)
    l1_na_data <- data.frame(cbind(l1[,1],!is.na(l1[,2:ncol(l1)])))
    names(l1_na_data) <- names_temp
    
    summed_rows <- aggregate(l1_na_data[,2:ncol(l1_na_data)], by=list(id=l1_na_data[,1]), FUN=sum)
    final_data <- cbind(summed_rows[,1],rowSums(summed_rows[,2:ncol(summed_rows)]))
    final_data[3,2]<-0
    final_data
    ids_missing <- final_data[which(final_data[,2]==0),1]
    
    l1_trimmed <- l1[!l1[,1] %in% ids_missing,]
    l2_trimmed <- l2[!l2[,1] %in% ids_missing,]
    # case 3
  }else if(parameters[1]>0 & parameters[3]>0){
    if(ncol(l2) == 1){
      names_temp <- names(l1)
      l1_na_data <- data.frame(cbind(l1[,1],!is.na(l1[,2:ncol(l1)])))
      names(l1_na_data) <- names_temp
      
      summed_rows <- aggregate(l1_na_data[,2:ncol(l1_na_data)], by=list(id=l1_na_data[,1]), FUN=sum)
      final_data <- cbind(summed_rows[,1],rowSums(summed_rows[,2:ncol(summed_rows)]))
      final_data[3,2]<-0
      final_data
      ids_missing <- final_data[which(final_data[,2]==0),1]
      
      l1_trimmed <- l1[!l1[,1] %in% ids_missing,]
      l2_trimmed <- l2[!l2[,1] %in% ids_missing,]
    } else{
      # prorcess missing in l1
      names_temp <- names(l1)
      l1_na_data <- data.frame(cbind(l1[,1],!is.na(l1[,2:ncol(l1)])))
      names(l1_na_data) <- names_temp
      
      summed_rows <- aggregate(l1_na_data[,2:ncol(l1_na_data)], by=list(id=l1_na_data[,1]), FUN=sum)
      if(ncol(summed_rows)==2){
        final_data <- cbind(summed_rows[,1],summed_rows[,2:ncol(summed_rows)])
      }else{
        final_data <- cbind(summed_rows[,1],rowSums(summed_rows[,2:ncol(summed_rows)]))
      }
      ids_missing <- final_data[which(final_data[,2]==0),1]
      
      # process missing in l2
      names_temp_2 <- names(l2)
      l2_na_data <- data.frame(cbind(l2[,1],!is.na(l2[,2:ncol(l2)])))
      names(l2_na_data) <- names_temp_2
      summed_rows_2 <- aggregate(l2_na_data[,2:ncol(l2_na_data)], by=list(id=l2_na_data[,1]), FUN=sum)
      if(ncol(summed_rows_2)==2){
        final_data_2 <- cbind(summed_rows_2[,1],summed_rows_2[,2:ncol(summed_rows_2)])
      }else{
        final_data_2 <- cbind(summed_rows_2[,1],rowSums(summed_rows_2[,2:ncol(summed_rows_2)]))
      }
      ids_missing_2 <- final_data_2[which(final_data_2[,2]==0),1]
      full_ids_missing <- ids_missing[ids_missing %in% ids_missing_2]
      
      # trim the dataframes
      l1_trimmed <- l1[!l1[,1] %in% full_ids_missing,]
      l2_trimmed <- l2[!l2[,1] %in% full_ids_missing,]
    }  
  }
  return(list('l1' = l1_trimmed, 'l2' = l2_trimmed))
}
# function that returns column names with missing data involved with interactions
return_missing_interactions_vector <- function(t1,t4,t5){
  missing_covs_vec <- c()
  for(row in 2:nrow(t1)){
    l1 <- t1[row,1]
    l2vec <- t1[row,'Equation']
    L2vec_collapse <- paste0(l2vec,collapse="")
    L2vec_sub <- substr(L2vec_collapse,1,nchar(L2vec_collapse)-1)
    L2vec_split <- strsplit(L2vec_sub,",")
    L2vec <- unlist(L2vec_split)
    
    # skip if there aren't any l2 covs
    if(!l2vec==''){
      # if the L1 is missing data
      if(is.element(l1,names(t4))){
        for(cov in L2vec){
          if(is.element(cov,names(t5))){
            missing_covs_vec <- c(missing_covs_vec,l1,cov)
          }else{
            missing_covs_vec <- c(missing_covs_vec,l1)
          }
        }      
      }else{
        for(cov in L2vec){
          if(is.element(cov,names(t5))){
            missing_covs_vec <- c(missing_covs_vec,cov)
          }
        }  
      }
    }
    if(t1[row,3]=='true'){
      if(is.element(t1[row,1],names(t4))){
        missing_covs_vec <- c(missing_covs_vec,t1[row,1])
      }
    }
  }
  return(unique(missing_covs_vec))
}
# function that calculates the 5 parameters to go in the c program
return_5_parameters <- function(equation_matrix,null_data,null_data2,L1_dat_names,selected_L1_covariates){
  # initialize variables
  parameters <- c(0,1,0,0,1)
  L2vec <- equation_matrix[['Equation']]
  
  # collapse all rows into a single string
  L2vec_collapse <- paste0(L2vec,collapse="")
  L2vec_sub <- substr(L2vec_collapse,1,nchar(L2vec_collapse)-1)
  L2vec_split <- strsplit(L2vec_sub,",")
  L2_vec_unique <- unique(unlist(unique(L2vec_split)))
  
  # alter the parameters
  # check to see if L2_vec has any values
  if(!identical(L2_vec_unique, character(0))){
    if(nchar(unique(L2vec_split)) > 0){
      for(cov in unlist(L2_vec_unique)){
        # calculate nvar2
        if( is.element(cov, names(null_data2))){
          parameters[1] <- parameters[1] + 1
        }
        # calculate fix2
        else{
          parameters[2] <- parameters[2] + 1
        }
      }
    }
  }
  
  # add fix2 to fix1
  parameters[4] <- parameters[4] + parameters[2]
  
  if(length(selected_L1_covariates > 0)){
    for(cov in selected_L1_covariates){
      # calculate nvar1
      if( is.element(cov, names(null_data))){
        parameters[3] <- parameters[3] + 1
      }
      # calculate fix1
      else{
        parameters[4] <- parameters[4] + 1
      }
    }
  }
  
  parameters[3] <- parameters[3] + 1
  
  # calculate ran1
  parameters[5] <- length(grep("*uj$",L1_dat_names,value=TRUE)) + 1
  
  return(parameters)
}
# function that adds a space at the end of the L1 and L2 dat files
prep_L1_L2_files <- function(){
  # add an extra space to each file to handle the NA as the last character
  f = file("L1out.dat",'r')
  lines_in_file <- 0
  while(TRUE){
    line = readLines(f,n=1)
    if(length(line)==0){
      break
    }
    lines_in_file <- lines_in_file + 1
  }
  close(f)
  
  f = file("L1out.dat",'r')
  o = file("L1.dat",'w')
  curr_row <- 0
  while(curr_row < lines_in_file){
    line = readLines(f,n=1)
    
    if(curr_row == lines_in_file-1){
      writeLines(text = paste0(line," "),con = o)
    }
    else{
      writeLines(text = line,con = o)
    }
    curr_row <- curr_row + 1
  }
  close(f)
  close(o)
  
  f = file("L2out.dat",'r')
  lines_in_file <- 0
  while(TRUE){
    line = readLines(f,n=1)
    if(length(line)==0){
      break
    }
    lines_in_file <- lines_in_file + 1
  }
  close(f)
  
  f = file("L2out.dat",'r')
  o = file("L2.dat",'w')
  curr_row <- 0
  while(curr_row < lines_in_file){
    line = readLines(f,n=1)
    
    if(curr_row == lines_in_file-1){
      writeLines(text = paste0(line," "),con = o)
    }
    else{
      writeLines(text = line,con = o)
    }
    curr_row <- curr_row + 1
  }
  close(f)
  close(o)
  
  write(" ",file="L1.dat",append=TRUE)
  write(" ",file="L2.dat",append=TRUE)
}
# function to trim the seed string
trim_seed_string <- function(seed){
  # trim the seed string
  trimmed_string <- seed
  trimmed_string <- trimws(trimmed_string)
  if(trimmed_string == ''){
    # generate random string
    trimmed_string = paste0(sample(LETTERS,20,TRUE),collapse = '')
  }else{
    if(nchar(trimmed_string) > 99){
      trimmed_string <- strtrim(trimmed_string,99)
    }
  }
  return(trimmed_string)
}
# function to replace L1 and L2 columns with imputed data
replace_missing_columns <- function(L1_data,L2_data,missing_cov_interactions,original_names,r_column,parameters,add_as_missing,imputation_iteration){
  imputed_l1 <- read.table(paste0('L1imp',imputation_iteration))
  imputed_l2 <- read.table(paste0('L2imp',imputation_iteration))
  
  imputed_l1 <- imputed_l1[order(imputed_l1[,1]),]
  imputed_l1 <- imputed_l1[,c(-1)]
  
  print(missing_cov_interactions)
  
  replaceL1 <- which(names(L1_data)%in%unique(missing_cov_interactions))
  replaceL2 <- which(names(L2_data)%in%unique(missing_cov_interactions))
  
  if(length(replaceL1) > 0){
    for(num in replaceL1){
      L1_data[,num]<-imputed_l1[,num]
      parameters[3]<-parameters[3]-1
      parameters[4]<-parameters[4]+1
    }
  }
  if(length(replaceL2) > 0){
    for(num in replaceL2){
      L2_data[,num]<-imputed_l2[,num]
      parameters[1]<-parameters[1]-1
      parameters[2]<-parameters[2]+1
      parameters[4]<-parameters[4]+1
    }
  }
  L1_data$add_r_in <- r_column
  if(add_as_missing){
    parameters[3]<-parameters[3]+1
  }else{
    parameters[4]<-parameters[4]+1
  }
  
  
  L1_data<-L1_data[,c(1,ncol(L1_data),2:(ncol(L1_data)-1))]
  names(L1_data)<-original_names
  
  return(list('L1' = L1_data,
              'L2' = L2_data,
              'param' = parameters))
}
# return updated L1 data with complete interactions
complete_interactions <- function(data, L1, L2, n1, n2, arguments){
  WCxXC <- L1
  WCxXC_temp <- L1
  num_of_cols_to_remove <- 0
  if(nrow(data)>1){
    
    data <- data[-1,]
    
    id <- names(L2)[1]
    
    
    for(i in 1:nrow(data)) {
      # get the row
      row <- data[i,]
      
      # split the L2 covs into a vector
      L2vec <- row[['Equation']]
      L2vec_collapse <- paste0(L2vec,collapse="")
      L2vec_sub <- substr(L2vec_collapse,1,nchar(L2vec_collapse)-1)
      L2vec_split <- strsplit(L2vec_sub,",")
      L2_vec_unique <- unique(L2vec_split)
      
      if(!identical(L2_vec_unique[[1]], character(0))){
        if(nchar(L2_vec_unique) > 0){
          if(is.element(as.character(row["L1"]),names(n1)) ){
            # missing L1
          } 
          # fully observed L1
          else{
            # iterate L2
            for(cov in unlist(L2_vec_unique)){
              # missing L2
              if( is.element(cov, names(n2))){
              } 
              # fully observed L2
              else{ # the magic
                arguments[4] <- arguments[4] + 1
                num_of_cols_to_remove <- num_of_cols_to_remove+1
                WCxXC_temp <- merge(x = L1, y = L2[,c(id,cov)],by=id,all.x = TRUE)
                #print(names(WCxXC_temp))
                #print(row[1,'L1'])
                WCxXC_temp[row[1,'L1']]
                WCxXC_temp[cov]
                
                #print("I is")
                #print(i)
                WCxXC_temp[paste0(substr(row[1,'L1'],0,3),"x",substr(cov,0,4))] <- WCxXC_temp[row[1,'L1']] * WCxXC_temp[cov]
                # merge the columns
                WCxXC <- cbind(WCxXC,WCxXC_temp[paste0(substr(row[1,'L1'],0,3),"x",substr(cov,0,4))])
              }
            }
          }
        }
      }
    }
  }
  return(list('p' = arguments, 'd' = WCxXC, 'n' = num_of_cols_to_remove))
}
# return updated L1 data with random effects
random_interactions <- function(data, L1, L2, n1, n2, arguments){
  ran_int <- L1
  ran_int_temp <- L1
  L2_clean <- L2
  if(nrow(data)>1){
    data = data[-1,]
    id <- names(L2)[1]
    
    for(i in 1:nrow(data)) {
      # get the row
      row <- data[i,]
      if(row$random_effect == "true"){
        # the covariate with the random effect
        ran_cov <- row$L1
        
        if(!paste0(ran_cov,"uj") %in% names(ran_int)){
          new_col <- paste0(ran_cov,"uj")
          ran_int[new_col] <- L1[ran_cov]
          arguments[5]<-arguments[5]+1
        }
      }
    }
  }
  return(list('p' = arguments, 'd' = ran_int))
}

#######################################################################
# Get output

# get L1cov_table and assign row names
return_L1_covariate_table <- function(L1_dat,null_data,response){
  L1cov_table <- read.table("L1cov.par")
  L1_name_vec <- L1_dat[null_data]
  L1_name_vec <- names(L1_name_vec)
  L1_name_vec <- unique(c(response, L1_name_vec))
  L1_cov_table_to_print <- L1cov_table
  row.names(L1_cov_table_to_print) <- L1_name_vec
  names(L1_cov_table_to_print) <- NULL
  
  return(list('table'=L1cov_table,
              'print'=L1_cov_table_to_print))
}
# get L2cov_table
return_L2_covariate_table <- function(null_data,equation_matrix,L1_dat,L2_dat,null_data2,response){
  L2cov_table <- read.table("L2cov.par")
  L2cov_table_to_print <- L2cov_table
  name_vec <- c(response)
  if(nrow(L2cov_table)==1&ncol(L2cov_table)==1){
    row.names(L2cov_table_to_print) <- name_vec
    names(L2cov_table_to_print) <- NULL
  }
  else{
    # intersect between L1 file and nulls
    x_missings <- null_data
    if(length(x_missings)>0){
      if(length(x_missings)==1){
        name_vec <- c(name_vec,x_missings)
      } else{
        name_vec <- c(name_vec,x_missings[1])
      }
    }
    if(nrow(equation_matrix) > 1){
      # add the random effects
      if(length(i <- grep("uj",names(L1_dat)))){
        name_vec <- c(name_vec,'')
        if(!exists('ran_effects_vector')){
          ran_effects_vector <- character(length(name_vec))
          ran_effects_vector[length(name_vec)]=gsub('uj','',names(L1_dat)[i])
        }
        else{
          length(ran_effects_vector) <- length(name_vec)
          ran_effects_vector[length(name_vec)]=gsub('uj','',names(L1_dat)[i])
        }
      }
    }
    # add the rest of x missing
    if(length(x_missings)>0){
      if(length(x_missings)!=1){
        name_vec <- c(name_vec,x_missings[2:length(x_missings)])
      } 
    }
    # find w missing
    w_missings <- null_data2
    if(length(w_missings)!=0){
      name_vec <- c(name_vec,w_missings)
    }
    name_vec <- unique(name_vec)
    row.names(L2cov_table_to_print) <- name_vec
    if(exists('ran_effects_vector')){
      length(ran_effects_vector) <- nrow(L2cov_table_to_print)
      ran_effects_vector[is.na(ran_effects_vector)]<-''
      L2cov_table_to_print$random <- ran_effects_vector
      L2cov_table_to_print<-L2cov_table_to_print[,c(ncol(L2cov_table_to_print),1:ncol(L2cov_table_to_print)-1)]
    }
    names(L2cov_table_to_print) <- NULL
  }
  return(list('table'=L2cov_table,
              'print'=L2cov_table_to_print))
}
# get alpha_table
return_alpha_table <- function(equation_matrix,response,null_data,null_data2,L1_dat,L2_dat){
  # get the alpha vector
  alpha_vec <- read.table("alp.par")
  source_table <- alpha_vec
  
  # get the alpha vector names
  if(nrow(equation_matrix) > 1){
    alpha_vec$m_cov <- ''
    alpha_vec$effect <- ''
    vector_of_mcovs <- c(response,equation_matrix$L1[equation_matrix$L1 %in% names(null_data)],names(L2_dat)[names(L2_dat) %in% names(null_data2)]) # replace BMI with input$resp
    vector_of_xmissing_effects <- c("Intercept",
                                    names(L1_dat)[2:length(names(L1_dat))][!names(L1_dat)[2:length(names(L1_dat))] %in% names(null_data)],
                                    names(L2_dat)[2:length(names(L2_dat))][!names(L2_dat)[2:length(names(L2_dat))] %in% names(null_data2)])
    vector_of_xmissing_effects <- unique(vector_of_xmissing_effects)
    remove_random_effects <- paste(vector_of_xmissing_effects,'uj',sep='')
    vector_of_xmissing_effects <- vector_of_xmissing_effects[!vector_of_xmissing_effects%in%remove_random_effects]
    vector_of_wmissing_effects <- c("Intercept",
                                    names(L2_dat)[2:length(names(L2_dat))][!names(L2_dat)[2:length(names(L2_dat))] %in% names(null_data2)])
    vector_of_wmissing_effects <- unique(vector_of_wmissing_effects)
    num_of_xmissing <- length(c(response,equation_matrix$L1[equation_matrix$L1 %in% names(null_data)]))
    inner_count=1
    # remove the id if it's in there
    vector_of_xmissing_effects <- vector_of_xmissing_effects[!vector_of_xmissing_effects%in%c(names(L2_dat)[1])]
    vector_of_wmissing_effects <- vector_of_wmissing_effects[!vector_of_wmissing_effects%in%c(names(L2_dat)[1])]
    # remove the response variable name
    vector_of_xmissing_effects <- vector_of_xmissing_effects[!vector_of_xmissing_effects%in%c(response)]
    # remove the na's
    vector_of_xmissing_effects  <- vector_of_xmissing_effects[!is.na(vector_of_xmissing_effects)]
    vector_of_wmissing_effects <- vector_of_wmissing_effects[!is.na(vector_of_wmissing_effects)]
    for(i in 1:length(vector_of_mcovs)){
      alpha_vec[inner_count,'m_cov'] <- vector_of_mcovs[i]
      if(i <= num_of_xmissing){
        for(var in vector_of_xmissing_effects){
          alpha_vec[inner_count,'effect'] <- var
          inner_count  <- inner_count +1
        } 
      }else{
        for(var in vector_of_wmissing_effects){
          alpha_vec[inner_count,'effect'] <- var
          inner_count  <- inner_count +1
        } 
      }
    }
    alpha_vec <- alpha_vec[,c(2,3,1)]
    colnames(alpha_vec) <- c('Outcomes','Covariates','Effects')
  }
  else{
    alpha_vec$m_cov <- names(L1_dat)[2]
    alpha_vec$effect <- ''
    alpha_vec <- alpha_vec[,c(2,3,1)]
    colnames(alpha_vec) <- c('Outcomes','Covariates','Effects')
  }
  alpha_vec_to_print <- alpha_vec
  
  return(list('table'=source_table,
              'print'=alpha_vec_to_print))
}
#return imputation header
return_imputation_header <- function(){
  f = file("mhlm2.out",'r')
  line = readLines(f,n=1)
  line = readLines(f,n=1)
  line = readLines(f,n=1)
  line = readLines(f,n=1)
  iter_num <- unlist(strsplit(trimws(line),"\\s+"))[4]
  line = readLines(f,n=1)
  line = readLines(f,n=1)
  line = readLines(f,n=1)
  loglike <- unlist(strsplit(trimws(line),"\\s+"))
  second <- unlist(strsplit(loglike[3],":"))[2]
  close(f)
  
  return(paste(
    "<div style=\"font-family: Times New Roman,Times,serif; font-size: 140%; display:inline;\">",
    "<p>Full Maximum Likelihood Estimation</p>",
    "<p>Convergence after",
    iter_num,
    "iterations</p>",
    "<p>Loglikelihood:",
    second,
    "</p>",
    "<p>Convergence as the difference in two consecutive loglikelihoods < 0.000001",
    "</div>"))
}
# return the theta list
return_theta_list <- function(imputation_number,
                              num_of_extra_cols,
                              parameters,
                              L1_names,
                              L2_names,
                              equation_matrix,
                              response){
  file.rename('L1.dat','L1_original.dat')
  file.rename('L2.dat','L2_original.dat')
  
  theta_list <- list()
  withProgress(message = 'Estimating HLM for imputed data',value=0,{
    for(i in 1:imputation_number){
      incProgress(1/imputation_number, detail=paste('Estimating Model on Imputation',i))
      # remove interaction columns for each imputation
      pre_trimmed_L1 <- read.table(paste0('L1imp',i-1))
      pre_trimmed_L1<-pre_trimmed_L1[,1:(ncol(pre_trimmed_L1)-num_of_extra_cols)]
      write.table(pre_trimmed_L1,paste0('L1imp',i-1),row.names=FALSE,col.names = FALSE)
      
      # rename the imputation file
      filename_1 <- paste0('L1imp',i-1)
      file.rename(filename_1,'L1.dat')
      filename_2 <- paste0('L2imp',i-1)
      file.rename(filename_2,'L2.dat')
      
      # estimate the hlm on the imputed data
      system(paste("./hlmm2v4",
                   parameters[1], 
                   parameters[2], 
                   parameters[3], 
                   parameters[4] - num_of_extra_cols, 
                   parameters[5]), 
             intern = TRUE)
      
      # ensure that the program didn't crash
      info = file.info("L1cov.par")
      empty = rownames(info[info$size == 0, ])
      if(length(empty) != 0){
        output$L1_var_mat_txt <- renderText({
          return("Something went wrong")
        })
      }
      else{
        # read in all the mhlm2 output
        
        # get the imputed L1 and L2 data
        L1_dat <- read.table("L1.dat",col.names = L1_names)
        L2_dat <- read.table("L2.dat",col.names = L2_names)
        
        missing_complete_summary <- return_L1_L2_nulls(L1_dat,L2_dat)
        mi <<- missing_complete_summary
        eq <<- equation_matrix
        L1 <<- L1_dat
        L2 <<- L2_dat
        null_data <- names(missing_complete_summary$L1_missing)
        null_data2 <- names(missing_complete_summary$L2_missing)
        
        # get the 5 tables
        L1_covariate_table <- return_L1_covariate_table(L1_dat,null_data,response)$table
        L2_covariate_table <- return_L2_covariate_table(null_data,
                                                        equation_matrix,
                                                        L1_dat,
                                                        L2_dat,
                                                        null_data2,
                                                        response)$table
        L2_covariate_table_with_labels <- return_L2_covariate_table(null_data,
                                                                    equation_matrix,
                                                                    L1_dat,
                                                                    L2_dat,
                                                                    null_data2,
                                                                    response)$print 
        alpha_data <- return_alpha_table(equation_matrix,
                                         response,
                                         null_data,
                                         null_data2,
                                         L1_dat,
                                         L2_dat)$table      
        alpha_data_with_labels <- return_alpha_table(equation_matrix,
                                                     response,
                                                     null_data,
                                                     null_data2,
                                                     L1_dat,
                                                     L2_dat)$print
        valp_table <- read.table("Valp.par")
        colnames(valp_table)<-NULL
        vcov_table <- read.table("Vcov.par")
        vcov_table_to_print <- vcov_table
        
        # make the theta in the order (sigma, l2var matrix or scalar, fixed effects vec, L2-L1 cov var, fixed var)
        current_theta <- list(L1_covariate_table,
                              L2_covariate_table,
                              L2_covariate_table_with_labels,
                              alpha_data,
                              alpha_data_with_labels,
                              valp_table,
                              vcov_table)
        theta_list[[i]] <- current_theta
      }
      # rename the file to it's original name
      print(paste('finished part',filename_1))
      file.rename('L1.dat',filename_1)
      file.rename('L2.dat',filename_2)
    }
  })
  
  
  # set the original L1 back to it's original name
  file.rename('L1_original.dat','L1.dat')
  file.rename('L2_original.dat','L2.dat')
  
  return(theta_list)
}

# return the theta for the fully observed data
return_theta_list_fully_observed <- function(num_of_extra_cols,
                                             parameters,
                                             L1_data,
                                             L2_data,
                                             equation_matrix,
                                             response){
  
  missing_complete_summary <- return_L1_L2_nulls(L1_data,L2_data)
  
  null_data <- names(missing_complete_summary$L1_missing)
  null_data2 <- names(missing_complete_summary$L2_missing)
  
  # get the 5 tables
  L1_covariate_table <- return_L1_covariate_table(L1_data,null_data,response)$table
  L2_covariate_table <- return_L2_covariate_table(null_data,
                                                  equation_matrix,
                                                  L1_data,
                                                  L2_data,
                                                  null_data2,
                                                  response)$table
  L2_covariate_table_with_labels <- return_L2_covariate_table(null_data,
                                                              equation_matrix,
                                                              L1_data,
                                                              L2_data,
                                                              null_data2,
                                                              response)$print 
  alpha_data <- return_alpha_table(equation_matrix,
                                   response,
                                   null_data,
                                   null_data2,
                                   L1_data,
                                   L2_data)$table      
  alpha_data_with_labels <- return_alpha_table(equation_matrix,
                                               response,
                                               null_data,
                                               null_data2,
                                               L1_data,
                                               L2_data)$print
  valp_table <- read.table("Valp.par")
  colnames(valp_table)<-NULL
  vcov_table <- read.table("Vcov.par")
  vcov_table_to_print <- vcov_table
  
  # make the theta in the order (sigma, l2var matrix or scalar, fixed effects vec, L2-L1 cov var, fixed var)
  theta <- list(L1_covariate_table,
                L2_covariate_table,
                L2_covariate_table_with_labels,
                alpha_data,
                alpha_data_with_labels,
                valp_table,
                vcov_table)
  
  eqc <<- equation_matrix
  
  return(theta)
}

# return the html outputs for non fully observed data

# returns the variances and means for all of the theta objects
return_theta_var_mean <- function(theta_list, equation_matrix){
  sigma_vector <- c()
  sigma_variance_vector <- c()
  tau_list <- list()
  tau_var_list <- list()
  
  for(i in 1:length(theta_list)){
    curr_theta <- theta_list[[i]]
    sigma_vector <- c(sigma_vector,curr_theta[[1]][1,1])
    # get the length of the covariance variance matrix
    cov_var_len <- ncol(curr_theta[[7]])
    sigma_variance_vector <- c(sigma_variance_vector,curr_theta[[7]][cov_var_len,cov_var_len])
    
    
    # get the tau dfs
    
    tau_list[[i]] <- curr_theta[[2]]
    tau_var_list[[i]] <- curr_theta[[7]][1:cov_var_len-1,1:cov_var_len-1]
    
    
    # get alpha
    if(i == 1){
      if(ncol(curr_theta[[5]])==3){
        first_col <- curr_theta[[5]][,3]
      }else if(ncol(curr_theta[[5]])==2){
        first_col <- curr_theta[[5]][,2]
      }
    }else if(i == 2){
      if(ncol(curr_theta[[5]])==3){
        second_col <- curr_theta[[5]][,3]
      }else if(ncol(curr_theta[[5]])==2){
        second_col <- curr_theta[[5]][,2]
      }
      alpha_df <- cbind(first_col,second_col)
    }else{
      if(ncol(curr_theta[[5]])==3){
        second_col <- curr_theta[[5]][,3]
      }else if(ncol(curr_theta[[5]])==2){
        second_col <- curr_theta[[5]][,2]
      }
      alpha_df <- cbind(alpha_df,second_col)
    }
    # get alpha variance
    if(i == 1){
      first_col_for_variance = unlist(diag(as.matrix(curr_theta[[6]])))
    }else if(i == 2){
      second_col_for_variance = unlist(diag(as.matrix(curr_theta[[6]])))
      alpha_variance_df <- cbind(first_col_for_variance,second_col_for_variance)
    }else{
      second_col_for_variance = unlist(diag(as.matrix(curr_theta[[6]])))
      alpha_variance_df <- cbind(alpha_variance_df,second_col_for_variance)
    }
  }
  tau_names <- row.names(curr_theta[[3]])
  
  if(nrow(equation_matrix) > 1){
    for(row in 2:nrow(equation_matrix)){
      if(equation_matrix[row,3]=='true'){
        tau_names <- c(tau_names,equation_matrix[row,1])
      }
    }
  }
  
  tau_names <- tau_names[tau_names != ""]
  tau_names <- paste0('&emsp;',tau_names,sep='')
  
  sample_alpha <- curr_theta[[5]]
  return(list('s1' = sigma_vector, 
              's2' = sigma_variance_vector,
              't1' = tau_list,
              't2' = tau_var_list,
              'tn' = tau_names,
              'a1' = alpha_df,
              'a2' = alpha_variance_df,
              'as' = sample_alpha
  )
  )
}

# handle sigma completely; returns the html output
handle_sigmas_non_fully_observed <- function(sigma_vector,sigma_variance_vector){
  # calculate Q
  sigma_Q <- mean(sigma_vector)
  
  # calculate Bs
  sigma_B <- 0
  for(sig in sigma_vector){
    sigma_B <- sigma_B + ((sig-sigma_Q)*(sig-sigma_Q))
  }
  
  # calculate U
  sigma_U <- mean(sigma_variance_vector)
  
  # calculate T
  sigma_T <- sigma_U+((1+(1/length(sigma_vector)))*sigma_B)
  
  # calculate s
  sigma_s <- ((1+(1/length(sigma_vector)))*sigma_B)/sigma_T
  
  return(
    return_sigma_HTML_non_fully_observed(
      sigma_Q,
      sigma_T,
      sigma_s
    ))
}

# function to return the html format of the sigma output
return_sigma_HTML_non_fully_observed <- function(sig_q,sig_t,sig_s){
  output <- HTML(paste("<div style=\"font-family: Times New Roman,Times,serif; font-size: 140%; display:inline;\">",
                       "<br><p><span >&sigma;<sup>2</sup>:</span><br> <span class=\"b\">",
                       round(sig_q,6),
                       "</span></p><p><span >Standard Error of &sigma;<sup>2</sup>:</span> <br><span class=\"b\">",
                       round(sqrt(sig_t),6),
                       "</span></p>",
                       "<br>",
                       "</span></p><p><span >FMI for &sigma;<sup>2</sup>:</span> <br><span class=\"b\">",
                       round(sig_s,6),
                       "</span></p></div>"))
  return(output)
}

handle_taus_non_fully_observed <- function(tau_list,tau_var_list,tau_names){
  # calculate Q
  tau_counter <- 0
  for(tau_df in tau_list){
    if(tau_counter ==0){
      tau_Q_df <- tau_df
    }else{
      tau_Q_df <- tau_Q_df + tau_df
    }
    tau_counter <- tau_counter + 1
  }
  tau_Q_df <- tau_Q_df/length(tau_list)
  
  # calculate B
  tau_counter <- 0
  for(tau_df in tau_list){
    if(tau_counter ==0){
      tau_B_df <- (tau_df - tau_Q_df)*(tau_df - tau_Q_df)
    }else{
      tau_B_df <- tau_B_df + ((tau_df - tau_Q_df)*(tau_df - tau_Q_df))
    }
    tau_counter <- tau_counter + 1
  }
  tau_B_df <- tau_B_df/(length(tau_list)-1)
  
  # calculate U
  tau_counter <- 0
  for(tau_var_df in tau_var_list){
    if(tau_counter ==0){
      tau_var_vector <- diag(as.matrix(tau_var_list))
      tau_var <- matrix(0,ncol(tau_Q_df),ncol(tau_Q_df))
      tau_var[upper.tri(tau_var,diag=TRUE)]<-tau_var_vector
      tau_var <- data.frame(tau_var)
      tau_var[lower.tri(tau_var)] <- t(tau_var)[lower.tri(tau_var)]
      tau_U_df <- tau_var
    }else{
      tau_var_vector <- diag(as.matrix(tau_var_list))
      tau_var <- matrix(0,ncol(tau_Q_df),ncol(tau_Q_df))
      tau_var[upper.tri(tau_var,diag=TRUE)]<-tau_var_vector
      tau_var <- data.frame(tau_var)
      tau_var[lower.tri(tau_var)] <- t(tau_var)[lower.tri(tau_var)]
      tau_U_df <- tau_U_df + tau_var
    }
    tau_counter <- tau_counter + 1
  }
  tau_U_df <- tau_U_df/length(tau_list)
  
  # calculate T
  tau_T_df <- tau_U_df+((1+(1/length(tau_list))*tau_B_df))
  
  # calculate S
  tau_S_df <- (1+(1/length(tau_list)))*(tau_B_df/tau_T_df)
  
  
  # set the row names
  row.names(tau_Q_df) <- tau_names
  row.names(tau_T_df) <- tau_names
  row.names(tau_S_df) <- tau_names
  
  return(
    return_tau_HTML_non_fully_observed(
      tau_Q_df,
      tau_T_df,
      tau_S_df
    ))
}

return_tau_HTML_non_fully_observed <- function(tau_q,tau_t,tau_s){
  output <- HTML(paste("<div style=\"font-family: Times New Roman,Times,serif; font-size: 140%; display:inline;\">",
                       "<p>Random Effects:</p>",
                       "<i>&tau;:</i>",
                       "<p>",
                       convert_to_HTML_table(tau_q),
                       "</p>",
                       "<br>",
                       "<p><span>Standard errors of &tau;:</span></p>",
                       "<p>",
                       convert_to_HTML_table(sqrt(tau_t)),
                       "</p>",
                       "<br>",
                       "<p><span>Fraction of Missing Information (FMI) for &tau;:</span></p>",
                       "<p>",
                       #round(tau_s,6),
                       convert_to_HTML_table(sqrt(tau_s)),
                       "</p>",
                       "</div>"))
  return(output)
}

handle_alphas_non_fully_observed <- function(alpha_df,alpha_variance_df,sample_alpha,equation_matrix){
  # handle Q
  alpha_Q_vector <- rowMeans(alpha_df)
  
  # handle B
  alpha_B_vector <- c()
  for(row in 1:nrow(alpha_df)){
    current_B <- 0
    for(i in 1:ncol(alpha_df)){
      value <- alpha_df[row,][i]
      current_B <- current_B + ((value-alpha_Q_vector[row])^2)
    }
    current_B <- current_B/(1/(ncol(alpha_df)-1))
    alpha_B_vector <- c(alpha_B_vector,current_B)
  }
  # handle U
  alpha_U_vector <- rowMeans(alpha_variance_df)
  
  # calculate T
  alpha_T_vector <- c()
  for(i in 1:length(alpha_B_vector)){
    alpha_T_vector <- c(alpha_T_vector,alpha_U_vector[i]+((1+(1/ncol(alpha_df)))*alpha_B_vector[i]))
  }
  
  # calculate t
  alpha_t_vector <- c()
  for(i in 1:length(alpha_Q_vector)){
    alpha_t_vector <- c(alpha_t_vector,alpha_Q_vector[i]/sqrt(alpha_T_vector[i]))
  }
  
  # calculate r
  alpha_r_vector <- c()
  for(i in 1:length(alpha_B_vector)){
    #alpha_r_vector <- c(alpha_r_vector,(1+(1/ncol(alpha_df)))*(alpha_B_vector[i]/alpha_U_vector[i]))
    alpha_r_vector <- c(alpha_r_vector,((1+(1/ncol(alpha_df)))*alpha_B_vector[i])/alpha_U_vector[i])
    
  }
  
  # calculate v
  alpha_v_vector <- c()
  for(i in 1:length(alpha_B_vector)){
    alpha_v_vector <- c(alpha_v_vector,(ncol(alpha_df)-1)*((1+(1/alpha_r_vector[i]))*(1+(1/alpha_r_vector[i]))))
  }
  
  # calculate p-value
  alpha_p_vector <- c()
  for(i in 1:length(alpha_B_vector)){
    alpha_p_vector <- c(alpha_p_vector,2*(1-pt(abs(alpha_t_vector[i]),alpha_v_vector[i])))
  }
  
  # calculate ci
  alpha_ci_lower_vector <- c()
  alpha_ci_upper_vector <- c()
  for(i in 1:length(alpha_B_vector)){
    alpha_ci_lower_vector <- c(alpha_ci_lower_vector,alpha_Q_vector[i] - (qt(0.975,alpha_v_vector[i]) * sqrt(alpha_T_vector[i])))
    alpha_ci_upper_vector <- c(alpha_ci_upper_vector,alpha_Q_vector[i] + (qt(0.975,alpha_v_vector[i]) * sqrt(alpha_T_vector[i])))
  }
  
  # calculate s
  alpha_s_vector <- c()
  for(i in 1:length(alpha_B_vector)){
    alpha_s_vector <- c(alpha_s_vector,(1+(1/ncol(alpha_df)))*(alpha_B_vector[i]/alpha_T_vector[i]))
  }
  
  return(
    return_fixed_effects_matrix(
      alpha_Q_vector,
      alpha_T_vector,
      alpha_t_vector,
      alpha_v_vector,
      alpha_p_vector,
      alpha_ci_lower_vector,
      alpha_ci_upper_vector,
      alpha_s_vector,
      sample_alpha,
      equation_matrix
    )
  )
}

# make the fixed effects matrix
return_fixed_effects_matrix <- function(alpha_Q,
                                        alpha_T,
                                        alpha_t,
                                        alpha_v,
                                        alpha_p,
                                        alpha_ci_lower,
                                        alpha_ci_upper,
                                        alpha_s,
                                        sample_alpha_output,
                                        equation_matrix){
  if(length(alpha_Q)==1){
    fixed_df <- data.frame(
      alpha_Q[1],
      sqrt(alpha_T[1]),
      alpha_t[1],
      round(alpha_v[1]),
      alpha_p[1],
      alpha_ci_lower[1],
      alpha_ci_upper[1],
      alpha_s[1]
    )
  }
  else{
    for(i in 1:length(alpha_Q)){
      if(i == 1){
        first_row <- c(alpha_Q[i],
                       sqrt(alpha_T[i]),
                       alpha_t[i],
                       round(alpha_v[i]),
                       alpha_p[i],
                       alpha_ci_lower[i],
                       alpha_ci_upper[i],
                       alpha_s[i])
      }else if(i == 2){
        second_row <- c(alpha_Q[i],
                        sqrt(alpha_T[i]),
                        alpha_t[i],
                        round(alpha_v[i]),
                        alpha_p[i],
                        alpha_ci_lower[i],
                        alpha_ci_upper[i],
                        alpha_s[i])
        fixed_df <- rbind(first_row,second_row)
      }else{
        second_row <- c(alpha_Q[i],
                        sqrt(alpha_T[i]),
                        alpha_t[i],
                        round(alpha_v[i]),
                        alpha_p[i],
                        alpha_ci_lower[i],
                        alpha_ci_upper[i],
                        alpha_s[i])
        fixed_df <- rbind(fixed_df,second_row)
      }
    }
  }
  
  fixed_df <- round(fixed_df,digits=6)
  fixed_df <- as.data.frame(fixed_df)
  fixed_df_row <- 1
  L1_cov_name_col <- c()
  L2_interaction_col <- c()
  for(row in 1:nrow(equation_matrix)){
    L1_cov_name_col[fixed_df_row]<-equation_matrix[row,1]
    L2_interaction_col[fixed_df_row]<-''
    if(equation_matrix[row,2]!=""){
      L2vec <- eq[row,'Equation']
      L2vec <- strsplit(L2vec,",")
      L2vec <- unlist(L2vec)
      for(L2_covariate in L2vec){
        fixed_df_row <- fixed_df_row + 1
        L2_interaction_col[fixed_df_row] <- L2_covariate
        L1_cov_name_col[fixed_df_row]<-''
      }
    }
    fixed_df_row <- fixed_df_row + 1
  }
  print(fixed_df)
  print(L1_cov_name_col)
  print(L2_interaction_col)
  fixed_df<-cbind(L2_interaction_col,fixed_df)
  fixed_df<-cbind(L1_cov_name_col,fixed_df)
  names(fixed_df)<-c('','',"Coefficient","Std. Err.","t-ratio","Approx. d.f.","p-value","Lower 95% CI","Upper 95% CI","FMI")
  row.names(fixed_df) <-NULL
  # 
  # if(ncol(sample_alpha_output)==3){
  #   fixed_df <- cbind(sample_alpha_output[,2],fixed_df)
  #   fixed_df <- cbind(sample_alpha_output[,1],fixed_df)
  #   fixed_df <- as.data.frame(fixed_df)
  #   names(fixed_df)<-c("","","Coefficient","Std. Err.","t-ratio","Approx. d.f.","p-value","Lower 95% CI","Upper 95% CI","FMI")
  #   row.names(fixed_df)<-NULL
  #   fixed_df[,1]<-as.character(fixed_df[,1])
  #   fixed_df[,2]<-as.character(fixed_df[,2])
  #   fixed_df[1,1]='Intercept'
  #   fixed_df[1,2]=''
  # }else if(ncol(sample_alpha_output)==2){
  #   fixed_df <- cbind(rep('Intercept',nrow(fixed_df)),fixed_df)
  #   fixed_df <- cbind(sample_alpha_output[,1],fixed_df)
  #   fixed_df <- as.data.frame(fixed_df,stringsAsFactors=FALSE)
  #   names(fixed_df)<-c("","","Coefficient","Std. Err","t-ratio","Approx. d.f.","p-value","Lower 95% CI","Upper 95% CI","FMI")
  #   row.names(fixed_df)<-NULL
  #   fixed_df[,1]<-as.character(fixed_df[,1])
  #   fixed_df[1,1]='Intercept'
  # }
  return(convert_fixed_effects_to_HTML(fixed_df))
}

convert_fixed_effects_to_HTML <- function(df){
  output <- '<table style="width:100%">'
  names_for_table <- c('',names(df))
  output<-paste0(output,
                 '<tr>')
  for(name in names_for_table){
    output<-paste0(output,
                   '<td>',
                   '<b>',
                   name,
                   '</b>',
                   '</td>')
  }
  output<-paste0(output,
                 '</tr>')
  
  
  front_gamma <- 0
  back_gamma <- 0
  curr_gamma <- paste0('&gamma;',
                       '<sub>',
                       front_gamma,
                       back_gamma,
                       '</sub>')
  for(row in 1:nrow(df)){
    output<-paste0(output,
                   '<tr>')
    if(row != 1){
      if(df[row,1]==''){
        back_gamma <- back_gamma+1
      }
      else{
        front_gamma<-front_gamma+1
        back_gamma<-0
      }
      curr_gamma <- paste0('&gamma;',
                           '<sub>',
                           front_gamma,
                           back_gamma,
                           '</sub>')
    }
    for(col in 1:ncol(df)){
      if(col==3){
        # insert the gamma
        output<-paste0(output,
                       '<td>',
                       '<i>',
                       curr_gamma,
                       '&nbsp;&nbsp;&nbsp;&nbsp;',
                       '</i>',
                       '</td>')
      }
      # insert the row value
      output<-paste0(output,
                     '<td>',
                     df[row,col],
                     '</td>')
    }
    output<-paste0(output,
                   '</tr>')
  }
  output <- paste0(output,
                   '</table>')
  return(output)
}

convert_to_HTML_table <- function(df){
  output <- '<table style="width:45%">'
  if(!is.null(row.names(df))){
    for(col in 1:ncol(df)){
      output <- paste0(output,
                       '<tr>')
      output <-paste0(output,
                      '<td>',
                      row.names(df)[col],
                      '</td>')
      for(row in 1:nrow(df)){
        if(is.numeric(df[row,col])){
          output <-paste0(output,
                          '<td>',
                          round(df[row,col],6),
                          '</td>')
        }else{
          output <-paste0(output,
                          '<td>',
                          df[row,col],
                          '</td>')
        }
        
      }
      output <- paste0(output,
                       '</tr>')
    }
  }else{
    for(col in 1:ncol(df)){
      output <- paste0(output,
                       '<tr>')
      for(row in 1:nrow(df)){
        if(is.numeric(df[row,col])){
          output <-paste0(output,
                          '<td>',
                          round(df[row,col],6),
                          '</td>')
        }
        else{
          output <-paste0(output,
                          '<td>',
                          df[row,col],
                          '</td>')
        }
      }
      output <- paste0(output,
                       '</tr>')
    }
  }
  output <- paste0(output,
                   '</table>')
  return(output)
}

# function to convert fully observed tables to html tables
convert_FO_to_HTML_table <- function(df){
  output <- '<table style="width:45%">'
  for(row in 1:nrow(df)){
    output <- paste0(output,
                     '<tr>')
    output <-paste0(output,
                    '<td>',
                    row.names(df)[row],
                    '</td>')
    for(col in 1:ncol(df)){
      if(is.numeric(df[row,col])){
        if(grepl('<br>&ta',row.names(df)[row])){
          output <-paste0(output,
                          '<td><br><br>',
                          round(df[row,col],6),
                          '</td>')
        }else if(grepl('Standard errors',row.names(df)[row])){
          output <-paste0(output,
                          '<td><br>',
                          round(df[row,col],6),
                          '</td>')
        }
        else{
          output <-paste0(output,
                          '<td>',
                          round(df[row,col],6),
                          '</td>')  
        }
      }else{
        if(grepl('<br>&ta',row.names(df)[row])){
          output <-paste0(output,
                          '<td><br><br>',
                          df[row,col],
                          '</td>')
        }else if(grepl('Standard errors',row.names(df)[row])){
          output <-paste0(output,
                          '<td><br>',
                          df[row,col],
                          '</td>')
        }else{
        output <-paste0(output,
                        '<td>',
                        df[row,col],
                        '</td>')
        }
      }
    }
    output <- paste0(output,
                     '</tr>')
  }
  output <- paste0(output,
                   '</table>')
  return(output)
}

# function to convert fully observed tables to html tables for downloading output
convert_FO_to_HTML_table_to_print <- function(df){
  output <- '<table>'
  for(col in 1:ncol(df)){
    output <- paste0(
      output,
      "<col width=150>"
    )
  }
  for(row in 1:nrow(df)){
    output <- paste0(output,
                     '<tr>')
    output <-paste0(output,
                    '<td>',
                    row.names(df)[row],
                    '</td>')
    for(col in 1:ncol(df)){
      if(is.numeric(df[row,col])){
        if(grepl('<br>&ta',row.names(df)[row])){
          output <-paste0(output,
                          '<td><br><br>',
                          round(df[row,col],6),
                          '</td>')
        }else if(grepl('Standard errors',row.names(df)[row])){
          output <-paste0(output,
                          '<td><br>',
                          round(df[row,col],6),
                          '</td>')
        }
        else{
          output <-paste0(output,
                          '<td>',
                          round(df[row,col],6),
                          '</td>')  
        }
      }else{
        if(grepl('<br>&ta',row.names(df)[row])){
          output <-paste0(output,
                          '<td><br><br>',
                          df[row,col],
                          '</td>')
        }else if(grepl('Standard errors',row.names(df)[row])){
          output <-paste0(output,
                          '<td><br>',
                          df[row,col],
                          '</td>')
        }else{
          output <-paste0(output,
                          '<td>',
                          df[row,col],
                          '</td>')
        }
      }
    }
    output <- paste0(output,
                     '</tr>')
  }
  output <- paste0(output,
                   '</table>')
  return(output)
}

# function to convert fully observed tables to html tables with no row names
convert_FO_no_row_names_to_HTML_table <- function(df){
  output <- '<table>'
  #output <- '<table>'
  for(col in 1:ncol(df)){
    output <- paste0(
      output,
      "<col width=150>"
    )
  }
  for(row in 1:nrow(df)){
    output <- paste0(output,
                     '<tr>')
    for(col in 1:ncol(df)){
      if(is.numeric(df[row,col])){
        if(grepl('<br>&ta',row.names(df)[row])){
          output <-paste0(output,
                          '<td><br><br>',
                          round(df[row,col],6),
                          '</td>')
        }else if(grepl('Standard errors',row.names(df)[row])){
          output <-paste0(output,
                          '<td><br>',
                          round(df[row,col],6),
                          '</td>')
        }
        else{
          output <-paste0(output,
                          '<td>',
                          round(df[row,col],6),
                          '</td>')  
        }
      }else{
        if(grepl('<br>&ta',row.names(df)[row])){
          output <-paste0(output,
                          '<td><br><br>',
                          df[row,col],
                          '</td>')
        }else if(grepl('Standard errors',row.names(df)[row])){
          output <-paste0(output,
                          '<td><br>',
                          df[row,col],
                          '</td>')
        }else{
          output <-paste0(output,
                          '<td>',
                          df[row,col],
                          '</td>')
        }
      }
    }
    output <- paste0(output,
                     '</tr>')
  }
  output <- paste0(output,
                   '</table>')
  return(output)
}

# function to convert missing tables to html tables
convert_MO_to_HTML_table <- function(df){
  output <- '<table style=\"width:45%;\">'
  for(row in 1:nrow(df)){
    output <- paste0(output,
                     '<tr>')
    output <-paste0(output,
                    '<td>',
                    row.names(df)[row],
                    '</td>')
    for(col in 1:ncol(df)){
      if(is.numeric(df[row,col])){
        if(grepl('Random effects',row.names(df)[row])){
          output <-paste0(output,
                          '<td><br>',
                          round(df[row,col],6),
                          '</td>')
        }else if(grepl('&tau;:',row.names(df)[row])){
          output <-paste0(output,
                          '<td><br>',
                          round(df[row,col],6),
                          '</td>')
        }
        else{
          output <-paste0(output,
                          '<td>',
                          round(df[row,col],6),
                          '</td>')  
        }
      }else{
        if(grepl('Random Effects',row.names(df)[row])){
          output <-paste0(output,
                          '<td><br>',
                          df[row,col],
                          '</td>')
        }else if(grepl('&tau;:',row.names(df)[row])){
          output <-paste0(output,
                          '<td><br>',
                          df[row,col],
                          '</td>')
        }
        else{
          output <-paste0(output,
                          '<td>',
                          df[row,col],
                          '</td>')
        }
      }
    }
    output <- paste0(output,
                     '</tr>')
  }
  output <- paste0(output,
                   '</table>')
  return(output)
}

handle_sigmas_fully_observed <- function(sig,sig_var){
  sigma_value <- as.data.frame(sig[1,1])
  row.names(sigma_value) <- c('&sigma;<sup>2</sup>:')
  sigma_value <- round(sigma_value,6)
  sigma_se <- as.data.frame(sqrt(sig_var))
  row.names(sigma_se) <- c('Standard Error of &sigma;<sup>2</sup>:')
  sigma_se <- round(sigma_se,6)
  
  
  
  output <- HTML(paste(
                       "<br><p><span ></span><br> <span class=\"b\">",
                       convert_to_HTML_table(sigma_value),
                       "</span></p><p><span ></span> <br><span class=\"b\">",
                       convert_to_HTML_table(sigma_se),
                       "</span></p>",
                       "<br>",
                       "</span></p>")
  )
  return(output)
}

handle_taus_fully_observed <- function(tau,tau_var,tau_names){
  if(ncol(tau)==1){
    row.names(tau) <- c('Intercept')
  }else{
    row.names(tau) <- tau_names
  }
  
  if(typeof(tau_var)=='double'){
    tau_var <- as.data.frame(tau_var)
    row.names(tau_var) <- c('Intercept')
  }else{
    row.names(tau_var) <- tau_names
  }
  
  output <- HTML(paste("<div style=\"font-family: Times New Roman,Times,serif; font-size: 140%; display:inline;\">",
                       "<p>Random Effects:</p>",
                       "<i>&tau;:</i>",
                       "<p>",
                       convert_to_HTML_table(tau),
                       "</p>",
                       "<br>",
                       "<p><span>Standard errors of &tau;:</span></p>",
                       "<p>",
                       convert_to_HTML_table(sqrt(tau_var)),
                       "</p>",
                       "</div>"))
  return(output)
}

handle_alpha_fully_observed <- function(alpha,alpha_variance,equation_matrix){
  alpha_se <- sqrt(alpha_variance)
  fixed_df <- cbind(alpha,alpha_se,(alpha/alpha_se))
  
  fixed_df <- round(fixed_df,digits=6)
  fixed_df <- as.data.frame(fixed_df)
  fixed_df_row <- 1
  L1_cov_name_col <- c()
  L2_interaction_col <- c()
  # set the names of the gammas
  for(row in 1:nrow(equation_matrix)){
    L1_cov_name_col[fixed_df_row]<-equation_matrix[row,1]
    L2_interaction_col[fixed_df_row]<-''
    if(equation_matrix[row,2]!=""){
      L2vec <- equation_matrix[row,'Equation']
      L2vec <- strsplit(L2vec,",")
      L2vec <- unlist(L2vec)
      for(L2_covariate in L2vec){
        fixed_df_row <- fixed_df_row + 1
        L2_interaction_col[fixed_df_row] <- L2_covariate
        L1_cov_name_col[fixed_df_row]<-''
      }
    }
    fixed_df_row <- fixed_df_row + 1
  }
  fixed_df<-cbind(L2_interaction_col,fixed_df)
  fixed_df<-cbind(L1_cov_name_col,fixed_df)
  names(fixed_df)<-c("","","Coefficient","Std. Err.","t-value")
  row.names(fixed_df) <-NULL
  
  return(convert_fixed_effects_to_HTML(fixed_df))
}

handle_sigma_tau_fully_observed <- function(sig,sig_var,tau,tau_var,tn){
  # create the data frames
  sigma_value <- as.data.frame(sig[1,1])
  sigma_se <- as.data.frame(sqrt(sig_var))
  
  # round the data to 6 decimal places
  sigma_value <- round(sigma_value,6)
  sigma_se <- round(sigma_se,6)
  
  # rename the columns
  names(sigma_value) <- 1:ncol(sigma_value)
  names(sigma_se) <- 1:ncol(sigma_se)
  
  # rename the rows
  row.names(sigma_value) <- c('&sigma;<sup>2</sup>:')
  row.names(sigma_se) <- c('Standard Error of &sigma;<sup>2</sup>:')
  
  # combine the sigma dataframes
  sigma_data <- rbind(sigma_value,sigma_se)
  
  tau_var_vector <- diag(as.matrix(tau_var))
  tau_var <- matrix(0,ncol(tau),ncol(tau))
  tau_var[upper.tri(tau_var,diag=TRUE)]<-tau_var_vector
  tau_var <- data.frame(tau_var)
  tau_var[lower.tri(tau_var)] <- t(tau_var)[lower.tri(tau_var)]
  
  # handle tau row names
  if(ncol(tau)==1){
    print('made it')
    print(tau)
    print(tn)
    row.names(tau) <- c('<br>&tau;<br>&emsp;Intercept')
    
  }else{
    print('made it2')
    print(tau)
    print(tn)
    tn[1] <- paste0('<br>&tau;<br>&emsp;','Intercept')
    row.names(tau) <- tn
  }
  
  if(typeof(tau_var)=='double'){
    tau_var <- as.data.frame(tau_var)
    print('made it3')
    row.names(tau_var) <- c('Standard errors of &tau;<br>&emsp;Intercept ')
  }else{
    print(tau_var)
    print(tn)
    tn[1] <- paste0('Standard errors of &tau;<br>&emsp;','Intercept')
    row.names(tau_var) <- paste(tn,' ',sep='')
  }
  
  # handle tau column names
  names(tau) <- 1:ncol(tau)
  names(tau_var) <- 1:ncol(tau_var)
  tau_data <- rbind(tau,tau_var)
  
  # round data to 6 digits
  tau_data <- round(tau_data,6)
  
  # prep sigma data to add to tau
  for(name in names(tau_data)){
    if(!name%in%names(sigma_data)){
      sigma_data[,name]=""
    }
  }
  combined_data <- rbind(sigma_data,tau_data)
  
  print(convert_FO_to_HTML_table(combined_data))
}

handle_sigma_tau_non_fully_observed <- function(sigma_vector,sigma_variance_vector,tau_list,tau_var_list,tau_names){
  # create the data frames
  # calculate Q
  sigma_Q <- mean(sigma_vector)
  
  # calculate Bs
  sigma_B <- 0
  for(sig in sigma_vector){
    sigma_B <- sigma_B + ((sig-sigma_Q)*(sig-sigma_Q))
  }
  # calculate U
  sigma_U <- mean(sigma_variance_vector)
  # calculate T
  sigma_T <- sigma_U+((1+(1/length(sigma_vector)))*sigma_B)
  # calculate s
  sigma_s <- ((1+(1/length(sigma_vector)))*sigma_B)/sigma_T
  
  # round the data to 6 decimal places
  sigma_Q <- round(sigma_Q,6)
  sigma_T <- round(sigma_T,6)
  sigma_s <- round(sigma_s,6)
  
  # cast to dataframes
  sigma_Q <- as.data.frame(sigma_Q)
  sigma_T <- as.data.frame(sigma_T)
  sigma_s <- as.data.frame(sigma_s)
  
  # set rownames
  row.names(sigma_Q) <- c('<b>Random effects:</b><br><i>&sigma;</i><sup>2</sup>:')
  row.names(sigma_T) <- c('Standard Error of <i>&sigma;</i><sup>2</sup>:')
  row.names(sigma_s) <- c('Fraction of missing information (FMI) for <i>&sigma;</i><sup>2</sup>:')
  
  # set column names
  names(sigma_Q) <- 1:ncol(sigma_Q)
  names(sigma_T) <- 1:ncol(sigma_T)
  names(sigma_s) <- 1:ncol(sigma_s)
  
  # combine the sigma dataframes
  sigma_data <- rbind(sigma_Q,sigma_T)
  sigma_data <- rbind(sigma_data,sigma_s)
  
  # calculate Q
  tau_counter <- 0
  for(tau_df in tau_list){
    if(tau_counter ==0){
      tau_Q_df <- tau_df
    }else{
      tau_Q_df <- tau_Q_df + tau_df
    }
    tau_counter <- tau_counter + 1
  }
  tau_Q_df <- tau_Q_df/length(tau_list)
  
  # calculate B
  tau_counter <- 0
  for(tau_df in tau_list){
    if(tau_counter ==0){
      tau_B_df <- (tau_df - tau_Q_df)*(tau_df - tau_Q_df)
    }else{
      tau_B_df <- tau_B_df + ((tau_df - tau_Q_df)*(tau_df - tau_Q_df))
    }
    tau_counter <- tau_counter + 1
  }
  tau_B_df <- tau_B_df/(length(tau_list)-1)
  
  # calculate U
  tau_counter <- 0
  for(tau_var_df in tau_var_list){
    if(tau_counter ==0){
      tau_var_vector <- diag(as.matrix(tau_var_df))
      tau_var <- matrix(0,ncol(tau_Q_df),ncol(tau_Q_df))
      tau_var[upper.tri(tau_var,diag=TRUE)]<-tau_var_vector
      tau_var <- data.frame(tau_var)
      tau_var[lower.tri(tau_var)] <- t(tau_var)[lower.tri(tau_var)]
      tau_U_df <- tau_var
    }else{
      tau_var_vector <- diag(as.matrix(tau_var_df))
      tau_var <- matrix(0,ncol(tau_Q_df),ncol(tau_Q_df))
      tau_var[upper.tri(tau_var,diag=TRUE)]<-tau_var_vector
      tau_var <- data.frame(tau_var)
      tau_var[lower.tri(tau_var)] <- t(tau_var)[lower.tri(tau_var)]
      tau_U_df <- tau_U_df + tau_var
    }
    tau_counter <- tau_counter + 1
  }
  tau_U_df <- tau_U_df/length(tau_list)
  
  print(tau_U_df)
  
  # calculate T
  tau_T_df <- tau_U_df+((1+(1/length(tau_list))*tau_B_df))
  
  # calculate S
  tau_S_df <- (1+(1/length(tau_list)))*(tau_B_df/tau_T_df)
  
  if(length(tau_names)==1){
    names_for_tau <- c('&emsp;Intercept')
  }else{
    tau_names[1] <- '&emsp;Intercept'
    names_for_tau <- tau_names
  }
  
  # set the row names
  row.names(tau_Q_df) <- names_for_tau
  row.names(tau_T_df) <- names_for_tau
  row.names(tau_S_df) <- names_for_tau
  
  # append the additional info needed for the tau names
  row.names(tau_Q_df)[1] <- paste0('<i>&tau;</i>:<br>',row.names(tau_Q_df)[1])
  row.names(tau_T_df)[1] <- paste0('Standard errors of <i>&tau;</i>:<br>',row.names(tau_T_df)[1])
  row.names(tau_S_df)[1] <- paste0('FMI for <i>&tau;</i>:<br>',row.names(tau_S_df)[1])
  
  # set column names
  names(tau_Q_df) <- 1:ncol(tau_Q_df)
  names(tau_T_df) <- 1:ncol(tau_T_df)
  names(tau_S_df) <- 1:ncol(tau_S_df)
  
  
  # combine the tau dataframes
  tau_data <- rbind(tau_Q_df,tau_T_df)
  tau_data <- rbind(tau_data,tau_S_df)
  
  # round data to 6 digits
  tau_data <- round(tau_data,6)
  
  # prep sigma data to add to tau
  for(name in names(tau_data)){
    if(!name%in%names(sigma_data)){
      sigma_data[,name]=""
    }
  }
  combined_data <- rbind(sigma_data,tau_data)
  return(convert_MO_to_HTML_table(combined_data))
}

return_calculated_outputs_fully_observed <- function(theta,equation_matrix){
  # handle sigma
  sigma_value <- theta[[1]]
  cov_var_len <- ncol(theta[[7]])
  sigma_variance <- theta[[7]][cov_var_len,cov_var_len]
  #sigma_output <- handle_sigmas_fully_observed(sigma_value,sigma_variance)
  
  # handle tau
  tau <- theta[[2]]
  tau_variance <- theta[[7]][1:cov_var_len-1,1:cov_var_len-1]
  tau_names <- row.names(theta[[3]])
  tau_names <- tau_names[tau_names!='']
  if(nrow(equation_matrix)>1){
    for(row in 2:nrow(equation_matrix)){
      if(equation_matrix[row,3]=='true'){
        tau_names <- c(tau_names,equation_matrix[row,1])
      }
    }
  }
  tau_names <- paste0('&emsp;',tau_names,sep='')
  
  #tau_output <- handle_taus_fully_observed(tau,tau_variance,tau_names)
  
  sigma_tau_output <- handle_sigma_tau_fully_observed(sigma_value,sigma_variance,tau,tau_variance,tau_names)
  
  # handle alpha
  alpha_vector <- theta[[4]][,1]
  alpha_var_vector <- unlist(diag(as.matrix(theta[[6]])))
  alpha_output <- handle_alpha_fully_observed(alpha_vector,alpha_var_vector,equation_matrix)
  
  return(
    paste(
      paste0(
        sigma_tau_output
      ),
      '<br><br><b>Fixed Effects:</b>',
      alpha_output
    )
  )
}
return_calculated_outputs_non_fully_observed <- function(theta_list, equation_matrix){
  theta_vars_and_means <- return_theta_var_mean(theta_list,equation_matrix)
  sigma_vector <- theta_vars_and_means$s1
  sigma_variance_vector <- theta_vars_and_means$s2
  
  tau_list <- theta_vars_and_means$t1
  tau_var_list <- theta_vars_and_means$t2
  tau_names <- theta_vars_and_means$tn
  
  print(tau_names)
  
  alpha_df <- theta_vars_and_means$a1
  alpha_variance_df <- theta_vars_and_means$a2
  sample_alpha <- theta_vars_and_means$as
  
  sigma_tau_output <- handle_sigma_tau_non_fully_observed(sigma_vector,sigma_variance_vector,tau_list,tau_var_list,tau_names)
  alpha_output <- handle_alphas_non_fully_observed(alpha_df,alpha_variance_df,sample_alpha,equation_matrix)
  return(paste0(sigma_tau_output,'<br><b>Fixed Effects:</b>',alpha_output))
}

# process the mi cases
process_second_imputation_step <- function(L1_data,
                                           L2_data,
                                           imputation_num,
                                           parameters,
                                           missing_cov_interactions,
                                           original_names,
                                           r_column,
                                           add_as_missing,
                                           equation_matrix,
                                           seed,
                                           selected_resp){
  
  # files produced by initial hlm: alp.par,logl.par,mhlm2.out,mhlm2.par,Valp.par,Vcov.par
  # initial L1 and L2 output: L1cov.par, L2cov.par
  
  # for loop that iterates through creating L1imp0-k and L2imp0-k
  withProgress(message = 'Estimating 2 Step imputation',value=0,detail='Imputation 1',{
    trimmed_seed <- trim_seed_string(seed)
 
    system(paste("./hlmm2micontext1",
                 parameters[1],
                 parameters[2],
                 parameters[3],
                 parameters[4],
                 parameters[5],
                 imputation_num,
                 trimmed_seed), intern = TRUE)
    
    file.rename('mhlm2.out','mhlm2_original.out')
    file.rename('mhlm2.par','mhlm2_original.par')
    file.rename('L1.dat','L1_original_2step')
    file.rename('L2.dat','L2_original_2step')
    
    for(i in 1:imputation_num){
      # impute missing data
      imputed_data <- replace_missing_columns(L1_data,
                                              L2_data,
                                              missing_cov_interactions,
                                              original_names,
                                              r_column,
                                              parameters,
                                              add_as_missing,
                                              i-1)
 
      L1_dat<-imputed_data$L1
      L2_dat<-imputed_data$L2
      updated_parameters<-imputed_data$param
      
      # estimate mhlm on data with mi
      missing_complete_summary <- return_L1_L2_nulls(L1_dat,L2_dat)
      null_data <- missing_complete_summary$L1_missing
      null_data2 <- missing_complete_summary$L2_missing
      
      response <- complete_interactions(equation_matrix,L1_dat,L2_dat,null_data,null_data2,updated_parameters)
      updated_parameters <- response$p
      num_of_extra_cols <- 0
      print(updated_parameters)
      print(num_of_extra_cols)
      response <- random_interactions(equation_matrix,response$d,L2_dat,null_data,null_data2,updated_parameters)

      updated_parameters <- response$p
      L1_dat <- response$d

      # trim any columns off L2.dat that arent interacting with R
      L2_adjusted_results <- adjust_l2_file(L2_dat,equation_matrix,updated_parameters)
      L2_dat <- L2_adjusted_results$L2
      updated_parameters <- L2_adjusted_results$p

      # write the tables out to files
      write.table(L1_dat,file = "L1out.dat",sep=" ", col.names = F, row.names = F)
      write.table(L2_dat,file="L2out.dat",sep=" ", col.names = F, row.names = F)
      prep_L1_L2_files()

      incProgress((.333/imputation_num),detail=paste('Estimating second MHLM for imputation number',i))
      
      # estimate mhlm
      system(paste("./hlmm2v4", updated_parameters[1], updated_parameters[2], updated_parameters[3], updated_parameters[4], updated_parameters[5]), intern = TRUE)

      # incProgress((.333/imputation_num),detail=paste('Getting imputation number',i,'step two'))
      # 
      # process the output of the mhlm
      info = file.info("L1cov.par")
      empty = rownames(info[info$size == 0, ])
      if(length(empty) != 0){
        print('something went wrong with processing a 2 step imputation hlmm2v4')
      }
      else{
        new_seed <- paste0(sample(LETTERS,20,TRUE),collapse = '')
        get_single_imputation(new_seed,updated_parameters)
        incProgress((.333/imputation_num),detail=paste('Getting imputation number',i,'step two'))
        # rename the files made from here
        file.rename('L1imp0',paste0('L1imp',(i-1),'_to_process'))
        file.rename('L2imp0',paste0('L2imp',(i-1),'_to_process'))
      }
    }
  })
  process_imp_files(imputation_num)
  
  #  return(list('l1' = l1_trimmed, 'l2' = l2_trimmed))
  
  #return(updated_parameters)
  
  # real one
  return(
    list(
      'p'=updated_parameters,
      'l1name'=names(L1_dat),
      'l2name'=names(L2_dat),
      'extra_cols'=num_of_extra_cols
    )
  )
}

process_imp_files <- function(imputation_number){
  for(num in 1:imputation_number){
    # drop the first column
    dataL1 <- read.table(paste0('L1imp',(num-1),'_to_process'))
    dataL1 <- dataL1[,c(-1)]
    write.table(dataL1,paste0('L1imp',(num-1),'_to_process'),
                col.names=F, quote=F,sep="  ", row.names = F)
    
    file.rename(paste0('L1imp',(num-1),'_to_process'),paste0('L1imp',(num-1)))
    file.rename(paste0('L2imp',(num-1),'_to_process'),paste0('L2imp',(num-1)))
  }
}

# get a single imputation for the mi case
get_single_imputation <- function(seed,parameters){
  # impute missing data
  trimmed_seed <- trim_seed_string(seed)
  system(paste("./hlmm2micontext1",
               parameters[1],
               parameters[2],
               parameters[3],
               parameters[4],
               parameters[5],
               1,
               trimmed_seed), intern = TRUE)
}

adjust_l2_file <- function(L2_dat,eq,parameters){
  # get anything that interacts with R
  sample <- eq[1,2]
  L2vec_collapse <- paste0(sample,collapse="")
  L2vec_sub <- substr(L2vec_collapse,1,nchar(L2vec_collapse)-1)
  L2vec_split <- strsplit(L2vec_sub,",")
  L2_vec_unique <- unlist(unique(L2vec_split))
  L2_vec_unique <- c(names(L2_dat)[1],L2_vec_unique)
  
  # get the columns of L2 to keep
  L2_covs_to_keep <- names(L2_dat)[names(L2_dat)%in%L2_vec_unique]
  
  # get the number to subtract from parameters[2]
  num_to_subtract <- length(L2_dat) - length(L2_covs_to_keep)
  
  # subtract from parameters[2]
  parameters[2] <- parameters[2] - num_to_subtract
  parameters[4] <- parameters[4] - num_to_subtract
  
  # subset L2
  L2_dat <- L2_dat[,L2_covs_to_keep]
  
  return(
    list(
      'L2'=L2_dat,
      'p'=parameters
    )
  )
}
# end output
#######################################################################