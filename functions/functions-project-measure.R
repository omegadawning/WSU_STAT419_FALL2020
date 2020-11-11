
# prepare data by converting to lower case, converting to cm from inches,
# removing duplicates,  and cleaning up gender and ethnicities
prepareMeasureData = function(measure)
  {
  # remove any columns that are only NA
  # measure <- measure[colSums(!is.na(measure)) > 0];
  
  # how many NAs per row
  na_rows <- rowSums(is.na(measure));
  # will remove NAs later after I select the cols I need
  
  # convert to lower case
  measure[[27]] <- tolower(measure[[27]]);
  measure[[28]] <- tolower(measure[[28]]);
  measure[[29]] <- tolower(measure[[29]]);
  measure[[30]] <- tolower(measure[[30]]);
  measure[[31]] <- tolower(measure[[31]]);
  measure[[33]] <- tolower(measure[[33]]);
  measure[[36]] <- tolower(measure[[36]]);
  
  # convert inches to cm
  for (row in 1:nrow(measure))
  {
    units = measure[row, "units"]
    if(units!='cm')
    {
      measure[row, 4:26] <- measure[row, 4:26]*2.54
      measure[row, 27] <- 'cm'
    }
  };
  
  
  # remove duplicates
  measure_remove_dup <- removeDuplicatesFromDataFrameAllColumns(measure);
  
  # clean up ethnicities
  measure_remove_dup$ethnicity[measure_remove_dup$ethnicity=='caucasain'] <- "caucasian"
  measure_remove_dup$ethnicity[measure_remove_dup$ethnicity=='asain'] <- "asian"
  measure_remove_dup$ethnicity[measure_remove_dup$ethnicity=='white non-hispanic'] <- "caucasian"
  measure_remove_dup$ethnicity[measure_remove_dup$ethnicity=='white'] <- "caucasian"
  
  # clean up gender
  measure_remove_dup$gender[measure_remove_dup$gender=='m'] <- "male"
  measure_remove_dup$gender[measure_remove_dup$gender=='f'] <- "female"
  
  # summary(measure_remove_dup);
  
  return(measure_remove_dup);
  }


sep_Male = function(x)
  {
  measure_male <- subset(x, gender=="male");
  return(measure_male);
  }

sep_Female = function(x)
  {
  measure_female <- subset(x, gender=="female");
  return(measure_female);
  }

arm_measurementsSubset = function(x)
  {
  # pull arm and height data
  x.1 <- x[,c(4,7,8,9,10,11,12,13,14,17)];
  
  # omit NAs
  x.2 <- na.omit(x.1);
    
  # find and remove outliers
  for(column in colnames(x.2))
    {
    Q1 <- quantile(x.2[[column]], .25);
    Q3 <- quantile(x.2[[column]], .75);
    IQR <- IQR(x.2[[column]]);
      
    x.2 <- subset(x.2, x.2[[column]]>(Q1-1.5*IQR) & x.2[[column]]<(Q3+1.5*IQR));
    }
  return(x.2);  
  }
