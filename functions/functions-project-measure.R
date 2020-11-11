
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

average_Dataframe = function(x, i)
  {
  avg_fromData <- colMeans(x, na.rm=TRUE);
  
  # convert the avgs to a dataframe
  avg.df <- as.data.frame(t(avg_fromData));
  
  # male_avg; # averages of male measurements
  # female_avg; # averages of female measurements
  
  # average the left and right arm/hand measurements
  m_avg_hl <- (avg.df$hand.length.left+avg.df$hand.length.right)/2;
  m_avg_hw <- (avg.df$hand.width.left+avg.df$hand.width.right)/2;
  m_avg_he <- (avg.df$hand.elbow.left+avg.df$hand.elbow.right)/2;
  m_avg_ea <- (avg.df$elbow.armpit.left+avg.df$elbow.armpit.right)/2; 
  
  # create vector for the new dataframe
  avg_row <- c(i, avg.df$height.NA, m_avg_hl, m_avg_hw, m_avg_he, m_avg_ea, avg.df$arm.span.NA);
  
  # create and populate average dataframe
  m_arm <- data.frame(matrix(ncol=7, nrow=0));
  m_arm <- rbind(m_arm, avg_row);
  colnames(m_arm) <- c('gender','avg.height', 'avg.hand.length', 'avg.hand.width', 'avg.hand.elbow', 'avg.elbow.armpit', 'avg.span');
  
  # average arm length for male and female using individual hand, forearm and upper arm measurements
  avg_arm_length <- m_arm$avg.hand.length+m_arm$avg.hand.elbow+m_arm$avg.elbow.armpit;
  
  # percentages of each part of arm for male and female
  # add to dataframe
  m_arm$hand.perc <- (m_arm$avg.hand.length/avg_arm_length)*100;
  m_arm$forearm.perc <- (m_arm$avg.hand.elbow/avg_arm_length)*100;
  m_arm$upper.arm.perc <- (m_arm$avg.elbow.armpit/avg_arm_length)*100;
  
  # percentages of arm span in respect to height
  m_arm$span.perc <- (m_arm$avg.span/m_arm$avg.height)*100;
  
  return(m_arm);
  }
