temp_data <- read.table("D:/Users/Omegadawning/Desktop/School/Fall_2020/Multivariate_Stat/personality-raw.txt", header=T, sep="|")
my_data <- as.data.frame.matrix(temp_data)

my_data <- my_data[-3]
temp_date <- my_data[2]

sep <- data.frame(do.call("rbind", strsplit(as.character(temp_date$date_test), " ", fixed=TRUE)))
sep <- sep[-2]

temp_df <- sep

#temp_df$X1 <- format(as.Date(temp_df$X1), "%U")
colnames(temp_df) <- "WEEK"

sep_trip <- data.frame(do.call("rbind", strsplit(as.character(sep$X1), "/", fixed=TRUE)))
year_df <- data.frame(matrix(sep_trip$X3))

year_df$matrix.sep_trip.X3. <- as.numeric(as.character(year_df$matrix.sep_trip.X3.))
colnames(year_df) <- "YEAR"
#temp_df$X1 <- as.Date(temp_df$X1, format = "%m/%d/%Y")

my_data <- my_data[-2]
my_data$YEAR <- year_df$YEAR
my_data$WEEK <- temp_df$WEEK

my_data <- my_data[, c("md5_email", "YEAR", "WEEK", "V01", "V02", "V03", "V04", "V05", "V06", "V07", "V08", "V09", "V10","V11", "V12", "V13", "V14", "V15", "V16", "V17", "V18", "V19", "V20", "V21", "V22", "V23","V24", "V25", "V26", "V27", "V28", "V29", "V30", "V31", "V32", "V33","V34", "V35", "V36", "V37", "V38", "V39", "V40", "V41", "V42", "V43", "V44", "V45", "V46","V47", "V48", "V49", "V50","V51", "V52", "V53", "V54", "V55", "V56", "V57", "V58", "V59", "V60")]

my_data$WEEK <- as.numeric(as.character(my_data$WEEK))
my_data <- my_data[order(-my_data$YEAR, -my_data$WEEK),]

list_temp <- unique(my_data$md5_email)

dup <- my_data
#dup <- dup %>% unique(md5_email)
set.seed(123)
dup <- dup %>% filter(! duplicated(md5_email))

dim(dup)
dim(temp_data)