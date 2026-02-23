
# Path to the original file dataset-imbalanced-10-all.csv
df <- read.csv("C:/bigdata/RADAR/dataset-imbalanced-10-all.csv")

# Clean class names.
df$target.class.name <- sub("-.*", "", df$target.class.name)

table(df$target.class.name) / nrow(df)

# 0:goodware 1:ransomware
table(df$target.class)

# Plot distribution.
t <- table(df$target.class.name)

barplot(t,
        main = "Class distribution",
        xlab = "", 
        ylab = "Counts", 
        col = "skyblue",
        border = "white",
        las = 2)



# undersample
ind.1 <- which(df$target.class==1)

ind.0 <- which(df$target.class==0)

set.seed(123)
ind.0 <- sample(ind.0, size = 6968, replace = F)

# Remove timestamp.
data <- df[c(ind.0,ind.1),-1]

# Remove class.name
data <- data[,-70]

data <- cbind(class=data$target.class.name, data)

data <- data[,-70]

# Add prefixes. 2-48 v1, 49-69 v2 (engineered)
features <- colnames(data)

names.with.prefix <- c("class",paste0("v1_", features[2:48]),paste0("v2_", features[49:69]))

colnames(data) <- names.with.prefix

table(data$class)


write.csv(data, "data/radar-multiclass/data.csv", row.names = F, quote = F)

##### Plots

df <- read.csv("data/radar-multiclass/data.csv")

t <- table(df$class)

barplot(t, 
        main = "Ransom Families Counts",
        xlab = "", 
        ylab = "Counts", 
        col = "skyblue",
        border = "white",
        las = 2)
