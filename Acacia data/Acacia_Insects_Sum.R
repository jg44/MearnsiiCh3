data <- read.csv("Hemiptera_Acacia.csv", colClasses=c("Sample"="character"))

data <- subset(data, select=-Date)

data <- subset(data, !grepl("A_sieberiana", data$Host), drop=T)
data <- subset(data, select=-Host)

data$Sample <- unlist(lapply(strsplit(data$Sample, "\\_"), "[[", 1))

data <- aggregate(. ~ Sample, data, sum)

rownames(data) <- data$Sample

data <- subset(data, select=-Sample)

write.csv(data, "csv/Hemiptera_Acacia.csv")
