set.seed(123)
df <- matrix(runif(600,0,10),ncol = 12)
colnames(df) <- paste("sample",1:12,sep = "")

rownames(df) <- sapply(1:50, function(x)paste(sample(LETTERS,3,replace = F),collapse = ""))

ggheatmap(df)
