# IMPORTANT FUNCTION
roundUpNice <- function(x, nice=c(29,59,75,99)) {
  if(length(x) != 1) stop("'x' must be of length 1")
  as.numeric(nice[[which(x <= nice)[[1]]]])
}

dt[,paste0(names(table)[j],"round"):= lapply(dt[[grep(names(table)[j],names(dt))[[1]]]],
                                             roundUpList)]

dt[,paste0(names(table)[j],"round"):= lapply(dt[[which(names(table)[j] == names(dt))[[1]]]],
                                             roundUpList)]

test2[,paste0(names(test2)[1],"round"):= as.numeric(lapply(test2[[which(names(test2)[1] == names(test2))[[1]]]],
                                             roundUpNice))]

test2[,paste0("AGE","_round") := lapply(test2[[grep("AGE",names(test2))[[1]]]],roundUpNice)]

grep("string",sapply(table,class))

dt %>% group_by(AGE_GRP) %>% summarize(lower = min(AGE))
