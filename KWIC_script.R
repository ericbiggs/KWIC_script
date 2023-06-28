# clear R's memory
rm(list=ls(all=TRUE))

# load the stringi package
library(stringi)

#library(stringi)



# load the paths to all the corpus files
corpus.files <- list.files(path="BNC_baby", pattern="\\.xml$", full.names=TRUE)

# vectorize the search expression
expr.match <- "<w c5=\"(DT0|AT0)\" hw=\"(each|every)\" pos=\"(ADJ|ART)\">(each|every) ?</w>"

# prepare an empty vector to collect all matches at the end of the first for loop
all.matches <- character()

for (i in 1:length(corpus.files)) { 
         
        #select the current corpus file (from 1 to 182) 
        corpus.file <- scan(corpus.files[i], what = "char", sep = "\n", quiet = TRUE) 
         
        #select the sentences in the current corpus file  
        sentences <- grep("<s n=", corpus.file, value = TRUE) 
         
        #select the sentences that contain at least one match  
        sent.with.matches <- grep(expr.match, sentences, ignore.case = TRUE, value = TRUE) 
         
        #get the number of matches per sentences 
        matches.per.sent <- stri_count(sent.with.matches, regex=expr.match, opts_regex=stri_opts_regex(case_insensitive = TRUE)) 
         
        #repeat the sentences as many times as there are matches 
        sent.per.match <- rep(sent.with.matches, matches.per.sent) 
         
        #locate the match positions 
        match.positions <- stri_locate_all(pattern=expr.match, sent.with.matches, regex=TRUE, opts_regex=stri_opts_regex(case_insensitive=TRUE)) 
         
        #if there are no matches, go to the next corpus  
        #if length(match.positions) == 0 {next} 
  
        #if there are no matches, go to the next corpus  
        if (length(match.positions) == 0) { next } 
         
        #if there are, prepare empty numberic vectors to store all match positions 
        all.starts <- numeric() 
        all.ends <- numeric() 
         
        #enter the second loop 
        for (j in 1:length(match.positions)) { 
            match.positions.df <- as.data.frame(match.positions[[j]]) 
            starts <- match.positions.df$start 
            ends <- match.positions.df$end 
            all.starts <- c(all.starts, starts) 
            all.ends <- c(all.ends, ends) 
        } #exit the second for loop 
         
        #tab-delimit the matches  
        delimited.matches <- paste(  
            substr(sent.per.match, 1, all.starts-1), "\t", # the left context 
            substr(sent.per.match, all.starts, all.ends), "\t", # the node 
            substr(sent.per.match, all.ends+1, nchar(sent.per.match)), # the right context 
            sep = "") # an empty seperator 
             
        #clean the delimited matches 
        delimited.matches.clean <- gsub("<.*?>", "", delimited.matches, ignore.case = TRUE) 
        delimited.matches.clean <- gsub(" *(\t) *", "\\1", delimited.matches.clean) 
         
        #prefix the name of the corpus file  
        delimited.matches.clean <- paste(basename(corpus.files[i]), delimited.matches.clean, sep = "\t") 
         
        #clean all the cleaned-up tab-delimited matches 
        all.matches <- c(all.matches, delimited.matches.clean) 
         
    } #exit the second for loop    
	
	# store results in file
	cat("corpus file\tleft context\tnode\tright context", all.matches, sep = "\n", file = "conc_bnc_BABY_1.txt") 


