
# Install Packages --------------------------------------------------------

#install.packages("tm")
#install.packages("NLP")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("austin", repos="http://R-Forge.R-project.org")
#install.packages("RColorBrewer")
#install.packages("wordcloud")
#install.packages("SnowballC")


##                     __       rm(list=ls())

library(tm)
library(NLP)
library(dplyr)
library(ggplot2)
library(austin)
library(beepr)
library(RColorBrewer)
library(wordcloud)
library(SnowballC)
#############################  
#############################  
#############################  
#############################      +++++ START HERE  ++++






# Set Working Directory ------------------------------------------------------------------

subject.s <- "Border_Mexico"
input.dir <- "Mex_Border_data"


## Setting the Project Directory    _
proj.dir <- "~/Box Sync/TEMPLE/WBCA/Other_Data/YTScraping/"
rawtext.data.dir <- "~/Documents/Data cloudless"

setwd(rawtext.data.dir)



files.v <- dir(input.dir, "\\.tab$")
head(files.v)


###### test read in the article subset
test.tab.df <- read.delim(paste(rawtext.data.dir, input.dir, files.v[1], sep="/"),
                     quote = "", stringsAsFactors=FALSE)
#View(head(test.tab.df))
nrow(test.tab.df)



################+++++++++++++++++ SELECT A SUBSET OF FILES TO WORK FROM
files.v <- files.v[7]
files.v





###############################################################
######################## A Function to CREATE A DATA FRAME OF SELECTED COMMENTS
###############################################################
make.comment.data.df <- function(files.v, input.dir, tol){
counter <- 1
#while(count < 200){}
com.colnames <- c("commentID", "user", "text")
com.array <- matrix(NA, nrow=1, ncol=3)
colnames(com.array) <- com.colnames  #++++++++++++++++++++++++++#@ storage list for final data output
  for(f in 1:length(files.v)){
    print(paste(files.v[f], counter, "total comments so far", sep=" "))
    tab.df <- NA
    tab.df <- read.delim(paste(rawtext.data.dir, input.dir, files.v[f], sep="/"), 
                         quote = "", stringsAsFactors=FALSE)
                             ####OLD#    text.v <- scan(paste(input.dir, files.v[i], sep="/"), what="character", sep="\n")
        for(r in 1:nrow(tab.df)){
            sto <- NA; user <- NA
            sto <- tab.df$text[r]
            user <- tab.df$authorName[r]
            
#@ preprocess sto  
              sto.list <- list()
              sto <- tolower(sto)
              sto.list <- strsplit(sto, "\\W")
              sto.text <- unlist(sto.list)
                 if(length (sto.text) >= tol ) {
                  #@ create list.head               
                  list.head <- NA
                  list.head <- paste("Com", r, files.v[f], sep="_")
                  list.head <- substring(list.head, 1, nchar(list.head)-4)
                  com.array <- rbind(com.array, c(list.head, user, sto))
                  counter <- counter+1
            }}}       
return(com.array)}





###############################################################
######################## and A Function to Read In those files and CREATE LIST TO MAKE CORPUS
###############################################################
make.tab.word.df.l <- function(files.v, input.dir, tol){
  tab.list <- list()    #@ storage list for final data output
  
  for(f in 1:length(files.v)){
    print(files.v[f])
    tab.df <- NA
    tab.df <- read.delim(paste(rawtext.data.dir, input.dir, files.v[f], sep="/"), 
                         quote = "", stringsAsFactors=FALSE)
    ####OLD#    text.v <- scan(paste(input.dir, files.v[i], sep="/"), what="character", sep="\n")
    for(r in 1:nrow(tab.df)){
      sto <- NA
      sto <- tab.df$text[r]
      
      #@ preprocess sto  
      sto.list <- list()
      sto <- tolower(sto)
      sto.list <- strsplit(sto, "\\W")
      sto.text <- unlist(sto.list)
      if(length (sto.text) >= tol ) {
        #@ create list.head               
        list.head <- NA
        list.head <- paste("Com", r, files.v[f], sep="_")
        list.head <- substring(list.head, 1, nchar(list.head)-4)
        tab.list[[list.head]] <- sto.text
      }}}          
  return(tab.list)}

   






#               ++++++++++++++       Code Begins here


###### read in the article subset
#@ CHECK SUBSET FROM ABOVE, RESET FILES.V IF NECESSARY
#files.v <- dir(input.dir, "\\.tab$")
files.v


########## ++++++++++++++++              This makes a list.data object if you are using .tab files (data frames)
###
list.data <- make.tab.word.df.l(files.v, input.dir, tol=33)
length(list.data)
beep("coin")
#View(names(list.data))
#head(list.data)



########################################################################### get data frame
comment.array <- make.comment.data.df(files.v, input.dir, tol=40)
nrow(comment.array)
View(head(comment.array))
#write.csv(comment.array, file = "ABCcomments.csv")   # save data frame
#getwd()
################################################################################################
################################ CODE FOR CHUNKING OUT A SUBSET OF USER COMMENTS BY MODULARITY
################################
################################
setwd(proj.dir)

nodelist.df <- read.csv("video1_nodes.csv", stringsAsFactors=FALSE)
#View(head(nodelist.df))
### test individual mod numbers 
#nodelist.df$Id[which(nodelist.df$modularity_class==1768)]


problem.users <- c("Mike Douglass", "xxk4rilsxx", "Daniel Brown", "loveinspired7",
                   "Jose Gomez", "Nitro Gaming", "Karl Lin", "tina Haynes", "Gerry Svchez")

#node.users1 <- "Nitro Gaming"  # look at comments for a specific user
########################################## FIRST Comment cluster
node.vals1 <- c(468, 1479, 819)
node.users1 <- NA

for (val in 1:length(node.vals1)){
  sto <- NA
  sto <- nodelist.df$Id[which(nodelist.df$modularity_class==node.vals1[val])]
  node.users1 <- c(node.users1, sto)
}
length(node.users1)
node.users1 <- node.users1[-which(node.users1 %in% problem.users==TRUE)]
length(node.users1)

comment.subset1 <- comment.array[0,]
for (user in 1:length(node.users1)){
  sto <- NA
  sto <- comment.array[which(comment.array[,2]==node.users1[user]),]
  comment.subset1 <- rbind(comment.subset1, sto)
}
nrow(comment.subset1)

code <- rep("grn/blu clique", nrow(comment.subset1))
comments1 <- cbind (code, comment.subset1)
#View((comments1))



########################################## SECOND Comment cluster
node.vals2 <- c(2555, 2249, 2819)
node.users2 <- NA

for (val in 1:length(node.vals2)){
  sto <- NA
  sto <- nodelist.df$Id[which(nodelist.df$modularity_class==node.vals2[val])]
  node.users2 <- c(node.users2, sto)
}
length(node.users2)
node.users2 <- node.users2[-which(node.users2 %in% problem.users==TRUE)]
length(node.users2)

comment.subset2 <- comment.array[0,]
for (user in 1:length(node.users2)){
  sto <- NA
  sto <- comment.array[which(comment.array[,2]==node.users2[user]),]
  comment.subset2 <- rbind(comment.subset2, sto)
}
nrow(comment.subset2)

code2 <- rep("purp/grey clique", nrow(comment.subset2))
comments2 <- cbind (code2, comment.subset2)
#View(head(comments2))

############################################################################ CLUSTERS DONE
comment.subset <- rbind (comments1, comments2)
dim(comment.subset)
View(comment.subset)



list.data <- list()

for (com in 1:nrow(comment.subset)) {
  sto <- NA
  sto <- comment.subset[com, 4]
  
  #@ preprocess sto  
  sto.list <- list()
  sto <- tolower(sto)
  sto.list <- strsplit(sto, "\\W")
  sto.text <- unlist(sto.list)
  if(length (sto.text) >= 33 ) {
    #@ create list.head               
    list.head <- NA
    list.head <- comment.subset[com,2]
    list.head <- substring(list.head, 1, nchar(list.head)-25)
    list.data[[list.head]] <- sto.text
  
  }}
length(list.data)



################################
################################
################################################################################################
################################################################################################




############
list.names <- names(list.data)
head(list.names)
#list.data[list.names[1:5]]



##


  # "\n" for newline separator. This may or may not be needed depeing on your data
  # May try with or without the "sep" argument, or try feeding into "sep" other kinds of seperator


####### Turn the list of comment files into a corpus data object
####### this kind of analyisscan only be executed on a corpus object
news.corpus <- Corpus(VectorSource(list.data))

#corpus[["1"]][["content"]]   # it can be good to try to visualize different aspects of your data objects
news.corpus <- tm_map(news.corpus, removeNumbers)
#my.stop <- stopwords("english")
my.stop <- c(stopwords("english"), stopwords("spanish"),
             "c","x", "s", "t", "", "m", "amp", "youtube", "www", "com", "tzswwcwube", "quot", "br",
             "billyenz", "billionz", "billions")
#my.stop
news.corpus <- tm_map(news.corpus, removeWords, my.stop )
beep("coin")


#!!!DocumentTermMatrix
dtm = DocumentTermMatrix(news.corpus)
dtma = removeSparseTerms(dtm, sparse = .998)
#@ including too many very sparse (infrequent) terms negatively impacts results
dtma
beep("coin")





#########################################
####### Running a wordfish model
#########################################
wfa1 <- wordfish(as.wfm(dtma), dir=c(1, 2), control = list(tol = 3.0e-5), verbose = T)
beep("coin")
beep("complete")



#head(wfa1)







# Explore Wordfish --------------------------------------------------------

#@ these commands isolate the document-level data
wfa1$docs <- names(list.data)

wfdocs.v <- wfa1[["docs"]]
theta <- wfa1[["theta"]]
alpha <- wfa1[["alpha"]]


#@ the [-1] indexing for these removes the empty strings ("") that are an artefact of this method
#@ you DO NOT always need this
#View(head(wfa1$words))
wf.words.v <- wfa1[["words"]]; wf.words.v <- wf.words.v[-1]
beta <- wfa1[["beta"]]; beta<-beta[-1]
psi <- wfa1[["psi"]]; psi <- psi [-1]


#####+++++++++++++++++++++++++++++++++++++++++++++ Start to view aspects of The IRT model!

### the sum of theta (document fixed effects) should be 0 if model converged correctly
sum(theta[which(theta>0)])
sum(theta[which(theta<0)])
mean(theta)
sum(theta)
#@ the fact that the sum of theta is NOT 0 means these data aren't great for this model


### View a histogram of the distribution of each key variable
hist(theta, breaks=30)  # document polarity (refined iteratively)
hist(alpha, breaks=30)  # fixed effect for document length
hist(beta, breaks=50)   # word polarity (refined iteratively)
hist(psi, breaks=50)    # fixed effect for term (aka ~type~) frequency


######################### Make Composite data objects
#View(wfdocs.v)

#######################  SOURCE is the news surce the article comes from  +++ NEEDS FIXING
source <- NA
for (i in 1:length(wfdocs.v)){
  sto<-NA
  sto <- gsub("[0-9]", "", wfdocs.v[i])
  sto <- gsub(".txt", "", sto)
  sto <- gsub("article","",sto)
  sto <- gsub("[:.:]","",sto)
  source[i] <- sto
}
#View(head(source))

#############################
####    FOR SUBSETS ONLY
############################
source <- comment.subset[,1]
user <- comment.subset[,3]



#######################  TITLE is the title with metadata stripped out  +++ NEEDS FIXING
title<-NA
for (i in 1:length(wfdocs.v)){
  sto<-NA
  sto <- substr(wfdocs.v[i],6,14 )
  title[i] <- sto
}
#View(head(title))

#######################  DECADE is the decade of the article  +++ NEEDS FIXING
decade<-NA    # DOES NOT WORK CORRECTLY for WITHIN VID COMMENT SUBSET
for (i in 1:length(wfdocs.v)){
  sto<-NA
  sto <- gsub("\\D", "", wfdocs.v[i])
  sto <- substr(sto,1,3)
  sto <- paste(sto, "0s", sep="")
  decade[i] <- sto
}
#View(head(decade))


### CREATE DOC DATA DATAFRAME
wf_docdata.df <- data.frame(wfdocs.v, source, decade, title, theta, alpha)
#View(wf_docdata.df)


#######################  WORD is the word without " and , #@ ideally
word<-NA
for (i in 1:length(wf.words.v)){
  sto<-NA
  #sto <- wf.words[i]
  sto <- gsub("\\W", "", wf.words.v[i])
  #sto <- substr(sto,1,3)
  word[i] <- sto
}
### CREATE WORD DATA DATAFRAME
wf_worddata.df <- data.frame(word, beta, psi)

#View(wf_worddata.df)







##### move working directory to the 'visualizations' repository

### create and set a visualization directory
vis.dir <- "/Visualizations/"
dir.create("Visualizations")
working.vis.dir <- paste(rawtext.data.dir, vis.dir, sep="" )
working.vis.dir
setwd(working.vis.dir)

subject.dir <- paste(subject.s, ".vis", "/", sep="")
dir.create(subject.dir)
setwd(paste(rawtext.data.dir, vis.dir, subject.dir, sep=""))
getwd()


############ Plot the two estimated document parameters: THETA against ALPHA, incl. METADATA #########
#@ Alpha = document fixed effect (control for length)
#@ Theta = document "polarity" - alignment along as-identified latent principle component

###### Label by USER color by CLIQUE
user_T_A_plot <- ggplot(data = wf_docdata.df, mapping = aes(x =theta, y = alpha, label = user, color=source)) + 
  geom_text(size = 1.2) + 
  labs(x = "Comment polarity: an optimized value (theta)", y = "Comment length: a fixed effect (alpha)") +
  guides(size = "none", color = guide_legend("")) + theme(legend.position="bottom") +
  labs(title = "Comment Polarity in comment network cliques of one thread scraped from YouTube", 
                      subtitle="'CNN reporter presses Trump: You promised Mexico would pay for wall'") 
user_T_A_plot 


ggsave(paste(subject.s, "SUBSET_user_T_A_plot.pdf",sep=""), device="pdf")



###### Color by SOURCE
source_T_A_plot <- ggplot(data = wf_docdata.df, mapping = aes(x =theta, y = alpha, label = title, color=source)) + 
  geom_text(size = .7) + 
  labs(x = "Doc polarity: an optimized value (theta)", y = "Doc length: a fixed effect (alpha)") +
  guides(size = "none", color = guide_legend("")) + theme(legend.position="bottom") +
  labs(title = paste (subject.s, 
                      " comments from ABC YouTube Videos:\n Article IDs plotted, shaded by comment thread source", sep="")) 
##     __ 
source_T_A_plot

ggsave(paste(subject.s, "SUBSET_source_T_A_plot.pdf",sep=""), device="pdf")



###### COLORLESS
source_T_A_plot <- ggplot(data = wf_docdata.df, mapping = aes(x =theta, y = alpha, label = title)) + 
  geom_text(size = 1) + 
  labs(x = "Doc polarity: an optimized value (theta)", y = "Doc length: a fixed effect (alpha)") +
  #guides(size = "none", color = guide_legend("")) + theme(legend.position="bottom") +
  labs(title = paste (subject.s, 
                      " comments from ABC YouTube Videos:\n Article IDs plotted, shaded by comment thread source", sep="")) 
##     __ 
source_T_A_plot

ggsave(paste(subject.s, "SUBSET_colorless_T_A_plot.pdf",sep=""), device="pdf")


######################## FIRST PLOT of two word parameters: BETA against PSI, basic black and white
#@***limiting by a critical max negitive psi value***
word_P_B_plot <- ggplot(data = wf_worddata.df, mapping = aes(x = beta, y = psi, label = word)) + 
  geom_text(data=subset(wf_worddata.df, psi>-20), size = 0.755) + 
  labs(x = "Word polarity: an optimized value (beta)", y = "Word frequency: a fixed effect (psi)") +
  #guides(size = "none", color = guide_legend("")) + 
  labs(title = "Vocabulary Polarity in comment network cliques of one thread scraped from YouTube", 
       subtitle="'CNN reporter presses Trump: You promised Mexico would pay for wall'")
######           __
word_P_B_plot


ggsave(paste(subject.s, "SUBSET_word_P_B_plot.pdf",sep=""), device="pdf")





######################## SO optimized!!
#wf_worddata.df_unreduced <- wf_worddata.df
## RESET wf_worddata.df if/when needed     __ wf_worddata.df <- data.frame(word, beta, psi)

######+++++++++++++++++++++++++++++++++++++++++++ Set which KEY TERMS should have BIG FONT
    
neutral <- "neutral"  # Grey
topA <- "Of Interest"      # Red
topB <- "Wall"      # 
topC <- "Money"      # 
topD <- "Trade"      # 
topE <- "Rape"     # 
topF <- "Voting"   # 
topG <- "Drugs"    # 
topH <- "Jobs"     # 
#topic_colors <- c()

wf_worddata.df$key <- neutral  ### SET / RESET default font SIZE/COLOR and word coding


ktA <- c("trump", "animal", "animals", "obama", "illegal", "mexico", "mexican", "caravan", 
         "jew", "jews", "jewish", "security", "secure", "national", "nation")
for(k in 1:length(ktA)){
  sto <- NA
  sto <- (which(word==ktA[k]))
  #print(sto)
  wf_worddata.df$key[sto] <-topA }   ### NEW FONT SIZE
#View(wf_worddata.df[which(wf_worddata.df$key==topA),])


ktB <- c("wall", "build", "steel", "concrete", "border", "dig", "tunnels", "bars", "invisible", "see")
for(k in 1:length(ktB)){
  sto <- NA
  sto <- (which(word==ktB[k]))
  wf_worddata.df$key[sto] <-topB }    ### NEW FONT SIZE
#View(wf_worddata.df[which(wf_worddata.df$key==topB),])

ktC <- c("money", "tax", "taxes", "taxpayers", "billion", "billions", "spend", "spent", "pay", "billionz")
for(k in 1:length(ktC)){
  sto <- NA
  sto <- (which(word==ktC[k]))
  wf_worddata.df$key[sto] <-topC }    ### NEW FONT SIZE
#View(wf_worddata.df[which(wf_worddata.df$key==topC),])

ktD <- c("nafta", "trade", "usmca")
for(k in 1:length(ktD)){
  sto <- NA
  sto <- (which(word==ktD[k]))
  wf_worddata.df$key[sto] <-topD }    ### NEW FONT SIZE
#View(wf_worddata.df[which(wf_worddata.df$key==topD),])


ktE <- c("rape", "raped", "raping", "rapes", "assault", "sexual", "victim", "rapists", "ptsd")
for(k in 1:length(ktE)){
  sto <- NA
  sto <- (which(word==ktE[k]))
  wf_worddata.df$key[sto] <-topE }    ### NEW FONT SIZE
#View(wf_worddata.df[which(wf_worddata.df$key==topE),])

ktF <- c("vote", "voting", "election", "elections", "electoral", "rights", "birthrights")
for(k in 1:length(ktF)){
  sto <- NA
  sto <- (which(word==ktF[k]))
  wf_worddata.df$key[sto] <-topF }    ### NEW FONT SIZE
#View(wf_worddata.df[which(wf_worddata.df$key==topF),])

ktG <- c("drug", "drugs", "illicit", "police", "marijuana", "heroin", "coke", "cocaine", "fentanyl")
for(k in 1:length(ktG)){
  sto <- NA
  sto <- (which(word==ktG[k]))
  wf_worddata.df$key[sto] <-topG }    ### NEW FONT SIZE
#View(wf_worddata.df[which(wf_worddata.df$key==topG),])


ktH <- c("job", "jobs", "economic", "work", "worker", "labor", "laborer", "farm", "farms", "agriculture")
for(k in 1:length(ktH)){
  sto <- NA
  sto <- (which(word==ktH[k]))
  wf_worddata.df$key[sto] <-topH }    ### NEW FONT SIZE
#View(wf_worddata.df[which(wf_worddata.df$key==topH),])


#+++++++++++++++  use this step to snip off extreem beta and psi vales to maximize plotting area

#lowpsi <- which(wf_worddata.df$psi <= (-9.9))
#lowbeta <- which(wf_worddata.df$beta <= (-4.4))
#wf_worddata.df <- wf_worddata.df[(-lowpsi), ]
#wf_worddata.df <- wf_worddata.df[(-lowbeta), ]

#which(wf_worddata.df$psi <= (-13))    ## check your work!




##########++++++++++++++++++++ SECOND PLOT, two word parameters: BETA against PSI, color by KEY TERMS #####
wordLegend_P_B_plot <- ggplot(data = wf_worddata.df, mapping = aes(x = beta, y = psi, label = word, color=key)) + 
  ylim(-11,.2)+ xlim(-5,6) +       #tight limits
  #ylim(-14,.2)+ xlim(-7,8) +     #more expanded limits
  geom_text(data=subset(wf_worddata.df, key== neutral), size = .85, color="gray") + 
  geom_text(data=subset(wf_worddata.df, key!= neutral), size = 2.2) +
  scale_color_discrete(l=40) +
  #scale_color_manual(values=c("#00008B", "#8B2323", "#006400", "goldenrod4", "#8B0A50" )) +
  guides(size = "none", color = guide_legend("")) + theme(legend.position = "top") +
  labs(x = "Word polarity: an optimized value (beta)", y = "Word frequency: a fixed effect (psi)") +
  labs(title = "Vocabulary Polarity in comment network cliques of one thread scraped from YouTube", 
       subtitle="'CNN reporter presses Trump: You promised Mexico would pay for wall'\n key terms bolded")

######           __
wordLegend_P_B_plot

ggsave(paste(subject.s, "_QQQQ_SUBSET_wordLegend_P_B_plot.pdf",sep=""), device="pdf")









beep("coin");beep("coin")
beep("complete")
beep("mario")





#
#
#
############### Trying out Wordclouds....?
############### Trying out Wordclouds....?
############### Trying out Wordclouds....?

#@ WC separate articles by 


###### Wordcloud!
cloud.dtm <- TermDocumentMatrix(news.corpus)
cloud.m <- as.matrix(cloud.dtm)
cloud.v <- sort(rowSums(cloud.m),decreasing=TRUE)
cloud.df <- data.frame(word = names(cloud.v),freq=cloud.v, stringsAsFactors = F)
cloud.df <- cloud.df[-1,]
View(head(cloud.df, 10))

### remove " " and , from words
clean.cloud.df <- cloud.df
cloud.df[5,]
word<-NA
for (i in 1:nrow(cloud.df)){
  sto<-NA
  #sto <- gsub("\\W", "", cloud.df[i,1])  # this approach isn't working
  sto <- cloud.df[i,1]
  sto <- substr(sto,2,(nchar(sto)-2))
  clean.cloud.df[i,1] <- sto
}
View(head(clean.cloud.df, 10))



set.seed(1234)
wordcloud(words = clean.cloud.df$word, freq = clean.cloud.df$freq, min.freq = 1,
          max.words=99, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


findAssocs(cloud.dtm, terms = "dadt", corlimit = 0.3)  ## doesn't work, presumably b/c of quotes and '/', ',' issues

cloud.dtm
head(cloud.m)

############### Trying out Wordclouds....?
############### Trying out Wordclouds....?
############### Trying out Wordclouds....?
#
#
#
#
#



############### Trying out Wordscores++++++++++++++++++++++++++++++ SKIP WORDSCORE BIT FOR NOW!
############### Trying out Wordscores
############### Trying out Wordscores
############### Trying out Wordscores



#install.packages("quanteda")
require(quanteda)
#install.packages("quanteda.corpora")
#require(quanteda.corpora)

files.v[c(4,47)]
ref <- c(1,47) # reference texts
vir <- 1:length(files.v) # SPS 2011 (short) is empty, thus not included
vir <- vir[-ref] # everything minus the reference texts
ref; vir

#?getdocs()
#?wfm()
str(corpus)
news.as.corpus<- corpus(news.corpus)
str(news.as.corpus)
r <- getdocs(news.as.corpus, ref)


#?classic.wordscores()
ws <- classic.wordscores(r, scores=c(0,1.4))



################
############### Trying out Wordscores
############### Trying out Wordscores
############### Trying out Wordscores
############### Trying out Wordscores+++++++++++++++++++++++++++++++++++++++++ END WORDSCORE
#
#

############################################################
############################################################
############################################################
############################################################
#+++++++++++++ Experimental Code
############################################################

#### Make List.DATAFRAME
list.colnames <- c("commentID", "text")
list.array <- matrix(NA, nrow=1, ncol=2)
colnames(list.array) <- list.colnames
list.array

length(list.data)

wordz <- list.data[list.names[2]]
wordz
#wordz.unl <- unlist(wordz, use.names=F)
#wordz.unl
wordz.paste <- paste(wordz, sep="")
wordz.paste
?unlist()

wordz
wordz.unl <- unlist(wordz, use.names=F)
wordz.unl


wordz.vec<-NA; wordz.paste <- NA
for (i in 1:length(wordz.unl)){
  sto<-NA
  sto <- gsub("\\W", "", wordz.unl[i])
  #sto <- substr(sto,1,3)
  wordz.vec[i] <- sto
}
str(wordz.vec)
wordz.paste <- paste(wordz.vec, sep=" ")
wordz.paste

wordz.coll <- paste(wordz, sep=" ")
wordz.coll
wordz.cln <- gsub("\\W", "", wordz.coll)
wordz.cln



#for(com in 1:length(list.data)){
for (com in 1:6){
  title <- NA; text <- NA
  title <- list.names[com]
  text <- paste(list.data[list.names[com]], sep=" ")
  
  list.array[com,1] <- title
  list.array[com,2] <- text
}
list.array

?matrix()





###############                       IGNORE THIS CHUNK (may need to draw from it later)       
#@ for each row in data frame
#@ pull relevant data, preprocess, add to a list          
#@ convert to a single string
text.v <- paste(text.v, collapse=" ")
#@ we're making it one big line here - then taking it to lowercase, split on nonword chars
##########text.lower.v <- tolower(text.v)
##########text.words.v <- strsplit(text.lower.v, "\\W")
#@ skipping tolower step - we already did that
text.words.v <- strsplit(text.v, "\\W")
#@ "\\W" finds everything that is a word
text.words.v <- unlist(text.words.v)
#text.words.v <- gsub("[:,:]", "",text.words.v)
#text.words.v <- gsub("[:':]", "",text.words.v)
#text.words.v <- gsub("[:\":]", "",text.words.v)
text.words.v <- text.words.v[which(text.words.v!="")]
#@ use index id as name of list
text.word.vector.l[[files.v[i]]] <- text.words.v



