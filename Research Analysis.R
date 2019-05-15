# Rahul Zalkikar
# University of Virginia - 2019
# Research Assistant 

##########    Importing the Data    ########## 
byte_data <- read.csv('C:\\Users\\rayzc\\OneDrive\\Pictures\\Documents\\byte-letters-1975-1980-finalcleanedv2.csv')
colnames(byte_data) 
theme_data <- byte_data[,27:56] #Theme counts
colnames(theme_data)
communicators_direct <- c()
counter <- 0

letterlinks <- read.csv('C:\\Users\\rayzc\\OneDrive\\Pictures\\Documents\\byteoriginendplaces.csv', header=TRUE)
letterlinks <- data.frame(letterlinks, headers=TRUE)
communicators_direct <- which(!(letterlinks[['ï..Origin.Place']] %in% letterlinks[['End.Place']])) #Indeces of letters that are in response to another location (another letter)
letterlink_themes <- theme_data[communicators_direct,] #Now we have themes that are associated with letter links!
dim(letterlink_themes)

#Save the Indexes of these letters that are referencing EXACTLY one other letter
#Analyze the associated themes to those letter(s) that are: (1) Doing the 'referencing'/'replying' (2) The original letter of source

crit_data = byte_data[byte_data['Criticism.of.Byte'] == 1, ]
dim(crit_data)
cat('% of letters praising Byte amongst those that critiqued Byte: ', 100*sum(crit_data['Praise.for.Byte'])/44)





##########    Going Deeper into Gender and Family themes    ##########
print(sum(theme_data[["Representation.of.gender.and.race"]]))
print(sum(theme_data[["Family..children..spouse"]]))
Gender_letterindeces <- which(theme_data[["Representation.of.gender.and.race"]] == 1)
Family_letterindeces <- which(theme_data[["Family..children..spouse"]] == 1)

GenderFamily_letterindeces <- unique(Gender_letterindeces[Gender_letterindeces%in%Family_letterindeces])
length(unique(Gender_letterindeces[Gender_letterindeces%in%Family_letterindeces]))

#Segment the Data
gendata <- byte_data[Gender_letterindeces,]
famdata <- byte_data[Family_letterindeces,]
gendataONLY <- byte_data[unique(Gender_letterindeces[!(Gender_letterindeces%in%GenderFamily_letterindeces)]),]
famdataONLY <- byte_data[unique(Family_letterindeces[!(Family_letterindeces%in%GenderFamily_letterindeces)]),]
genfamdata <- byte_data[unique(Gender_letterindeces[Gender_letterindeces%in%Family_letterindeces]),]

genfamthemedata <- genfamdata[,27:56]
genthemedata <- gendata[,27:56]
famthemedata <- famdata[,27:56]
genONLYthemedata <- gendataONLY[,27:56]
famONLYthemedata <- famdataONLY[,27:56]

details <- c(5,11,15,16,20,18) #Important letter properties - Gender, Location, Month, Year, Page, and Letter Title
genfamthemedetails <- genfamdata[,details]
genthemedetails <- gendata[,details]
famthemedetails <- famdata[,details]
genONLYthemedetails <- gendataONLY[,details]
famONLYthemedetails <- famdataONLY[,details]

famONLYthemedetails #Example 





# RUN ONE OF THE FOLLOWING:

#If you want to conduct aggregate thematic analysis
theme_data <- theme_data[,-1]
total_letters <- 676 #total Byte letters

#If you want focused analysis on letter replies in Byte (direct communication instances)
theme_data <- letterlink_themes 
total_letters <- 97

#GenderFamily
theme_data <- genfamthemedata
total_letters <- 10

#AllGender
theme_data <- genthemedata
total_letters <- 14

#AllFamily
theme_data <- famthemedata
total_letters <- 15

#AllGender No Family
theme_data <- genONLYthemedata
total_letters <- 4

#AllFamily No Gender
theme_data <- famONLYthemedata
total_letters <- 5


# RUN THE REST:

# We remove 'Other' themes for now...
Other_themes <- (theme_data[,29])
Other_themes <- Other_themes[Other_themes != ""]
Unique_themes <- unique(Other_themes)
Unique_themes

write.table(Unique_themes, "Other Themes.txt")

theme_data$Other <- NULL
themes <- colnames(theme_data)
themes

##########    Creating Theme Counts & Set-up   ########## 
theme_counts <- rep(0,length(themes))
counter <- 1
for (i in theme_data){
  theme_counts[counter] <- sum(i)
  counter <- counter + 1
}

theme_prop <- rep(0, length(themes))
counter = 1
for (i in themes){
  theme_prop[counter] <- theme_counts[counter]/dim(theme_data)[1]
  if (theme_prop[counter] > 0.50){
    print(i)
  }
  if (theme_prop[counter] < 0.05){
    print(i)
  }
  counter <- counter + 1
}

length(theme_data)
length(themes)
length(theme_counts)

for (i in c(1:length(themes))){
  print(themes[i])
  print(theme_counts[i])
}


theme_counts_df = data.frame(themes, theme_counts)
theme_counts_df[order(theme_counts),]

plot(theme_counts_df)

# Make a vector of all elements.
elems <- colnames(theme_data[,0:28])
elems
# Make a sparse matrix
library(Matrix)
s <- Matrix(as.matrix(theme_data[elems]), sparse=TRUE)
# calculate co-occurrences
m <- (t(s) %*% s)
dim(m)
m <- as.matrix((m))
m <- as.data.frame(m)
print(m)

write.csv(m,file="theme_cooccurance_matrix.csv")
#Still have to change the co-occurance values for two of the same themes to 0 instead of their total count

##########    Function to determine statistical significance    ########## 
significance <- function(pvalue, alpha){
  if(pvalue == alpha || pvalue < alpha){return(TRUE)}
  else{return(FALSE)}
}

##########    Function to Set Up Testing of Thematic Dependence    ########## 
theme_dependence <- function(theme1, theme2){
  c <- which(themes %in% theme1) #Index of Theme 1
  a <- which(themes %in% theme2) #Index of Theme 2
  t1 <- theme_data[,c] 
  t2 <- theme_data[,a]
  c_a <- sum(t1 & t2) #Number of common values between Theme 1 and Theme 2
  prop_c_a <- c_a/total_letters # Prop of Theme 1 and Theme 2 Letters
  prop_c <- sum(t1)/total_letters # Prop of Theme 1 letters
  prop_a <- sum(t2)/total_letters # Prop of Theme 2 letters
  
  count_a_c <- sum(t2)*prop_c #Count of t2 Letters that SHOULD also include t1 theme
  popprop_a_c <- count_a_c/total_letters #Prop of t2 Letters SHOULD also include t1 theme
  
  count_c_a <- sum(t1)*prop_a #Count of t1 Letters that SHOULD also include t2 theme
  popprop_c_a <- count_c_a/total_letters #Prop of t1 Letters SHOULD also include t2 theme
  
  #These should be equal
  print(popprop_c_a)
  print(popprop_a_c)
  
  mu <- popprop_c_a
  print(paste0("Proportion of Theme 1 Letters that SHOULD also include Theme 2: ", mu))
  print(paste0("Observed proportion of Theme 1 Letters that included Theme 2: ", prop_c_a))
  
  return(c(mu, c_a))
}



##########    Statistical Test for Significance for Dependence between Commentary and Announcements    ##########
mu <- theme_dependence("Announcement.for.Byte.readers", "Commentary..opinion")[[1]]
count <- theme_dependence("Announcement.for.Byte.readers", "Commentary..opinion")[[2]]
binom.test(count, total_letters, mu, alternative="greater") #Two-sided

##########    Significance of P-value from Statistical Test (alpha level of 0.05)    ##########
p.val <- 0.02646 #As Observed from binomial test
alpha_level <- 0.05
significance(p.val, alpha_level) #Is the p value statistically significant?

#Conclusion: Dependence between Commentary and Announcement themes is statistically significant





##########    Statistical Test for Significance for Dependence between Gender/Race and Family    ##########
mu <- theme_dependence("Representation.of.gender.and.race", "Family..children..spouse")[[1]]
count <- theme_dependence("Representation.of.gender.and.race", "Family..children..spouse")[[2]]
binom.test(count, total_letters, mu, alternative="greater") #Two-sided

##########    Significance of P-value from Statistical Test (alpha level of 0.05)    ##########
p.val <- 0 #As Observed from binomial test
alpha_level <- 0.05
significance(p.val, alpha_level) #Is the p value statistically significant?

#Conclusion: Dependence between Gender/Race and Family themes is statistically significant






##########    Statistical Test for Significance for Dependence between Telecommunications and Software Development    ##########
mu <- theme_dependence("BBS..modems..telecommunications", "Software.Distributor.Distribution")[[1]]
count <- theme_dependence("BBS..modems..telecommunications", "Software.Distributor.Distribution")[[2]]
binom.test(count, total_letters, mu, alternative="greater") #Two-sided

##########    Significance of P-value from Statistical Test (alpha level of 0.05)    ##########
p.val <- .424 #As Observed from binomial test
alpha_level <- 0.05
significance(p.val, alpha_level) #Is the p value statistically significant?

#Conclusion: Dependence between Telecommunications and Software Development themes is NOT statistically significant






##########    Statistical Test for Significance for Dependence between Programming and Product Rec/Warn    ##########
mu <- theme_dependence("Programming", "Product.service.recommendation.or.warning")[[1]]
count <- theme_dependence("Programming", "Product.service.recommendation.or.warning")[[2]]
binom.test(count, total_letters, mu, alternative="greater") #Two-sided

##########    Significance of P-value from Statistical Test (alpha level of 0.05)    ##########
p.val <- .06685 #As Observed from binomial test
alpha_level <- 0.05
significance(p.val, alpha_level) #Is the p value statistically significant?

#Conclusion: Dependence between Programming and Product Rec/Warn themes is NOT statistically significant






##########    Statistical Test for Significance for Dependence between Personal Finance and Work Life    ##########
mu <- theme_dependence("Personal.finance", "Work..professional.life..business.finances")[[1]]
count <- theme_dependence("Personal.finance", "Work..professional.life..business.finances")[[2]]
binom.test(count, total_letters, mu, alternative="greater") #Two-sided

##########    Significance of P-value from Statistical Test (alpha level of 0.05)    ##########
p.val <- 0.008303 #As Observed from binomial test
alpha_level <- 0.05
significance(p.val, alpha_level) #Is the p value statistically significant?

#Conclusion: Dependence between Personal Finance and Work Life themes is statistically significant


##########    By now it should be established that BYTE is, statistically speaking, a community of print for computer hobbyists   ########## 






##########    Importing the Data To Analyze Letter Communication   ########## 

#Make correlation matrix from co-occurance matrix
library("Hmisc")
m <- as.matrix(m)
cor.matrix <- rcorr(m)

# Extract the correlation coefficients and save them
head(cor.matrix$r)
corcoef.matrix <- cor.matrix$r
write.csv(cor.matrix$r,file="theme_correlation_coef_matrix.csv")

#check matrix dimensions
dim(corcoef.matrix)

#melt the correlation matrix
library(reshape2)
melted_corcoef <- melt(corcoef.matrix)
head(melted_corcoef)

#visualize correlation matrix - this has redudant information and is too hard to visualize because of the number of themes
library(ggplot2)
ggplot(data = melted_corcoef, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

#Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
#Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

#Make heatmap that is easier to visualize - for each theme
ggplot(data =melted_corcoef[melted_corcoef$Var2==colnames(m)[10],], aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

#This might still be difficult to visualize - Which themes are very highly correlated?
Ave.cor <- mean(melted_corcoef$value[melted_corcoef$value!=1]) #Average correlation between themes
Ave.cor

#A correlation coefficient of .4349 is fairly high...but lets keep exploring
melted_corcoef[melted_corcoef$value>0.9 & melted_corcoef$value!=1,] #Above 90% correlation
write.csv(as.matrix(melted_corcoef[melted_corcoef$value>0.9 & melted_corcoef$value!=1,]),file="theme_correlation_coef_matrix_90.csv")
melted_corcoef[melted_corcoef$value>0.8 & melted_corcoef$value!=1,] #Above 80% correlation
write.csv(as.matrix(melted_corcoef[melted_corcoef$value>0.8 & melted_corcoef$value!=1,]),file="theme_correlation_coef_matrix_80.csv")
melted_corcoef[melted_corcoef$value>0.7 & melted_corcoef$value!=1,] #Above 70% correlation
write.csv(as.matrix(melted_corcoef[melted_corcoef$value>0.7 & melted_corcoef$value!=1,]),file="theme_correlation_coef_matrix_70.csv")

melted_corcoef$value[melted_corcoef$value<0] #Negative correlations are of relatively low strength - although they could be interesting to explore

#Calculate the distance
distance <- dist(cor(m), "euclidean")
# Perform the hierarchical clustering using the average linkage
hc_ave <- hclust(distance, "average")
# Perform the hierarchical clustering using the complete linkage
hc_comp <- hclust(distance, "complete")

hc_comp$labels <- c("Academia","Announcement","Arts/Music","Telecomun","Club/Group","Commentary",
                    "ComputerEd","ConsumerRetail","Correction","Critique","Disability","FamilyLife", "Gaming","Novice",
                    "SouceCode","NotesAge","PersonalFinance","Piracy","Praise","Printers","ProductRec/Warn",
                    "Programming","Gender/Race","RequestAdvice","RequestByte","Software","TechnicalTip", "WorkFinance")

hc_ave$labels <- c("Academia","Announcement","Arts/Music","Telecomun","Club/Group","Commentary",
                   "ComputerEd","ConsumerRetail","Correction","Critique","Disability","FamilyLife", "Gaming","Novice",
                   "SouceCode","NotesAge","PersonalFinance","Piracy","Praise","Printers","ProductRec/Warn",
                   "Programming","Gender/Race","RequestAdvice","RequestByte","Software","TechnicalTip", "WorkFinance")

#Dendograms
dend_comp <- as.dendrogram(hc_comp)
dend_ave <- as.dendrogram(hc_ave)
plot(hc_comp, main = 'Complete Linkage') #More distinct clusters here
plot(hc_ave, main = 'Average Linkage') #Interesting to see that both these clusters represent family and gender/race themes as distinctly clustered together

#Colored Dendograms w/cuts
library(dendextend)
dend_comp_20 <- color_branches(dend_comp, h=.5)
plot(dend_comp_20, main = 'Complete Linkage', ylab = "Height")
dend_ave_20 <- color_branches(dend_ave, h=.5)
plot(dend_ave_20, main = 'Average Linkage', ylab = "Height")

#Infer Relationships by comparison of two clustering methods!
