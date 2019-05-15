# The Gender Binary of Computing: Challenging Sexism in Technology

My [article](https://zalkikar.github.io/) explores the role of gender in early computing history (1970-1980) through the analysis of computer hobbyist letters published in Byte magazine. 

This repo contains R code used for exploratory data analysis. It creates summary statistics, basic visualizations, co-occurrence matrices, and (complete/average linkage) clusterings of the thematic elements contained in hobbyist letters. 

It contains an Excel table (byte-letters-1975-1980-finalcleanedv2.csv) with the following letter data:
* Coder	(KD: Kevin Driscoll, RZ: Rahul Zalkikar)
* Letter Code	(YEAR.MONTH.LETTER#)
* Title	(Letter Author Title)
* Affiliation	(Letter Author Affiliation)
* Gender (Letter Author Gender)
* Street (Letter Author Street)
* City (Letter Author City)
* State	(Letter Author State)
* Country	(Letter Author Country)
* Postal Code	(Letter Author Postal Code)
* Location (Letter Author Location)
* BingGeo	(BingGeo Location)
* Latitude	
* Longitude	
* Month	(Month of Letter Publication)
* Year (Year of Letter Publication)
* Which column?	(This will be the 'Letter' Column)
* Letter Title	
* Letter number	(Chronological Letter Order #)
* Page #	(Page of Letter in *Byte*)
* Reference: Title or topic	(Info of Reference if one is present)
* Reference: Month	
* Reference: Year	
* Reference: Page	
* In Reply To: Full Letter Code	(Letter Code that Letter is Replying to)
* Did the Editors reply in-line? (Editor Reply to Letter?)

and the following letter themes (presence of a theme is indicated by a 1, and is otherwise 0):
* AI or robotics	
* Academia and research	
* Announcement for Byte readers	
* Arts, music	
* BBS, modems, telecommunications	
* Club or user group	
* Commentary, opinion	
* Computer Education/Teaching/Library	
* Computer store or other consumer/retail	
* Correction to an article	
* Criticism of Byte	
* Disability	
* Family, children, spouse	
* Gaming	
* Identifies as novice	
* Includes source code	
* Notes their own age	
* Personal finance	
* Piracy/CopyProtection	
* Praise for Byte	Printers	
* Product/service recommendation or warning	
* Programming	
* Representation of gender (and race) 
* Request for help, assistance, advice	
* Request to Byte editors re: magazine	
* Software Distributor/Distribution	
* Technical tip or how-to	
* Work, professional life, business finances	
* Other

Author Name was removed for privacy and a lack of justification for inclusion (was not used in the research itself).
Letter Notes and In Reply To Notes were removed since there also were not used in the research itself.

It also contains a second Excel table (byteoriginendplaces.csv), which contains a subset of columns from the primary Excel table formatted for determining letter replies between hobbyists.

Each figure.png in the repo corresponds to a letter of interest in the article.

This research was conducted at the University of Virginia with mentorship from [Professor Kevin Driscoll](http://kevindriscoll.info/).
