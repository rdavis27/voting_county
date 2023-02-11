library(tidyverse)
library(readxl)
library(ggplot2)
#library(reshape2)

library(urbnmapr)
#library(ggrepel)
#library(maps)
library(leaflet)
library(sf)
library(rgeos)
library(tigris)

input_dir <- "input/"
input2_dir <- "input2/"
data_dir  <- "data/"
gvv <- NULL # verifier dataframe for current year and state
hvv <- NULL # verifier dataframe for current year, state, and equipment

shinyServer(
    function(session,input,output) {
        options(width = 140, readr.show_progress = FALSE)
        options(max.print=999999)
        
        states <- c("Alabama","Alaska","Arizona","Arkansas","California",
                    "Colorado","Connecticut","Delaware","District of Columbia","Florida",
                    "Georgia","Hawaii","Idaho","Illinois","Indiana",
                    "Iowa","Kansas","Kentucky","Louisiana","Maine",
                    "Maryland","Massachusetts","Michigan","Minnesota","Mississippi",
                    "Missouri","Montana","Nebraska","Nevada","New Hampshire",
                    "New Jersey","New Mexico","New York","North Carolina","North Dakota",
                    "Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island",
                    "South Carolina","South Dakota","Tennessee","Texas","Utah",
                    "Vermont","Virginia","Washington","West Virginia","Wisconsin",
                    "Wyoming")
        stabbr <- c("AL","AK","AZ","AR","CA",
                    "CO","CT","DE","DC","FL",
                    "GA","HI","ID","IL","IN",
                    "IA","KS","KY","LA","ME",
                    "MD","MA","MI","MN","MS",
                    "MO","MT","NE","NV","NH",
                    "NJ","NM","NY","NC","ND",
                    "OH","OK","OR","PA","RI",
                    "SC","SD","TN","TX","UT",
                    "VT","VA","WA","WV","WI","WY")
        createStates_President_2000_2020 <- function(){
            xx <- read_delim(paste0(input_dir,"countypres_2000-2020.csv"), ",",
                             col_names = TRUE, col_types = "dcccccccddcc")
            colnames(xx) <- c("YEAR","STATE","ST","COUNTY","COUNTY_FIPS","OFFICE",
                              "CANDIDATE","PARTY","VOTES","TOTALVOTES","VERSION","MODE")
            years <- seq(2000, 2020, by = 4)
            for (year in years){ # change to years
                for (st in stabbr){
                    yy <- xx[xx$YEAR == year,]
                    yy <- yy[yy$ST == st,]
                    yy <- yy[yy$OFFICE == 'US PRESIDENT',]
                    yy$PARTY_CANDIDATE <- paste0(yy$PARTY,"_",yy$CANDIDATE)
                    yy <- yy[,c("COUNTY","PARTY_CANDIDATE","VOTES","MODE")]
                    yy <- yy %>% group_by(COUNTY,PARTY_CANDIDATE) %>%
                        summarize(VOTES=sum(VOTES))
                    yy <- yy %>% spread(PARTY_CANDIDATE, VOTES)
                    yynames <- names(yy)
                    yyparty <- yynames
                    gnames <<- yynames
                    for (i in 2:NCOL(yy)){
                        yyparty[i] <- unlist(str_split(yynames[i],"_"))[1]
                        fullname   <- unlist(str_split(yynames[i],"_"))[2]
                        fullname <- gsub(" JR$","",fullname)
                        names <- unlist(str_split(fullname," "))
                        yynames[i] <- names[length(names)]
                        yynames[i] <- str_to_title(yynames[i])
                    }
                    yyparty[yyparty == "DEMOCRAT"]    <- "DEM"
                    yyparty[yyparty == "REPUBLICAN"]  <- "REP"
                    yyparty[yyparty == "GREEN"]       <- "GRN"
                    yyparty[yyparty == "LIBERTARIAN"] <- "LIB"
                    yyparty[yyparty == "OTHER"]       <- "OTH"
                    yynames[yyparty == "DEM"] <- paste0(yynames[yyparty == "DEM"],"_D")
                    yynames[yyparty == "REP"] <- paste0(yynames[yyparty == "REP"],"_R")
                    yynames[yyparty == "GRN"] <- paste0(yynames[yyparty == "GRN"],"_G")
                    yynames[yyparty == "LIB"] <- paste0(yynames[yyparty == "LIB"],"_L")
                    yynames[yyparty == "OTH"] <- paste0(yynames[yyparty == "OTH"],"_O")
                    names(yy) <- yynames
                    write(paste(yyparty, collapse = " "), paste0(data_dir,st,"_President","_",year,".csv"))
                    write_delim(yy, paste0(data_dir,st,"_President","_",year,".csv"), append = TRUE, col_names = TRUE)
                }
            }
        }
        createCA <- function(){
            xx <- read_excel(paste0(input_dir,"CA20_csv-candidates.xlsx"), sheet = "csv-candidates")
            pp <- xx[xx$'Contest Name' == "President",]
            pp$'Party Name'[is.na(pp$'Party Name')] <- pp$'Candidate Name'[is.na(pp$'Party Name')]
            pp <- pp[,c("County Name","Party Name","Vote Total")]
            names(pp)[1] <- "COUNTY"
            pp <- pp %>% spread('Party Name','Vote Total')
            namespp <- names(pp)
            namespp[namespp == 'Democratic'] <- "DEM"
            namespp[namespp == 'Republican'] <- "REP"
            namespp <- paste0('"',namespp,'"')
            write(paste(namespp, collapse = " "), paste0(data_dir,"CA_President_2020.csv"))
            names(pp)[names(pp) == 'Democratic'] <- "BIDEN"
            names(pp)[names(pp) == 'Republican'] <- "TRUMP"
            write_delim(pp, paste0(data_dir,"CA_President_2020.csv"), append = TRUE, col_names = TRUE)

            names(xx) <- c("ELECTION_DATE","ELECTION_NAME","COUNTY_ID","County Name","CONTEST_ID","Contest Name","CANDIDATE_ID","Candidate Name","INCUMBENT_FLAG","WRITE_IN_FLAG","PARTY_ID","Party Name","Vote Total")
            pp <- xx[grepl("^United States Representative District ",xx$`Contest Name`),]
            pp$'Party Name'[is.na(pp$'Party Name')] <- pp$`Candidate Name`[is.na(pp$'Party Name')]
            pp <- pp[,c("County Name","Contest Name","Party Name","Vote Total")]
            names(pp)[1] <- "COUNTY"
            names(pp)[2] <- "DISTRICT"
            pp$DISTRICT <- substring(pp$DISTRICT, 39)
            pp <- pp %>%
                group_by(COUNTY, DISTRICT, `Party Name`) %>%
                summarise(`Vote Total` = sum(`Vote Total`))
            pp <- pp %>% spread(`Party Name`,`Vote Total`)
            namespp <- names(pp)
            namespp[namespp == 'Democratic'] <- "DEM"
            namespp[namespp == 'Republican'] <- "REP"
            namespp <- paste0('"',namespp,'"')
            write(paste(namespp, collapse = " "), paste0(data_dir,"CA_House_2020.csv"))
            write_delim(pp, paste0(data_dir,"CA_House_2020.csv"), append = TRUE, col_names = TRUE)

            xx <- read_excel(paste0(input_dir,"CA18_csv-candidates.xlsx"), sheet = "Sheet1")
            names(xx) <- c("ELECTION_DATE","ELECTION_NAME","COUNTY_ID","County Name","CONTEST_ID","Contest Name","CANDIDATE_ID","Candidate Name","INCUMBENT_FLAG","WRITE_IN_FLAG","PARTY_ID","Party Name","Vote Total")
            pp <- xx[grepl("^United States Representative District ",xx$`Contest Name`),]
            pp$'Party Name'[is.na(pp$'Party Name')] <- pp$`Candidate Name`[is.na(pp$'Party Name')]
            pp <- pp[,c("County Name","Contest Name","Party Name","Vote Total")]
            names(pp)[1] <- "COUNTY"
            names(pp)[2] <- "DISTRICT"
            pp$DISTRICT <- substring(pp$DISTRICT, 39)
            pp <- pp %>%
                group_by(COUNTY, DISTRICT, `Party Name`) %>%
                summarise(`Vote Total` = sum(`Vote Total`))
            pp <- pp %>% spread(`Party Name`,`Vote Total`)
            namespp <- names(pp)
            namespp[namespp == 'Democratic'] <- "DEM"
            namespp[namespp == 'Republican'] <- "REP"
            namespp <- paste0('"',namespp,'"')
            write(paste(namespp, collapse = " "), paste0(data_dir,"CA_House_2018.csv"))
            write_delim(pp, paste0(data_dir,"CA_House_2018.csv"), append = TRUE, col_names = TRUE)

            filenamex <- paste0(input_dir,"CA_Registered00.csv")
            xxparty <- read_delim(filenamex, ' ', col_names = FALSE, n_max = 1)
            xx0 <- read_delim(filenamex, ' ', skip = 1)
            xx <- xx0[,c(1,4:12)]
            xxparty <- xxparty[c(1,4:12)]
            if (input$noparty == "Split by Ratio"){
                TotalReg <- xx$Democratic + xx$Republican
                xx$Democratic <- xx$Democratic + xx$NoParty * (xx$Democratic / TotalReg)
                xx$Republican <- xx$Republican + xx$NoParty * (xx$Republican / TotalReg)
                xx$NoParty <- 0
            }
            else if (input$noparty == "Count as Dem"){
                xx$Democratic <- xx$Democratic + xx$NoParty
                xx$NoParty <- 0
            }
            else if (input$noparty == "Count as Rep"){
                xx$Republican <- xx$Republican + xx$NoParty
                xx$NoParty <- 0
            }
            else{
                xx$Democratic <- xx$Democratic + (xx$NoParty / 2)
                xx$Republican <- xx$Republican + (xx$NoParty / 2)
                xx$NoParty <- 0
            }
            xxparty <- paste0('"',xxparty,'"')
            write(paste(xxparty, collapse = " "), paste0(data_dir,"CA_Registered_2020.csv"))
            write_delim(xx, paste0(data_dir,"CA_Registered_2020.csv"), append = TRUE, col_names = TRUE)
        }        
        createFL <- function(){
            #Data from https://results.elections.myflorida.com/Index.asp?ElectionDate=11/3/2020&DATAMODE= (Federal Offices, Download Results)?
            xx <- read_delim(paste0(input_dir,"FL_All20_tab.csv"), '\t')
            pp <- xx[xx$RaceCode == "PRE",]
            pp$PartyCode[pp$PartyCode == "WRI"] <- pp$CanNameLast[pp$PartyCode == "WRI"]
            pp <- pp[,c("CountyName","PartyCode","CanVotes")]
            names(pp)[1] <- "COUNTY"
            pp <- pp %>% spread(PartyCode,CanVotes)
            write(paste(names(pp), collapse = " "), paste0(data_dir,"FL_President_2020.csv"))
            write_delim(pp, paste0(data_dir,"FL_President_2020.csv"), append = TRUE, col_names = TRUE)

            pp <- xx[xx$RaceCode == "USR",]
            pp$PartyCode[pp$PartyCode == "WRI"] <- pp$CanNameLast[pp$PartyCode == "WRI"]
            pp <- pp[,c("CountyName","Juris1num","PartyCode","CanVotes")] # add Juris1num
            names(pp)[1] <- "COUNTY"
            names(pp)[2] <- "DISTRICT"
            pp <- pp %>% spread(PartyCode,CanVotes)
            write(paste(names(pp), collapse = " "), paste0(data_dir,"FL_House_2020.csv"))
            write_delim(pp, paste0(data_dir,"FL_House_2020.csv"), append = TRUE, col_names = TRUE)

            xx <- read_delim(paste0(input_dir,"FL18_11062018Election.csv"), '\t')
            xx2 <<- xx
            pp <- xx[xx$RaceCode == "USR",]
            pp$PartyCode[pp$PartyCode == "WRI"] <- pp$CanNameLast[pp$PartyCode == "WRI"]
            pp$PartyCode[pp$PartyCode == "NPA"] <- pp$CanNameLast[pp$PartyCode == "NPA"] #additional duplicates
            pp <- pp[,c("CountyName","Juris1num","PartyCode","CanVotes")] # add Juris1num
            names(pp)[1] <- "COUNTY"
            names(pp)[2] <- "DISTRICT"
            pp <- pp %>% spread(PartyCode,CanVotes)
            write(paste(names(pp), collapse = " "), paste0(data_dir,"FL_House_2018.csv"))
            write_delim(pp, paste0(data_dir,"FL_House_2018.csv"), append = TRUE, col_names = TRUE)
            
            pp <- xx[xx$RaceCode == "USS",]
            pp$PartyCode[pp$PartyCode == "WRI"] <- pp$CanNameLast[pp$PartyCode == "WRI"]
            #pp$PartyCode[pp$PartyCode == "NPA"] <- pp$CanNameLast[pp$PartyCode == "NPA"] #additional duplicates
            pp <- pp[,c("CountyName","PartyCode","CanVotes")] # remove Juris1num
            names(pp)[1] <- "COUNTY"
            pp <- pp %>% spread(PartyCode,CanVotes)
            write(paste(names(pp), collapse = " "), paste0(data_dir,"FL_Senate_2018.csv"))
            write_delim(pp, paste0(data_dir,"FL_Senate_2018.csv"), append = TRUE, col_names = TRUE)
            
            pp <- xx[xx$RaceCode == "GOV",]
            pp$PartyCode[pp$PartyCode == "WRI"] <- pp$CanNameLast[pp$PartyCode == "WRI"]
            pp$PartyCode[pp$PartyCode == "NPA"] <- pp$CanNameLast[pp$PartyCode == "NPA"] #additional duplicates
            pp$PartyCode[pp$PartyCode == "REF"] <- pp$CanNameLast[pp$PartyCode == "REF"] #additional duplicates
            pp <- pp[,c("CountyName","PartyCode","CanVotes")] # remove Juris1num
            names(pp)[1] <- "COUNTY"
            pp <- pp %>% spread(PartyCode,CanVotes)
            write(paste(names(pp), collapse = " "), paste0(data_dir,"FL_Governor_2018.csv"))
            write_delim(pp, paste0(data_dir,"FL_Governor_2018.csv"), append = TRUE, col_names = TRUE)
            
            xx <- read_delim(paste0(input_dir,"FL16_11082016Election.csv"), '\t')
            xx2 <<- xx
            pp <- xx[xx$RaceCode == "PRE",]
            pp$PartyCode[pp$PartyCode == "WRI"] <- pp$CanNameLast[pp$PartyCode == "WRI"]
            #pp$PartyCode[pp$PartyCode == "NPA"] <- pp$CanNameLast[pp$PartyCode == "NPA"] #additional duplicates
            pp <- pp[,c("CountyName","PartyCode","CanVotes")]
            names(pp)[1] <- "COUNTY"
            pp <- pp %>% spread(PartyCode,CanVotes)
            write(paste(names(pp), collapse = " "), paste0(data_dir,"FL_President_2016.csv"))
            write_delim(pp, paste0(data_dir,"FL_President_2016.csv"), append = TRUE, col_names = TRUE)
        }
        createFL22Governor <- function(){
            filenamex <- paste0(input_dir,"Governor_FL_221108.csv")
            xxparty <- read_delim(filenamex, '\t', col_names = FALSE, n_max = 1)
            xx1 <- read_delim(filenamex, '\t', skip = 1)
            xx <- as.data.frame(xx1)
            ###xx <- xx[,c(1,4,6,15)]
            ###names(xx) <- c("COUNTY","Abbott","O'Rourke","Other")
            ###xxparty <- c("COUNTY","REP","DEM","OTHER")
            xx$COUNTY <- str_to_title(xx$COUNTY)
            ###xx <- xx[!is.na(xx$Abbott),]
            xx <- xx[xx$COUNTY != "Total",]
            ###xx$Other <- xx$Other - xx$Abbott - xx$`O'Rourke`
            write(paste(xxparty, collapse = " "), paste0(data_dir,"FL_Governor_2022.csv"))
            write_delim(xx, paste0(data_dir,"FL_Governor_2022.csv"), append = TRUE, col_names = TRUE)
        }
        createGA <- function(){
            xx0 <- read_excel(paste0(input_dir,"GA20_detail.xlsx"), sheet = "1", skip = 2)
            xx <- xx0[,c(1,6,11,16)]
            names(xx) <- c("COUNTY","Trump","Biden","Jorgensen")
            xxparty <- c("COUNTY","REP","DEM","LIB")
            write(paste(xxparty, collapse = " "), paste0(data_dir,"GA_President_2020.csv"))
            write_delim(xx, paste0(data_dir,"GA_President_2020.csv"), append = TRUE, col_names = TRUE)

            xx0 <- read_excel(paste0(input_dir,"GA20_detail.xlsx"), sheet = "2", skip = 2)
            xx <- xx0[,c(1,6,11,16)]
            names(xx) <- c("COUNTY","Perdue","Ossoff","Hazel")
            xxparty <- c("COUNTY","REP","DEM","LIB")
            write(paste(xxparty, collapse = " "), paste0(data_dir,"GA_Senate_2020.csv"))
            write_delim(xx, paste0(data_dir,"GA_Senate_2020.csv"), append = TRUE, col_names = TRUE)
        }
        createIA <- function(){
            filenamex <- paste0(input_dir,"IA_President00.csv")
            xxparty <- read_delim(filenamex, ' ', col_names = FALSE, n_max = 1)
            xx0 <- read_delim(filenamex, ' ', skip = 1)
            xx <- xx0[,1:11]
            xxparty <- xxparty[1:11]
            xxed <- xx[xx$TYPE == "ElectionDay",-2]
            xxab <- xx[xx$TYPE == "Absentee",-2]
            xxto <- xx[xx$TYPE == "Total",-2]
            xxparty <- xxparty[-2]
            xxto$COUNTY <- xxed$COUNTY
            write(paste(xxparty, collapse = " "), paste0(data_dir,"IA_President_2020.csv"))
            write_delim(xxto, paste0(data_dir,"IA_President_2020.csv"), append = TRUE, col_names = TRUE)

            filenamex <- paste0(input_dir,"IA_Senate00.csv")
            xxparty <- read_delim(filenamex, ' ', col_names = FALSE, n_max = 1)
            xx0 <- read_delim(filenamex, ' ', skip = 1)
            xx <- xx0[,1:7]
            xxparty <- xxparty[1:7]
            xxed <- xx[xx$TYPE == "ElectionDay",-2]
            xxab <- xx[xx$TYPE == "Absentee",-2]
            xxto <- xx[xx$TYPE == "Total",-2]
            xxparty <- xxparty[-2]
            xxto$COUNTY <- xxed$COUNTY
            write(paste(xxparty, collapse = " "), paste0(data_dir,"IA_Senate_2020.csv"))
            write_delim(xxto, paste0(data_dir,"IA_Senate_2020.csv"), append = TRUE, col_names = TRUE)

            filenamey <- paste0(data_dir,"IA_House_2020.csv")
            for (i in 1:4){
                filenamex <- paste0(input_dir,"IA_House",i,"_00.csv")
                xxparty <- read_delim(filenamex, ' ', col_names = FALSE, n_max = 1)
                xx0 <- read_delim(filenamex, ' ', skip = 1)
                xx <- xx0[,1:5]
                xxparty <- xxparty[1:5]
                xxed <- xx[xx$TYPE == "ElectionDay",-2]
                xxab <- xx[xx$TYPE == "Absentee",-2]
                xxto <- xx[xx$TYPE == "Total",-2]
                xxparty <- xxparty[-2]
                xxto$COUNTY <- xxed$COUNTY
                xxto$DISTRICT <- i
                xxout <- xxto[,c(1, NCOL(xxto), 2:(NCOL(xxto)-1))]
                xxparty$X6 <- "DISTRICT"
                xxparty <- xxparty[c(1,NCOL(xxparty),2:(NCOL(xxparty)-1))]
                if (i == 1){
                    write(paste(xxparty, collapse = " "), filenamey)
                    write_delim(xxout, filenamey, append = TRUE, col_names = TRUE)
                }
                else{
                    write_delim(xxout, filenamey, append = TRUE, col_names = FALSE)
                }
            }
            
            filenamex <- paste0(input_dir,"IA_Registered00.csv")
            xxparty <- read_delim(filenamex, ' ', col_names = FALSE, n_max = 1)
            xx0 <- read_delim(filenamex, ' ', skip = 1)
            xx <- xx0[,1:5]
            xxparty <- xxparty[1:5]
            if (input$noparty == "Split by Ratio"){
                TotalReg <- xx$DemActive + xx$RepActive
                xx$DemActive <- xx$DemActive + xx$NoPartyAct * (xx$DemActive / TotalReg)
                xx$RepActive <- xx$RepActive + xx$NoPartyAct * (xx$RepActive / TotalReg)
                xx$NoPartyAct <- 0
            }
            else if (input$noparty == "Count as Dem"){
                xx$DemActive <- xx$DemActive + xx$NoPartyAct
                xx$NoPartyAct <- 0
            }
            else if (input$noparty == "Count as Rep"){
                xx$RepActive <- xx$RepActive + xx$NoPartyAct
                xx$NoPartyAct <- 0
            }
            else{
                xx$DemActive <- xx$DemActive + (xx$NoPartyAct / 2)
                xx$RepActive <- xx$RepActive + (xx$NoPartyAct / 2)
                xx$NoPartyAct <- 0
            }
            write(paste(xxparty, collapse = " "), paste0(data_dir,"IA_Registered_2020.csv"))
            write_delim(xx, paste0(data_dir,"IA_Registered_2020.csv"), append = TRUE, col_names = TRUE)
        }
        createME <- function(){
            xx <- read_excel(paste0(input_dir,"ME20_presandvisecnty1120.xlsx"), sheet = "Statewide", skip = 2)
            names(xx) <- c("CTY","COUNTY","BIDEN","De La Fuente","Hawkins","Jorgensen","TRUMP","Others","Blank","TBC")
            xx <- xx[grepl(" Total", xx$COUNTY) | grepl(" UOCAVA", xx$COUNTY),]
            xx <- xx[,2:(NCOL(xx)-2)] # delete Blank as well
            xx$COUNTY <- gsub(" Totals", "", xx$COUNTY)
            xx$COUNTY <- gsub(" Total", "", xx$COUNTY)
            xx$COUNTY <- gsub("STATE UOCAVA", "UOCACA", xx$COUNTY)
            xx <- xx[xx$COUNTY != "Statewide",]
            namesxx <- names(xx)
            namesxx[2:6] <- c("DEM","ALL","GRN","LIB","REP")
            write(paste(namesxx, collapse = " "), paste0(data_dir,"ME_President_2020.csv"))
            write_delim(xx, paste0(data_dir,"ME_President_2020.csv"), append = TRUE, col_names = TRUE)

            xx <- read_excel(paste0(input_dir,"ME20_ussenator1120.xlsx"), sheet = "US Senator", skip = 2)
            names(xx) <- c("CTY","COUNTY","COLLINS","GIDEON","Linn","Savage","Others","Blank","TBC")
            xx <- xx[grepl(" Total", xx$COUNTY) | grepl(" UOCAVA", xx$COUNTY),]
            xx <- xx[,2:(NCOL(xx)-2)] # delete Blank as well
            xx$COUNTY <- gsub(" Totals", "", xx$COUNTY)
            xx$COUNTY <- gsub(" Total", "", xx$COUNTY)
            xx$COUNTY <- gsub("STATE UOCAVA", "UOCACA", xx$COUNTY)
            xx <- xx[xx$COUNTY != "State",]
            namesxx <- names(xx)
            namesxx[2:5] <- c("REP","DEM","IND1","IND2")
            write(paste(namesxx, collapse = " "), paste0(data_dir,"ME_Senate_2020.csv"))
            write_delim(xx, paste0(data_dir,"ME_Senate_2020.csv"), append = TRUE, col_names = TRUE)
            
            xx <- read_excel(paste0(input_dir,"repcongress1120.xlsx"), sheet = "Dist 1", skip = 2)
            names(xx) <- c("DISTRICT","CTY","COUNTY","REP","DEM","Others","Blank","TBC")
            xx$DISTRICT <- 1
            xx <- xx[grepl(" Total", xx$COUNTY, ignore.case = TRUE) |
                     grepl(" UOCAVA", xx$COUNTY, ignore.case = TRUE),]
            xx <- xx[,c(3,1,4:(NCOL(xx)-2))] # delete Blank as well
            xx1 <- xx[xx$COUNTY != "District 1 Total",]
            
            xx <- read_excel(paste0(input_dir,"repcongress1120.xlsx"), sheet = "Dist 2", skip = 2)
            names(xx) <- c("DISTRICT","CTY","COUNTY","REP","DEM","Others","Blank","TBC")
            xx$DISTRICT <- 2
            xx <- xx[grepl(" Total", xx$COUNTY) | grepl(" UOCAVA", xx$COUNTY),]
            xx <- xx[,c(3,1,4:(NCOL(xx)-2))] # delete Blank as well
            xx <- xx[xx$COUNTY != "District 2 Total",]

            xx <- rbind(xx1,xx)
            xx$COUNTY <- gsub(" Totals", "", xx$COUNTY, ignore.case = TRUE)
            xx$COUNTY <- gsub(" Total", "", xx$COUNTY, ignore.case = TRUE)
            xx$COUNTY <- gsub("STATE UOCAVA", "UOCACA", xx$COUNTY, ignore.case = TRUE)
            namesxx <- names(xx)
            write(paste(namesxx, collapse = " "), paste0(data_dir,"ME_House_2020.csv"))
            write_delim(xx, paste0(data_dir,"ME_House_2020.csv"), append = TRUE, col_names = TRUE)
        }
        createMI <- function(){
            xx0 <- read_excel(paste0(input_dir,"MI20_2020GEN_MI_CENR_BY_COUNTY.xlsx"), sheet = "2020GEN_MI_CENR_BY_COUNTY")
            xx <- xx0[xx0$OfficeDescription == "President of the United States 4 Year Term (1) Position",]
            xx <- xx[,c("CountyName","CandidateLastName","CandidateVotes")]
            names(xx) <- c("COUNTY", "Candidate", "Votes")
            xx <- xx %>% spread("Candidate", "Votes")
            xx <- xx[!is.na(row.names),]
            xx <- xx[!is.na(xx$COUNTY), names(xx) != "<NA>"]
            xxparty <- names(xx)
            xxparty[xxparty == 'Biden'] <- "DEM"
            xxparty[xxparty == 'Trump'] <- "REP"
            xxparty <- paste0('"',xxparty,'"')
            write(paste(xxparty, collapse = " "), paste0(data_dir,"MI_President_2020.csv"))
            write_delim(xx, paste0(data_dir,"MI_President_2020.csv"), append = TRUE, col_names = TRUE)

            xx <- xx0[xx0$OfficeDescription == "United States Senator 6 Year Term (1) Position",]
            xx <- xx[,c("CountyName","CandidateLastName","CandidateVotes")]
            names(xx) <- c("COUNTY", "Candidate", "Votes")
            xx <- xx %>% spread("Candidate", "Votes")
            xx <- xx[!is.na(row.names),]
            xx <- xx[!is.na(xx$COUNTY), names(xx) != "<NA>"]
            xxparty <- names(xx)
            xxparty[xxparty == 'Peters'] <- "DEM"
            xxparty[xxparty == 'James'] <- "REP"
            xxparty <- paste0('"',xxparty,'"')
            write(paste(xxparty, collapse = " "), paste0(data_dir,"MI_Senate_2020.csv"))
            write_delim(xx, paste0(data_dir,"MI_Senate_2020.csv"), append = TRUE, col_names = TRUE)
        }
        createMN <- function(){
            xx0 <- read_excel(paste0(input_dir,"MN20_2020-general-federal-state-results-by-precinct-official.xlsx"),
                              sheet = "Precinct-Results")
            xx <- xx0[,c("PCTNAME","COUNTYNAME","USPRSDFL","USPRSR","USPRSIA","USPRSGP",
                         "USPRSINDKW","USPRSINDBP","USPRSSLP","USPRSSWP","USPRSLIB","USPRSWI")]
            xx <- xx %>%
                group_by(COUNTYNAME) %>%
                summarise(USPRSDFL = sum(USPRSDFL),USPRSR = sum(USPRSR),USPRSIA = sum(USPRSIA),
                          USPRSGP = sum(USPRSGP),USPRSINDKW = sum(USPRSINDKW),USPRSINDBP = sum(USPRSINDBP),
                          USPRSSLP = sum(USPRSSLP),USPRSSWP = sum(USPRSSWP),USPRSLIB = sum(USPRSLIB),
                          USPRSWI = sum(USPRSWI))
            names(xx) <- c("COUNTY","Biden","Trump","IndAll","Green","West",
                           "Pierce","SocLib","SocWrk","Lib","WriteIn")
            partyxx <- names(xx)
            partyxx[2:3] <- c("DEM","REP")
            write(paste(partyxx, collapse = " "), paste0(data_dir,"MN_President_2020.csv"))
            write_delim(xx, paste0(data_dir,"MN_President_2020.csv"), append = TRUE, col_names = TRUE)
            
            xx <- xx0[,c("PCTNAME","COUNTYNAME","USSENDFL","USSENR","USSENLMN","USSENGLC","USSENWI","USREPTOTAL")]
            xx <- xx %>%
                group_by(COUNTYNAME) %>%
                summarise(USSENDFL = sum(USSENDFL),USSENR = sum(USSENR),USSENLMN = sum(USSENLMN),
                          USSENGLC = sum(USSENGLC),USSENWI = sum(USSENWI))
            names(xx) <- c("COUNTY","DEM","REP","LegMar","LegCan","WriteIn")
            partyxx <- names(xx)
            write(paste(partyxx, collapse = " "), paste0(data_dir,"MN_Senate_2020.csv"))
            write_delim(xx, paste0(data_dir,"MN_Senate_2020.csv"), append = TRUE, col_names = TRUE)
            
            xx <- xx0[,c("PCTNAME","COUNTYNAME","CONGDIST","USREPDFL","USREPR","USREPLMN","USREPGLC","USREPWI","USREPTOTAL")]
            xx <- xx %>%
                group_by(COUNTYNAME, CONGDIST) %>%
                summarise(USREPDFL = sum(USREPDFL),USREPR = sum(USREPR),USREPLMN = sum(USREPLMN),
                          USREPGLC = sum(USREPGLC),USREPWI = sum(USREPWI))
            names(xx) <- c("COUNTY","DISTRICT","DEM","REP","LegMar","LegCan","WriteIn")
            partyxx <- names(xx)
            write(paste(partyxx, collapse = " "), paste0(data_dir,"MN_House_2020.csv"))
            write_delim(xx, paste0(data_dir,"MN_House_2020.csv"), append = TRUE, col_names = TRUE)
        }
        createNC <- function(){
            xx <- read_delim(paste0(input_dir,"NC20_results_pct_20201103.csv"), '\t')
            pp <- xx[xx$`Contest Name` == "US PRESIDENT",]
            pp$`Choice Party`[is.na(pp$`Choice Party`)] <- "NA"
            pp <- pp[,c("County","Choice Party","Total Votes")]
            names(pp)[1] <- "COUNTY"
            pp <- pp %>%
                group_by(COUNTY,`Choice Party`) %>%
                summarise(`Total Votes` = sum(`Total Votes`, na.rm = TRUE))
            pp <- pp %>% spread(`Choice Party`,`Total Votes`)
            write(paste(names(pp), collapse = " "), paste0(data_dir,"NC_President_2020.csv"))
            write_delim(pp, paste0(data_dir,"NC_President_2020.csv"), append = TRUE, col_names = TRUE)

            pp <- xx[xx$`Contest Name` == "US SENATE",]
            pp$`Choice Party`[is.na(pp$`Choice Party`)] <- "NA"
            pp <- pp[,c("County","Choice Party","Total Votes")]
            names(pp)[1] <- "COUNTY"
            pp <- pp %>%
                group_by(COUNTY,`Choice Party`) %>%
                summarise(`Total Votes` = sum(`Total Votes`, na.rm = TRUE))
            pp <- pp %>% spread(`Choice Party`,`Total Votes`)
            write(paste(names(pp), collapse = " "), paste0(data_dir,"NC_Senate_2020.csv"))
            write_delim(pp, paste0(data_dir,"NC_Senate_2020.csv"), append = TRUE, col_names = TRUE)

            pp <- xx[grepl("^US HOUSE OF REPRESENTATIVES DISTRICT ",xx$`Contest Name`),]
            pp <- pp[,c("County","Contest Name","Choice Party","Total Votes")]
            names(pp) <- c("COUNTY","DISTRICT","PARTY","VOTES")
            pp$VOTES <- as.numeric(gsub(",","",pp$VOTES))
            pp$DISTRICT <- substring(pp$DISTRICT, 37)
            pp <- pp %>%
                group_by(COUNTY, DISTRICT, PARTY) %>%
                summarise(VOTES = sum(VOTES))
            pp <- pp %>% spread(PARTY, VOTES)
            namespp <- names(pp)
            write(paste(namespp, collapse = " "), paste0(data_dir,"NC_House_2020.csv"))
            write_delim(pp, paste0(data_dir,"NC_House_2020.csv"), append = TRUE, col_names = TRUE)
        }
        createPA <- function(){
            xx0 <- read_csv(paste0(input_dir,"PA20_Official_1152021032923PM.CSV"))
            xx <- xx0[xx0$`Office Name` == "President of the United States",]
            xx$Pty_Candidate <- paste(xx$`Party Name`, word(xx$`Candidate Name`,1))
            gsub(",","",xx$Pty_Candidate)
            xx <- xx[,c("County Name","Pty_Candidate","Votes")]
            names(xx) <- c("COUNTY", "Candidate", "Votes")
            xx <- xx %>% spread("Candidate", "Votes")
            xx <- xx[!is.na(row.names),]
            xx <- xx[!is.na(xx$COUNTY), names(xx) != "<NA>"]
            xxparty <- word(names(xx), 1)
            xxparty[xxparty == "Democratic"] <- "DEM"
            xxparty[xxparty == "Republican"] <- "REP"
            names(xx)[xxparty == "DEM"] <- "BIDEN"
            names(xx)[xxparty == "REP"] <- "TRUMP"
            write(paste(xxparty, collapse = " "), paste0(data_dir,"PA_President_2020.csv"))
            write_delim(xx, paste0(data_dir,"PA_President_2020.csv"), append = TRUE, col_names = TRUE)

            rr0 <- read_excel(paste0(input_dir,"PA201116_currentvotestats.xls"), sheet = "Reg Voter", skip = 1)
            xx <- rr0[,c(1,3,5,7,9)]
            names(xx) <- c("COUNTY","Democratic","Republican","NoParty","Other")
            xxparty <- c("COUNTY","DEM","REP","NOP","OTH")
            xx <- xx[xx$COUNTY != "Totals:",]
            if (input$noparty == "Split by Ratio"){
                TotalReg <- xx$Democratic + xx$Republican
                xx$Democratic <- xx$Democratic + xx$NoParty * (xx$Democratic / TotalReg)
                xx$Republican <- xx$Republican + xx$NoParty * (xx$Republican / TotalReg)
                xx$NoParty <- 0
            }
            else if (input$noparty == "Count as Dem"){
                xx$Democratic <- xx$Democratic + xx$NoParty
                xx$NoParty <- 0
            }
            else if (input$noparty == "Count as Rep"){
                xx$Republican <- xx$Republican + xx$NoParty
                xx$NoParty <- 0
            }
            else{
                xx$Democratic <- xx$Democratic + (xx$NoParty / 2)
                xx$Republican <- xx$Republican + (xx$NoParty / 2)
                xx$NoParty <- 0
            }
            write(paste(xxparty, collapse = " "), paste0(data_dir,"PA_Registered_2020.csv"))
            write_delim(xx, paste0(data_dir,"PA_Registered_2020.csv"), append = TRUE, col_names = TRUE)
        }
        createSC <- function(){
            xx <- read_excel(paste0(input_dir,"SC20_detail.xlsx"), sheet = "2", skip = 2)
            xx <- xx[,c(1,9,16,23,30,37)]
            names(xx) <- c("COUNTY","Hawkins","TRUMP","De La Fuente","Jorgensen","BIDEN")
            xx <- xx[xx$COUNTY != "Total:",]
            namesxx <- c("COUNTY","GRN","REP","ALN","LIB","DEM")
            write(paste(namesxx, collapse = " "), paste0(data_dir,"SC_President_2020.csv"))
            write_delim(xx, paste0(data_dir,"SC_President_2020.csv"), append = TRUE, col_names = TRUE)

            xx <- read_excel(paste0(input_dir,"SC20_detail.xlsx"), sheet = "3", skip = 2)
            xx <- xx[,c(1,9,16,23,30)]
            names(xx) <- c("COUNTY","Bledsoe","GRAHAM","HARRISON","Write-In")
            xx <- xx[xx$COUNTY != "Total:",]
            namesxx <- c("COUNTY","CON","REP","DEM","WRI")
            write(paste(namesxx, collapse = " "), paste0(data_dir,"SC_Senate_2020.csv"))
            write_delim(xx, paste0(data_dir,"SC_Senate_2020.csv"), append = TRUE, col_names = TRUE)

            xx <- read_excel(paste0(input_dir,"SC20_detail.xlsx"), sheet = "4", skip = 2)
            xx <- xx[,c(1,2,9,16,22,23)]
            names(xx) <- c("COUNTY","DISTRICT","REP","DEM","CON","WRI")
            xx <- xx[xx$COUNTY != "Total:",]
            xx$DISTRICT <- 1
            xx$CON <- 0
            yy <- xx
            
            xx <- read_excel(paste0(input_dir,"SC20_detail.xlsx"), sheet = "5", skip = 2)
            xx <- xx[,c(1,2,16,23,9,30)]
            names(xx) <- c("COUNTY","DISTRICT","REP","DEM","CON","WRI")
            xx <- xx[xx$COUNTY != "Total:",]
            xx$DISTRICT <- 2
            yy <- rbind(yy, xx)
            
            xx <- read_excel(paste0(input_dir,"SC20_detail.xlsx"), sheet = "6", skip = 2)
            xx <- xx[,c(1,2,9,16,22,23)]
            names(xx) <- c("COUNTY","DISTRICT","REP","DEM","CON","WRI")
            xx <- xx[xx$COUNTY != "Total:",]
            xx$DISTRICT <- 3
            xx$CON <- 0
            yy <- rbind(yy, xx)
            
            xx <- read_excel(paste0(input_dir,"SC20_detail.xlsx"), sheet = "7", skip = 2)
            xx <- xx[,c(1,2,16,23,9,30)]
            names(xx) <- c("COUNTY","DISTRICT","REP","DEM","CON","WRI")
            xx <- xx[xx$COUNTY != "Total:",]
            xx$DISTRICT <- 4
            yy <- rbind(yy, xx)
            
            xx <- read_excel(paste0(input_dir,"SC20_detail.xlsx"), sheet = "8", skip = 2)
            xx <- xx[,c(1,2,9,16,22,23)]
            names(xx) <- c("COUNTY","DISTRICT","REP","DEM","CON","WRI")
            xx <- xx[xx$COUNTY != "Total:",]
            xx$DISTRICT <- 5
            xx$CON <- 0
            yy <- rbind(yy, xx)
            
            xx <- read_excel(paste0(input_dir,"SC20_detail.xlsx"), sheet = "9", skip = 2)
            xx <- xx[,c(1,2,16,23,9,30)]
            names(xx) <- c("COUNTY","DISTRICT","REP","DEM","CON","WRI")
            xx <- xx[xx$COUNTY != "Total:",]
            xx$DISTRICT <- 6
            yy <- rbind(yy, xx)
            
            xx <- read_excel(paste0(input_dir,"SC20_detail.xlsx"), sheet = "10", skip = 2)
            xx <- xx[,c(1,2,9,16,22,23)]
            names(xx) <- c("COUNTY","DISTRICT","REP","DEM","CON","WRI")
            xx <- xx[xx$COUNTY != "Total:",]
            xx$DISTRICT <- 7
            xx$CON <- 0
            yy <- rbind(yy, xx)
            write(paste(names(yy), collapse = " "), paste0(data_dir,"SC_House_2020.csv"))
            write_delim(yy, paste0(data_dir,"SC_House_2020.csv"), append = TRUE, col_names = TRUE)
        }
        createTX <- function(){
            xx <- read_excel(paste0(input_dir,"TX20_CountybyCountyCanvassReport.xlsx"), sheet = "Voter Turnout Report")
            #"ELECTION DATE-NAME,OFFICE NAME,CANDIDATE NAME,COUNTY NAME,TOTAL VOTES PER OFFICE PER COUNTY"
            pp <- xx[xx$`OFFICE NAME` == "PRESIDENT/VICE-PRESIDENT",]
            pp$Party <- "OTH"
            pp$Party[pp$`CANDIDATE NAME` == "JOSEPH R. BIDEN/KAMALA D. HARRIS"] <- "DEM"
            pp$Party[pp$`CANDIDATE NAME` == "DONALD J. TRUMP/MICHAEL R. PENCE"] <- "REP"
            pp <- pp[,c("COUNTY NAME","Party","TOTAL VOTES PER OFFICE PER COUNTY")]
            names(pp) <- c("COUNTY", "Party", "Votes")
            names(pp)[1] <- "COUNTY"
            pp <- pp %>%
                group_by(COUNTY, Party) %>%
                summarise(Votes = sum(as.numeric(gsub(",","",Votes)), na.rm = TRUE))
            pp <- pp %>% spread(Party, Votes)
            namespp <- names(pp)
            pp <- pp %>% rename("Biden" = "DEM", "Trump" = "REP")
            write(paste(namespp, collapse = " "), paste0(data_dir,"TX_President_2020_old.csv"))
            write_delim(pp, paste0(data_dir,"TX_President_2020_old.csv"), append = TRUE, col_names = TRUE)

            pp <- xx[xx$`OFFICE NAME` == "U. S.  SENATOR",]
            pp$Party <- "OTH"
            pp$Party[pp$`CANDIDATE NAME` == "MARY \"MJ\" HEGAR"] <- "DEM"
            pp$Party[pp$`CANDIDATE NAME` == "JOHN CORNYN"] <- "REP"
            pp <- pp[,c("COUNTY NAME","Party","TOTAL VOTES PER OFFICE PER COUNTY")]
            names(pp) <- c("COUNTY", "Party", "Votes")
            names(pp)[1] <- "COUNTY"
            pp <- pp %>%
                group_by(COUNTY, Party) %>%
                summarise(Votes = sum(as.numeric(gsub(",","",Votes)), na.rm = TRUE))
            pp <- pp %>% spread(Party, Votes)
            namespp <- names(pp)
            pp <- pp %>% rename("Hegar" = "DEM", "Cornyn" = "REP")
            write(paste(namespp, collapse = " "), paste0(data_dir,"TX_Senate_2020.csv"))
            write_delim(pp, paste0(data_dir,"TX_Senate_2020.csv"), append = TRUE, col_names = TRUE)

            zz <- read_excel(paste0(input_dir,"TX20_OfficialCanvassReport.xlsx"), sheet = "Official Canvass Report")
            zz <- zz[,c("OFFICE NAME","CANDIDATE NAME","PARTY"),]
            zz$`CANDIDATE NAME` <- gsub(" \\(I\\)","",zz$`CANDIDATE NAME`)
            pp <- xx[grepl("^U. S. REPRESENTATIVE DISTRICT ",xx$`OFFICE NAME`),]
            pp <- merge(pp, zz, by=c("OFFICE NAME","CANDIDATE NAME"))
            pp <- pp[,c("COUNTY NAME","OFFICE NAME","PARTY","TOTAL VOTES PER OFFICE PER COUNTY")]
            names(pp) <- c("COUNTY","DISTRICT","PARTY","VOTES")
            pp$VOTES <- as.numeric(gsub(",","",pp$VOTES))
            pp$DISTRICT <- trimws(substring(pp$DISTRICT, 30))
            pp <- pp %>%
                group_by(COUNTY, DISTRICT, PARTY) %>%
                summarise(VOTES = sum(VOTES))
            pp <- pp %>% spread(PARTY, VOTES)
            namespp <- names(pp)
            write(paste(namespp, collapse = " "), paste0(data_dir,"TX_House_2020.csv"))
            write_delim(pp, paste0(data_dir,"TX_House_2020.csv"), append = TRUE, col_names = TRUE)

            pp <- read_delim(paste0(input_dir,"TX21_House34.csv"), '\t')
            pp <- pp[,c(1,3,4,6,8,10)]
            pp <- pp[c(4:NROW(pp)),]
            names(pp)[1:2] <- c("COUNTY","DISTRICT")
            pp$DISTRICT <- 34
            namespp <- names(pp)
            write(paste(namespp, collapse = " "), paste0(data_dir,"TX_House_2021.csv"))
            write_delim(pp, paste0(data_dir,"TX_House_2021.csv"), append = TRUE, col_names = TRUE)
            
            filenamex <- paste0(input_dir,"TX_Senate_2018_00.csv")
            xxparty <- read_delim(filenamex, '\t', col_names = FALSE, n_max = 1)
            xx <- read_delim(filenamex, '\t', skip = 1)
            #"County,REP,DEM,LIB,Votes,Voters,TurnOut"
            xx <- xx[,1:4]
            xxparty <- xxparty[1:4]
            write(paste(xxparty, collapse = " "), paste0(data_dir,"TX_Senate_2018.csv"))
            write_delim(xx, paste0(data_dir,"TX_Senate_2018.csv"), append = TRUE, col_names = TRUE)

            filenamex <- paste0(input_dir,"TX_Governor_2018_00.csv")
            xxparty <- read_delim(filenamex, '\t', col_names = FALSE, n_max = 1)
            xx <- read_delim(filenamex, '\t', skip = 1)
            #"County,REP,DEM,LIB,Votes,Voters,TurnOut"
            xx <- xx[,1:4]
            xxparty <- xxparty[1:4]
            write(paste(xxparty, collapse = " "), paste0(data_dir,"TX_Governor_2018.csv"))
            write_delim(xx, paste0(data_dir,"TX_Governor_2018.csv"), append = TRUE, col_names = TRUE)
            
            filenamex <- paste0(input_dir,"TX_President_2016_00.csv")
            xxparty <- read_delim(filenamex, '\t', col_names = FALSE, n_max = 1)
            xx <- read_delim(filenamex, '\t', skip = 1)
            #"County,REP,DEM,LIB,...,Votes,Voters,TurnOut"
            xx <- xx[,1:(NCOL(xx)-3)]
            xxparty <- xxparty[1:(NCOL(xxparty)-3)]
            write(paste(xxparty, collapse = " "), paste0(data_dir,"TX_President_2016.csv"))
            write_delim(xx, paste0(data_dir,"TX_President_2016.csv"), append = TRUE, col_names = TRUE)

            filenamex <- paste0(input_dir,"TX20_Registered00.csv")
            xxparty <- read_delim(filenamex, '\t', col_names = FALSE, n_max = 1)
            xx0 <- read_delim(filenamex, '\t', skip = 1)
            xx <- xx0[,c(1,4:5)]
            xx <- xx[xx$COUNTY != "Statewide Total",]
            xxparty <- xxparty[c(1,4:5)]
            write(paste(xxparty, collapse = " "), paste0(data_dir,"TX_Registered_2020.csv"))
            write_delim(xx, paste0(data_dir,"TX_Registered_2020.csv"), append = TRUE, col_names = TRUE)
        }
        createTX22Governor <- function(){
            filenamex <- paste0(input_dir,"Governor_TX_sos_221128.csv")
            xxnames <- read_delim(filenamex, '\t', col_names = FALSE, n_max = 1)
            xx1 <- read_delim(filenamex, '\t', skip = 1)
            xx <- as.data.frame(xx1)
            xx <- xx[,c(1,4,6,15)]
            names(xx) <- c("COUNTY","Abbott","O'Rourke","Other")
            xxparty <- c("COUNTY","REP","DEM","OTHER")
            xx$COUNTY <- str_to_title(xx$COUNTY)
            xx <- xx[!is.na(xx$Abbott),]
            xx <- xx[xx$COUNTY != "All Counties",]
            xx$Other <- xx$Other - xx$Abbott - xx$`O'Rourke`
            write(paste(xxparty, collapse = " "), paste0(data_dir,"TX_Governor_2022.csv"))
            write_delim(xx, paste0(data_dir,"TX_Governor_2022.csv"), append = TRUE, col_names = TRUE)
        }
        createWI <- function(){
            xx0 <- read_excel(paste0(input_dir,"WI_County by County Report all offices.xlsx"), sheet = "Sheet2", skip = 6)
            xx <- xx0[,c(-2,-12,-13)] # delete NA column
            nerrs <- 0
            for (i in 1:NROW(xx))
            {
                tot <- sum(xx[i,3:NCOL(xx)])
                if (tot != xx[i,2]){
                    nerrs <- nerrs + 1
                }
            }
            nerrs <<- nerrs
            xx <- xx[,-2] # delete total column
            partyxx <- c("COUNTY","DEM","REP","CON","IND1","IND2","IND3","IND4","IND5","IND6","IND7","IND8","IND9","IND10")
            names(xx) <- c("COUNTY","Biden","Trump","CON","IND1","IND2","IND3","IND4","IND5","IND6","IND7","IND8","IND9","IND10")
            write(paste(partyxx, collapse = " "), paste0(data_dir,"WI_President_2020.csv"))
            write_delim(xx, paste0(data_dir,"WI_President_2020.csv"), append = TRUE, col_names = TRUE)

            yy <- data.frame(COUNTY=character(),DEM=integer(),REP=integer(),OTH=integer())
            for (i in 1:8){
                sheet <- paste0("Sheet",i+2)
                xx0 <- read_excel(paste0(input_dir,"WI_County by County Report all offices.xlsx"), sheet = sheet, skip = 5)
                xx <- xx0[,c(1,3,4,5)]
                names(xx) <- c("COUNTY","TOT","DEM","REP")
                xx$OTH <- xx$TOT - xx$DEM - xx$REP
                xx$TOT <- i
                xx <- xx[xx$COUNTY != "Office Totals:",]
                yy <- rbind(yy,xx)
            }
            partyxx <- c("COUNTY","DISTRICT","DEM","REP","OTH")
            names(xx) <- partyxx
            write(paste(partyxx, collapse = " "), paste0(data_dir,"WI_House_2020.csv"))
            write_delim(yy, paste0(data_dir,"WI_House_2020.csv"), append = TRUE, col_names = TRUE)
        }
        getlabels <- function(type){
            if (input$measure == "Percent change"){
                tshift <- "% Change in"
            }
            else if (input$measure == "Percent ratio"){
                tshift <- "% Ratio of"
            }
            else{
                tshift <- "Shift in"
            }
            if (input$units == "Percent"){
                tunits <- "Vote Share"
            }
            else{
                tunits <- "Votes"
            }
            if (type == "map"){
                tnote <- input$titlenote
            }
            else if (input$party == "Margin"){
                if (input$titlenote == ""){
                    tnote <- "(positive direction is more Democratic)"
                }
                else{
                    tnote <- input$titlenote
                }
            }
            else{
                tnote <- input$titlenote
            }
            if (input$district == ""){
                tstate2 <- input$state2
            }
            else{
                tstate2 <- paste0(input$state2,"-",input$district)
            }
            racex <- paste0(input$racex,"_",input$yearx)
            racey <- paste0(input$racey,"_",input$yeary)
            if (input$racey == "Registered"){
                title <- paste(tshift, input$party, "Voters for",
                               racex, "to", input$party, racey,
                               "in", tstate2,
                               "counties", tnote)
                ylabel <- paste(tshift, input$party, "Voters to", input$party,
                                racey)
            }
            else if (input$mapvar2 == "MARGIN1"){
                title <- paste(input$party, tunits, "in", racex,
                               "Race in", tstate2,
                               "counties", tnote)
                ylabel <- paste(input$party, tunits, "in", racex)
            }
            else if (input$mapvar2 == "MARGIN2"){
                title <- paste(input$party, tunits, "in", racey,
                               "Race in", tstate2,
                               "counties", tnote)
                ylabel <- paste(input$party, tunits, "in", racey)
            }
            else if (input$fcounty != ""){
                title <- paste(tshift, input$party, tunits, "from",
                               racex, "to", racey,
                               "Race in", tstate2,input$fcounty,
                               "County", tnote)
                ylabel <- paste(tshift, input$party, tunits, "for", racey)
            }
            else{
                title <- paste(tshift, input$party, tunits, "from",
                               racex, "to", racey,
                               "Race in", tstate2,
                               "counties", tnote)
                ylabel <- paste(tshift, input$party, tunits, "for", racey)
            }
            xlabel <- paste0(input$party," ",tunits," for ", racex,
                             "\nSources: see http://econdataus.com/voting_stats.htm")
            title2 <- title
            counties_in <- "counties"
            if (input$mapvar == "MARGIN1"){
                title2 <- paste("Margin", tunits, "in", racex, "Race in", tstate2,
                                counties_in, tnote)
            }
            else if (input$mapvar == "MARGIN2"){
                title2 <- paste("Margin", tunits, "in", racey, "Race in", tstate2,
                                counties_in, tnote)
            }
            labels <- c(title, xlabel, ylabel, title2)
            return(labels)
        }
        output$myUsage <- renderUI({
            includeHTML("voting_stats.htm")
        })
        output$myPlot <- renderPlot({
            xx <- getdata()
            if (input$units == "Count"){
                xx <- xx[-NROW(xx),]
            }
            if (names(xx)[2] == "DISTRICT")
            {
                names(xx)[1:10] <- c("COUNTY","DISTRICT","DEM1","REP1","MARGIN1","TOTAL1","DEM2","REP2","MARGIN2","TOTAL2")
            }
            else{
                names(xx)[1:9] <- c("COUNTY","DEM1","REP1","MARGIN1","TOTAL1","DEM2","REP2","MARGIN2","TOTAL2")
            }
            if (!input$displaytotal){
                xx <- xx[xx$COUNTY != "TOTAL",]
            }
            if (input$party == "Democrat"){
                preparty <- "DEM"
                party1 <- "DEM1"
            }
            else if (input$party == "Republican"){
                preparty <- "REP"
                party1 <- "REP1"
            }
            else if (input$party == "Total"){
                preparty <- "TOT"
                party1 <- "TOTAL1"
            }
            else{
                preparty <- "MAR"
                party1 <- "MARGIN1"
            }
            party_sh <- paste0(preparty,"_SH")
            party1n <- "TOT1_N"
            party2n <- "TOT2_N"
            xx$Party <- ""
            if (input$xlimit != ""){
                vlimit <- as.numeric(unlist(strsplit(input$xlimit, ",")))
                vparty <- unlist(strsplit(input$xparty, ","))
                xx$Party <- vparty[length(vparty)]
                xx$Party[xx[["MARGIN1"]] < vlimit[1]] <- vparty[1]
                for (i in 1:length(vlimit)){
                    xx$Party[xx[["MARGIN1"]] >= vlimit[i] & xx[["MARGIN1"]] < vlimit[i+1]] <- vparty[i+1]
                }
            }
            if (input$sizefor2){
                xx$Votes <- xx[[party2n]]
            }
            else{
                xx$Votes <- xx[[party1n]]
            }
            isParty <- NULL
            for (i in 1:length(vparty)){
                isParty <- c(isParty, any(xx$Party == vparty[i]))
            }
            gg <- ggplot(xx, aes_string(x=party1, y=party_sh))
            gg <- gg + geom_point(data=xx, alpha=0.7,
                                  aes_string(color="Party",size="Votes"))
            if (input$party == "Margin"){
                gg <- gg + geom_abline(intercept=0, slope=-1, color="gray", linetype="dashed")
            }
            if (input$party == "Margin" | input$units == "Count"){
                gg <- gg + geom_vline(xintercept=0, color="gray")
            }
            if (input$measure != "Percent ratio"){
                gg <- gg + geom_hline(yintercept=0, color="gray")
            }
            vcolor <- unlist(strsplit(input$xcolor, ","))
            vcolor <- vcolor[isParty]
            if (length(vcolor) > 1){
                gg <- gg + scale_fill_manual(values = vcolor) # Bar Plot
                gg <- gg + scale_color_manual(values = vcolor) # Line Graph
            }
            vrange_n <- as.numeric(unlist(strsplit(input$vrange, ",")))
            if (input$vbreaks != ""){
                vbreaks_n <- as.numeric(unlist(strsplit(input$vbreaks, ",")))
                gg <- gg + scale_size_continuous(range = vrange_n,
                                                 breaks = vbreaks_n)
            }
            else if (input$vtrans != "" & substr(input$vtrans,1,1) != "#"){
                gg <- gg + scale_size_continuous(range = vrange_n,
                                                 trans = input$vtrans)
            }
            else{
                gg <- gg + scale_size_continuous(range = vrange_n)
                #gg <- gg + scale_size_continuous(range = c(1,4))
            }
            if (input$measure == "Percent change"){
                tshift <- "% Change in"
            }
            else if (input$measure == "Percent ratio"){
                tshift <- "% Ratio of"
            }
            else{
                tshift <- "Shift in"
            }
            if (input$units == "Percent"){
                tunits <- "Vote Share"
            }
            else{
                tunits <- "Votes"
            }
            if (input$party == "Margin"){
                if (input$titlenote == ""){
                    tnote <- "(positive direction is more Democratic)"
                }
                else{
                    tnote <- input$titlenote
                }
            }
            else{
                tnote <- input$titlenote
            }
            if (input$district == ""){
                tstate2 <- input$state2
            }
            else{
                tstate2 <- paste0(input$state2,"-",input$district)
            }
            racex <- paste0(input$racex,"_",input$yearx)
            racey <- paste0(input$racey,"_",input$yeary)
            if (input$racey == "Registered"){
                gg <- gg + ggtitle(paste(tshift, input$party, "Voters for",
                                         racex, "to", input$party, racey,
                                         "in", tstate2,
                                         "counties", tnote))
                gg <- gg + ylab(paste(tshift, input$party, "Voters to", input$party,
                                      racey))
            }
            else if (input$fcounty != ""){
                gg <- gg + ggtitle(paste(tshift, input$party, tunits, "from",
                                         racex, "to", racey,
                                         "Race in", tstate2,input$fcounty,
                                         "County", tnote))
                gg <- gg + ylab(paste(tshift, input$party, tunits, "for", racey))
            }
            else{
                gg <- gg + ggtitle(paste(tshift, input$party, tunits, "from",
                                         racex, "to", racey,
                                         "Race in", tstate2,
                                         "counties", tnote))
                gg <- gg + ylab(paste(tshift, input$party, tunits, "for", racey))
            }
            gg <- gg + xlab(paste0(input$party," ",tunits," for ", racex,
                                   "\nSources: see http://econdataus.com/voting_stats.htm"))
            if (input$fcounty != "" & names(xx)[2] == "DISTRICT"){
                xx$LABEL <- xx$DISTRICT
            }
            else{
                xx$LABEL <- xx$COUNTY
            }
            if (input$showrow){
                xx$LABEL <- paste0(xx$LABEL,"-",row.names(xx))
            }
            xx$POS   <- 2 # RESET TO 2
            spos1 <- unlist(strsplit(input$pos1, ","))
            xx$POS[xx$LABEL %in% spos1] <- 1
            spos3 <- unlist(strsplit(input$pos3, ","))
            xx$POS[xx$LABEL %in% spos3] <- 3
            xx$VJUST <- 0.5
            xx$VJUST[xx$POS == 1] <- -1
            xx$VJUST[xx$POS == 3] <- 2
            xx$PREPEND <- ""
            xx$PREPEND[xx$POS == 2] <- "  "
            xx$LABEL <- paste0(xx$PREPEND,xx$LABEL)
            if (input$fpop != ""){
                kpop <- 1000 * as.numeric(input$fpop)
                if (kpop > 0){
                    xx$LABEL[xx$TOT1_N < kpop] <- ""
                }
            }
            if (input$party == "Democrat"){
                gg <- gg + annotate("text", x = xx$DEM1, y =xx$DEM_SH, label = xx$LABEL,
                                    color="red", hjust = 0, vjust = xx$VJUST, size = input$tsize)
            }
            else if (input$party == "Republican"){
                gg <- gg + annotate("text", x = xx$REP1, y =xx$REP_SH, label = xx$LABEL,
                                    color="red", hjust = 0, vjust = xx$VJUST, size = input$tsize)
            }
            else if (input$party == "Total"){
                gg <- gg + annotate("text", x = xx$TOTAL1, y =xx$TOT_SH, label = xx$LABEL,
                                    color="red", hjust = 0, vjust = xx$VJUST, size = input$tsize)
            }
            else{
                gg <- gg + annotate("text", x = xx$MARGIN1, y =xx$MAR_SH, label = xx$LABEL,
                                    color="red", hjust = 0, vjust = xx$VJUST, size = input$tsize)
            }
            xx <- NULL
            yy <- NULL
            if(input$xscale != ""){
                sxx <- unlist(strsplit(input$xscale, ","))
                xx <- as.numeric(sxx)
                if (length(sxx) == 3){
                    gg <- gg + scale_x_continuous(breaks = seq(xx[1],xx[2],xx[3]),
                                                  minor_breaks = seq(xx[1],xx[2],xx[3]))
                }
                else if (length(sxx) == 4){
                    gg <- gg + scale_x_continuous(breaks = seq(xx[1],xx[2],xx[3]),
                                                  minor_breaks = seq(xx[1],xx[2],xx[4]))
                }
            }
            if(input$yscale != ""){
                syy <- unlist(strsplit(input$yscale, ","))
                yy <- as.numeric(syy)
                if (length(syy) == 3){
                    gg <- gg + scale_y_continuous(breaks = seq(yy[1],yy[2],yy[3]),
                                                  minor_breaks = seq(yy[1],yy[2],yy[3]))
                }
                else if (length(syy) == 4){
                    gg <- gg + scale_x_continuous(breaks = seq(yy[1],yy[2],yy[3]),
                                                  minor_breaks = seq(yy[1],yy[2],yy[4]))
                }
            }
            if (length(xx) >= 2){
                if (length(yy) >= 2){
                    gg <- gg + coord_cartesian(xlim = c(xx[1], xx[2]), ylim = c(yy[1], yy[2]))
                }
                else{
                    gg <- gg + coord_cartesian(xlim = c(xx[1], xx[2]))
                }
            }
            else if (length(yy) >= 2){
                gg <- gg + coord_cartesian(ylim = c(yy[1], yy[2]))
            }
            return(gg)
        }, height = 600, width = 1000)
        #},)
        output$myTable1 <- renderPrint({
            if (input$createfiles){
                cat("Create Files\n")
                # createCA()
                # createFL()
                # createGA()
                # createIA()
                # createME()
                # createMI()
                # createMN()
                # createNC()
                # createPA()
                # createSC()
                # createTX()
                # createWI()
                # createStates_President_2000_2020()
                createFL22Governor()
                createTX22Governor()
            }
            xxlist <- getdatax(input$state2)
            xxparty <- xxlist[[1]]
            xids    <- xxlist[[2]]
            xx0     <- xxlist[[3]]
            idem <- which(xxparty == "DEM")
            irep <- which(xxparty == "REP")
            xx0$TOTAL <- 0
            ind <- c(1,NCOL(xx0),idem,irep)
            for (i in 2:(NCOL(xx0)-1)){
                if (i != idem & i != irep){
                    ind <- c(ind,i)
                }
            }
            xx <- as.data.frame(xx0[ind])
            xx$TOTAL <- rowSums(xx[3:NCOL(xx)])
            dp <- 0
            if (input$units == "Percent"){
                for (i in 3:NCOL(xx)){
                    xx[,i] <- 100 * xx[,i] / xx$TOTAL
                }
                dp <- 2
            }
            for (i in 2:NCOL(xx)){
                xx[,i] <- format(round(xx[,i], dp), big.mark=",", scientific=FALSE)
            }
            print(xx)
        })
        output$myTable2 <- renderPrint({
            dd <- getdata()
            firstn <- 2
            if (names(dd)[2] == "DISTRICT"){
                firstn <- 3
            }
            #dd <- dd[,1:(NCOL(dd)-4)]
            dd <- dd[,1:18]
            dp <- 2
            for (i in firstn:NCOL(dd)){
                dd[,i] <- format(round(dd[,i], dp), big.mark=",", scientific=FALSE)
            }
            if (input$measure == "Percent change"){
                tshift <- "Percent change in"
            }
            else if (input$measure == "Percent ratio"){
                tshift <- "Percent ratio of"
            }
            else{
                tshift <- "Shift in"
            }
            if (input$units == "Percent"){
                tunits <- "Vote Share"
            }
            else{
                tunits <- "Votes"
            }
            if (input$party == "Margin"){
                if (input$titlenote == ""){
                    tnote <- "(positive direction is more Democratic)"
                }
                else{
                    tnote <- input$titlenote
                }
            }
            else{
                tnote <- input$titlenote
            }
            if (input$district == ""){
                tstate2 <- input$state2
            }
            else{
                tstate2 <- paste0(input$state2,"-",input$district)
            }
            racex <- paste0(input$racex,"_",input$yearx)
            racey <- paste0(input$racey,"_",input$yeary)
            if (input$racey == "Registered"){
                title <- paste0(tshift," Voters for ",
                                racex," to ",racey,
                                " Voters in ",tstate2," counties (",
                                input$units,")")
            }
            else{
                title <- paste0(tshift," ",tunits," from ",
                                racex," to ",racey,
                                " Race in ",tstate2," counties (",
                                input$units,")")
            }
            cat(paste0(title,"\n\n"))
            print(dd)
        })
        getpop <- function(state2){
            filename <- paste0("input2/","co-est2020.csv")
            xx <- read_csv(filename)
            istate <- which(stabbr == state2)
            xx <- xx[xx$STNAME == states[istate],]
            xx <- xx[grepl(" County",xx$CTYNAME),]
            xx$CTYNAME <- gsub(" County","",xx$CTYNAME)
            pyear <- input$yeary
            if (pyear < 2010) pyear <- 2010
            if (pyear > 2020) pyear <- 2020
            pname <- paste0("POPESTIMATE",pyear)
            pp <- data.frame(xx$CTYNAME,xx[[pname]])
            names(pp) <- c("COUNTY","POPULATION")
            return(pp)
        }
        output$myTable3 <- renderPrint({
            dd <- getdata()
            firstn <- 2
            if (names(dd)[2] == "DISTRICT"){
                firstn <- 3
            }
            #dd <- dd[,1:(NCOL(dd)-4)]
            dd <- dd[,c(1,12,19:NCOL(dd))]
            
            dp <- 2
            for (i in firstn:NCOL(dd)){
                if (is.numeric(unlist(dd[,i]))){
                    dd[,i] <- format(round(dd[,i], dp), big.mark=",", scientific=FALSE)
                }
            }
            if (input$measure == "Percent change"){
                tshift <- "Percent change in"
            }
            else if (input$measure == "Percent ratio"){
                tshift <- "Percent ratio of"
            }
            else{
                tshift <- "Shift in"
            }
            if (input$units == "Percent"){
                tunits <- "Vote Share"
            }
            else{
                tunits <- "Votes"
            }
            if (input$party == "Margin"){
                if (input$titlenote == ""){
                    tnote <- "(positive direction is more Democratic)"
                }
                else{
                    tnote <- input$titlenote
                }
            }
            else{
                tnote <- input$titlenote
            }
            if (input$district == ""){
                tstate2 <- input$state2
            }
            else{
                tstate2 <- paste0(input$state2,"-",input$district)
            }
            racex <- paste0(input$racex,"_",input$yearx)
            racey <- paste0(input$racey,"_",input$yeary)
            if (input$racey == "Registered"){
                title <- paste0(tshift," Voters for ",
                                racex," to ",racey,
                                " Voters in ",tstate2," counties (",
                                input$units,")")
            }
            else{
                title <- paste0(tshift," ",tunits," from ",
                                racex," to ",racey,
                                " Race in ",tstate2," counties (",
                                input$units,")")
            }
            cat(paste0(title,"\n\n"))
            print(dd)
        })
        getLN <- function(state2){
            dd <- getdata_state(state2)
            firstn <- 2
            if (names(dd)[2] == "DISTRICT"){
                firstn <- 3
            }
            dd <- dd[,c(1,12,19:NCOL(dd))]
            zxevars <<- input$xevars #DEBUG-RM
            form <- paste0("MAR_SH ~ ",input$xevars[1])
            if (length(input$xevars) > 1){
                for (i in input$xevars[2:length(input$xevars)]){
                    form <- paste0(form," + ",i)
                }
            }
            if (input$standardize){
                dd <- dd %>% mutate_at(c("POPULATION"), ~(scale(.) %>% as.vector))
            }
            if (NROW(dd) == 0){
                return(NULL)
            }
            zstate2 <<- state2
            zform <<- form
            #lmfit <- lm(as.formula(form), data = dd)
            lmfit = tryCatch({
                lm(as.formula(form), data = dd)
            }, warning = function(w){
                #print(paste0("----> WARNING: ",w))
            }, error = function(e){
                #print(paste0("****> ERROR: ",e))
                return(NULL)
            }, finally = {
                
            })
            zlmfit <<- lmfit
            return(lmfit)
        }
        output$myLN <- renderPrint({
            if (input$lnallstates){
                Estimate <- StdError <- tvalue <- pvalue <- numeric(0)
                xx <- data.frame(Estimate,StdError,tvalue,pvalue)
                #stabbr <- c("TX","WI","GA") #DEBUG-TEST
                for (state2 in stabbr){
                    ff <- getLN(state2)
                    if (is.null(ff)){
                        Estimate <- StdError <- tvalue <- 0
                        pvalue <- NA
                        dd <- data.frame(Estimate,StdError,tvalue,pvalue)
                    }
                    else{
                        ss <- summary(ff)
                        dd <- as.data.frame(ss$coefficients)
                        names(dd) <- c("Estimate","StdError","tvalue","pvalue")
                        dd <- dd[grepl(input$lnmatch,rownames(dd),ignore.case = FALSE),]
                    }
                    if (NROW(dd) > 0){
                        rownames(dd)[1] <- state2
                        dd$state2 <- state2
                        dd$nn <- seq(1:NROW(dd))
                        xx <- rbind(xx,dd)
                    }
                }
                xx$mvalue <- 0
                zxx0 <<-xx #DEBUG-RM
                xx$mvalue[xx$pvalue < as.numeric(input$lnlimit)] <- 1
                xx$mvalue[xx$pvalue < as.numeric(input$lnlimit) & xx$Estimate < 0] <- -1
                save_LN_Map <<- xx
                print(xx)
            }
            else{
                ff <- getLN(input$state2)
                #print(lmfit$coefficients)
                print(summary(ff))
                zff <<- ff #DEBUG-RM
            }
        })
        output$myVoteData <- renderPrint({
            states <- c("CA","FL","GA","IA","ME","MI","MN","NC","PA","SC","TX","WI")
            races <- c("President", "Senate", "House", "Governor", "Registered")
            years <- seq(input$year_first, input$year_last, by = input$year_step)
            files <- data.frame(matrix(ncol = length(states)+2, nrow = 0))
            colnames(files) <- c("Year", "Race", states)
            for (year in years){
                for (race in races){
                    newrow <- c(year, race)
                    tot <- 0
                    for (state in states){
                        filename <- paste0(data_dir,state,"_",race,"_",year,".csv")
                        cnt <- 0
                        if (file.exists(filename)){
                            cnt <- length(readLines(filename)) - 2
                            tot <- tot + 1
                        }
                        newrow <- c(newrow, cnt)
                    }
                    if (tot > 0){
                        files[nrow(files)+1,] = newrow
                    }
                }
            }
            races <- c("Map_Parms", "Plot_Parms")
            for (race in races){
                newrow <- c(0, race)
                tot <- 0
                for (state in states){
                    filename <- paste0(data_dir,state,"_",race,".csv")
                    cnt <- 0
                    if (file.exists(filename)){
                        cnt <- length(readLines(filename)) - 1
                        tot <- tot + 1
                    }
                    newrow <- c(newrow, cnt)
                }
                if (tot > 0){
                    files[nrow(files)+1,] = newrow
                }
            }
            print(files)
            cat("\n")
            cat("2000-2020  President           All States\n")
            cat("2021       House, District 34  TX\n")
        })
        output$myLeaflet <- renderLeaflet({
            dd <- getdata() # COUNTY,DEM1,REP1,MARGIN1,TOTAL1,DEM2,REP2,MARGIN2,TOTAL2,
                            # DEM_SH,REP_SH,MAR_SH,TOT_SH,DEM1_N,REP1_N,MAR1_N,TOT1_N,TOT2_N
            dd <- dd[dd$COUNTY != "TOTAL",]
            #zdd <<- dd #DEBUG-RM

            #dd <- dd[dd$DEM1 != 0 & dd$REP1 != 0 & dd$DEM2 != 0 & dd$REP2 != 0,] #Include only 2-party races
            mapvar <- input$mapvar2
            ee <- dd[!is.na(dd[[mapvar]]),]
            if (input$maplimitset2 == "Auto set to min,max"){
                minlimit <- floor(min(ee[[mapvar]]))
                maxlimit <- ceiling(max(ee[[mapvar]]))
                maplimits <- paste0(minlimit,",",maxlimit)
                updateTextInput(session, "maplimits2", value = maplimits)
            }
            else if (input$maplimitset2 == "Auto set balanced"){
                minlimit <- floor(min(ee[[mapvar]]))
                maxlimit <- ceiling(max(ee[[mapvar]]))
                abslimit <- max(abs(minlimit), abs(maxlimit))
                stepsize <- ceiling(abslimit/5)
                abslimit <- ceiling(abslimit/stepsize) * stepsize
                maplimits <- paste0("-",abslimit,",",abslimit,",",stepsize)
                updateTextInput(session, "maplimits2", value = maplimits)
            }
            limits <- unlist(strsplit(input$maplimits2, ","))
            if (length(limits) <= 2){
                pal <- colorNumeric(input$mapcolors2, dd[[mapvar]])
            }
            else if (length(limits) == 3){
                bins <- seq.int(limits[1], limits[2], limits[3])
                pal <- colorBin(input$mapcolors2, domain = dd[[mapvar]], bins = bins)
            }
            else{
                bins <- limits
                pal <- colorBin(input$mapcolors2, domain = dd[[mapvar]], bins = bins)
            }
            
            istate <- which(stabbr %in% input$state2)
            counties <- rgdal::readOGR("gz_2010_us_050_00_20m.json")
            counties <- counties[counties$STATE == statsd[istate],]
            cc <- st_as_sf(counties)
            cc$NAME   <- toupper(cc$NAME)
            dd$COUNTY <- toupper(dd$COUNTY)
            ee <- cc %>% left_join(dd, by = c("NAME" = "COUNTY"))
            ee$var <- ee[[input$mapvar2]]
            if (input$mapvar2 == "EQUIP"){
                titletext <- paste0("<b>Voting Equipment used in ",input$state2," counties in ",input$yeary,"</b><br>")
                ecolors <- unlist(strsplit(input$ecolors2,","))
                elabels <- rep("None",length(ecolors))
                elabs <- unlist(strsplit(input$elabels2,","))
                iequip <- as.numeric(sort(unique(ee$EQUIP)))
                mm <- length(iequip)
                nn <- mm
                if (sum(is.na(ee$EQUIP)) > 0){
                    nn <- nn+1
                    elabels[nn] <- "NA"
                }
                ilegend <- seq(1,nn)
                ecolors <- ecolors[1:nn]
                elabels <- elabels[1:nn]
                ee$ILEGEND <- nn # for NA
                for (i in 1:(nn-1)){
                    ee$ILEGEND[ee$EQUIP == iequip[i]] <- i
                    mask <- 1
                    for (j in 1:length(elabs)){
                        if (bitwAnd(iequip[i],mask) > 0){
                            if (elabels[i] == "None"){
                                elabels[i] <- paste0(elabs[j])
                            }
                            else{
                                elabels[i] <- paste0(elabs[j],"/",elabels[i])
                            }
                        }
                        mask <- mask*2
                        #print(paste0("elabels[",i,"]=",elabels[i])) #DEBUG-RM
                    }
                }
                pal <- colorNumeric(ecolors, ilegend)
                zee <<- ee #DEBUG-RM
                mm <- leaflet(ee) %>%
                    addTiles() %>%
                    addControl(titletext, position = "topright", className="map-title") %>%
                    addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.5,
                                fillColor = ~pal(ILEGEND),
                                label = ~paste0(NAME, ": ", formatC(var, big.mark = ","))) %>%
                    #addLegend(pal = pal, values = ~(var), opacity = 1.0)
                    addLegend(values = ~(var), opacity = 1.0,
                              colors = ecolors, labels = elabels)
                #           labFormat = labelFormat(transform = function(x) round(10^x)))
            }
            else{
                labels <- getlabels("map")
                titletext <- paste0("<b>",labels[4],"</b><br>")
                mm <- leaflet(ee) %>%
                    addTiles() %>%
                    addControl(titletext, position = "topright", className="map-title") %>%
                    addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.5,
                                fillColor = ~pal(var),
                                label = ~paste0(NAME, ": ", formatC(var, big.mark = ","))) %>%
                    addLegend(pal = pal, values = ~(var), opacity = 1.0)
                #           labFormat = labelFormat(transform = function(x) round(10^x)))
            }
            print(mm)
        })
        output$myLN_Map <- renderLeaflet({
            dd <- save_LN_Map
            mapvar <- input$mapvar3
            ee <- dd[!is.na(dd[[mapvar]]),]
            if (input$maplimitset3 == "Auto set to min,max"){
                minlimit <- floor(min(ee[[mapvar]]))
                maxlimit <- ceiling(max(ee[[mapvar]]))
                maplimits <- paste0(minlimit,",",maxlimit)
                updateTextInput(session, "maplimits3", value = maplimits)
            }
            else if (input$maplimitset3 == "Auto set balanced"){
                minlimit <- floor(min(ee[[mapvar]]))
                maxlimit <- ceiling(max(ee[[mapvar]]))
                abslimit <- max(abs(minlimit), abs(maxlimit))
                stepsize <- ceiling(abslimit/5)
                abslimit <- ceiling(abslimit/stepsize) * stepsize
                maplimits <- paste0("-",abslimit,",",abslimit,",",stepsize)
                updateTextInput(session, "maplimits3", value = maplimits)
            }
            limits <- unlist(strsplit(input$maplimits3, ","))
            if (length(limits) <= 2){
                pal <- colorNumeric(input$mapcolors3, dd[[mapvar]])
            }
            else if (length(limits) == 3){
                bins <- seq.int(limits[1], limits[2], limits[3])
                pal <- colorBin(input$mapcolors3, domain = dd[[mapvar]], bins = bins)
            }
            else{
                bins <- limits
                pal <- colorBin(input$mapcolors3, domain = dd[[mapvar]], bins = bins)
            }
            tag.map.title <- tags$style(HTML("
                  .leaflet-control.map-title {
                    !transform: translate(-50%,20%);
                    position: fixed !important;
                    left: 38%;
                    text-align: center;
                    padding-left: 10px;
                    padding-right: 10px;
                    background: rgba(255,255,255,0.75);
                    font-weight: normal;
                    font-size: 14px;
                  }
            "))
            line1 <- paste0("States where Regression of Shift on Population by County has a p-value < ",
                            input$lnlimit)
            line2 <- paste0("(shift in Margin Vote Share from ",input$racex,"_",input$yearx," to ",
                            input$racey,"_",input$yeary,", red=negative coefficient)")
            titletext <- paste0("<b>",line1,"</b><br>",line2)
            title <- tags$div(
                tag.map.title, HTML(titletext)
            )
            lstates <- states(cb=T)
            lstates <- lstates[!(lstates$STUSPS %in% c("AK","DC","LA")),] #omit NAs
            dd <- lstates %>%
                left_join(dd, by = c("STUSPS" = "state2")) # match on state abbv
            leaflet(dd) %>%
                addTiles() %>%
                addControl(title, position = "topright", className="map-title") %>%
                setView(-96, 37.8, 4) %>%
                addLegend(pal = pal, values = dd[[mapvar]], opacity = 0.7, title = NULL,
                          position = "bottomright") %>%
                addPolygons(
                    fillColor = ~pal(dd[[mapvar]]),
                    weight = 1, #was 2
                    opacity = 1,
                    color = "white", #was "white"
                    dashArray = "1", #was "3"
                    fillOpacity = 0.7,
                    popup = paste0(dd$NAME,"<br>",round(dd[[mapvar]],2))
                )
        })
        output$myggMap <- renderPlot({
            dd <- getdata() # COUNTY,DEM1,REP1,MARGIN1,TOTAL1,DEM2,REP2,MARGIN2,TOTAL2,
                            # DEM_SH,REP_SH,MAR_SH,TOT_SH,DEM1_N,REP1_N,MAR1_N,TOT1_N,,TOT2_N
            dd <- dd[dd$COUNTY != "TOTAL",]

            dd$county_name <- paste(str_to_upper(dd$COUNTY), "COUNTY")
            counties$county_name <- str_to_upper(counties$county_name)
            scounties <- counties %>%
                filter(state_abbv == input$state2)
            if (input$state2 != ""){
                stcounty <- paste0(input$state2,"|",dd$county_name)
                stcounty_chng <- read_csv("stcounty_chng.csv")
                for (i in 1:NROW(stcounty_chng)){
                    dd$county_name[stcounty == stcounty_chng$old[i]] <- stcounty_chng$new[i]
                }
            }
            election_data <- left_join(dd, scounties, by = "county_name")
            counties1_not2 <- dd$county_name[!dd$county_name %in% scounties$county_name]
            counties2_not1 <- scounties$county_name[!scounties$county_name %in% dd$county_name]
            if (length(counties1_not2) > 0){
                print("counties1_not2:")
                print(counties1_not2)
            }
            if (length(counties2_not1) > 0){
                print("counties2_not1:")
                print(counties2_not1)
            }
            mapcolors <- unlist(strsplit(input$mapcolors, ","))
            fipref <- "state.txt" # read from local memory
            ff <<- read.csv(fipref, sep = "|")
            st_cities <- read.csv("us_cities.csv")
            if (input$state2 != ""){
                istate <- ff$STATE[ff$STUSAB == input$state2]
                sstate <- sprintf("%02d", istate)
                st_cities<-subset(st_cities,country.etc==input$state2)
                stdata <- urbnmapr::countydata[startsWith(urbnmapr::countydata$county_fips, sstate),]
            }
            else{
                stdata <- urbnmapr::countydata
            }
            labels <- getlabels("map")
            xlabel <- paste0("longitude\nSources: see http://econdataus.com/voting_stats.htm")
            mapvar <- input$mapvar
            if (mapvar == "DEM1"){mapvar <- names(dd)[2]}
            else if (mapvar == "REP1"){mapvar <- names(dd)[3]}
            else if (mapvar == "DEM2"){mapvar <- names(dd)[6]}
            else if (mapvar == "REP2"){mapvar <- names(dd)[7]}
            
            gg <- election_data %>%
                ggplot(aes_string("long", "lat", group = "group", fill = mapvar)) +
                geom_polygon(color = NA) +
                coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
                labs(fill = "Shift in Margin") +
                ggtitle(labels[4]) +
                xlab(xlabel) +
                ylab("latitude")
            maplimits <- input$maplimits
            if (input$maplimitset == "Auto set to min,max"){
                minlimit <- floor(min(election_data[[mapvar]]))
                maxlimit <- ceiling(max(election_data[[mapvar]]))
                maplimits <- c(minlimit, maxlimit)
                updateTextInput(session, "maplimits", value = maplimits)
            }
            else if (input$maplimitset == "Auto set balanced"){
                minlimit <- floor(min(election_data[[mapvar]]))
                maxlimit <- ceiling(max(election_data[[mapvar]]))
                maplimits <- max(abs(minlimit), abs(maxlimit))
                updateTextInput(session, "maplimits", value = maplimits)
            }
            if (maplimits == ""){
                gg <- gg + scale_fill_gradientn(
                    colors = mapcolors,
                    na.value = "white",
                    guide = guide_colorbar(title.position = "top"))
            }
            else{
                maplimits <- unlist(strsplit(input$maplimits, ","))
                if (length(maplimits) == 1){
                    maplimits <- c(paste0("-",maplimits[1]), maplimits[1])
                }
                gg <- gg + scale_fill_gradientn(
                    colors = mapcolors,
                    na.value = "white",
                    limits = as.numeric(maplimits),
                    guide = guide_colorbar(title.position = "top"))
            }
            skipcity <- unlist(strsplit(input$skipcity, ","))
            showcity <- unlist(strsplit(input$showcity, ","))
            if (input$state2 != ""){
                for (i in 1:dim(st_cities)[1]){
                    city <- substr(st_cities$name[i], 1, nchar(as.character(st_cities$name[i]))-3)
                    if (city %in% skipcity) next
                    if (city %in% showcity | st_cities$pop[i] > input$minpop){
                        gg <- gg + annotate(geom = "point", x = st_cities$long[i], y = st_cities$lat[i])
                        gg <- gg + annotate(geom = "text", x = st_cities$long[i] + input$longoff,
                                            y = st_cities$lat[i], label = city)
                    }
                }
            }
            print(gg)
        }, height = 900, width = 1200)
        states <- c("Alabama","Alaska","Arizona","Arkansas","California",
                    "Colorado","Connecticut","Delaware","DC","Florida",
                    "Georgia","Hawaii","Idaho","Illinois","Indiana",
                    "Iowa","Kansas","Kentucky","Louisiana","Maine",
                    "Maryland","Massachusetts","Michigan","Minnesota","Mississippi",
                    "Missouri","Montana","Nebraska","Nevada","New Hampshire",
                    "New Jersey","New Mexico","New York","North Carolina","North Dakota",
                    "Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island",
                    "South Carolina","South Dakota","Tennessee","Texas","Utah",
                    "Vermont","Virginia","Washington","West Virginia","Wisconsin",
                    "Wyoming")
        stabbr <- c("AL","AK","AZ","AR","CA",
                    "CO","CT","DE","DC","FL",
                    "GA","HI","ID","IL","IN",
                    "IA","KS","KY","LA","ME",
                    "MD","MA","MI","MN","MS",
                    "MO","MT","NE","NV","NH",
                    "NJ","NM","NY","NC","ND",
                    "OH","OK","OR","PA","RI",
                    "SC","SD","TN","TX","UT",
                    "VT","VA","WA","WV","WI","WY")
        statid <- c(  1 ,  2 ,  4 ,  5 ,  6 ,
                      8 ,  9 , 10 , 11 , 12 ,
                     13 , 15 , 16 , 17 , 18 ,
                     19 , 20 , 21 , 22 , 23 ,
                     24 , 25 , 26 , 27 , 28 ,
                     29 , 30 , 31 , 32 , 33 ,
                     34 , 35 , 36 , 37 , 38 ,
                     39 , 40 , 41 , 42 , 44 ,
                     45 , 46 , 47 , 48 , 49 ,
                     50 , 51 , 53 , 54 , 55 , 56 , 11 )
        statsd <- c("01","02","04","05","06",
                    "08","09","10","11","12",
                    "13","15","16","17","18",
                    "19","20","21","22","23",
                    "24","25","26","27","28",
                    "29","30","31","32","33",
                    "34","35","36","37","38",
                    "39","40","41","42","44",
                    "45","46","47","48","49",
                    "50","51","53","54","55","56","11")
        getStateAbbr <- function(str){
            for (i in 1:length(states)){
                pattern <- paste0("^",states[i])
                if (any(grep(pattern, str))){
                    str <- gsub(pattern, stabbr[i], str)
                    break
                }
            }
            return(str)
        }
        #############################################################
        # getdata
        #############################################################
        getdatax <- function(state2){
            filenamex <- paste0(data_dir,state2,"_",input$racex,"_",input$yearx,".csv")
            xxparty <- read_delim(filenamex, ' ', col_names = FALSE, n_max = 1)
            xids <- 1
            if (grepl("^House",input$racex)){
                xids <- 2
            }
            xx0 <- read_delim(filenamex, ' ', skip = 1)
            # Remove columns where __party starts with X_, rows where COUNTY starts with X_
            xx0 <- xx0[,!grepl("^X_",xxparty)]
            xxparty <- xxparty[,!grepl("^X_",xxparty)]
            xx0 <- xx0[ !grepl("^X_",xx0$COUNTY),]
            xx0$COUNTY <- str_to_title(xx0$COUNTY) # COUNTY,all_parties
            xxlist <- list(xxparty, xids, xx0)
            return(xxlist)
        }
        getdatay <- function(state2){
            filenamey <- paste0(data_dir,state2,"_",input$racey,"_",input$yeary,".csv")
            yyparty <- read_delim(filenamey, ' ', col_names = FALSE, n_max = 1)
            yids <- 1
            if (grepl("^House",input$racey)){
                yids <- 2
            }
            yy0 <- read_delim(filenamey, ' ', skip = 1)
            # Remove columns where __party starts with X_, rows where COUNTY starts with X_
            yy0 <- yy0[,!grepl("^X_",yyparty)]
            yyparty <- yyparty[,!grepl("^X_",yyparty)]
            yy0 <- yy0[ !grepl("^X_",yy0$COUNTY),]
            yy0$COUNTY <- str_to_title(yy0$COUNTY) # COUNTY,all_parties
            yylist <- list(yyparty, yids, yy0)
            return(yylist)
        }
        getdata_state <- function(state2){
            xxlist <- getdatax(state2)
            xxparty <- xxlist[[1]]
            xids    <- xxlist[[2]]
            xx0     <- xxlist[[3]]
            yylist <- getdatay(state2)
            yyparty <- yylist[[1]]
            yids    <- yylist[[2]]
            yy0     <- yylist[[3]]

            xx0$MARGIN1 <- 0
            xx0$TOTAL1 <- rowSums(xx0[,(xids+1):(NCOL(xx0)-1)], na.rm = TRUE) # excludes MARGIN1
            idem <- which(xxparty == "DEM")
            irep <- which(xxparty == "REP")
            xx <- xx0[,c(1:xids,idem,irep,NCOL(xx0)-1,NCOL(xx0))] # COUNTY,[DISTRICT],DEM,REP,MARGIN1,TOTAL1
            
            yy0$MARGIN2 <- 0
            yy0$TOTAL2 <- rowSums(yy0[,(yids+1):(NCOL(yy0)-1)], na.rm = TRUE) # excludes MARGIN2
            idem <- which(yyparty == "DEM")
            irep <- which(yyparty == "REP")
            yy <- yy0[,c(1:yids,idem,irep,NCOL(yy0)-1,NCOL(yy0))] # COUNTY,[DISTRICT],DEM,REP,MARGIN2,TOTAL2
            
            if (xids == 2 & yids == 1){ # aggregate if only this is House race
                xids <- 1
                xx <- xx[,-2]
                xxparty <- xxparty[-2]
                names(xx) <- c("COUNTY","DEM1","REP1","MARGIN1","TOTAL1")
                if (input$distype == "1 & 2-party"){ # remove NA county/districts
                    xx <- xx[!is.na(xx$DEM1) & !is.na(xx$REP1),]
                    narm <- TRUE
                }
                else if (input$distype == "2-party"){ # remove entire NA counties
                    narm <- FALSE
                }
                else{ # All (and previously Non-zero)
                    xx$DEM1[!is.na(xx$DEM1)] <- 0
                    xx$REP1[!is.na(xx$REP1)] <- 0
                    narm <- TRUE
                }
                xx <- xx %>%
                    group_by(COUNTY) %>%
                    summarise(DEM1 = sum(DEM1, na.rm = narm), REP1 = sum(REP1, na.rm = narm),
                              MARGIN1 = sum(MARGIN1, na.rm = narm), TOTAL1 = sum(TOTAL1, na.rm = narm))
                if (input$distype != "All"){
                    xx <- xx[xx$DEM1 != 0 & xx$REP1 != 0,]
                }
                xx <- as.data.frame(xx)
            }
            if (yids == 2 & xids == 1){ # aggregate if only this is House race
                yids <- 1
                yy <- yy[,-2]
                yyparty <- yyparty[-2]
                names(yy) <- c("COUNTY","DEM2","REP2","MARGIN2","TOTAL2")
                if (input$distype == "1 & 2-party"){
                    yy <- yy[!is.na(yy$DEM2) & !is.na(yy$REP2),]
                    narm <- TRUE
                }
                else if (input$distype == "2-party"){
                    narm <- FALSE
                }
                else{ # All (and previously Non-zero)
                    yy$DEM2[!is.na(yy$DEM2)] <- 0
                    yy$REP2[!is.na(yy$REP2)] <- 0
                    narm <- TRUE
                }
                yy <- yy %>%
                    group_by(COUNTY) %>%
                    summarise(DEM2 = sum(DEM2, na.rm = narm), REP2 = sum(REP2, na.rm = narm),
                              MARGIN2 = sum(MARGIN2, na.rm = narm), TOTAL2 = sum(TOTAL2, na.rm = narm))
                if (input$distype != "All"){
                    yy <- yy[yy$DEM2 != 0 & yy$REP2 != 0,]
                }
                yy <- as.data.frame(yy)
            }
            zxx1 <<- xx #DEBUG-RM
            zyy1 <<- yy #DEBUG-RM
            if (xids == 2 & yids == 2){
                dd <- as.data.frame(merge(xx, yy, by = c("COUNTY","DISTRICT"), all = TRUE))
                ddnames <- names(dd)
                names(dd) <- c("COUNTY","DISTRICT","DEM1","REP1","MARGIN1","TOTAL1","DEM2","REP2","MARGIN2","TOTAL2")
                ddtot <- data.frame("TOTAL","",sum(dd$DEM1,na.rm=TRUE),sum(dd$REP1,na.rm=TRUE),
                                    sum(dd$MARGIN1,na.rm=TRUE),sum(dd$TOTAL1,na.rm=TRUE),
                                    sum(dd$DEM2,na.rm=TRUE),sum(dd$REP2,na.rm=TRUE),
                                    sum(dd$MARGIN2,na.rm=TRUE),sum(dd$TOTAL2,na.rm=TRUE))
            }
            else{
                dd <- as.data.frame(merge(xx, yy, by = "COUNTY"))
                ddnames <- names(dd)
                names(dd) <- c("COUNTY","DEM1","REP1","MARGIN1","TOTAL1","DEM2","REP2","MARGIN2","TOTAL2")
                ddtot <- data.frame("TOTAL",sum(dd$DEM1,na.rm=TRUE),sum(dd$REP1,na.rm=TRUE),
                                    sum(dd$MARGIN1,na.rm=TRUE),sum(dd$TOTAL1,na.rm=TRUE),
                                    sum(dd$DEM2,na.rm=TRUE),sum(dd$REP2,na.rm=TRUE),
                                    sum(dd$MARGIN2,na.rm=TRUE),sum(dd$TOTAL2,na.rm=TRUE))
            }
            names(ddtot) <- names(dd)
            dd <- rbind(dd, ddtot)
            if (input$fcounty != ""){
                dd <- dd[dd$COUNTY == input$fcounty,]
            }
            else if (input$district != "" & xids == 2){
                dd <- dd[dd$DISTRICT == input$district,]
            }
            if (xids == 2){
                #"COUNTY","DISTRICT","DEM1","REP1","MARGIN1","TOTAL1","DEM2","REP2","MARGIN2","TOTAL2"
                if (input$distype == "2-party"){
                    dd <- dd[!is.na(dd$DEM1) & !is.na(dd$REP1) & !is.na(dd$DEM2) & !is.na(dd$REP2),]
                }
                if (!input$showdist){
                    xids <- 1
                    dd <- dd[,-2]
                    ddnames <- ddnames[-2]
                    dd <- dd %>%
                        group_by(COUNTY) %>%
                        summarise(DEM1 = sum(DEM1, na.rm = TRUE), REP1 = sum(REP1, na.rm = TRUE),
                                  MARGIN1 = sum(MARGIN1, na.rm = TRUE), TOTAL1 = sum(TOTAL1, na.rm = TRUE),
                                  DEM2 = sum(DEM2, na.rm = TRUE), REP2 = sum(REP2, na.rm = TRUE),
                                  MARGIN2 = sum(MARGIN2, na.rm = TRUE), TOTAL2 = sum(TOTAL2, na.rm = TRUE))
                    if (input$distype != "All"){
                        dd <- dd[dd$DEM1 != 0 & dd$REP1 != 0 & dd$DEM2 != 0 & dd$REP2 != 0,]
                    }
                    dd <- as.data.frame(dd)
                }
            }
            DEM1_N <- dd$DEM1
            REP1_N <- dd$REP1
            MAR1_N <- dd$MARGIN1
            TOT1_N <- dd$TOTAL1
            TOT2_N <- dd$TOTAL2
            dd$MARGIN1 <- dd$DEM1 - dd$REP1
            dd$MARGIN2 <- dd$DEM2 - dd$REP2
            if (input$dronly){
                dd$TOTAL1 <- dd$DEM1 + dd$REP1
                dd$TOTAL2 <- dd$DEM2 + dd$REP2
            }
            if (input$racey == "Registered"){
                if (input$measure == "Percent change"){
                    dd$DEM_SH <- 100 * (dd$DEM2 - dd$DEM1) / dd$DEM1
                    dd$REP_SH <- 100 * (dd$REP2 - dd$REP1) / dd$REP1
                    dd$MAR_SH <- 100 * (dd$MARGIN2 - dd$MARGIN1) / dd$MARGIN1
                    dd$TOT_SH <- 100 * (dd$TOTAL2 - dd$TOTAL1) / dd$TOTAL1
                }
                else if (input$measure == "Percent ratio"){
                    dd$DEM_SH <- 100 * dd$DEM1 / dd$DEM2
                    dd$REP_SH <- 100 * dd$REP1 / dd$REP2
                    dd$MAR_SH <- 100 * dd$MARGIN1 / dd$MARGIN2
                    dd$TOT_SH <- 100 * dd$TOTAL1 / dd$TOTAL2
                }
                else{
                    dd$DEM_SH <- dd$DEM2 - dd$DEM1
                    dd$REP_SH <- dd$REP2 - dd$REP1
                    dd$MAR_SH <- dd$MARGIN2 - dd$MARGIN1
                    dd$TOT_SH <- dd$TOTAL2 - dd$TOTAL1
                }
            }
            if (input$units == "Percent"){
                dd$DEM1 <- 100 * dd$DEM1 / dd$TOTAL1
                dd$REP1 <- 100 * dd$REP1 / dd$TOTAL1
                dd$MARGIN1 <- 100 * dd$MARGIN1 / dd$TOTAL1
                dd$DEM2 <- 100 * dd$DEM2 / dd$TOTAL2
                dd$REP2 <- 100 * dd$REP2 / dd$TOTAL2
                dd$MARGIN2 <- 100 * dd$MARGIN2 / dd$TOTAL2
                dd$TOTAL1 <- dd$DEM1 + dd$REP1
                dd$TOTAL2 <- dd$DEM2 + dd$REP2
            }
            if (input$racey != "Registered"){
                if (input$measure == "Percent change"){
                    dd$DEM_SH <- 100 * (dd$DEM2 - dd$DEM1) / dd$DEM1
                    dd$REP_SH <- 100 * (dd$REP2 - dd$REP1) / dd$REP1
                    dd$MAR_SH <- 100 * (dd$MARGIN2 - dd$MARGIN1) / dd$MARGIN1
                    dd$TOT_SH <- 100 * (dd$TOTAL2 - dd$TOTAL1) / dd$TOTAL1
                }
                else if (input$measure == "Percent ratio"){
                    dd$DEM_SH <- 100 * dd$DEM1 / dd$DEM2
                    dd$REP_SH <- 100 * dd$REP1 / dd$REP2
                    dd$MAR_SH <- 100 * dd$MARGIN1 / dd$MARGIN2
                    dd$TOT_SH <- 100 * dd$TOTAL1 / dd$TOTAL2
                }
                else{
                    dd$DEM_SH <- dd$DEM2 - dd$DEM1
                    dd$REP_SH <- dd$REP2 - dd$REP1
                    dd$MAR_SH <- dd$MARGIN2 - dd$MARGIN1
                    dd$TOT_SH <- dd$TOTAL2 - dd$TOTAL1
                }
            }
            names(dd)[1:9] <- ddnames
            dd$DEM1_N <- DEM1_N
            dd$REP1_N <- REP1_N
            dd$MAR1_N <- MAR1_N
            dd$TOT1_N <- TOT1_N
            dd$TOT2_N <- TOT2_N
            dd$EQUIP <- 0
            if (!is.null(input$xmodel)){
                #vv <- gvv[grepl(input$xmodel,gvv$Model),]
                #vv <- gvv[gvv$Model %in% input$xmodel,]
                #dd$EQUIP[toupper(dd$COUNTY) %in% toupper(vv$Jurisdiction)] <- 1
                for (i in 1:length(input$xmodel)){
                    #dd[[input$xmodel[i]]] <- 1
                    vv <- ivv[ivv$Model == input$xmodel[i],]
                    dd[[input$xmodel[i]]] <- 0
                    dd[[input$xmodel[i]]][toupper(dd$COUNTY) %in% toupper(vv$Jurisdiction)] <- 1
                    dd[[input$xmodel[i]]] <- as.factor(dd[[input$xmodel[i]]])
                    dd$EQUIP[toupper(dd$COUNTY) %in% toupper(vv$Jurisdiction)] <-
                        dd$EQUIP[toupper(dd$COUNTY) %in% toupper(vv$Jurisdiction)] + 2^(i-1)
                }
            }
            else if (!is.null(input$xmake)){
                #vv <- gvv[grepl(input$xmake,gvv$Make),]
                #vv <- gvv[gvv$Make %in% input$xmake,]
                #dd$EQUIP[toupper(dd$COUNTY) %in% toupper(vv$Jurisdiction)] <- 1
                for (i in 1:length(input$xmake)){
                    #dd[[input$xmake[i]]] <- 1
                    vv <- hvv[hvv$Make == input$xmake[i],]
                    dd[[input$xmake[i]]] <- 0
                    dd[[input$xmake[i]]][toupper(dd$COUNTY) %in% toupper(vv$Jurisdiction)] <- 1
                    dd[[input$xmake[i]]] <- as.factor(dd[[input$xmake[i]]])
                    dd$EQUIP[toupper(dd$COUNTY) %in% toupper(vv$Jurisdiction)] <-
                        dd$EQUIP[toupper(dd$COUNTY) %in% toupper(vv$Jurisdiction)] + 2^(i-1)
                }
            }
            else if (!is.null(input$xequipment)){
                #vv <- gvv[grepl(input$xequipment,gvv$'Equipment Type'),]
                #vv <- gvv[gvv$'Equipment Type' %in% input$xequipment,]
                #dd$EQUIP[toupper(dd$COUNTY) %in% toupper(vv$Jurisdiction)] <- 1
                for (i in 1:length(input$xequipment)){
                    #dd[[input$xequipment[i]]] <- 1
                    vv <- gvv[gvv$'Equipment Type' == input$xequipment[i],]
                    dd[[input$xequipment[i]]] <- 0
                    dd[[input$xequipment[i]]][toupper(dd$COUNTY) %in% toupper(vv$Jurisdiction)] <- 1
                    dd[[input$xequipment[i]]] <- as.factor(dd[[input$xequipment[i]]])
                    dd$EQUIP[toupper(dd$COUNTY) %in% toupper(vv$Jurisdiction)] <-
                        dd$EQUIP[toupper(dd$COUNTY) %in% toupper(vv$Jurisdiction)] + 2^(i-1)
                }
            }
            pp <- getpop(state2)
            pp$COUNTY <- str_to_title(pp$COUNTY) # ensure match on population
            zdd1 <<- dd #DEBUG-RM
            zpp1 <<- pp #DEBUG-RM
            dd <- merge(dd, pp, by = "COUNTY", all.x = TRUE) #include even if no match on population
            names(dd) <- gsub(" ","_",names(dd))
            names(dd) <- gsub("-","_",names(dd))
            names(dd) <- gsub("/","_",names(dd))
            names(dd) <- gsub("&","_",names(dd))
            names(dd) <- gsub("\\(","",names(dd))
            names(dd) <- gsub("\\)","",names(dd))
            zdd <<- dd
            updateCheckboxGroupInput(session,"xevars",
                                     choices = names(dd)[19:NCOL(dd)],
                                     selected = names(dd)[20:NCOL(dd)])
            dd
        }
        getdata <- reactive({
            dd <- getdata_state(input$state2)
            return(dd)
        })
        observeEvent(input$mapsave,{
            eventid <- "Map"
            parmid <- c("minpop", "longoff", "skipcity",
                        "showcity", "maplimitset", "maplimits",
                        "mapyear","mapvar","mapcolors")
            filename <- paste0(data_dir,input$state2,"_",eventid,"_Parms.csv")
            if (file.exists(filename)){
                parms <- read_csv(filename)
                newversion <- max(parms$version) + 1
            }
            else{
                parms <- data.frame(version=integer(),
                                    label=character(),
                                    value=character(),
                                    stringsAsFactors = FALSE)
                newversion <- 1
            }
            nr <- NROW(parms)
            version <- rep(newversion, length(parmid))
            label <- parmid
            value <- NULL
            for (i in 1:length(parmid)){
                value <- c(value, input[[parmid[i]]])
            }
            aparms <- data.frame(version, label, value)
            parms <- rbind(parms, aparms)
            write_csv(parms, filename)
        })
        observe({
            eventid <- "Plot"
            loadid <- "plotload"
            parmid <- c("showrow", "pos1", "pos3", "xscale", "yscale",
                        "xlimit", "xcolor", "xparty", "noparty",
                        "vlimit", "vshape", "vdesc")
            parmup <- c("checkbox", "pos1", "pos3", "xscale", "yscale",
                        "xlimit", "xcolor", "xparty", "select",
                        "vlimit", "vshape", "vdesc")
            filename <- paste0(data_dir,input$state2,"_",eventid,"_Parms.csv")
            if (file.exists(filename)){
                parms <- read_csv(filename)
                loadversion <- input[[loadid]]
                pp <- parms[parms$version == loadversion,]
                for (i in 1:length(parmid)){
                    if (parmup[i] == "numeric"){
                        updateNumericInput(session, parmid[i], value = pp$value[pp$label == parmid[i]])
                    }
                    else if (parmup[i] == "select"){
                        updateSelectInput(session, parmid[i], selected = pp$value[pp$label == parmid[i]])
                    }
                    else if (parmup[i] == "checkbox"){
                        updateCheckboxInput(session, parmid[i], value = as.logical(pp$value[pp$label == parmid[i]]))
                    }
                    else if (parmup[i] == "radio"){
                        updateRadioButtons(session, parmid[i], selected = pp$value[pp$label == parmid[i]])
                    }
                    else{
                        updateTextInput(session, parmid[i], value = pp$value[pp$label == parmid[i]])
                    }
                }
            }
        })
        observeEvent(input$plotsave,{
            eventid <- "Plot"
            parmid <- c("showrow", "pos1", "pos3", "xscale", "yscale",
                        "xlimit", "xcolor", "xparty", "noparty",
                        "vlimit", "vshape", "vdesc")
            filename <- paste0(data_dir,input$state2,"_",eventid,"_Parms.csv")
            if (file.exists(filename)){
                parms <- read_csv(filename)
                newversion <- max(parms$version) + 1
            }
            else{
                parms <- data.frame(version=integer(),
                                    label=character(),
                                    value=character(),
                                    stringsAsFactors = FALSE)
                newversion <- 1
            }
            nr <- NROW(parms)
            version <- rep(newversion, length(parmid))
            label <- parmid
            value <- NULL
            for (i in 1:length(parmid)){
                value <- c(value, input[[parmid[i]]])
            }
            aparms <- data.frame(version, label, value)
            parms <- rbind(parms, aparms)
            write_csv(parms, filename)
        })
        observe({
            eventid <- "Map"
            loadid <- "mapload"
            parmid <- c("minpop", "longoff", "skipcity",
                        "showcity", "maplimitset", "maplimits",
                        "mapyear","mapvar","mapcolors")
            parmup <- c("numeric", "numeric", "skipcity",
                        "showcity", "select", "maplimits",
                        "numeric","select","mapcolors")
            filename <- paste0(data_dir,input$state2,"_",eventid,"_Parms.csv")
            if (file.exists(filename)){
                parms <- read_csv(filename)
                loadversion <- input[[loadid]]
                pp <- parms[parms$version == loadversion,]
                for (i in 1:length(parmid)){
                    if (parmup[i] == "numeric"){
                        updateNumericInput(session, parmid[i], value = pp$value[pp$label == parmid[i]])
                    }
                    else if (parmup[i] == "select"){
                        updateSelectInput(session, parmid[i], selected = pp$value[pp$label == parmid[i]])
                    }
                    else if (parmup[i] == "checkbox"){
                        updateCheckboxInput(session, parmid[i], value = pp$value[pp$label == parmid[i]])
                    }
                    else if (parmup[i] == "radio"){
                        updateRadioButtons(session, parmid[i], selected = pp$value[pp$label == parmid[i]])
                    }
                    else{
                        updateTextInput(session, parmid[i], value = pp$value[pp$label == parmid[i]])
                    }
                }
            }
        })
        observe({
            filename <- paste0("verifier/verifier-machines",input$yeary,".csv")
            if (file.exists(filename)){
                vv <- read_csv(filename,skip = 1)
                #vv <- read.csv(filename,skip = 1,row.names = NULL)
                istate <- which(stabbr == input$state2)
                vv <- vv[vv$State == states[istate],]
                vv$Jurisdiction <- gsub("[A-Za-z /-]*\\(","",vv$Jurisdiction,ignore.case = TRUE)
                vv$Jurisdiction <- gsub("\\)","",vv$Jurisdiction,ignore.case = TRUE)
                vv$Jurisdiction <- gsub(" County$","",vv$Jurisdiction,ignore.case = TRUE)
                gvv <<- vv #DEBUG-RM
                #print(paste0("##### NROW(gvv)=",NROW(gvv))) #DEBUG-CO
                choicelist <- sort(unique(vv$'Equipment Type'))
                updateSelectInput(session = session,"xequipment",choices = choicelist)
                updateSelectInput(session = session,"xmake",choices = "")
                updateSelectInput(session = session,"xmodel",choices = "")
            }
            else{
                print(paste0("########## ",filename," NOT FOUND"))
            }
            
        })
        observeEvent(input$xequipment,{
            vv <- gvv[gvv$'Equipment Type' %in% input$xequipment,]
            choicelist <- sort(unique(vv$Make))
            updateSelectInput(session = session,"xmake",choices = choicelist)
            updateSelectInput(session = session,"xmodel",choices = "")
            hvv <<- vv
            #print(paste0("##### NROW(hvv)=",NROW(hvv))) #DEBUG-CO
        })
        observeEvent(input$xmake,{
            vv <- hvv[hvv$Make %in% input$xmake,]
            choicelist <- sort(unique(vv$Model))
            updateSelectInput(session = session,"xmodel",choices = choicelist)
            ivv <<- vv
            #print(paste0("##### NROW(ivv)=",NROW(ivv))) #DEBUG-CO
        })
        observe({
            cat(file=stderr(), paste0("v3: ",input$state2," ",input$tabs,"\n"))
        })
    }
)
