
library(proj4)
library(ggplot2)
names(options())

recorded_data <- read.csv("recorded_data.csv", stringsAsFactors=FALSE)
recorded_data <- recorded_data[recorded_data[,"X_gps_precision"]<5, ]
dim(recorded_data)

recorded_locs <- project(as.matrix(recorded_data[,c('X_gps_longitude', 'X_gps_latitude')]),'+proj=laea +lat_0=5 +lon_0=20 +ellps=WGS84 +units=m +no_defs')

presampled_locs <- read.csv("../../fieldwork/csv/arusha_testsite_GID100m.csv", stringsAsFactors=FALSE)

laea_presampled_locs <- project(as.matrix(presampled_locs[,c("x","y")]),  '+proj=laea +lat_0=5 +lon_0=20 +ellps=WGS84 +units=m +no_defs')
GID_100m_record = rep(NA, dim(recorded_locs)[1])
for(i in 1:dim(recorded_locs)[1]){
    dist_topresampled <-  sqrt((laea_presampled_locs[,1] - recorded_locs[i,1])^2 + (laea_presampled_locs[,2] - recorded_locs[i,2])^2)
    if(min(dist_topresampled)<100){
        GID_100m_record[i] = presampled_locs[which.min(dist_topresampled),"GID_100m"]          
    }
}
table(GID_100m_record)


dist(recorded_locs[GID_100m_record=="E1838-S919-18", ])


lesssample_data=recorded_data[GID_100m_record%in%names(table(GID_100m_record))[table(GID_100m_record)<2],]

lesssample_data[lesssample_data$restrict=="no",]

plot(recorded_locs)
points(recorded_locs[GID_100m_record%in%names(table(GID_100m_record))[table(GID_100m_record)>2],], col=2)
points(recorded_locs[recorded_data$restrict=="no"&GID_100m_record%in%names(table(GID_100m_record))[table(GID_100m_record)<2],], col=3)
legend(1838000, -916000, c("location with \nmore than 2 measurements","location with \nless than 2 measurements \nno restriction"), cex=0.9,col=c(2, 3), pch=c(1,1))

presampled_nonrecorded_GID <- presampled_locs[!presampled_locs[,"GID_100m"]%in%GID_100m_record, "GID_100m"]
presampled_nonrecorded_GID

duplicated_ssid <- names(table(recorded_data$ssid))[table(recorded_data$ssid)>1]

duplicated_ssid_diffsample <- NULL
for(s in duplicated_ssid){
    if(dim(unique(cbind(GID_100m_record[recorded_data[,"ssid"]==s], recorded_data[recorded_data[,"ssid"]==s,'depth'])))[1]>1){
        duplicated_ssid_diffsample <- c(duplicated_ssid_diffsample, s)
    }
}
duplicated_ssid_diffsample

recorded_data[recorded_data$ssid==duplicated_ssid_diffsample[1], ]


duplicatedSSID_GID <- GID_100m_record[recorded_data[,"ssid"]%in%duplicated_ssid_diffsample]
unique(duplicatedSSID_GID)

unique(recorded_data[recorded_data[,"ssid"]%in%duplicated_ssid_diffsample, 'today'])


plot(recorded_locs[!is.na(GID_100m_record)&!recorded_data[,"ssid"]%in%duplicated_ssid_diffsample,],col=1, xlim=range(recorded_locs[,1]), ylim=range(recorded_locs[,2]), xlab="x", ylab="y")
points(recorded_locs[is.na(GID_100m_record),],col=4)
points(recorded_locs[!is.na(GID_100m_record)&recorded_data[,"ssid"]%in%duplicated_ssid_diffsample,], col=2)
points(laea_presampled_locs[!presampled_locs[,"GID_100m"]%in%GID_100m_record,], col=3, pch=3)
legend(1838000, -924000,c("recorded, presampled, unique ssid", "presampled but not recored", "recorded but not presampled", "recorded, presampled, non unique ssid"), cex=0.9, col=c(1,3, 4,2), pch=c(1,3,1,1))

crop_scout_data <- read.csv("recorded_crop_scout_data.csv", stringsAsFactors=FALSE)
cob_count_data <- read.csv("recorded_cob_count_data.csv", stringsAsFactors=FALSE)

laea_crop_scout_locs <- project(as.matrix(crop_scout_data[,c('X_gps_longitude', 'X_gps_latitude')]),'+proj=laea +lat_0=5 +lon_0=20 +ellps=WGS84 +units=m +no_defs')
laea_cob_counts_locs <- project(as.matrix(cob_count_data[,c('X_gps_longitude', 'X_gps_latitude')]),'+proj=laea +lat_0=5 +lon_0=20 +ellps=WGS84 +units=m +no_defs')


GID_100m_record_cropscout = rep(NA, dim(laea_crop_scout_locs)[1])
for(i in 1:dim(laea_crop_scout_locs)[1]){
    dist_topresampled <-  sqrt((laea_presampled_locs[,1] - laea_crop_scout_locs[i,1])^2 + (laea_presampled_locs[,2] - laea_crop_scout_locs[i,2])^2)
    if(min(dist_topresampled)<100){
        GID_100m_record_cropscout[i] = presampled_locs[which.min(dist_topresampled),"GID_100m"]          
    }
}

plot(laea_crop_scout_locs, pch=2, xlim=range(recorded_locs[,1]), ylim=range(recorded_locs[,2]))
points(recorded_locs, col=2)
points(laea_cob_counts_locs, col=3)
legend(1840000, -924000, c("Crop Scout Location", "Soil Recorded Location", "Cob Counts"), col=c(1,2,3), pch=c(2,1,1))
