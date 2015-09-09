
library(proj4)
library(ggplot2)

recorded_data <- read.csv("../data/recorded_data.csv", stringsAsFactors=FALSE)
recorded_data <- recorded_data[recorded_data[,"X_gps_precision"]<5, ]
dim(recorded_data)

recorded_locs <- project(as.matrix(recorded_data[,c('X_gps_longitude', 'X_gps_latitude')]),'+proj=laea +lat_0=5 +lon_0=20 +ellps=WGS84 +units=m +no_defs')

presampled_locs <- read.csv("../data/arusha_testsite_GID100m.csv", stringsAsFactors=FALSE)

laea_presampled_locs <- project(as.matrix(presampled_locs[,c("x","y")]),  '+proj=laea +lat_0=5 +lon_0=20 +ellps=WGS84 +units=m +no_defs')
GID_100m_record = rep(NA, dim(recorded_locs)[1])
for(i in 1:dim(recorded_locs)[1]){
    dist_topresampled <-  sqrt((laea_presampled_locs[,1] - recorded_locs[i,1])^2 + (laea_presampled_locs[,2] - recorded_locs[i,2])^2)
    if(min(dist_topresampled)<100){
        GID_100m_record[i] = presampled_locs[which.min(dist_topresampled),"GID_100m"]          
    }
}
table(GID_100m_record[recorded_data[,'depth']=='top'])

dist(recorded_locs[GID_100m_record=="E1838-S919-18"&recorded_data[,'depth']=="top", ])

lesssample_data=recorded_data[GID_100m_record%in%names(table(GID_100m_record))[table(GID_100m_record)<2],]

lesssample_data[lesssample_data$restrict=="no",]

plot(recorded_data[,c('X_gps_longitude', 'X_gps_latitude')], xlab="Longitude", ylab="Latitude")
points(recorded_data[GID_100m_record%in%names(table(GID_100m_record))[table(GID_100m_record)>2],c('X_gps_longitude', 'X_gps_latitude')], col=2)
points(recorded_data[recorded_data$restrict=="no"&GID_100m_record%in%names(table(GID_100m_record))[table(GID_100m_record)<2],c('X_gps_longitude', 'X_gps_latitude')], col=3)
legend(36.542, -3.49, c("location with more than 2 measurements","location with less than 2 measurements no depth restriction"), cex=0.9,col=c(2, 3), pch=c(1,1))

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


plot(recorded_data[!is.na(GID_100m_record)&!recorded_data[,"ssid"]%in%duplicated_ssid_diffsample,c('X_gps_longitude', 'X_gps_latitude')],col=1, xlim=range(recorded_data[,'X_gps_longitude']), ylim=range(recorded_data[,'X_gps_latitude']), xlab="Longitude", ylab="Latitude")
points(recorded_data[is.na(GID_100m_record),c('X_gps_longitude', 'X_gps_latitude')],col=4)
points(recorded_data[!is.na(GID_100m_record)&recorded_data[,"ssid"]%in%duplicated_ssid_diffsample,c('X_gps_longitude', 'X_gps_latitude')], col=2)
points(presampled_locs[!presampled_locs[,"GID_100m"]%in%GID_100m_record,c("x","y")], col=3, pch=3)
legend(36.562, -3.482,c("recorded, presampled, unique ssid", "presampled but not recorded", "recorded but not presampled", "recorded, presampled, non unique ssid"), cex=0.9, col=c(1,3, 4,2), pch=c(1,3,1,1))

crop_scout_data <- read.csv("../data/recorded_crop_scout_data.csv", stringsAsFactors=FALSE)
cob_count_data <- read.csv("../data/recorded_cob_count_data.csv", stringsAsFactors=FALSE)

laea_crop_scout_locs <- project(as.matrix(crop_scout_data[,c('X_gps_longitude', 'X_gps_latitude')]),'+proj=laea +lat_0=5 +lon_0=20 +ellps=WGS84 +units=m +no_defs')
laea_cob_counts_locs <- project(as.matrix(cob_count_data[,c('X_gps_longitude', 'X_gps_latitude')]),'+proj=laea +lat_0=5 +lon_0=20 +ellps=WGS84 +units=m +no_defs')


GID_100m_record_cropscout = rep(NA, dim(laea_crop_scout_locs)[1])
for(i in 1:dim(laea_crop_scout_locs)[1]){
    dist_topresampled <-  sqrt((laea_presampled_locs[,1] - laea_crop_scout_locs[i,1])^2 + (laea_presampled_locs[,2] - laea_crop_scout_locs[i,2])^2)
    if(min(dist_topresampled)<100){
        GID_100m_record_cropscout[i] = presampled_locs[which.min(dist_topresampled),"GID_100m"]          
    }
}

plot(crop_scout_data[,c('X_gps_longitude', 'X_gps_latitude')], pch=2, xlim=range(recorded_data[,c('X_gps_longitude')]), ylim=range(recorded_data[,c('X_gps_latitude')]), xlab="Longitude", ylab="Latitude")
points(recorded_data[,c('X_gps_longitude', 'X_gps_latitude')], col=2)
points(cob_count_data[,c('X_gps_longitude', 'X_gps_latitude')], col=3)
legend(36.56, -3.485, c("Crop Scout Location", "Soil Recorded Location", "Cob Counts"), col=c(1,2,3), pch=c(2,1,1))
