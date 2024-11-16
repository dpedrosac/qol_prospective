# This is code to display the available data at the distinct German postal codes;
# Code developed by David Pedrosa

# Version 1.4 # 2024-06-12

# ==================================================================================================
## Specify packages of interest and load them automatically if needed
packages = c(
			"sf", "dplyr", "Rcpp", "RColorBrewer", "XML", "ggmap", "tidyverse", "readxl",
			"viridis", "stringr", "spdep", "proj4", "ggplot2", "R6", "readr", "maps", "stringr",
			"sf", "terra", "mapproj", "classInt", "ggthemes") # packages which may be needed over the course

## Load or install all packages defined above
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

# ==================================================================================================
## In case of multiple people working on one project, this helps to create an automatic script
username = Sys.info()["login"]
if (username == "dpedr") {
wdir = "D:/qol_prospective/"
} else if (username == "dpedrosac") {
wdir = "/media/storage/qol_prospective/"
}
setwd(wdir)

# ==================================================================================================
## 1. Load spatial data to workspace
# Load spatial data and convert to dataframe (ggplot2 routines later require dataframes)
spatial_dataGER <- st_read(dsn = file.path(wdir, "data"), layer = "plz-3stellig") %>%
	mutate(area_sqkm = st_area(.) / 1e6)  # st_area returns area in square meters

plz_ids <- spatial_dataGER %>% # extract the plz, assign them an id and remove spatial data
  st_as_sf() %>%
  st_drop_geometry() %>%
  mutate(id = row_number())

# ==================================================================================================
## 2. Load KBV data ("Aerztedichte") (source: https://gesundheitsdaten.kbv.de/cms/html/16402.php)
# csv-data from xlsx modified so that columns have meaningful names (see below) and data is without German "Umlaute"
df_KBV <- read.csv2(file.path(wdir, "data", "aerztedichte_kbv_mod3.csv"))
colnames(df_KBV)[1] = "Regionalschluessel" # no idea why this is necessary!
cols_of_interest = c("Regionalschluessel", "PLZ", "Name", "Regionstyp", "density_physicians_total_by100K_pop", "density_physicians_neurologist_by100K_pop")

df_KBV <- df_KBV %>% # pad postal codes with leading zeros if necessary
  mutate(PLZ = str_pad(PLZ, width = 3, side = "left", pad = "0"))

# Select columns of interest, convert Regionstyp to character, and separate data into different data frames based on Regionstyp
df_KBV_kreise <- df_KBV %>%
  select(all_of(cols_of_interest)) %>%
  mutate(Regionstyp = as.character(Regionstyp)) %>%
  filter(Regionstyp == "Kreise")

df_KBV_ROregionen <- df_KBV %>%
  select(all_of(cols_of_interest)) %>%
  mutate(Regionstyp = as.character(Regionstyp)) %>%
  filter(Regionstyp == "Raumordnungsregionen")

# ==================================================================================================
## 3. Load "Raumordnungsregionen" (source: https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&ved=2ahUKEwjKi7L85dj0AhVB_rsIHT9YChgQFnoECAIQAQ&url=https%3A%2F%2Fwww.bbsr.bund.de%2FBBSR%2FDE%2Fforschung%2Fraumbeobachtung%2FRaumabgrenzungen%2Fdeutschland%2Fregionen%2FRaumordnungsregionen%2Fraumordnungsregionen-2017.xlsx%3F__blob%3DpublicationFile%26v%3D3&usg=AOvVaw3-EcnHoYwbArJyRCivHcqK)
# csv-data from xlsx modified so that no German "Umlaute" are present
df_ROregionen <- read.csv2(file.path(wdir, "data", "raumordnungsregionen_mod.csv"))

# Load PLZ data (source: https://www.suche-postleitzahl.org/downloads)
# csv-data from xlsx modified so that columns have meaningful names (see below) and data is without German "Umlaute"
df_plz2kreis <- read.csv2(file.path(wdir, "data", "zuordnung_plz_ort_landkreis.csv"))
df_plz2kreis <- df_plz2kreis %>%
  mutate(plz3 = str_pad(plz3, width = 3, side = "left", pad = "0")) %>% # adds padding "0" to the left
  mutate(plz2 = str_pad(plz2, width = 2, side = "left", pad = "0")) # adds padding "0" to the left

# ==================================================================================================
## 3. Merge dataframes to one dataframe
# Create csv-file with all information (inhabitants, size, etc.) and remove empty rows
if (file.exists(file.path(wdir, "data", "demographics_per_postal_code.csv"))){
	demographics_df <- read.csv(file.path(wdir, "data", "demographics_per_postal_code.csv"))
} else {

	# Create empty dataframe
	len_dataframe <- length(unique(spatial_dataGER$plz)) # Assuming spatial_dataGER$plz is already defined
	demographics_df <- tibble(
	  plz = rep(NA, len_dataframe),
	  size = rep(NA, len_dataframe),
	  inhabitants = rep(NA, len_dataframe),
	  sqm = rep(NA, len_dataframe),
	  physicians = rep(NA, len_dataframe),
	  neurologists = rep(NA, len_dataframe)
	)

	# Geospatial data and further information from https://www.suche-postleitzahl.org/downloads
	# No bigger modifications needes besides replacing Umlauts and "ß"	
	df_temp <- read.csv(file.path(wdir, "data", "plz_einwohner.csv"))
	df_temp <- df_temp %>%
		mutate(plz = str_pad(as.character(plz), width = 5, pad = "0"))
		
	iter <- 0
	
	# Start for-loop to assign data
	cat("Assigning data from source to geospatial data ...\n")
	pb = txtProgressBar(min = 0, max = length(unique(spatial_dataGER$plz)), initial = 0, style=3)
	for (value in unique(spatial_dataGER$plz)) {
		iter <- iter + 1
		setTxtProgressBar(pb, iter)
		ind_plz1 <- which(substr(df_temp$plz, 1,3)==value)
		ind_plz2 <- which(spatial_dataGER$plz==value)
		ind_plz3 <- which(df_KBV_kreise$PLZ==value)
		
		demographics_df[iter, 1] = as.character(value)
		if (nchar(value) < 3){
			demographics_df[iter, 1] = as.character(paste0("0", value))
		}
		demographics_df[iter, 2] = sum(spatial_dataGER$area_sqkm[ind_plz2])
		demographics_df[iter, 3] = sum(df_temp$einwohner[ind_plz1])
		demographics_df[iter, 4] = demographics_df[iter, 3] / demographics_df[iter, 2]
		if (length(ind_plz3)>1){
			demographics_df[iter, 5] = mean(df_KBV_kreise$density_physicians_total_by100K_pop[ind_plz3])
			demographics_df[iter, 6] = mean(df_KBV_kreise$density_physicians_neurologist_by100K_pop[ind_plz3]) 
		} else if (identical(ind_plz3, integer(0))) {
			if (identical(df_plz2kreis$landkreis[which(df_plz2kreis$plz3==value)][1], "")){
				next
			}
			#break
			
			found_kreise <- unique(df_plz2kreis$landkreis[which(df_plz2kreis$plz3==value)])
			found_kreise = found_kreise[found_kreise != ""][1]
			region = df_ROregionen$ROR11name[which(df_ROregionen$krs17name==found_kreise)]
			if (identical(region, character(0))){
				region = df_ROregionen$ROR11name[which(df_ROregionen$krs17name==paste0(found_kreise, ", Stadt"))]
			}

			if (identical(region, character(0))){
				region = df_ROregionen$ROR11name[which(df_ROregionen$krs17name==strsplit(found_kreise, split=" ")[[1]][2] )]
			}

			
			demographics_df[iter, 5] = df_KBV_ROregionen$density_physicians_total_by100K_pop[which(df_KBV_ROregionen$Name==region)] 
			demographics_df[iter, 6] = df_KBV_ROregionen$density_physicians_neurologist_by100K_pop[which(df_KBV_ROregionen$Name==region)] 
		} else {
			demographics_df[iter, 5] = df_KBV$density_physicians_total_by100K_pop[ind_plz3]
			demographics_df[iter, 6] = df_KBV$density_physicians_neurologist_by100K_pop[ind_plz3]
		}
		if (iter==length(spatial_dataGER$plz)) cat("\nDone!\n")
	}
	close(pb)
	cat("\nDone!\n")
}
demographics_df <- demographics_df[!duplicated(demographics_df), ] # remove duplicates





# Some PLZ need manual refinement, especially major cities

#Berlin
berlin_plzs <- c("101", "102", "103", "104", "105", "106", "107", "108", "109", 
                 "120", "121", "122", "123", "124", "125", "126", "130", "131", 
                 "133", "134", "135", "136", "140", "141")
ind_BRL <- demographics_df$plz %in% berlin_plzs
demographics_df$physicians[ind_BRL] <- df_KBV$density_physicians_total_by100K_pop[
  which(df_KBV$Name == "Berlin, Stadt")]
demographics_df$neurologists[ind_BRL] <- df_KBV$density_physicians_neurologist_by100K_pop[
  which(df_KBV$Name == "Berlin, Stadt")]

# Dresden
ind_DRE <- which(	demographics_df$plz=="010" | 
					demographics_df$plz=="011" |
					demographics_df$plz=="012" | 
					demographics_df$plz=="013")
demographics_df$physicians[ind_DRE] = df_KBV$density_physicians_total_by100K_pop[which(df_KBV$Name=="Dresden, Stadt")]				
demographics_df$neurologists[ind_DRE] = df_KBV$density_physicians_neurologist_by100K_pop[which(df_KBV$Name=="Dresden, Stadt")]				

# Duesseldorf
ind_DUS <- which(	demographics_df$plz=="402" | 
					demographics_df$plz=="404" |
					demographics_df$plz=="405" | 
					demographics_df$plz=="406" |
					demographics_df$plz=="407")
demographics_df$physicians[ind_DUS] = df_KBV$density_physicians_total_by100K_pop[which(df_KBV$Name=="Duesseldorf, Stadt")]				
demographics_df$neurologists[ind_DUS] = df_KBV$density_physicians_neurologist_by100K_pop[which(df_KBV$Name=="Duesseldorf, Stadt")]				

# Hamburg
ind_HH <- which(	demographics_df$plz=="200" | 
					demographics_df$plz=="201" |
					demographics_df$plz=="202" | 
					demographics_df$plz=="203" |
					demographics_df$plz=="204" |
					demographics_df$plz=="205" |
					demographics_df$plz=="210" |
					demographics_df$plz=="211" |
					demographics_df$plz=="212" |
					demographics_df$plz=="223" |
					demographics_df$plz=="224" |
					demographics_df$plz=="225" |
					demographics_df$plz=="226" |
					demographics_df$plz=="227" |
					demographics_df$plz=="228" )
demographics_df$physicians[ind_HH] = df_KBV$density_physicians_total_by100K_pop[which(df_KBV$Name=="Hamburg, Stadt")]				
demographics_df$neurologists[ind_HH] = df_KBV$density_physicians_neurologist_by100K_pop[which(df_KBV$Name=="Hamburg, Stadt")]				

# Hannover
ind_HAN <- which(	demographics_df$plz=="301" | 
					demographics_df$plz=="304" |
					demographics_df$plz=="305" |
					demographics_df$plz=="306" | 
					demographics_df$plz=="308" | 
					demographics_df$plz=="309" )
demographics_df$physicians[ind_HAN] = df_KBV$density_physicians_total_by100K_pop[which(df_KBV$Name=="Region Hannover")]				
demographics_df$neurologists[ind_HAN] = df_KBV$density_physicians_neurologist_by100K_pop[which(df_KBV$Name=="Region Hannover")]				


# Cologne
cologne_plzs <- c("506", "507", "508", "509", "510", "511")
ind_COL <- demographics_df$plz %in% cologne_plzs
cologne_physicians <- df_KBV$density_physicians_total_by100K_pop[which(df_KBV$Name == "Koeln, Stadt")]
cologne_neurologists <- df_KBV$density_physicians_neurologist_by100K_pop[which(df_KBV$Name == "Koeln, Stadt")]

demographics_df$physicians[ind_COL] <- cologne_physicians
demographics_df$neurologists[ind_COL] <- cologne_neurologists

# Stuttgart
stuttgart_plzs <- c("701", "703", "704", "705", "706", "707", "708")
ind_STU <- demographics_df$plz %in% stuttgart_plzs
stuttgart_physicians <- df_KBV$density_physicians_total_by100K_pop[which(df_KBV$Name == "Stuttgart, Stadt")]
stuttgart_neurologists <- df_KBV$density_physicians_neurologist_by100K_pop[which(df_KBV$Name == "Stuttgart, Stadt")]

demographics_df$physicians[ind_STU] <- stuttgart_physicians
demographics_df$neurologists[ind_STU] <- stuttgart_neurologists

# Munich
munich_plzs <- c("803", "805", "806", "808", "809", "812", "813", "814", 
                 "815", "816", "817", "818", "819", "856")
ind_MUN <- demographics_df$plz %in% munich_plzs
munich_physicians <- df_KBV$density_physicians_total_by100K_pop[which(df_KBV$Name == "Muenchen, Stadt")]
munich_neurologists <- df_KBV$density_physicians_neurologist_by100K_pop[which(df_KBV$Name == "Muenchen, Stadt")]

demographics_df$physicians[ind_MUN] <- munich_physicians
demographics_df$neurologists[ind_MUN] <- munich_neurologists

demographics_df <- demographics_df %>% # pad postal codes with leading zeros if necessary
  mutate(plz = str_pad(plz, width = 3, side = "left", pad = "0"))

write.csv(demographics_df,file.path(wdir, "data", "demographics_per_postal_code.csv"), row.names = FALSE) # not working until fix for missing "0" is found


# ==================================================================================================
## Prepare data for plotting population per skm
merge.shp <- sp::merge(spatial_dataGER, demographics_df, by = "plz") 
# merge.shp <- merge.shp[order(merge.shp$order), ]

# Get some meaningful breaks (manually)
manual_breaks <- c(100,150,300,1000,4000)
minVal <- min(merge.shp$sqm, na.rm = T)
maxVal <- max(merge.shp$sqm, na.rm = T)

labels <- c()
brks <- c(minVal, manual_breaks, maxVal)

for(idx in 1:length(brks)){
  labels <- c(labels,round(brks[idx + 1], 2))
}

labels <- labels[1:length(labels)-1]
# define a new variable on the data set just as above
merge.shp$brks <- cut(merge.shp$sqm, 
                     breaks = brks, 
                     include.lowest = TRUE, 
                     labels = labels)

brks_scale <- levels(merge.shp$brks)
labels_scale <- rev(brks_scale)

fig <- ggplot(data=merge.shp) +
	geom_sf(aes(fill=brks, size=.1), color="#666666") + 
	scale_fill_brewer("", drop = FALSE, na.value = "black") + 
	ggplot2::theme_void() +
	ggtitle("People per square kilometers") +
	theme_map() +
    theme(
      legend.position = c(0.5, 0.03),
      legend.text.align = 0,
      legend.background = element_rect(fill = alpha('white', 0.0)),
      legend.text = element_text(hjust = 0, color = "#4e4d47", size= 18),
      plot.title = element_text(hjust = 0.5, color = "#4e4d47", size =24),
      plot.subtitle = element_text(hjust = 0.5, color = "#4e4d47", size=18,
                                   margin = margin(b = -0.1, 
                                                   t = -0.1, 
                                                   l = 2, 
                                                   unit = "cm"), 
                                   debug = F),
      legend.title = element_text(size = 8),
      plot.margin = unit(c(.5,.5,.2,.5), "cm"),
      panel.spacing = unit(c(-.1,0.2,.2,0.2), "cm"),
      panel.border = element_blank(),
      plot.caption = element_text(size = 6, 
                                  hjust = 0.92, 
                                  margin = margin(t = 0.2, 
                                                  b = 0, 
                                                  unit = "cm"), 
                                  color = "#939184")) +
	
	#theme(legend.position = "bottom")+
	labs(x = NULL, 
         y = NULL, 
         title = "Germany's regional demographics", 
         subtitle = "Average population per square kilometer, 2019", 
         caption = "Map CC-BY-SA; Author: AG Bewegungsstoerungen und Neuromodulation, UKGM; \nGeometries: plz-suche.org, 2021; \nData: Zensus, 2011", 
		 size = 18) +
	scale_fill_manual(
          values = rev(brewer.pal(7, "Blues")),
          breaks = rev(brks_scale),
          name = "Population [inhabitants per km^2]",
          #size = 18,
		  drop = FALSE,
          labels = labels_scale,
          guide = guide_legend(
            direction = "horizontal",
            keyheight = unit(2, units = "mm"),
            keywidth = unit(70 / length(labels), units = "mm"),
            title.position = 'top',
            title.hjust = 0.5,
            label.hjust = 1,
            nrow = 1,
            byrow = T,
            reverse = T,
            label.position = "bottom"
          )
      )
fig 

# ==================================================================================================
## Start plotting available number of questionnaires per postal code
df_quest <- read_excel(file.path(wdir, "data", "data_QoL_fin.xlsx"))
colnames(df_quest) <- df_quest[1, ]
df_quest <- df_quest[-c(1, 2), ]

df_data = data.frame(plz=demographics_df$plz, 
					 count_questionnaires=rep(0, length(demographics_df$plz)))
iter <- 0
for (value in df_data$plz) {
	iter <- iter + 1
	if (length(which(substr(df_quest$postal_code,1,3)==value))>0) { 
		df_data$count_questionnaires[iter] <- length(which(substr(df_quest$postal_code,1,3)==value))
	}
}

merge.shp <- sp::merge(spatial_dataGER, df_data, by = "plz")

# Get some meaningful breaks (manually)
manual_breaks <- c(0,1,2,3,4,5)
minVal <- min(merge.shp$count_questionnaires, na.rm = T)
maxVal <- max(merge.shp$count_questionnaires, na.rm = T)

labels <- c()
brks2 <- c(-1, manual_breaks, maxVal)

for(idx in 1:length(brks2)){
  labels <- c(labels,round(brks2[idx + 1], 2))
}

labels <- labels[1:length(labels)-1]
# define a new variable on the data set just as above
merge.shp$brks2 <- cut(merge.shp$count_questionnaires, 
                     breaks = brks2, 
                     include.lowest = TRUE, 
                     labels = labels)

brks_scale <- levels(merge.shp$brks2)
labels_scale <- rev(brks_scale)

fig2 <- ggplot(data=merge.shp) +
	geom_sf(aes(fill=brks2, size=.1), color="#666666") + 
	# geom_polygon(aes(fill = brks), na.rm=FALSE, rule="evenodd", position="identity") +
	#geom_path(data = merge.shp, aes(x = long, 
    #                               y = lat, 
    #                               group = group), 
    #          color = "white", size = 0.001) +
    #coord_map() +
	scale_fill_brewer("", drop = FALSE, na.value = "black") + 
	ggplot2::theme_void() +
	ggtitle("Available questionnaires") +
	theme_map() +
    theme(
      legend.position = c(0.5, 0.03),
      legend.text.align = 0,
      legend.background = element_rect(fill = alpha('white', 0.0)),
      legend.text = element_text(hjust = 0, color = "#4e4d47", size=18),
      plot.title = element_text(hjust = 0.5, color = "#4e4d47", size=24),
      plot.subtitle = element_text(hjust = 0.5, color = "#4e4d47", size=18, 
                                   margin = margin(b = -0.1, 
                                                   t = -0.1, 
                                                   l = 2, 
                                                   unit = "cm"), 
                                   debug = F),
      legend.title = element_text(size = 8),
      plot.margin = unit(c(.5,.5,.2,.5), "cm"),
      panel.spacing = unit(c(-.1,0.2,.2,0.2), "cm"),
      panel.border = element_blank(),
      plot.caption = element_text(size = 6, 
                                  hjust = 0.92, 
                                  margin = margin(t = 0.2, 
                                                  b = 0, 
                                                  unit = "cm"), 
                                  color = "#939184")) +
	
	#theme(legend.position = "bottom")+
	labs(x = NULL, 
         y = NULL, 
         title = "Origin of available questionnaires", 
         subtitle = "Number of questionnaires per postal code", 
         caption = "Map CC-BY-SA; Author: AG Bewegungsstoerungen und Neuromodulation, UKGM; Geometries: plz-suche.org, 2021; Data: own questionnaire") +
	scale_fill_manual(
          values = rev(brewer.pal(7, "Blues")),
          breaks = rev(brks_scale),
          name = "No. of questionnaires",
          drop = FALSE,
          labels = labels_scale,
          guide = guide_legend(
            direction = "horizontal",
            keyheight = unit(2, units = "mm"),
            keywidth = unit(70 / length(labels), units = "mm"),
            title.position = 'top',
            title.hjust = 0.5,
            label.hjust = 1,
            nrow = 1,
            byrow = T,
            reverse = T,
            label.position = "bottom"
          )
      )
fig2
