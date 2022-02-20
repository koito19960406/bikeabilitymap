pacman::p_load(tidyverse,OpenImageR,here,stringr,knitr,DBI)


# loop through image folders and resize images ----------------------------
img_db<-dbConnect(RSQLite::SQLite(), dbname = here("data/img_db.sqlite"))
for (city in c("Singapore","Tokyo")){
  # create output_dir
  output_dir<-here(paste0("inst/app/www/img_resized_",city))
  if (!dir.exists(output_dir)){
    dir.create(output_dir)
  } else {
    print("Dir already exists!")
  }
  
  # resize images if the old folder still exists
  folder_path<-here(paste0("inst/app/www/img_stitched_",city))
  if (dir.exists(folder_path)){
    img_file_list<-list.files(folder_path,full.names = TRUE)
    for (image_file in img_file_list){
      image <- readImage(image_file)
      resized <- resizeImage(image, width = 100, height = 400, normalize_pixels =TRUE)
      writeImage(resized,here(paste0(output_dir,image_file)))
    }
  }
  # if not then convert resized images to data uri and store in sqlite database
  else{
    # initilize dataframe for pano_id and uri
    df<-data.frame() %>% 
      mutate(pano_id="",
             uri="")
    file_list<-list.files(output_dir)
    for (file in file_list){
      # convert image to uri
      pano_id<-str_remove(file,".jpg")
      uri<-image_uri(here(paste0(output_dir,"/",file)))
      df<-df %>% 
        add_row(pano_id=pano_id,
                uri=uri)
    }
    dbWriteTable(img_db, tolower(city), df)
  }
}
dbDisconnect(img_db)

