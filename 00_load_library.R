# Library -----------------------------------------------------------------

pacman::p_load(tidyverse, data.table, lubridate, glue, fs, dtplyr) # 기본 라이브러리 
pacman::p_load(rjson, jsonlite) # JSON

pacman::p_load(plotly, patchwork) # 시각화
pacman::p_load(tidymodels, dtwclust) # ML

pacman::p_load(pbapply) # Progress bar apply

pacman::p_load(httr) # api

# 0. 작업 위치 구하기 ------------------------------------------------------------

project_name = gsub('.+\\/(.+)$', '\\1', rstudioapi::getActiveProject())

dir_home = glue('E:/01_R/PROJECT/{project_name}')
dir_office = glue('D:/study/r/PROJECT/{project_name}/')

get.where_are_you = function(){
  
  # 현재 위치 
  current_place = Sys.getenv("COMPUTERNAME")
  
  # 집 vs 사무실
  dir_home = glue('E:/01_R/PROJECT/{project_name}')
  dir_office = glue('D:/study/r/PROJECT/{project_name}/')
  
  if(current_place == 'DESKTOP-12J3S0E'){
    
    setwd(dir_home)
    print('You are now in Home')
    return('home')
    
  } else if(current_place == 'SUNGWOOK'){
    
    setwd(dir_office)
    print('You are now in Office')
    return('office')
    
  }
  
}

where_are_you = get.where_are_you()

# 파일 경로

get.work_data = function(work_date = NULL){
  
  if(is.null(work_date)){
    
    work_dir = max(list_dir$path)
    
  } else {
    
    work_dir = str_remove_all(work_date, '-')
    
  }
  
  if(where_are_you == 'home'){
    
    ch_dir = glue('{dir_home}/{work_dir}')
    
  } else if(where_are_you == 'office'){
    
    ch_dir = glue('{dir_office}/{work_dir}')
  }
  
  res_dir = structure(
    c(glue('{ch_dir}/people.csv'),
      glue('{ch_dir}/event.csv'),
      glue('{ch_dir}/ids.csv'),
      glue('{ch_dir}/idc_inven.csv'),
      glue('{ch_dir}/idc_gold.csv'),
      glue('{ch_dir}/idc_info.csv'),
      glue('{ch_dir}/idc_login.csv')
    ),
    names = c('people', 'event', 'ids', 'idc_inven', 'idc_gold', 'idc_info', 'idc_login')
  )
  return(res_dir)
  
}

# 0. 구글 시트 ----------------------------------------------------------------

get.google_sheet = function(){
  
  # if(where_are_you == 'home'){
  #   setwd(dir_home)
  #   source('google_sheet_home.R')
  #   
  # } else if(where_are_you == 'office'){
  #   setwd(dir_office)
  #   source('g_sheet_office.R')
  #   
  # }
  # 
  # print(sprintf('GoogleSheet4 Loaded for %s', where_are_you))
  source('google_sheet_home.R')
}

get.google_sheet()


# 0. ggplot ---------------------------------------------------------------

get_bgfilled = function(gg, bg_color = '#cccaca'){
  
  gg + theme(plot.background = element_rect(fill = bg_color),
             legend.background = element_rect(fill = bg_color),
             legend.box.background = element_rect(fill = bg_color)
  ) -> res
  return(gg)
}

get_save = function(plot = plt, w = 8, h = 4, d = 300){
  
  ggsave('plot.png', plot = plt, width = w, height = h, dpi = d)
}
