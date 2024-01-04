# pacman::p_load(tidyverse, lubridate, glue, fs, data.table)
# pacman::p_load(googlesheets4)

pacman::p_load(tidyverse, data.table, lubridate, glue, fs, dtplyr) # 기본 라이브러리 
pacman::p_load(rjson, jsonlite) # JSON

pacman::p_load(plotly, patchwork) # 시각화
pacman::p_load(tidymodels, dtwclust) # ML

pacman::p_load(pbapply) # Progress bar apply

pacman::p_load(httr) # api
pacman::p_load(googlesheets4)

# 0. GS Renewal -----------------------------------------------------------

gs_master = function(){
  
  call_api = function() {
    
    path_key_url = 'https://github.com/aworklab/everydayPython/files/13760630/sqlstudy-247501-3e961ff7fbf5.json'
    googlesheets4::gs4_auth(path = path_key_url)
    
  }
  
  call_api()
  
  # check env
  
  if(!('code_gs' %in% ls(envir = .GlobalEnv))){
    
    assign('code_gs', new.env(), envir = .GlobalEnv)
    
  }
  
  # loader master key value 
  
  gs_sys_master = '1CA-1Z9wPWVEXcxGcSZ9_dUXeii_uj9bQDnCgkuBylPc'
  loader_master = googlesheets4::read_sheet(gs_sys_master, 'loader')
  
  assign('loader_master', loader_master, envir = code_gs)
  
  # Load Full key list 
  
  loader_master %>% 
    filter(!is.na(key_id)) %>% 
    mutate(fit = map(key_id, 
                     ~{
                       googlesheets4::read_sheet(.x, 'loader', col_types = c('cccc'))
                     })) -> loader
  
  loader %>% select(-key_id) %>% 
    unnest(fit) -> loader
  
  assign('loader', loader, envir = code_gs)
  
  # Assign Project name 
  
  print(sprintf('Loader Key id assiged : %s', paste0(unique(loader$project_id), collapse = ', ')))
}

gs_loader = function(sheet_is = 'loader', project_is = 'RRS'){
  
  # Master Key가 없으므로 gs_master 실행을 요청
  tryCatch(
    {length(ls(code_gs))},
    error = function(e){
      gs_master()
    }
  )
  
  if(length(ls(code_gs)) == 0){
    
    gs_master()
    
  }
  
  # Sheet 지정 값이 없음
  if(is.null(sheet_is) || sheet_is == ''){
    
    return(print('No Sheet'))
  }
  
  # Env Loader에서 필요한 ss 정보 검색
  loader_wanted = code_gs$loader %>% filter(project_id == project_is & sheets == sheet_is)
  
  
  # 필요한 정보를 찾았을 경우 GS에서 값을 호출
  if(nrow(loader_wanted) != 0){
    
    loader = googlesheets4::read_sheet(loader_wanted$ss, loader_wanted$sheets)
    
    print(sprintf('[%s]Sheet %s loaded', project_is, sheet_is))
    
    return(loader)
  
  # Sheet 정보가 없을 경우 중지  
  } else {
    
    return(print(sprintf('[%s]Sheet %s is not found', project_is, sheet_is)))
    
  }
  
}


# 0. GS Control -----------------------------------------------------------

gs_max = function(sheet = ''){
  
  call_api = function() {
    
    path_key = 'E:/01_R/script/sqlstudy-247501-3e961ff7fbf5.json'
    googlesheets4::gs4_auth(path = path_key)
  }
  call_api()
  
  # check env
  
  if(!('code_gs' %in% ls(envir = .GlobalEnv))){
    
    assign('code_gs', new.env(), envir = .GlobalEnv)
    
  }
  
  # check loader 
  
  if(!'loader' %in% ls(code_gs)){
    
    gs_sys = '1CgrVo4NdeYvrC9Z1dwHJxDifE-bNudVlmaxcg-67hnI'
    loader = googlesheets4::read_sheet(gs_sys, 'loader', col_types = c('cccc'))
    
    assign('loader', loader, envir = code_gs)
    
  }
  
  # env에 해당 값 있는지 확인
  
  if(sheet == ''){
    
    return(print('no sheet'))
  }
  
  # sheet 정보 확인
  
  sheet_info = code_gs$loader %>% filter(sheets == sheet)
  sheet_summary = googlesheets4::gs4_get(sheet_info$ss)$sheets
  sheet_grid = dplyr::filter(sheet_summary, name == sheet)$grid_rows
  
  # 마지막 정보 호출
  
  names = sprintf('%s!%s:%s', sheet_info$sheets, 1, 2)
  names = googlesheets4::read_sheet(sheet_info$ss, range = names) %>% names
  
  ranged = sprintf('%s!%s:%s', sheet_info$sheets, (sheet_grid-10), sheet_grid)
  ranged = googlesheets4::read_sheet(sheet_info$ss, range = ranged)
  names(ranged) = names
  
  return(ranged)
}

gs_update = function(sheet = '', info = NULL){
  
  # gs4_auth_configure(api_key = 'AIzaSyAM9MiGMmHS2oNi_R_IVYrJ4LdcreXWAxU')
  
  call_api = function() {
    
    # googlesheets4::gs4_auth_configure(api_key = 'AIzaSyAM9MiGMmHS2oNi_R_IVYrJ4LdcreXWAxU')
    path_key = 'E:/01_R/script/sqlstudy-247501-3e961ff7fbf5.json'
    googlesheets4::gs4_auth(path = path_key)
  }
  call_api()
  
  # check env
  
  if(!('code_gs' %in% ls(envir = .GlobalEnv))){
    
    assign('code_gs', new.env(), envir = .GlobalEnv)
    
  }
  
  # check loader 
  
  if(!'loader' %in% ls(code_gs)){
    
    gs_sys = '1CgrVo4NdeYvrC9Z1dwHJxDifE-bNudVlmaxcg-67hnI'
    loader = googlesheets4::read_sheet(gs_sys, 'loader', col_types = c('cccc'))
    assign('loader', loader, envir = code_gs)
    
  }
  
  # env에 해당 값 있는지 확인
  
  if(sheet == ''){
    
    return(print('no sheet'))
    
  } else if (is.null(info)){
    
    return(print('nothing to update'))
    
  }
  
  t_info = dplyr::filter(code_gs$loader, sheets == sheet)
  
  while(1){
    
    loader = tryCatch({
      googlesheets4::write_sheet(info, ss = t_info$ss, sheet = t_info$sheets)
    },
    error = function(e) {e}
    )
    
    if(inherits(loader, 'error')){
      googlesheets4::gs4_deauth()
      googlesheets4::gs4_auth('aworklab@gmail.com')
    } else {
      
      break
      
    }
    
  } # while문 종료
  
  # write_sheet(info, ss = t_info$ss, sheet = t_info$sheets)
  print(sprintf('update in %s', t_info$sheets))
  
  
}

gs_append = function(the_sheet = '', info = NULL){
  
  call_api = function() {
    
    path_key = 'E:/01_R/script/sqlstudy-247501-3e961ff7fbf5.json'
    googlesheets4::gs4_auth(path = path_key)
  }
  call_api()
  
  # check env
  
  if(!('code_gs' %in% ls(envir = .GlobalEnv))){
    
    assign('code_gs', new.env(), envir = .GlobalEnv)
    
  }
  
  # check loader 
  
  if(!'loader' %in% ls(code_gs)){
    
    gs_sys = '1CgrVo4NdeYvrC9Z1dwHJxDifE-bNudVlmaxcg-67hnI'
    loader = googlesheets4::read_sheet(gs_sys, 'loader', col_types = c('cccc'))
    assign('loader', loader, envir = code_gs)
    
  }
  
  # env에 해당 값 있는지 확인
  
  if(sheet == ''){
    
    return(print('no sheet'))
    
  } else if (is.null(info)){
    
    return(print('nothing to update'))
    
  } else if (nrow(info) == 0){
    
    return(print('nrow is 0'))
    
  }
  
  t_info = dplyr::filter(code_gs$loader, sheets == the_sheet)
  
  googlesheets4::sheet_append(t_info$ss, info, sheet = the_sheet)
  
  print(sprintf('appended in %s', t_info$sheets))
  
}



# 0. code -----------------------------------------------------------------
# 
# pacman::p_load(stringi)
# 
# df = gs_loader('idx_fail')
# 
# df %>% 
#   select(nick) %>% 
#   mutate(nick2 = map_chr(nick, 
#                      ~{
#                        temp = .x
#                        res = stri_unescape_unicode(gsub('<U\\+(....)>', '\\\\u\\1', temp))
#                        return(res)
#                      }
#                      ))
# 
# gsub('<U\\+(....)>', '\\\\u\\1', 'ス<U+30FC>パ<U+30FC>田之助')
# stri_unescape_unicode(gsub('<U\\+(....)>', '\\\\u\\1', 'ス<U+30FC>パ<U+30FC>田之助'))
# stri_unescape_unicode('\\u30FC')
# 
# temp = 'ス<U+30FC>パ<U+30FC>田之助'
# stri_unescape_unicode(gsub('<U\\+(....)>', '\\\\u\\1', temp))
# 
# 
# df

