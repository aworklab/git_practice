pacman::p_load(tidyverse, lubridate, glue, fs, data.table)
pacman::p_load(googlesheets4)

# 0. GS Control -----------------------------------------------------------

gs_loader = function(sheet = 'loader', force = F, mode = 'rrs'){
  
  call_api = function() {
    
    path_key = 'D:/study/r/sqlstudy-247501-3e961ff7fbf5.json'
    googlesheets4::gs4_auth(path = path_key)
    
  }
  
  call_api()
  
  # check env
  
  if(!('code_gs' %in% ls(envir = .GlobalEnv))){
    
    assign('code_gs', new.env(), envir = .GlobalEnv)
    
  }
  
  # check loader 
  
  if(!'loader' %in% ls(code_gs)){
    
    list(bs = '1CgrVo4NdeYvrC9Z1dwHJxDifE-bNudVlmaxcg-67hnI',
         rrs = '17w3bEZfWk8m9zmGwmq4LzY4jut1HfdJQkpFupcw7nps'
    ) -> gs_sys_list
    
    gs_sys = eval(parse(text = glue('gs_sys_list${mode}')))
    
    loader = googlesheets4::read_sheet(gs_sys, 'loader', col_types = c('cccc'))
    
    assign('loader', loader, envir = code_gs)
    
  }
  
  # env에 해당 값 있는지 확인
  
  if(sheet == ''){
    
    return(print('no sheet'))
  }
  
  if(!sheet %in% ls(code_gs) | (sheet %in% ls(code_gs) & force == T)){
    
    call_api()
    
    t_info = dplyr::filter(code_gs$loader, sheets == sheet)
    
    if(!is.na(t_info$col_type)){
      
      temp = googlesheets4::read_sheet(t_info$ss, sheet, col_type = t_info$col_type)
      
    } else {
      
      temp =  googlesheets4::read_sheet(t_info$ss, sheet)
      
    }
    
    assign(sheet, temp, envir = code_gs)
    
    print(sprintf('%s assigned', sheet))
    
    return(temp)
    
  } else {
    
    print(sprintf('%s already existing', sheet))
    
    return(get(sheet, envir = code_gs))
    
  }
}

gs_loader2 = function(sheet = 'loader', force = F, mode = 'rrs'){
  
  call_api = function() {
    
    path_key = 'D:/study/r/sqlstudy-247501-3e961ff7fbf5.json'
    googlesheets4::gs4_auth(path = path_key)
    
  }
  
  call_api()
  
  # check env
  
  if(!('code_gs' %in% ls(envir = .GlobalEnv))){
    
    assign('code_gs', new.env(), envir = .GlobalEnv)
    
  }
  
  # check loader 
  
  if(!'loader' %in% ls(code_gs)){
    
    list(bs = '1CgrVo4NdeYvrC9Z1dwHJxDifE-bNudVlmaxcg-67hnI',
         rrs = '17w3bEZfWk8m9zmGwmq4LzY4jut1HfdJQkpFupcw7nps'
    ) -> gs_sys_list
    
    gs_sys = eval(parse(text = glue('gs_sys_list${mode}')))
    
    loader = googlesheets4::read_sheet(gs_sys, 'loader', col_types = c('cccc'))
    
    assign('loader', loader, envir = code_gs)
    
  }
  
  # env에 해당 값 있는지 확인
  
  if(sheet == ''){
    
    return(print('no sheet'))
  }
  
  call_api()
  
  t_info = dplyr::filter(code_gs$loader, sheets == sheet)
  
  if(!is.na(t_info$col_type)){
    
    temp = googlesheets4::read_sheet(t_info$ss, sheet, col_type = t_info$col_type)
    
  } else {
    
    temp =  googlesheets4::read_sheet(t_info$ss, sheet)
    
  }
  
  assign(sheet, temp, envir = code_gs)
  
  print(sprintf('%s assigned', sheet))
  
  return(temp)
  
  
}

gs_max = function(sheet = ''){
  
  call_api = function() {
    
    path_key = 'D:/study/r/sqlstudy-247501-3e961ff7fbf5.json'
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
    path_key = 'D:/study/r/sqlstudy-247501-3e961ff7fbf5.json'
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
    
    path_key = 'D:/study/r/sqlstudy-247501-3e961ff7fbf5.json'
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