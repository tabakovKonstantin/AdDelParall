algorithm_AdDel = function(all_sign, start_points, steps_forward, steps_back, nameFun, max_step, limit_step, iterate_vicinity) {
  
  # ## Input validation ##
  #   
  #   if (steps_forward < 1 || ( is.integer( steps_forward )  ) { 
  #     print("Error. Steps_forward  integer value >= 1 ")
  #     return (0)
  #   }
  #   
  #   if(steps_back < steps_forward || is.integer(steps_back ) ) {
  #     print("Error. Steps_back < Steps_forward  ")
  #     return (0)
  #   }
  #   
  #   if(max_step > 0 || (max_step % 10) == 0) {
  #     print("Error. Max_step integer value > 0  ")
  #     return (0)
  #   }
  #   
  #   if(limit_step >= 0 || (max_step % 10) == 0) {
  #     print("Error. Limit_step integer value >= 0  ")
  #     return (0)
  #   }
  #   
  #   if(percent_vicinity> 0 && percent_vicinity=< 100) {
  #     print("Error. percent_vicinityinteger value > 0 ")
  #     return (0)
  #   }
  ptm = proc.time()
  
  ## Start log ##
  dir.create(paste( path_to_log, name_folder, sep = "/" ), showWarnings = FALSE, recursive = FALSE)
  
  log = c("Algorithm_AdDel", as.character.Date ( Sys.time() ), " " ,"all_sign: ", paste( " # ", as.character( all_sign )), " " , "start_points: ", paste( " # ", as.character( start_points ) ), " " , "steps_forward: ", as.character( steps_forward ), " " , "steps_back: ", as.character( steps_back ), " " , "nameFun: ", as.character( nameFun ), " " , "max_step: ", as.character( max_step ), " " , "limit_step: ", as.character( limit_step ), " " , "iterate_vicinity: ", as.character( iterate_vicinity) )             
  
  name_file = "start_log"
  tmp = str_split_fixed( as.character.Date( Sys.time() ), " " , 2 )
  tmp[2] = str_replace_all( tmp[2], ":", "-" ) 
  name_file = paste( tmp[2], name_file , sep = "_" )
  name_file = paste( name_file, ".txt", sep = "" )
  name_file = paste( name_folder, name_file, sep = "/" )
  name_file = paste( path_to_log, name_file, sep = "/" )
  console = paste( path_to_log, name_folder, "console.txt", sep = "/")
  
  writeLines( log, name_file )
  
  ## Initialization values ##
  args = list()
  
  record_cortege = list()
  record_cortege[1] = "init"
  record_cortege[2] = "init"
  names(record_cortege) = c( "error", "signs" )
  
  all_errors = c()
  
  all_list_record = list()
  
  log_table = data.table( 1 : (length(start_points) ) )
  setnames(log_table, "V1", "sings")
  log_table[, error := 0]
  log_table[, flag := FALSE]
  log_table[, algorithm := "RF"]  
  sings_vec = log_table[, sings]
  flag_vec = log_table[, flag]
  error_vec = log_table[, error]
  
  ## Beginning of the algorithm ##  
  for( i in 1 : length( start_points ) ) {
    
    print("************************************")
    print( "Start_points:" )
    print( start_points[[i]] )
    print( "" )
    
    ## First solution ##
    current_solution = start_points[[i]]
    
    args[1] = list( current_solution )
    #args[1] = current_solution 
    initial_error = do.call( nameFun, args )
    
    record_cortege$error = initial_error
    record_cortege$signs = current_solution
    
    all_errors = c(all_errors, record_cortege$error)
    #     writeLines( all_errors, "errors.csv")
    
    stock_optimism = limit_step
    step = 1
    as.numeric( step ) 
    
    while( step <= max_step & stock_optimism >= 0 ) { 
      
      print( paste("Step:", step) )
      print( paste("Stock_optimism:", stock_optimism) )
      
      log = c("Console:>",  as.character.Date( Sys.time() ), "Step", step, "Stock_optimism", stock_optimism, paste(c(record_cortege$error, record_cortege$signs), collapse=" "))
      writeLines( log, console)
      
      ## Adding signs ##
      for( step_f in 1 : steps_forward  ) {   
        
        ## Creating vicinity solutions ##
        size_vicinity = round( (length( all_sign ) * iterate_vicinity/ 100) )
        vicinity = sample( setdiff( all_sign, current_solution ), size_vicinity ) 
        
        ##  Search for the best solutions ##
        
        ## Initialization ##
        tmp_solution =  unique( c( current_solution[[1]], vicinity[1]) )
        
        args[1] = list( tmp_solution )
        
        min_error = do.call(nameFun, args)
        best_solution = tmp_solution
        
        res = foreach(j = 2 : length( vicinity ) , .multicombine=TRUE) %dopar% {
          
          tmp_solution =  unique( c( current_solution[[1]], vicinity[j]) )
          tmp_error= QSARF_target(tmp_solution)
          list(tmp_solution, tmp_error)
          
        }
        
        min_error = search.best.solution(res)[[2]]
        current_solution[1] = list(search.best.solution(res)[[1]])
        
        ## Adding new record ##      
        if( min_error < record_cortege$error ) {
          record_cortege$error = min_error
          record_cortege$signs = best_solution 
          
          all_errors = c(all_errors, record_cortege$error)
          #           writeLines(all_errors, "errors.csv")
        }
      }
      
      ## Delete signs ##
      for( step_b in 1 : steps_back ) {   
        
        ##  Search for the best solutions ##
        
        ## Initialization ##
        tmp_solution =  setdiff(current_solution[[1]], current_solution[[1]][1])
        
        args[1] = list( tmp_solution )
        
        min_error = do.call( nameFun, args )
        best_solution = tmp_solution
        
        res = foreach(j = 2 : length( current_solution[[1]] ), .multicombine=TRUE) %dopar% {
          
          tmp_solution =  setdiff(current_solution[[1]], current_solution[[1]][j])
          tmp_error= QSARF_target(tmp_solution)
          list(tmp_solution, tmp_error)
          
        }
        
        min_error = search.best.solution(res)[[2]]
        current_solution[1] = list(search.best.solution(res)[[1]])
        
        ## Adding new record ##      
        if( min_error < record_cortege$error ) {
          record_cortege$error = min_error
          record_cortege$signs = best_solution   
          
          all_errors = c(all_errors, record_cortege$error)
          #           writeLines(all_errors, "errors.csv")
          
        }
        
      }
      
      print( paste("current error:", record_cortege$error) )
      
      ## Check for degradation solutions ##
      if ( record_cortege$error < initial_error ) {
        stock_optimism = limit_step
        initial_error = record_cortege$error  
      } else { 
        stock_optimism = stock_optimism - 1
      }  
      
      # Next iteration ##  
      step = step + 1
    } 
    ## Adding new record_cortege ##
    all_list_record[[i]] = record_cortege   
    
    print( "Record_list for this start_point:" )
    print( all_list_record[[i]] )
    
    
    ## log_table ##    
    
    string = all_list_record[[i]][[2]][1]
    if(length(all_list_record[[i]][[2]]) > 1) {
      for( count in 2 : length(all_list_record[[i]][[2]]) ) {
        string = paste ( string, all_list_record[[i]][[2]][count],  sep = " " )
      }
    }    
    
    sings_vec[i] = string
    error_vec[i] = all_list_record[[i]][[1]]
    flag_vec[i] = TRUE
    log_table[, sings := sings_vec]
    log_table[, error :=  error_vec]
    log_table[, flag := flag_vec]
    
    save_log_table = paste( name_folder, "log_table.csv", sep = "/" )
    save_log_table = paste( path_to_log, save_log_table, sep = "/" )
    write.csv2( log_table, save_log_table )
    
  }
  
  proc.time() - ptm
  
  return ( all_list_record )
}
