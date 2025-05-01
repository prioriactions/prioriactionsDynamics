#' Check if solvers are working
#'
#' Provides the status of solver. Being TRUE if it's working fine and FALSE in otherwise.
#'
#' @param package `character` object. Posible values: "gurobi", and "cbc".
#'
#' @examples
#' available_to_solve("cplex")
#'

#' @noRd
available_to_solve <- function(package = ""){
  
  # define primitive data
  nPlants     <- 1
  nWarehouses <- 1
  # Warehouse demand in thousands of units
  Demand      <- c(10)
  # Plant capacity in thousands of units
  Capacity    <- c(20)
  # Fixed costs for each plant
  FixedCosts  <- c(100)
  # Transportation costs per thousand units
  TransCosts  <- c(100)
  
  flowidx <- function(w, p) {nPlants * (w-1) + p}
  
  # Build model
  model <- list()
  model$modelname <- 'facility'
  model$modelsense <- 'min'
  
  # initialize data for variables
  model$lb       <- 0
  model$ub       <- c(rep(1, nPlants),   rep(Inf, nPlants * nWarehouses))
  model$vtype    <- c(rep('B', nPlants), rep('C', nPlants * nWarehouses))
  model$obj      <- c(FixedCosts, TransCosts)
  model$varnames <- c(paste0(rep('Open',nPlants),1:nPlants),
                      sprintf('Trans%d,%d',
                              c(mapply(rep,1:nWarehouses,nPlants)),
                              1:nPlants))
  
  # build production constraint matrix
  A1 <- Matrix::spMatrix(nPlants, nPlants, i = c(1:nPlants), j = (1:nPlants), x = -Capacity)
  A2 <- Matrix::spMatrix(nPlants, nPlants * nWarehouses,
                         i = c(mapply(rep, 1:nPlants, nWarehouses)),
                         j = mapply(flowidx,1:nWarehouses,c(mapply(rep,1:nPlants,nWarehouses))),
                         x = rep(1, nWarehouses * nPlants))
  A3 <- Matrix::spMatrix(nWarehouses, nPlants)
  A4 <- Matrix::spMatrix(nWarehouses, nPlants * nWarehouses,
                         i = c(mapply(rep, 1:nWarehouses, nPlants)),
                         j = mapply(flowidx,c(mapply(rep,1:nWarehouses,nPlants)),1:nPlants),
                         x = rep(1, nPlants * nWarehouses))
  model$A           <- rbind(cbind(A1, A2), cbind(A3, A4))
  model$rhs         <- c(rep(0, nPlants),   Demand)
  model$sense       <- c(rep('<=', nPlants), rep('==', nWarehouses))
  model$constrnames <- c(sprintf('Capacity%d',1:nPlants),
                         sprintf('Demand%d',1:nWarehouses))
  
  
  if(package == "gurobi"){
    # set parameters
    params <- list()
    params$TimeLimit <- 0.01
    params$LogToConsole <- 0
    model$sense <- replace(model$sense, model$sense == "==", "=")
    
    sol <- invisible(try(gurobi::gurobi(model, params), silent = TRUE))
  }
  else if(package == "cbc"){
    # set parameters
    cbc_args <- list()
    cbc_args$sec <- "0.01"
    cbc_args$log <- "0"
    
    constraints_minus_equal <- which(model$sense != "<=")
    constraints_plus_equal <- which(model$sense == "<=")
    row_ub <- model$rhs
    row_ub[constraints_minus_equal] <- Inf
    row_lb <- model$rhs
    row_lb[constraints_plus_equal] <- -Inf
    
    sol <- invisible(try(rcbc::cbc_solve(obj = model$obj,
                                         mat = model$A,
                                         is_integer = ifelse(model$vtype == "B", TRUE, FALSE),
                                         row_ub = row_ub,
                                         row_lb = row_lb,
                                         col_lb = rep(0, length(model$vtype)),
                                         col_ub = model$ub,
                                         max = ifelse(model$modelsense == "min", FALSE, TRUE),
                                         cbc_args = cbc_args), silent = TRUE))
  }
  
  if(inherits(sol, "try-error")){
    return(FALSE)
  }
  else{
    return(TRUE)
  }
}