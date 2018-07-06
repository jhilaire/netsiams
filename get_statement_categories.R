get_statement_categories <- function() {
  cat <- list()
  cat[["broad"]]      <- c("Z2"="Pathways", "Z3"="Costs", "Z4"="Technology", "Z5"="Climate", "Z6"="Other")
  cat[["boundaries"]] <- c("H2"="2C", "H3"="1.5C", "H4"="Delay", "H5"="Socio-Econ.", "H6"="Portfolio", "H7"="Lim. Tech.")
  cat[["dynamics"]]   <- c("D1"="Dynamics", "D2"="Upscaling", "D3"="Residual", "D4"="Fossil fuels", "D6"="Flexibility")
  cat[["costs"]]      <- c("C1"="Lower", "C3"="Burden", "C4"="Distribution", "C5"="Carbon price")
  cat[["tech"]]       <- c("T1"="Versatile", "T2"="Food price", "T5"="Obstruction")
  cat[["climate"]]    <- c("O1"="Overshoot", "O2"="Tipping")
  cat[["other"]]      <- c("R1"="Important")
  
  return(cat)
}