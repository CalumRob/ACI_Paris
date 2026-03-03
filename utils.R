# =============================================================================
# utils.R — Shared helpers for ACI_Paris R scripts
# =============================================================================

# 1. Load environment variables from .env
.env_path <- ".env"
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  .doc_path <- rstudioapi::getActiveDocumentContext()$path
  if (nzchar(.doc_path)) {
    .candidate <- file.path(dirname(.doc_path), ".env")
    if (file.exists(.candidate)) .env_path <- .candidate
  }
}

if (file.exists(.env_path)) {
  readRenviron(.env_path)
  message("[utils.R] Loaded environment variables from: ", .env_path)
} else {
  warning("[utils.R] .env file not found. Set environment variables manually or create a .env file.")
}

.get_project_root <- function() {
  root <- Sys.getenv("ACI_PARIS_ROOT", unset = "")
  if (nchar(root) > 0) return(root)
  warning("[utils.R] ACI_PARIS_ROOT not set. Falling back to working directory: ", getwd())
  return(getwd())
}

PROJECT_ROOT <- .get_project_root()

proj_path <- function(...) {
  file.path(PROJECT_ROOT, ...)
}

# 3. Database connection helper
get_db_conn_supa <- function() {
  required_vars <- c("DB_NAME_REMOTE", "DB_HOST_REMOTE", "DB_PORT_REMOTE", "DB_USER_REMOTE", "DB_PASSWORD_REMOTE")
  missing_vars  <- required_vars[Sys.getenv(required_vars) == ""]
  if (length(missing_vars) > 0) {
    stop("[utils.R] Missing required env vars for DB connection: ",
         paste(missing_vars, collapse = ", "),
         "\nPlease set them in your .env file.")
  }
  
  DBI::dbConnect(
    RPostgres::Postgres(),
    dbname   = Sys.getenv("DB_NAME_REMOTE"),
    host     = Sys.getenv("DB_HOST_REMOTE"),
    port     = as.integer(Sys.getenv("DB_PORT_REMOTE")),
    user     = Sys.getenv("DB_USER_REMOTE"),
    password = Sys.getenv("DB_PASSWORD_REMOTE")
  )
}
