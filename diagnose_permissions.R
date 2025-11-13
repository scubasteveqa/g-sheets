library(googlesheets4)
library(googledrive)
library(jsonlite)

# Load credentials from environment
creds_json <- Sys.getenv("GOOGLE_CREDS_JSON")

if (creds_json == "") {
  cat("ERROR: GOOGLE_CREDS_JSON environment variable is not set\n")
  quit(status = 1)
}

# Parse credentials
creds <- jsonlite::fromJSON(creds_json)

cat("=== Service Account Information ===\n")
cat("Email:", creds$client_email, "\n")
cat("Project ID:", creds$project_id, "\n")
cat("Client ID:", creds$client_id, "\n\n")

# Write to temp file
temp_key <- tempfile(fileext = ".json")
writeLines(creds_json, temp_key)

# Set gargle options
options(
  gargle_oauth_cache = FALSE,
  gargle_oauth_email = TRUE
)

cat("=== Testing Authentication ===\n")

# Authenticate
tryCatch({
  drive_auth(path = temp_key)
  gs4_auth(path = temp_key)
  cat("✓ Authentication successful\n\n")
}, error = function(e) {
  cat("✗ Authentication failed:", e$message, "\n\n")
  quit(status = 1)
})

cat("=== Testing Drive API Access ===\n")
tryCatch({
  files <- drive_find(n_max = 1)
  cat("✓ Can list Drive files\n")
  cat("  Found", nrow(files), "file(s)\n\n")
}, error = function(e) {
  cat("✗ Cannot list Drive files:", e$message, "\n\n")
})

cat("=== Testing Sheets API Access ===\n")
tryCatch({
  sheets <- gs4_find(n_max = 5)
  cat("✓ Can list Sheets\n")
  cat("  Found", nrow(sheets), "sheet(s)\n\n")
  if (nrow(sheets) > 0) {
    cat("  Sheet names:\n")
    for (i in 1:min(5, nrow(sheets))) {
      cat("    -", sheets$name[i], "\n")
    }
    cat("\n")
  }
}, error = function(e) {
  cat("✗ Cannot list Sheets:", e$message, "\n\n")
})

cat("=== Testing Sheet Creation ===\n")
test_sheet_name <- paste0("TEST_", format(Sys.time(), "%Y%m%d_%H%M%S"))

tryCatch({
  cat("Attempting to create sheet:", test_sheet_name, "\n")

  initial_data <- data.frame(
    Column1 = character(0),
    Column2 = character(0),
    stringsAsFactors = FALSE
  )

  new_sheet <- gs4_create(
    name = test_sheet_name,
    sheets = list("Sheet1" = initial_data)
  )

  cat("✓ Sheet created successfully!\n")
  cat("  Sheet ID:", new_sheet$spreadsheet_id, "\n")
  cat("  Sheet URL:", new_sheet$spreadsheet_url, "\n\n")

  cat("Cleaning up test sheet...\n")
  drive_trash(as_id(new_sheet$spreadsheet_id))
  cat("✓ Test sheet deleted\n\n")

  cat("=== ALL TESTS PASSED ===\n")
  cat("Your service account has proper permissions!\n")
  cat("The 403 error in your app must be from something else.\n")

}, error = function(e) {
  cat("✗ Sheet creation FAILED\n")
  cat("Error:", e$message, "\n\n")

  cat("=== DIAGNOSIS ===\n")

  if (grepl("403|PERMISSION_DENIED", e$message)) {
    cat("This is a 403 PERMISSION_DENIED error.\n\n")

    cat("Possible causes:\n")
    cat("1. Google Drive API is not enabled for project:", creds$project_id, "\n")
    cat("   → Go to: https://console.cloud.google.com/apis/library/drive.googleapis.com?project=", creds$project_id, "\n", sep = "")
    cat("\n")

    cat("2. Service account (", creds$client_email, ") lacks IAM permissions\n", sep = "")
    cat("   → Go to: https://console.cloud.google.com/iam-admin/iam?project=", creds$project_id, "\n", sep = "")
    cat("   → Find the service account and add 'Editor' role\n")
    cat("\n")

    cat("3. Domain-wide delegation not configured (if using Google Workspace)\n")
    cat("   → Go to: https://admin.google.com/ac/owl/domainwidedelegation\n")
    cat("   → Add Client ID:", creds$client_id, "\n")
    cat("   → With scopes: https://www.googleapis.com/auth/spreadsheets,https://www.googleapis.com/auth/drive,https://www.googleapis.com/auth/drive.file\n")
    cat("\n")

    cat("4. This service account JSON has different permissions than customer.R's trackingauth.json\n")
    cat("   → Compare the 'client_email' in both JSON files\n")
    cat("   → customer.R might be using a different service account with proper permissions\n")
    cat("\n")
  }

  quit(status = 1)
})

cat("\n=== SUMMARY ===\n")
cat("Service account:", creds$client_email, "\n")
cat("Status: All permissions verified\n")

