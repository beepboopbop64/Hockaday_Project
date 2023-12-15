library(readxl)

# Specify the file path (replace with your actual file path)
file_path <- '../test_upload_data/11_2/The Hockaday Data - 8.1.23 to 11.1.23.xlsx'

# List all sheets in the Excel file
sheets <- excel_sheets(file_path)
print(sheets)
# Find the sheet that contains "Service Logs"
target_sheet <- grep("Service Logs", sheets, value = TRUE)

# Check if the target sheet exists
if(length(target_sheet) > 0) {
  # Read the data from the specific sheet with headers
  data <- read_excel(file_path, sheet = target_sheet)
  print(data)
} else {
  print("No sheet contains 'Service Logs'")
}
