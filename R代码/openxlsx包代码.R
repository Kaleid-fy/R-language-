library(openxlsx)
#####################***********************************************************************************

wb <- createWorkbook()
addWorksheet(wb, "cellIs")
addWorksheet(wb, "Moving Row")
addWorksheet(wb, "Moving Col")
addWorksheet(wb, "Dependent on 1")
addWorksheet(wb, "Duplicates")
addWorksheet(wb, "containsText")
addWorksheet(wb, "colourScale", zoom = 30)
addWorksheet(wb, "databar")
addWorksheet(wb, "between")
addWorksheet(wb, "logical operators")

negStyle <- createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")
posStyle <- createStyle(fontColour = "#006100", bgFill = "#C6EFCE")

## rule applies to all each cell in range
writeData(wb, "cellIs", -5:5)
writeData(wb, "cellIs", LETTERS[1:11], startCol=2)
conditionalFormatting(wb, "cellIs", cols=1, rows=1:11, rule="!=0", style = negStyle)
conditionalFormatting(wb, "cellIs", cols=1, rows=1:11, rule="==0", style = posStyle)

## highlight row dependent on first cell in row
writeData(wb, "Moving Row", -5:5)
writeData(wb, "Moving Row", LETTERS[1:11], startCol=2)
conditionalFormatting(wb, "Moving Row", cols=1:2, rows=1:11, rule="$A1<0", style = negStyle)
conditionalFormatting(wb, "Moving Row", cols=1:2, rows=1:11, rule="$A1>0", style = posStyle)

## highlight column dependent on first cell in column
writeData(wb, "Moving Col", -5:5)
writeData(wb, "Moving Col", LETTERS[1:11], startCol=2)
conditionalFormatting(wb, "Moving Col", cols=1:2, rows=1:11, rule="A$1<0", style = negStyle)
conditionalFormatting(wb, "Moving Col", cols=1:2, rows=1:11, rule="A$1>0", style = posStyle)

## highlight entire range cols X rows dependent only on cell A1
writeData(wb, "Dependent on 1", -5:5)
writeData(wb, "Dependent on 1", LETTERS[1:11], startCol=2)
conditionalFormatting(wb, "Dependent on 1", cols=1:2, rows=1:11, rule="$A$1<0", style = negStyle)
conditionalFormatting(wb, "Dependent on 1", cols=1:2, rows=1:11, rule="$A$1>0", style = posStyle)

## highlight duplicates using default style
writeData(wb, "Duplicates", sample(LETTERS[1:15], size = 10, replace = TRUE))
conditionalFormatting(wb, "Duplicates", cols = 1, rows = 1:10, type = "duplicates")

## cells containing text
fn <- function(x) paste(sample(LETTERS, 10), collapse = "-")
writeData(wb, "containsText", sapply(1:10, fn))
conditionalFormatting(wb, "containsText", cols = 1, rows = 1:10, type = "contains", rule = "A")

## colourscale colours cells based on cell value
df <- read.xlsx(system.file("readTest.xlsx", package = "openxlsx"), sheet = 4)
writeData(wb, "colourScale", df, colNames=FALSE)  ## write data.frame

## rule is a vector or colours of length 2 or 3 (any hex colour or any of colours())
## If rule is NULL, min and max of cells is used. Rule must be the same length as style or NULL.
conditionalFormatting(wb, "colourScale", cols=1:ncol(df), rows=1:nrow(df),
                      style = c("black", "white"), 
                      rule = c(0, 255), 
                      type = "colourScale")

setColWidths(wb, "colourScale", cols = 1:ncol(df), widths = 1.07)
setRowHeights(wb, "colourScale", rows = 1:nrow(df), heights = 7.5) 

## Databars
writeData(wb, "databar", -5:5)
conditionalFormatting(wb, "databar", cols = 1, rows = 1:11, type = "databar") ## Default colours

## Betweem
# Highlight cells in interval [-2, 2]
writeData(wb, "between", -5:5)
conditionalFormatting(wb, "between", cols = 1, rows = 1:11, type = "between", rule = c(-2,2))

## Logical Operators
# You can use Excels logical Opertors
writeData(wb, "logical operators", 1:10)
conditionalFormatting(wb, "logical operators", cols = 1, rows = 1:10,
                      rule = "OR($A1=1,$A1=3,$A1=5,$A1=7)")

saveWorkbook(wb, "conditionalFormattingExample.xlsx", TRUE)


#########################################################################
## Databar Example

wb <- createWorkbook()
addWorksheet(wb, "databar")

## Databars
writeData(wb, "databar", -5:5, startCol = 1)
conditionalFormatting(wb, "databar", cols = 1, rows = 1:11, type = "databar") ## Defaults

writeData(wb, "databar", -5:5, startCol = 3)
conditionalFormatting(wb, "databar", cols = 3, rows = 1:11, type = "databar", border = FALSE)

writeData(wb, "databar", -5:5, startCol = 5)
conditionalFormatting(wb, "databar", cols = 5, rows = 1:11, 
                      type = "databar", style = c("#a6a6a6"), showValue = FALSE) 

writeData(wb, "databar", -5:5, startCol = 7)
conditionalFormatting(wb, "databar", cols = 7, rows = 1:11, 
                      type = "databar", style = c("#a6a6a6"), showValue = FALSE, gradient = FALSE) 

writeData(wb, "databar", -5:5, startCol = 9)
conditionalFormatting(wb, "databar", cols = 9, rows = 1:11, 
                      type = "databar", style = c("#a6a6a6", "#a6a6a6"), showValue = FALSE, gradient = FALSE)

saveWorkbook(wb, file = "databarExample.xlsx", overwrite = TRUE)





#####################***********************************************************************************

######writeDataTable
######Write to a worksheet and format as an Excel table
## see package vignettes for further examples.

#####################################################################################
## Create Workbook object and add worksheets
wb <- createWorkbook()
addWorksheet(wb, "S1")
addWorksheet(wb, "S2")
addWorksheet(wb, "S3")


#####################################################################################
## -- write data.frame as an Excel table with column filters
## -- default table style is "TableStyleMedium2"

writeDataTable(wb, "S1", x = iris)

writeDataTable(wb, "S2", x = mtcars, xy = c("B", 3), rowNames = TRUE,
               tableStyle = "TableStyleLight9")

df <- data.frame("Date" = Sys.Date()-0:19,
                 "T" = TRUE, "F" = FALSE,
                 "Time" = Sys.time()-0:19*60*60,
                 "Cash" = paste("$",1:20), "Cash2" = 31:50,
                 "hLink" = "https://CRAN.R-project.org/", 
                 "Percentage" = seq(0, 1, length.out=20),
                 "TinyNumbers" = runif(20) / 1E9,  stringsAsFactors = FALSE)

## openxlsx will apply default Excel styling for these classes
# class(df$Cash) <- c(class(df$Cash), "currency")
# class(df$Cash2) <- c(class(df$Cash2), "accounting")
# class(df$hLink) <- "hyperlink"
class(df$Percentage) <- c(class(df$Percentage), "percentage")
# class(df$TinyNumbers) <- c(class(df$TinyNumbers), "scientific")

writeDataTable(wb, "S3", x = df, startRow = 4, rowNames = TRUE, tableStyle = "TableStyleMedium9")

#####################################################################################
## Additional Header Styling and remove column filters

writeDataTable(wb, sheet = 1, x = iris, startCol = 7, headerStyle = createStyle(textRotation = 45),
               withFilter = FALSE)


##################################################################################### 
## Save workbook
## Open in excel without saving file: openXL(wb)

saveWorkbook(wb, "writeDataTableExample.xlsx", overwrite = TRUE)


#####################***********************************************************************************
#######writeData {openxlsx}
#######Write an object to a worksheet
## Options for default styling (These are the defaults)
options("openxlsx.borderColour" = "black")
options("openxlsx.borderStyle" = "thin")
options("openxlsx.dateFormat" = "mm/dd/yyyy")
options("openxlsx.datetimeFormat" = "yyyy-mm-dd hh:mm:ss")
options("openxlsx.numFmt" = NULL)

## Change the default border colour to #4F81BD
options("openxlsx.borderColour" = "#4F81BD")


#####################################################################################
## Create Workbook object and add worksheets
wb <- createWorkbook()

## Add worksheets
addWorksheet(wb, "Cars")
addWorksheet(wb, "Formula")


x <- mtcars[1:6,]
writeData(wb, "Cars", x, startCol = 2, startRow = 3, rowNames = TRUE)

#####################################################################################
## Bordering

writeData(wb, "Cars", x, rowNames = TRUE, startCol = "O", startRow = 3, 
          borders="surrounding", borderColour = "black") ## black border

writeData(wb, "Cars", x, rowNames = TRUE, 
          startCol = 2, startRow = 12, borders="columns")

writeData(wb, "Cars", x, rowNames = TRUE, 
          startCol="O", startRow = 12, borders="rows")


#####################################################################################
## Header Styles

hs1 <- createStyle(fgFill = "#DCE6F1", halign = "CENTER", textDecoration = "italic",
                   border = "Bottom")

writeData(wb, "Cars", x, colNames = TRUE, rowNames = TRUE, startCol="B",
          startRow = 23, borders="rows", headerStyle = hs1, borderStyle = "dashed")


hs2 <- createStyle(fontColour = "#ffffff", fgFill = "#4F80BD",
                   halign = "center", valign = "center", textDecoration = "bold",
                   border = "TopBottomLeftRight")

writeData(wb, "Cars", x, colNames = TRUE, rowNames = TRUE,
          startCol="O", startRow = 23, borders="columns", headerStyle = hs2)




##################################################################################### 
## Hyperlinks
## - vectors/columns with class 'hyperlink' are written as hyperlinks'

v <- rep("https://CRAN.R-project.org/", 4)
names(v) <- paste("Hyperlink", 1:4) # Optional: names will be used as display text
class(v) <- 'hyperlink'
writeData(wb, "Cars", x = v, xy = c("B", 32))


##################################################################################### 
## Formulas
## - vectors/columns with class 'formula' are written as formulas'

df <- data.frame(x=1:3, y = 1:3,
                 z = paste(paste0("A", 1:3+1L), paste0("B", 1:3+1L), sep = " + "),
                 stringsAsFactors = FALSE)

class(df$z) <- c(class(df$z), "formula")

writeData(wb, sheet = "Formula", x = df)


##################################################################################### 
## Save workbook
## Open in excel without saving file: openXL(wb)

saveWorkbook(wb, "writeDataExample.xlsx", overwrite = TRUE)


##################################################################################### 
# Merge cells

## Create a new workbook
wb <- createWorkbook()

## Add a worksheet
addWorksheet(wb, "Sheet 1")
addWorksheet(wb, "Sheet 2")

## Merge cells: Row 2 column C to F (3:6)
mergeCells(wb, "Sheet 1", cols = 2, rows = 3:6)

## Merge cells:Rows 10 to 20 columns A to J (1:10)
mergeCells(wb, 1, cols = 1:10, rows = 10:20)

## Intersecting merges
mergeCells(wb, 2, cols = 1:10, rows = 1)
mergeCells(wb, 2, cols = 5:10, rows = 2)
mergeCells(wb, 2, cols = c(1,10), rows = 12) ## equivalent to 1:10 as only min/max are used
#mergeCells(wb, 2, cols = 1, rows = c(1,10)) # Throws error because intersects existing merge

## remove merged cells
removeCellMerge(wb, 2, cols = 1, rows = 1) # removes any intersecting merges
mergeCells(wb, 2, cols = 1, rows = 1:10) # Now this works
writeData(wb, 2,20, xy = c("A", 1))
## Save workbook
saveWorkbook(wb, "mergeCellsExample.xlsx", overwrite = TRUE)
