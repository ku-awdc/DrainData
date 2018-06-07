library('DrainData')

testfiles <- system.file("extdata", c("A.xlsx", "B.xlsx"), package="DrainData")
testdir <- system.file("extdata", package="DrainData")

stopifnot(length(testfiles)==2)
stopifnot(length(testdir)==1 && testdir!="")

files <- c(testfiles, testdir, testdir, 'cock.txt', system.file("DESCRIPTION", package="DrainData"))

# Test a basic run:
newdat1 <- DrainData:::read_files(files, merge_by_name=TRUE, quiet=FALSE, current_therapy=DrainData:::blank_therapy_df)
stopifnot(length(unique(newdat1$Therapy$Identifier))==2)
stopifnot(all(newdat1$Data$Identifier %in% newdat1$Therapy$Identifier))
stopifnot(all(newdat1$Events$Identifier %in% newdat1$Therapy$Identifier))

# Test that matching identifiers based on name/DOB works:
newdat2 <- DrainData:::read_files(files, merge_by_name=TRUE, quiet=FALSE, current_therapy=newdat1$Therapy)
stopifnot(all(newdat2$Therapy$Identifier %in% newdat1$Therapy$Identifier))


# Basic test of full function:
dd <- DrainData(files)
head(dd$GetTherapy())
head(dd$GetData())
head(dd$GetEvents())
