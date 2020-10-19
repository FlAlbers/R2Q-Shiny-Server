#test um bib-Format zusammen zu fügen

library(bib2df)
bib1 <- as.character("../mergebib_test/Entwurfsprojekt.bib")
bib2 <- as.character("../mergebib_test/TestbibR.bib")
paths <- c(bib1, bib2)
x <- lapply(paths, bib2df)

res <- dplyr::bind_rows(x)

df2bib(res, file = "../mergebib_test/mergedbib.bib")


