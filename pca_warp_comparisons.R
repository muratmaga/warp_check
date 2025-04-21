library(geomorph)
library(SlicerMorphR)
#use large_ape_skulls LM file from SlicerMorph sample data
proc.path = "./procrustes/2025-04-20_19_35_56/outputData.csv"
boas.path = "./boas/2025-04-20_19_36_18/outputData.csv"

proc.gpa = arrayspecs(read.csv(proc.path)[,-c(1:3)], p=41, k=3)
boas.gpa = arrayspecs(read.csv(boas.path)[,-c(1:3)], p=41, k=3)

proc.pca = gm.prcomp(A=proc.gpa)
boas.pca = gm.prcomp(A=boas.gpa)

proc.PC1 = proc.pca$x[,1]
boas.PC1 = boas.pca$x[,1]

proc.preds = shape.predictor(proc.gpa, x = proc.PC1, Intercept = FALSE, 
                             pred1 = min(proc.PC1), pred2 = max(proc.PC1)) 

boas.preds = shape.predictor(boas.gpa, x = boas.PC1, Intercept = FALSE, 
                             pred1 = min(boas.PC1), pred2 = max(boas.PC1)) 

#you need to save the file without compression from Slicer to be able to read into R.

ref.model = read.ply("/Users/amaga/Downloads/Gor_template_ASCII.ply")
ref.lm = read.markups.fcsv("/Users/amaga/Downloads/Gorilla_template_LM1.fcsv")

boas.PC1.max = warpRefMesh(ref.model, ref.lm, ref=boas.preds$pred2)
boas.PC1.min = warpRefMesh(ref.model, ref.lm, ref=boas.preds$pred1)

proc.PC1.max = warpRefMesh(ref.model, ref.lm, ref=proc.preds$pred2)
proc.PC1.min = warpRefMesh(ref.model, ref.lm, ref=proc.preds$pred1)


open3d()
shade3d(proc.PC1.max, col="blue")
writePLY(con="/Users/amaga/Desktop/proc_PC1_max.ply")

open3d()
shade3d(proc.PC1.min, col="blue")
writePLY(con="/Users/amaga/Desktop/proc_PC1_min.ply")

open3d()
shade3d(boas.PC1.max, col="blue")
writePLY(con="/Users/amaga/Desktop/boas_PC1_max.ply")

open3d()
shade3d(boas.PC1.min, col="blue")
writePLY(con="/Users/amaga/Desktop/boas_PC1_min.ply")
