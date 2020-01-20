# project02445

/src
/project 1 
/model_eval.r  
Denne fil indeholder kode der først konstruere en dataframe ud fra dataen opgiver i armdata.Rdata 
Dernæst konstrueres to modeller KNN og et klassifikations træ - disse trænes ved leave one out cross validation og testes.
(Der laves også en baseline som gætter tilfældigt ) 
Filen indeholder dernæst en McNemar test der sammenligner de to modeller samt baseline.
Derudover laves en masse plots af dataen - disse ligger i bunden af filen. 

/stat_test.r 
Først bliver en dataframe konstrueret. Dernæst laves der anova test i et for loop for alle 300 variable, altså x1, x2...x100 og
y1, y2, z1, z2 osv. 
Derudover indeholder filen også en del plots, bl.a. af p-værdierne. 

/project2
/phosphordata.r 
Denne fil indlæser data fra fosfor_data.Rdata 
Der laves dernæst lineær regression samt ikke lineær regression i R. Der laves plot med begge fits. 
Dernæst laves der en ANCOVA, af DGT som forklarende variabel og OlsenP bagefter. 
