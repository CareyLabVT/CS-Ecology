# cs-ecology data coallate

rm(list=ls())

options(scipen = 999)
library(dplyr)

setwd('/Users/nicoleward/Documents/VirginiaTech/Manuscripts/CS_Collaboration/rawdata/csv') 
files<-list.files("/Users/nicoleward/Documents/VirginiaTech/Manuscripts/CS_Collaboration/rawdata/csv")

#for (i in 1:length(files)) assign(files[i], read.csv(files[i]))
List = lapply(files, read.csv, stringsAsFactors=FALSE)

require(plyr)
df<-rbind.fill(lapply(List,function(y){as.data.frame((y),stringsAsFactors=FALSE)}))
df$paper_ID<-seq(1:nrow(df))


#data = matrix(data = NA, nrow = (nrow(List[[1]])*length(files)), ncol = 100)
data = matrix(data = NA, nrow = (nrow(df)), ncol = 100)



counter = 1
counter2 = 0
#for(k in 1:length(files)){## line 430 in 1001_1500 is messed up, 3 papers one line...
for(k in 1:nrow(df)){#

  yr.in<-df$PY[k]
  id.in<-df$paper_ID[k]
  aff_1<-df$C1[k]
  aff_2<-df$RP[k]
    counter2 = counter2+1
    
    name=strsplit(aff_1,',')
    if(is.na(aff_2)==FALSE){
      name2=strsplit(aff_2,',')
    } else(name2=NA)
    nameorg = data.frame(name)
    nameorg2 = data.frame(name2)

    counter = 1
    if(nrow(nameorg)>0){
    for (i in 1:length(nameorg[,1])) { 
      if ((length(grep("Comp", nameorg[i,1])) != 0) || (length(grep("COMP", nameorg[i,1])) != 0) ||
        (length(grep("Dept", nameorg[i,1])) != 0) || (length(grep("Fac", nameorg[i,1])) != 0) || 
          (length(grep("Biol", nameorg[i,1])) != 0) || (length(grep("Grp", nameorg[i,1])) != 0) || 
          (length(grep("Ctr", nameorg[i,1])) != 0) || (length(grep("Lab", nameorg[i,1])) != 0)  ||
          (length(grep("Inst", nameorg[i,1])) != 0) || (length(grep("DEPT", nameorg[i,1])) != 0) ||
          (length(grep("BIOL", nameorg[i,1])) != 0) || (length(grep("GRP", nameorg[i,1])) != 0) ||
          (length(grep("CTR", nameorg[i,1])) != 0) || (length(grep("LAB", nameorg[i,1])) != 0)  ||
          (length(grep("INST", nameorg[i,1])) != 0) || (length(grep("FAC", nameorg[i,1])) != 0) ||
        (length(grep("UNIV", nameorg[i,1])) != 0) || (length(grep("univ", nameorg[i,1])) != 0) ||
          (length(grep("Sch", nameorg[i,1])) != 0) || (length(grep("SCH", nameorg[i,1])) != 0)){
        data[counter2,counter+2] = as.character(factor(nameorg[i,1]))
        counter = counter + 1
        data[counter2,1]<-id.in
        data[counter2,2]<-yr.in
      }
    }
    }
    if(nrow(nameorg)==0){
      for(i in 1:length(nameorg2[,1])){
        if ((length(grep("Comp", nameorg2[i,1])) != 0) || (length(grep("COMP", nameorg2[i,1])) != 0) ||
          (length(grep("Dept", nameorg2[i,1])) != 0) || (length(grep("Fac", nameorg2[i,1])) != 0) || 
            (length(grep("Biol", nameorg2[i,1])) != 0) || (length(grep("Grp", nameorg2[i,1])) != 0) || 
            (length(grep("Ctr", nameorg2[i,1])) != 0) || (length(grep("Lab", nameorg2[i,1])) != 0)  ||
            (length(grep("Inst", nameorg2[i,1])) != 0) || (length(grep("DEPT", nameorg2[i,1])) != 0) ||
            (length(grep("BIOL", nameorg2[i,1])) != 0) || (length(grep("GRP", nameorg2[i,1])) != 0) ||
            (length(grep("CTR", nameorg2[i,1])) != 0) || (length(grep("LAB", nameorg2[i,1])) != 0)  ||
            (length(grep("INST", nameorg2[i,1])) != 0) || (length(grep("FAC", nameorg2[i,1])) != 0)||
            (length(grep("Sch", nameorg2[i,1])) != 0) || (length(grep("SCH", nameorg2[i,1])) != 0)) {
          data[counter2,counter+2] = as.character(factor(nameorg2[i,1]))
          counter = counter + 1
          data[counter2,1]<-id.in
          data[counter2,2]<-yr.in
        } 
      }
    }
  }



#names = names[1:(counter-1),]
clean<-data[rowSums(is.na(data)) != ncol(data),]
clean<-as.data.frame(clean)


count_na <- function(x) sum(!is.na(x))

clean$count_aff <- clean %>%apply(., 1, count_na)
index<-max(clean$count_aff)
results<-matrix(data = NA, nrow = (nrow(clean)), ncol = index)
results<-as.data.frame(results)
categories<-as.data.frame(matrix(data=NA,nrow=nrow(clean),ncol = 13))
names(categories)[names(categories)==c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10","V11","V12","V13")] <- 
  c("paper_ID","Non_Env_Biology","Env_Biology","Earth_Science","Math","Computer_Science",
    "Social_Sciences","Humanities","Chemistry","Engineering","Physics","Education","Architecture")

clean.pre<-clean
leftover<-as.data.frame(matrix(data=NA,nrow=10000,ncol=2))
names(leftover)[names(leftover)==c("V1","V2")]<-c("aff","paper_ID")

for(i in 1:nrow(clean)){
  in.j<-clean$count_aff[i]
  id.in<-as.numeric(as.character(clean[i,1]))
  results[i,1]<-id.in
  categories$paper_ID[i]<-id.in
  for(j in 1:in.j){
    cat.index<-as.character(clean[i,j+2])
    counter = 1
    if ((length(grep("comp sci", cat.index)) != 0)|| (length(grep("Comp Sci", cat.index)) != 0) ||
        (length(grep("COMP SCI", cat.index)) != 0)||(length(grep("Computat Sci", cat.index)) != 0) ||
        (length(grep("COMPUTAT SCI", cat.index)) != 0)||
        (length(grep("MADALGO", cat.index)) != 0)|| (length(grep("Computat", cat.index)) != 0)|| 
        (length(grep("Complejidad", cat.index)) != 0)|| (length(grep("Eugene Lawler", cat.index)) != 0)||
        (length(grep("Planck Inst", cat.index)) != 0)||  (length(grep("Dworkin Lab", cat.index)) != 0)){
      results[i,counter+1] = "Computer Science"
      counter = counter + 1
      clean[i,j+2]<-NA
      categories$Computer_Science[i]<-1
    }
    if ((length(grep("math", cat.index)) != 0)|| (length(grep("Math", cat.index)) != 0) ||
        (length(grep("MATH", cat.index)) != 0)|| (length(grep("Stat", cat.index)) != 0) ||
        (length(grep("STAT", cat.index)) != 0)||(length(grep("Santa Fe Inst", cat.index)) != 0)||          
        (length(grep("Nevanlinna Inst", cat.index)) != 0)||(length(grep("DEPT MATEMAT", cat.index)) != 0)){
      results[i,counter+1] = "Math"
      counter = counter + 1
      clean[i,j+2]<-NA
      categories$Math[i]<-1
    }
    if ((length(grep("Culture", cat.index)) != 0)|| (length(grep("CULTURE", cat.index)) != 0)||
        (length(grep("POLIT", cat.index)) != 0)|| (length(grep("Polit", cat.index)) != 0)||
        (length(grep("PSYCHIAT", cat.index)) != 0) || (length(grep("Psychiat", cat.index)) != 0)||
        (length(grep("GOVT", cat.index)) != 0) || (length(grep("Govt", cat.index)) != 0)||
        (length(grep("ANTHROPOL", cat.index)) != 0) || (length(grep("Anthropol", cat.index)) != 0)||
        (length(grep("SOCIAL", cat.index)) != 0) || (length(grep("Social", cat.index)) != 0)||
        (length(grep("Law", cat.index)) != 0)||(length(grep("LAW", cat.index)) != 0)||
        (length(grep("Woodrow Wilson Sch", cat.index)) != 0)||(length(grep("Oxford Martin Sch", cat.index)) != 0)||               
        (length(grep("Pondichery", cat.index)) != 0)|| (length(grep("Saami Studies", cat.index)) != 0)||      
        (length(grep("Ctr Noumea", cat.index)) != 0)||(length(grep("Sociol", cat.index)) != 0)|| 
        (length(grep("COMMERCE", cat.index)) != 0)||(length(grep("BUSINESS", cat.index)) != 0)){
      results[i,counter+1] = "Social Sciences"
      counter = counter + 1
      clean[i,j+2]<-NA
      categories$Social_Science[i]<-1
    }
    if ((length(grep("Engn", cat.index)) != 0)|| (length(grep("ENGN", cat.index)) != 0)||
        (length(grep("Control Sci", cat.index)) != 0) ){
      results[i,counter+1] = "Engineering"
      counter = counter + 1
      clean[i,j+2]<-NA
      categories$Engineering[i]<-1
    }
    if ((length(grep("Educ", cat.index)) != 0)|| (length(grep("CURRICULUM", cat.index)) != 0)){
      results[i,counter+1] = "Education"
      counter = counter + 1
      clean[i,j+2]<-NA
      categories$Education[i]<-1
    }
    if ((length(grep("Architecture", cat.index)) != 0)){
      results[i,counter+1] = "Architecture"
      counter = counter + 1
      clean[i,j+2]<-NA
      categories$Architecture[i]<-1
    }
    
    if ((length(grep("English", cat.index)) != 0)|| (length(grep("ENGLISH", cat.index)) != 0) ||
        (length(grep("Latin", cat.index)) != 0)||(length(grep("LATIN", cat.index)) != 0)||
        (length(grep("LIBERAL ARTS", cat.index)) != 0)||(length(grep("BUNTING INST", cat.index)) != 0)||
        (length(grep("LITERATURE", cat.index)) != 0)||(length(grep("BUNTING INST", cat.index)) != 0)||
        (length(grep("ARCHEOL", cat.index)) != 0)||(length(grep("Arqueol", cat.index)) != 0)){
      results[i,counter+1] = "Humanities"
      counter = counter + 1
      clean[i,j+2]<-NA
      categories$Humanities[i]<-1
    }
    if ((length(grep("Phys", cat.index)) != 0)|| (length(grep("PHYS", cat.index)) != 0) ||
        (length(grep("Langmuir", cat.index)) != 0)|| (length(grep("LANGMUIR", cat.index)) != 0)||
        (length(grep("Planck Inst", cat.index)) != 0)||(length(grep("Gleb Wataghin", cat.index)) != 0)||             
        (length(grep("Reg Bariloche", cat.index)) != 0)|| (length(grep("Inst Balseiro", cat.index)) != 0)|| 
        (length(grep("Ctr Atom Bariloche", cat.index)) != 0)||  (length(grep("Nucl", cat.index)) != 0)|| 
        (length(grep("NUCL", cat.index)) != 0)){
      results[i,counter+1] = "Physics"
      counter = counter + 1
      clean[i,j+2]<-NA
      categories$Physics[i]<-1
    }
    if ((length(grep("Chem", cat.index)) != 0)|| (length(grep("CHEM", cat.index)) != 0)){
      results[i,counter+1] = "Chemistry"
      counter = counter + 1
      clean[i,j+2]<-NA
      categories$Chemistry[i]<-1
    }
    if ((length(grep("geo", cat.index)) != 0)|| (length(grep("Geo", cat.index)) != 0) ||
        (length(grep("GEO", cat.index)) != 0)|| (length(grep("Geol", cat.index)) != 0) ||
        (length(grep("Earth", cat.index)) != 0)|| (length(grep("GEOL", cat.index)) != 0) ||
        (length(grep("EARTH", cat.index)) != 0)|| (length(grep("Meteorol", cat.index)) != 0)||
        (length(grep("Spatial", cat.index)) != 0)|| (length(grep("SPATIAL", cat.index)) != 0)||
        (length(grep("Energy", cat.index)) != 0)|| (length(grep("ENERGY", cat.index)) != 0)||
        (length(grep("CLIMATE", cat.index)) != 0) || (length(grep("Climate", cat.index)) != 0)||
        (length(grep("Hydrol", cat.index)) != 0)|| (length(grep("HYDROL", cat.index)) != 0)||
        (length(grep("Minerals", cat.index)) != 0) || (length(grep("MINERALS", cat.index)) != 0)||
        (length(grep("Polar", cat.index)) != 0) || (length(grep("POLAR", cat.index)) != 0)||
        (length(grep("Space", cat.index)) != 0)|| (length(grep("SPACE", cat.index)) != 0)||
        (length(grep("ATMOSPHER", cat.index)) != 0)|| (length(grep("Atmosfera", cat.index)) != 0)||                        
        (length(grep("Clima", cat.index)) != 0)|| (length(grep( "LOW TEMP", cat.index)) != 0)||                                        
        (length(grep("Parsons Lab", cat.index)) != 0)||  (length(grep("Ft Collins Sci", cat.index)) != 0)||                                       
        (length(grep("Busgen Inst", cat.index)) != 0)||  (length(grep("Macaulay Inst", cat.index)) != 0)||                                 
        (length(grep("Buesgen Inst", cat.index)) != 0)||  (length(grep("Svalbard UNIS", cat.index)) != 0)||                            
        (length(grep("Ctr Svalbard", cat.index)) != 0)||  (length(grep("Terra Jaume", cat.index)) != 0)||                         
        (length(grep("Ore Deposits", cat.index)) != 0)||   (length(grep("Isotope", cat.index)) != 0)||                      
        (length(grep("Min", cat.index)) != 0)||   (length(grep("Nordiques", cat.index)) != 0)||                                     
        (length(grep("Ambientales", cat.index)) != 0)|| (length(grep("Inst Arctic", cat.index)) != 0)||                                     
        (length(grep("Radiocarbon", cat.index)) != 0)||    (length(grep("Radioisotope", cat.index)) != 0)||              
        (length(grep("Hydrophys", cat.index)) != 0)||   (length(grep("QUATERNARY", cat.index)) != 0)||                                  
        (length(grep("REMOTE SENSING", cat.index)) != 0)||   (length(grep("PALAEONTOL", cat.index)) != 0)){
      results[i,counter+1] = "Earth_Science"
      counter = counter + 1
      clean[i,j+2]<-NA
      categories$Earth_Science[i]<-1
    }
    if ((length(grep("Bio", cat.index)) != 0)|| (length(grep("Bot", cat.index)) != 0) ||
      (length(grep("Evol", cat.index)) != 0) || (length(grep("Eco", cat.index)) != 0)||
      (length(grep("Ent", cat.index)) != 0) || (length(grep("Orn", cat.index)) != 0)||
      (length(grep("Zoo", cat.index)) != 0) || (length(grep("Plant", cat.index)) != 0)||
       (length(grep("Life Sci", cat.index)) != 0)|| (length(grep("Genet", cat.index)) != 0) ||
      (length(grep("Limn", cat.index)) != 0)|| (length(grep("GENET", cat.index)) != 0) ||
      (length(grep("Species", cat.index)) != 0) || (length(grep("SPECIES", cat.index)) != 0)||
      (length(grep("Aquat", cat.index)) != 0) || (length(grep("AQUAT", cat.index)) != 0)|
       (length(grep("Vida", cat.index)) != 0)|| (length(grep("BIOL", cat.index)) != 0) || 
      (length(grep("Biol", cat.index)) != 0)||(length(grep("BIO", cat.index)) != 0) || 
      (length(grep("BOT", cat.index)) != 0)|| (length(grep("EVOL", cat.index)) != 0) || 
      (length(grep("ECO", cat.index)) != 0)||(length(grep("ENT", cat.index)) != 0) || 
      (length(grep("ORN", cat.index)) != 0)||(length(grep("ZOO", cat.index)) != 0) || 
      (length(grep("PLANT", cat.index)) != 0)||(length(grep("ZOOL", cat.index)) != 0)|| 
      (length(grep("LIFE", cat.index)) != 0)|| (length(grep("VIDA", cat.index)) != 0)||
      (length(grep("Nat", cat.index)) != 0)||(length(grep("Ecosyst", cat.index)) != 0) || 
      (length(grep("ECOSYST", cat.index)) != 0)|| (length(grep("NAT", cat.index)) != 0)||
      (length(grep("TROP", cat.index)) != 0) || (length(grep("Trop", cat.index)) != 0)||
      (length(grep("POPULAT", cat.index)) != 0) || (length(grep("Populat", cat.index)) != 0)||
      (length(grep("PALEONTOL", cat.index)) != 0) || (length(grep("Paleontol", cat.index)) != 0)||
      (length(grep("PALEOBOT", cat.index)) != 0) || (length(grep("Paleobot", cat.index)) != 0)||
      (length(grep("FRESHWATER", cat.index)) != 0) || (length(grep("Freshwater", cat.index)) != 0) ||
      (length(grep("ROCKY MT", cat.index)) != 0) || (length(grep("Rocky Mt", cat.index)) != 0)||
      (length(grep("LIMN", cat.index)) != 0) || (length(grep("Limnol", cat.index)) != 0)||
      (length(grep("LIMNOL", cat.index)) != 0)  ||(length(grep("BIRD", cat.index)) != 0) || 
      (length(grep("Bird", cat.index)) != 0)|| (length(grep("IMMUNOECOL", cat.index)) != 0) || 
      (length(grep("Immunoecol", cat.index)) != 0)||(length(grep("NEMATOL", cat.index)) != 0) || 
      (length(grep("Nematol", cat.index)) != 0)||(length(grep("ANIM", cat.index)) != 0) || 
      (length(grep("Anim", cat.index)) != 0)||(length(grep("Marine", cat.index)) != 0)|| 
      (length(grep("MARINE", cat.index)) != 0) ||
       (length(grep("Ocean", cat.index)) != 0)|| (length(grep("Reef", cat.index)) != 0)||
       (length(grep("Estuar", cat.index)) != 0)|| (length(grep("ESTUAR", cat.index)) != 0)||
       (length(grep("OCEAN", cat.index)) != 0)|| (length(grep("REEF", cat.index)) != 0)||
       (length(grep("Oceanog", cat.index)) != 0)|| (length(grep("OCEANOG", cat.index)) != 0)||
      (length(grep("PALEOBIOL", cat.index)) != 0) || (length(grep("Paleobiol", cat.index)) != 0)||
      (length(grep("wildlife", cat.index)) != 0)|| (length(grep("Env", cat.index)) != 0) ||
        (length(grep("Fish", cat.index)) != 0) || (length(grep("Management", cat.index)) != 0)||
        (length(grep("forest", cat.index)) != 0) || (length(grep("Res", cat.index)) != 0)||
        (length(grep("Wildlife", cat.index)) != 0) || (length(grep("WILDLIFE", cat.index)) != 0)||
        (length(grep("FOREST", cat.index)) != 0) || (length(grep("Forest", cat.index)) != 0)||
        (length(grep("FISH", cat.index)) != 0) || (length(grep("fish", cat.index)) != 0) ||
        (length(grep("ENV", cat.index)) != 0) || (length(grep("RES", cat.index)) != 0)||
        (length(grep("Watershed", cat.index)) != 0) || (length(grep("WATERSHED", cat.index)) != 0)||
        (length(grep("Rivers", cat.index)) != 0) || (length(grep("RIVERS", cat.index)) != 0)||
        (length(grep("LAND", cat.index)) != 0) || (length(grep("Land", cat.index)) != 0)||
        (length(grep("Soils", cat.index)) != 0) || (length(grep("SOILS", cat.index)) != 0)||
        (length(grep("Soil", cat.index)) != 0) || (length(grep("SOIL", cat.index)) != 0)||
        (length(grep("Ictiol", cat.index)) != 0) || (length(grep("ICTIOL", cat.index)) != 0)||
        (length(grep("Water", cat.index)) != 0) || (length(grep("WATER", cat.index)) != 0)||
       (length(grep("Agr", cat.index)) != 0)|| (length(grep("CROP", cat.index)) != 0) ||
        (length(grep("Crop", cat.index)) != 0)||(length(grep("AGR", cat.index)) != 0) ||
        (length(grep("AGRON", cat.index)) != 0) || (length(grep("Agron", cat.index)) != 0)||
        (length(grep("Range", cat.index)) != 0) || (length(grep("RANGE", cat.index)) != 0)||
        (length(grep("Hort", cat.index)) != 0) || (length(grep("HORT", cat.index)) != 0)||
        (length(grep("Horticul", cat.index)) != 0)|| (length(grep("HORTICUL", cat.index))!= 0)||
        (length(grep("Smithsonian", cat.index)) != 0)|| (length(grep("SMITHSONIAN", cat.index))!= 0)||
          (length(grep("LAKE", cat.index)) != 0)|| (length(grep("Lake", cat.index))!= 0)||
        (length(grep("LAKES", cat.index)) != 0)|| (length(grep("Lakes", cat.index))!= 0)||
      (length(grep("Aridas", cat.index)) != 0)|| (length(grep("ARIDAS", cat.index))!= 0)||
        (length(grep("Grassland", cat.index)) != 0) || (length(grep("GRASSLAND", cat.index)) != 0)||
      (length(grep("Appalachian Lab", cat.index)) != 0) || (length(grep("Moreau Lab", cat.index)) != 0)||
      (length(grep("Alaska Sci Ctr", cat.index)) != 0) || (length(grep("Wetland", cat.index)) != 0)||
      (length(grep("TechnoSuruga Lab Co Ltd", cat.index)) != 0) || (length(grep("Field Sci", cat.index)) != 0)||
      (length(grep("Florestais", cat.index)) != 0) || (length(grep("Ethol", cat.index)) != 0)||
      (length(grep("Konrad Lorenz", cat.index)) != 0) ||(length(grep("Foret", cat.index)) != 0)||                                     
      (length(grep("Conservat", cat.index)) != 0) || (length(grep("Terradynam", cat.index)) != 0)||                             
      (length(grep("LINCGlobal", cat.index)) != 0) || (length(grep("DNA Barcoding", cat.index)) != 0)||
      (length(grep("Volcani", cat.index)) != 0) ||(length(grep("Mediterranean", cat.index)) != 0)||                  
      (length(grep("Wildlands", cat.index)) != 0) ||(length(grep("Conservat", cat.index)) != 0)||                                        
      (length(grep("Fram", cat.index)) != 0) ||(length(grep("James Hutton", cat.index)) != 0)||                                        
      (length(grep("Percy Fitzpatrick", cat.index)) != 0) ||(length(grep("W INDIES LAB", cat.index)) != 0)||                                             
      (length(grep("BARNES LAB", cat.index)) != 0) || (length(grep("Clodomiro Picado", cat.index)) != 0)||                                    
      (length(grep("Forets", cat.index)) != 0) || (length(grep("CORAIL", cat.index)) != 0)||                                   
      (length(grep("Recanati Kaplan", cat.index)) != 0) || (length(grep("Edward Grey", cat.index)) != 0)||                                         
      (length(grep("Coevoluc" , cat.index)) != 0) ||(length(grep("Marinha & Ambiental", cat.index)) != 0)||  
      (length(grep("Farlington Ringing", cat.index)) != 0)||(length(grep("W INDIE LAB", cat.index)) != 0)||                        
      (length(grep("INSTAAR", cat.index)) != 0) ||   (length(grep("Leetown Sci", cat.index)) != 0)||                                          
      (length(grep("Mediterrani", cat.index)) != 0) ||(length(grep("Friday Harbor Labs", cat.index)) != 0)||                           
      (length(grep("Arthur Rylah", cat.index)) != 0) ||  (length(grep("Florida Integrated", cat.index)) != 0)||                               
      (length(grep("Desert", cat.index)) != 0) ||  (length(grep("Mt Studies", cat.index)) != 0)||                                
      (length(grep("LINC Global", cat.index)) != 0) ||     (length(grep("NSW Dept Primary Ind", cat.index)) != 0)||                                     
      (length(grep("Brackenridge Field", cat.index)) != 0) ||  (length(grep("Coastal Monitoring", cat.index)) != 0)||                      
      (length(grep("Avancats Blanes", cat.index)) != 0) ||   (length(grep("River", cat.index)) != 0)||                                          
      (length(grep("Christchurch Sci Ctr", cat.index)) != 0) || (length(grep("Silvicultura", cat.index)) != 0)||  
      (length(grep("Dendro", cat.index)) != 0) ||     (length(grep("St Anthony Falls Lab", cat.index)) != 0)||                                    
      (length(grep("Ambiental Amazonia", cat.index)) != 0) || (length(grep("Macroecol", cat.index)) != 0)||                                            
      (length(grep("Cryoptogamy", cat.index)) != 0) ||   (length(grep("Ciencies Mar", cat.index)) != 0)||                                 
      (length(grep("Herpetol", cat.index)) != 0) || (length(grep("Avanzados Blanes", cat.index)) != 0)||                    
      (length(grep("Marinha & Ambiental", cat.index)) != 0) || (length(grep("Horn Point Lab", cat.index)) != 0)||                                           
      (length(grep("Vegetal", cat.index)) != 0) || (length(grep("Lab Costero Calfuco", cat.index)) != 0)||                                      
      (length(grep("Aquaculture", cat.index)) != 0) ||  (length(grep("Schweiz Vogelwarte", cat.index)) != 0)||                                       
      (length(grep("Dauphin Isl Sea", cat.index)) != 0) || (length(grep("Ag Sci", cat.index)) != 0)||                                        
      (length(grep("Fdn Alfred Wegener", cat.index)) != 0) ||  (length(grep("Vogelforsch Vogelwarte Helgoland", cat.index)) != 0)||            
      (length(grep("Mediterraneo", cat.index)) != 0) ||      (length(grep("Dendrocronol", cat.index)) != 0)||                       
      (length(grep("Insect Sci", cat.index)) != 0) ||(length(grep("Schweiz Vogelwarte", cat.index)) != 0)||                          
      (length(grep("Phytol", cat.index)) != 0) || (length(grep("Rech Halieut", cat.index)) != 0)||                                         
      (length(grep("Ichtyol", cat.index)) != 0) ||  (length(grep("Medio Ambiente", cat.index)) != 0)||                              
      (length(grep("Ambiental Amazonia", cat.index)) != 0) || (length(grep("Wildbiol", cat.index)) != 0)||                
      (length(grep("Erken Lab", cat.index)) != 0) || (length(grep("Pesquisa Amazonia", cat.index)) != 0)||                              
      (length(grep("Louis Calder", cat.index)) != 0) || (length(grep("Russell Labs", cat.index)) != 0)||                                         
      (length(grep("Medioambientales", cat.index)) != 0) || (length(grep("Konrad Lorenz", cat.index)) != 0)||                                
      (length(grep("Hoffmann Inst", cat.index)) != 0) ||  (length(grep("Complutense", cat.index)) != 0)||                             
      (length(grep("Ralph M Parsons", cat.index)) != 0) || (length(grep( "Alfred Wegener Inst", cat.index)) != 0)||                              
      (length(grep("Tree Ring Lab", cat.index)) != 0) ||(length(grep("Ctr Wood", cat.index)) != 0)||                                             
      (length(grep("LINCGlobal", cat.index)) != 0) || (length(grep("Okol", cat.index)) != 0)||                                                
      (length(grep("Dept Vertebrates", cat.index)) != 0) ||  (length(grep( "Desert", cat.index)) != 0)||                                  
      (length(grep("Meereskunde", cat.index)) != 0) ||  (length(grep("Streamside Studies", cat.index)) != 0)||                                   
      (length(grep("ARS CEREAL RUST", cat.index)) != 0) || (length(grep("CTR COASTAL STUDIES", cat.index)) != 0)||                                      
      (length(grep("RUSSELL LABS", cat.index)) != 0) ||(length(grep("BLACK MT LABS", cat.index)) != 0)||                                            
      (length(grep("MEDITERRANEO", cat.index)) != 0) ||   (length(grep( "PRAIRIE SCI", cat.index)) != 0)||                                       
      (length(grep( "COWEETA", cat.index)) != 0) ||  (length(grep("COMMUNITY DYNAM", cat.index)) != 0)||                                      
      (length(grep( "ARBEITSGRP VERHALTENSFORSCH", cat.index)) != 0) ||  (length(grep("INSECT", cat.index)) != 0)||                                       
      (length(grep("CONSERVAT", cat.index)) != 0) ||  (length(grep("AVIAN", cat.index)) != 0)||                                           
      (length(grep("RIVER", cat.index)) != 0) ||  (length(grep( "FRIDAY HARBOR", cat.index)) != 0)||                             
      (length(grep( "SHRUB SCI LAB", cat.index)) != 0) ||   (length(grep("BELLE W BARUCH", cat.index)) != 0)||                                      
      (length(grep("BOREAL INST", cat.index)) != 0) ||  (length(grep("EKOL", cat.index)) != 0)||                                      
      (length(grep( "PESTICIDE LAB", cat.index)) != 0) ||  (length(grep("ASKO", cat.index)) != 0)||                                                 
      (length(grep("GREELEY MEM LAB", cat.index)) != 0) || (length(grep("BUCKHOUT", cat.index)) != 0)||                                         
      (length(grep( "INVERTEBRATES", cat.index)) != 0) ||(length(grep( "BOYCE THOMPSON INST", cat.index)) != 0)||
      (length(grep("Pasture Sci", cat.index)) != 0)){
      results[i,counter+1] = "Env_Biology"
      counter = counter + 1
      clean[i,j+2]<-NA
      categories$Env_Biology[i]<-1
    }

    if ((length(grep("Communicable", cat.index)) != 0)|| (length(grep("Pharm", cat.index)) != 0)||
        (length(grep("INFECT", cat.index)) != 0)|| (length(grep("Infect", cat.index)) != 0)||
        (length(grep("Hlth", cat.index)) != 0)|| (length(grep("HLTH", cat.index)) != 0)||
        (length(grep("Food", cat.index)) != 0) || (length(grep("FOOD", cat.index)) != 0)||
        (length(grep("NEUROBIOL", cat.index)) != 0) || (length(grep("Neurobiol", cat.index)) != 0)||
        (length(grep("Bacteriol", cat.index)) != 0)||(length(grep("MICROBIOL", cat.index)) != 0) || 
        (length(grep("BACTERIOL", cat.index)) != 0) ||(length(grep("Microbiol", cat.index)) != 0) ||
        (length(grep("EPIDEMIOL", cat.index)) != 0)|| (length(grep("Epidemiol", cat.index)) != 0)||
        (length(grep("Serv Alta Tecno", cat.index)) != 0)||        (length(grep("Aarhus Inst", cat.index)) != 0)||                                            
        (length(grep("Aronoff Lab", cat.index)) != 0)||(length(grep("Charles Perkins", cat.index)) != 0)||                                                
        (length(grep("Vet", cat.index)) != 0)||             (length(grep("Genom", cat.index)) != 0)||                                        
        (length(grep("Enabling Technol", cat.index)) != 0)||            (length(grep("MED", cat.index)) != 0)||                                                            
        (length(grep("NUTR", cat.index)) != 0)||           (length(grep("PARASITOL", cat.index)) != 0)||                                                     
        (length(grep("Radiol", cat.index)) != 0)||          (length(grep( "Neurogenet", cat.index)) != 0)||                                             
        (length(grep("David Clark Labs", cat.index)) != 0)||         (length(grep("Dis Control", cat.index)) != 0)||                                           
        (length(grep("Salud", cat.index)) != 0)||                       
        (length(grep("Med", cat.index)) != 0)||         (length(grep("NUCATS", cat.index)) != 0)||                            
        (length(grep("Pathol", cat.index)) != 0)||          (length(grep("Canc", cat.index)) != 0)||                          
        (length(grep("Helmholtz Ctr", cat.index)) != 0)||  (length(grep("Karolinska Inst", cat.index)) != 0)||                                             
        (length(grep("Gulbenkian Inst", cat.index)) != 0)||         (length(grep("Inst Vogelforsch", cat.index)) != 0)||                     
        (length(grep("Neurosci", cat.index)) != 0)||         (length(grep("Liggins Inst", cat.index)) != 0)||                                                       
        (length(grep("Haartman Inst", cat.index)) != 0)||           (length(grep("Virol", cat.index)) != 0)||                                                         
        (length(grep("HUCH Lab", cat.index)) != 0)|| (length(grep("Tech St Jerome", cat.index)) != 0)||                                   
        (length(grep("Inst Butantan", cat.index)) != 0)|| (length(grep("Forens", cat.index)) != 0)||                                                    
        (length(grep("Helmintol", cat.index)) != 0)||   (length(grep("Salut", cat.index)) != 0)||                                                 
        (length(grep("Microorganisms", cat.index)) != 0)||   (length(grep("Stress Adaptat", cat.index)) != 0)||                         
        (length(grep("Outbreak Anal", cat.index)) != 0)||   (length(grep("Gen", cat.index)) != 0)||                                                   
        (length(grep("Verhaltensphysiol", cat.index)) != 0)||                         
        (length(grep("Parsitol", cat.index)) != 0)||   (length(grep("INEUCI", cat.index)) != 0)||                                                     
        (length(grep("Inst Venezolano", cat.index)) != 0)||(length(grep("BABRAHAM INST", cat.index)) != 0)||                                              
        (length(grep("NEISON LABS", cat.index)) != 0)||                                                        
        (length(grep("VERO BEACH LAB", cat.index)) != 0)|| (length(grep("PATHOL", cat.index)) != 0)||                                           
        (length(grep("REG HOSP", cat.index)) != 0)||  (length(grep("RADIAT", cat.index)) != 0)||                                                    
        (length(grep("NELSON LABS", cat.index)) != 0)||   (length(grep("CORSON LAB", cat.index)) != 0)||                                                         
        (length(grep( "CTR LOUIS EMBERGER", cat.index)) != 0)|| (length(grep(  "OPHTHALMOL", cat.index)) != 0)||                                       
        (length(grep("PHARM", cat.index)) != 0)|| (length(grep("BARNES LAB", cat.index)) != 0)){
      results[i,counter+1] = "Non_Env_Biology"
      counter = counter + 1
      clean[i,j+2]<-NA
      categories$Non_Env_Biology[i]<-1
    }
    if ((length(grep("Schermerhorn Ex 1014A", cat.index)) != 0)|| (length(grep("Martin; Schmid", cat.index)) != 0) ||
        (length(grep("Rene; Schluetting", cat.index)) != 0)||(length(grep("Shannon L.; Scharf", cat.index)) != 0)||
        (length(grep("MD 20742 USA;", cat.index)) != 0)||(length(grep("Denmark;", cat.index)) != 0)||
        (length(grep("MA 01366 USA;", cat.index)) != 0)||(length(grep("Worcester Polytech Inst", cat.index)) != 0)||
      (length(grep("Inst Univ Naval", cat.index)) != 0)||(length(grep("Australia;", cat.index)) != 0)||                                 
    (length(grep("Karen; Scheibling", cat.index)) != 0)||(length(grep("M. Elizabeth; Schnitzer", cat.index)) != 0)||                             
    (length(grep("Jennifer S.; Schnitzer", cat.index)) != 0)||(length(grep("WA 98195 USA;", cat.index)) != 0)||                               
    (length(grep("Torrance C.; Schenck", cat.index)) != 0)||                                       
    (length(grep("Jenny M.; Schmid", cat.index)) != 0)||(length(grep("England;", cat.index)) != 0)||                                    
    (length(grep("Austria;", cat.index)) != 0)||                                             
    (length(grep("T.; Schleuning", cat.index)) != 0)||(length(grep("Germany;", cat.index)) != 0)||                                    
    (length(grep("J.; Schurr", cat.index)) != 0)||                                          
    (length(grep("209 Univ Lab Bldg", cat.index)) != 0)||(length(grep("David A.; Schoener", cat.index)) != 0)||                                  
    (length(grep("E. E.; Schoelynck", cat.index)) != 0)||(length(grep("Spain;", cat.index)) != 0)||                                
    (length(grep("Kyle A.; Schlaepfer", cat.index)) != 0)||(length(grep("WY 82071 USA;", cat.index)) != 0)||                           
    (length(grep("Scotland;", cat.index)) != 0)||(length(grep("Xiaojuan; Schmid", cat.index)) != 0)||                                    
    (length(grep("Arie; Scheffer", cat.index)) != 0)||(length(grep("Schanzlestr 1", cat.index)) != 0)||                                       
    (length(grep("Malaysia;", cat.index)) != 0)||(length(grep("Germany;", cat.index)) != 0)||                                   
    (length(grep("John M.; Scheibling", cat.index)) != 0)||(length(grep("Weston H.; Schwartz", cat.index)) != 0)||                                 
    (length(grep("TN 37996 USA;", cat.index)) != 0)||(length(grep("Japan;", cat.index)) != 0)||                                   
    (length(grep("Dan F. B.; Schmid", cat.index)) != 0)||(length(grep("J. P.; Schipper", cat.index)) != 0)||                                     
    (length(grep("4200 Smith Sch Rd", cat.index)) != 0)||(length(grep("Riley; Schreck", cat.index)) != 0)||                                      
    (length(grep("Maria M.; Schnitzer", cat.index)) != 0)||(length(grep("Mickal; Schmitt", cat.index)) != 0)||
    (length(grep("Johanna; Schneider", cat.index)) != 0)||(length(grep("Anita C.; Schuetz", cat.index)) != 0)||                                   
    (length(grep("GOTTSCHALK", cat.index)) != 0)||(length(grep("SCHRODER", cat.index)) != 0)||                                            
    (length(grep("SCHAFFER", cat.index)) != 0)||(length(grep("SCHOENER", cat.index)) != 0)||                                            
    (length(grep("ULTSCH", cat.index)) != 0)||(length(grep("RUSCH", cat.index)) != 0)||                                               
    (length(grep("STEINWASCHER", cat.index)) != 0)||(length(grep("SCHWARTZKOPF", cat.index)) != 0)||                                        
    (length(grep("SCHLESINGER", cat.index)) != 0)||(length(grep("Susan M.; Schuur", cat.index)) != 0)||                                    
    (length(grep("Stephen; Schreeg", cat.index)) != 0)||(length(grep("Kira A.; Scheibling", cat.index)) != 0)||                                 
    (length(grep("Dept Salut Publ", cat.index)) != 0)||(length(grep("Mark; Scheu", cat.index)) != 0)||                                         
    (length(grep("Stephanie E.; Scheef", cat.index)) != 0)||(length(grep("Elizabeth E.; Scheuerell", cat.index)) != 0)||                            
    (length(grep("ZA-1207 Schagen", cat.index)) != 0)||(length(grep("Alexandra R.; Mueller-Schaerer", cat.index)) != 0)||                      
    (length(grep("Yan; Schaffner", cat.index)) != 0)||(length(grep("Dept 3166", cat.index)) != 0)||                                           
    (length(grep("Argentina;", cat.index)) != 0)||(length(grep("Dauphin Isl Sea Lab", cat.index)) != 0)||                         
    (length(grep("NL-9166 NZ Schiermonnikoog", cat.index)) != 0)||(length(grep("Jonathan B.; Schindler", cat.index)) != 0)||                              
    (length(grep("Australia;", cat.index)) != 0)||(length(grep("Spain;", cat.index)) != 0)||                                     
    (length(grep("Switzerland;", cat.index)) != 0)||                                             
    (length(grep("Annett; Schmid", cat.index)) != 0)||(length(grep("France;", cat.index)) != 0)||                                   
    (length(grep("France;", cat.index)) != 0)||(length(grep("Eike Lena; Schultheiss", cat.index)) != 0)||                              
    (length(grep("Katrin; Schleuning", cat.index)) != 0)||(length(grep("Australia;", cat.index)) != 0)||                             
    (length(grep("Mark E.; Schloeder", cat.index)) != 0)||(length(grep("Wales;", cat.index)) != 0)||                                
    (length(grep("Eleanor A.; Schutzenhofer", cat.index)) != 0)||(length(grep("MO 63130 USA;", cat.index)) != 0)||                        
    (length(grep("Min A.; Mueller-Schaerer", cat.index)) != 0)||(length(grep("PR 00936 USA;", cat.index)) != 0)||                            
    (length(grep("Christian C.; Schneeberger", cat.index)) != 0)||(length(grep("Jana; Schmid", cat.index)) != 0)||                                        
    (length(grep("FL 32611 USA;", cat.index)) != 0)||                              
    (length(grep("Spain;", cat.index)) != 0)||(length(grep("Lindsay A.; Schmid", cat.index)) != 0)||
    (length(grep("Pascal A.; Scherer-Lorenzen", cat.index)) != 0)||(length(grep("MT 59715 USA;", cat.index)) != 0)||                              
    (length(grep("CO 80309 USA;", cat.index)) != 0)||(length(grep("Zona Ctr", cat.index)) != 0)||                                            
    (length(grep("NY 12604 USA;", cat.index)) != 0)||(length(grep("C. M.; Schimel", cat.index)) != 0)||                                      
    (length(grep("P.; Schmidt", cat.index)) != 0)||(length(grep("OR 97331 USA;", cat.index)) != 0)||                             
    (length(grep("B. K.; Schoolmaster", cat.index)) != 0)||(length(grep("FED CTR", cat.index)) != 0)||                                             
    (length(grep("CIUDAD UNIV RODRIGO FACIO", cat.index)) != 0)||(length(grep("DEPT SCI", cat.index)) != 0)||                                            
    (length(grep("INSTR & CONTROLS DIV", cat.index)) != 0)||(length(grep("SCHENECTADY", cat.index)) != 0)||                                         
    (length(grep("Magne; Schei", cat.index)) != 0)||(length(grep("Norway;", cat.index)) != 0)||                                      
    (length(grep("Wolfgang; Schnecker", cat.index)) != 0)||(length(grep("NC 27708 USA;", cat.index)) != 0)||                              
    (length(grep("Louise M.; Facelli", cat.index)) != 0)||                                  
    (length(grep("Key Lab", cat.index)) != 0)||                                            
    (length(grep("Austria;", cat.index)) != 0)||(length(grep("Switzerland;", cat.index)) != 0)||                                
    (length(grep("Schweiz Vogelwarte", cat.index)) != 0)||(length(grep("Egbert H.; Scheffer", cat.index)) != 0)||                                
    (length(grep("Pauline A. C.; Schulten", cat.index)) != 0)||(length(grep("Wolfgang; Schnyder", cat.index)) != 0)||                                  
    (length(grep("Sally J.; Schmitt", cat.index)) != 0)||(length(grep("Casey P.; Schindler", cat.index)) != 0)||                                 
    (length(grep("CA 95616 USA;", cat.index)) != 0)||                            
  (length(grep("Joseph K.; Schweitzer", cat.index)) != 0)||                               
    (length(grep("Cameron; Schmid", cat.index)) != 0)||                                         
    (length(grep("Germany; ", cat.index)) != 0)||                                         
    (length(grep("Gordon W.; Schindler", cat.index)) != 0)||                                
    (length(grep("WI 53201 USA;", cat.index)) != 0)||                            
    (length(grep("MT 59812 USA;", cat.index)) != 0)||(length(grep("Ctr Univ", cat.index)) != 0)||                                            
    (length(grep("Julius; Schaefer", cat.index)) != 0)||                                    
    (length(grep("MN 55414 USA;", cat.index)) != 0)||(length(grep("The Laboratory", cat.index)) != 0)||                                      
    (length(grep("Western & No Serv Ctr", cat.index)) != 0)||                                            
    (length(grep("M.; Scheu", cat.index)) != 0)||(length(grep("M.; Schmid", cat.index)) != 0)||                                          
    (length(grep("Germany;", cat.index)) != 0)||(length(grep("France;", cat.index)) != 0)||                                     
    (length(grep("Univ Santiago de Compostela", cat.index)) != 0)||(length(grep("Ilyas; Schmidt", cat.index)) != 0)||                                      
    (length(grep("Germany;", cat.index)) != 0)||(length(grep("Ctr Cient & Tecnol Mendoza", cat.index)) != 0)||                          
    (length(grep("Amber N.; Schoener", cat.index)) != 0)||(length(grep("Malaysia;", cat.index)) != 0)||                                  
    (length(grep("A. C. W.; Scheu", cat.index)) != 0)||(length(grep("England;", cat.index)) != 0)||                                  
    (length(grep("Julia; Schmid-Araya", cat.index)) != 0)||(length(grep("Raphael; Schaub", cat.index)) != 0)||                                    
    (length(grep("David I.; Schiel", cat.index)) != 0)||                                          
    (length(grep("Christiane; Schumacher", cat.index)) != 0)||(length(grep("Elisabeth; Schmid", cat.index)) != 0)||                                   
    (length(grep("France;", cat.index)) != 0)||(length(grep("Switzerland;", cat.index)) != 0)||                              
    (length(grep("Schotzau & Simmen", cat.index)) != 0)||(length(grep("Germany;", cat.index)) != 0)||                                    
    (length(grep("Christopher F.; Schwaderer", cat.index)) != 0)||(length(grep("MI 49060 USA;", cat.index)) != 0)||                           
    (length(grep("Daniel C.; Schroeter", cat.index)) != 0)||                                           
    (length(grep("NY 10027 USA;", cat.index)) != 0)||(length(grep("Francois P.; Schoonmaker", cat.index)) != 0)||                            
    (length(grep("Brandon T.; Schmitz", cat.index)) != 0)||                                            
    (length(grep("CA 93106 USA;", cat.index)) != 0)||(length(grep("FL 33199 USA;", cat.index)) != 0)||                        
    (length(grep("IA 50011 USA;", cat.index)) != 0)||(length(grep("John F.; Schutte", cat.index)) != 0)||                                    
    (length(grep("Christiane; Schulze", cat.index)) != 0)||(length(grep("Switzerland;", cat.index)) != 0)||                                
    (length(grep("Thomas; Mueller-Schaerer", cat.index)) != 0)||                            
    (length(grep("Switzerland;", cat.index)) != 0)||(length(grep("Andrew; Schmid", cat.index)) != 0)||                                      
    (length(grep("AK 99645 USA;", cat.index)) != 0)||(length(grep("USGS Alaska Sci Ctr", cat.index)) != 0)||                      
    (length(grep("Susann; Schmid", cat.index)) != 0)||(length(grep("Lowestoft Lab", cat.index)) != 0)||                                       
    (length(grep("Anne M.; Schall", cat.index)) != 0)||(length(grep("Inst Leonidas & Maria Deane", cat.index)) != 0)||                         
    (length(grep("Christian K.; Schindler", cat.index)) != 0)||(length(grep("France;", cat.index)) != 0)||                                  
    (length(grep("Mueller Lab 417", cat.index)) != 0)||                                          
    (length(grep("IL 62901 USA;", cat.index)) != 0)||(length(grep("Richard J.; Schofield", cat.index)) != 0)||                               
    (length(grep("Germany;", cat.index)) != 0)||(length(grep("Lab Maritimo Guia", cat.index)) != 0)||                                   
    (length(grep("Brett A.; Schwarzkopf", cat.index)) != 0)||                                         
    (length(grep("Germany;", cat.index)) != 0)||(length(grep("Barlett Sch", cat.index)) != 0)||                                         
    (length(grep("Roland; Schaedler", cat.index)) != 0)||(length(grep("Germany;", cat.index)) != 0)||                                     
    (length(grep("Canada;", cat.index)) != 0)||(length(grep("Michael J.; Schmidt", cat.index)) != 0)||                                 
    (length(grep("NY 12545 USA;", cat.index)) != 0)||(length(grep("Judith; Scholz", cat.index)) != 0)||                                      
    (length(grep("Dept U2", cat.index)) != 0)||(length(grep("Germany;", cat.index)) != 0)||                                
    (length(grep("RI 02881 USA;", cat.index)) != 0)||(length(grep("AZ 85287 USA;", cat.index)) != 0)||                             
    (length(grep("CA 95616 USA;", cat.index)) != 0)||                          
    (length(grep("Stephan; Scheu", cat.index)) != 0)||(length(grep("Germany;", cat.index)) != 0)||                                  
    (length(grep("Eric P.; Schielke", cat.index)) != 0)||(length(grep("MT 59812 USA;", cat.index)) != 0)||                              
    (length(grep("Cloe; Schmid", cat.index)) != 0)||(length(grep("Germany;", cat.index)) != 0)||                                   
    (length(grep("AZ 86011 USA;", cat.index)) != 0)||                           
    (length(grep("Nina; Scherer-Lorenzen", cat.index)) != 0)||                              
    (length(grep("Jonathan W.; Schindler", cat.index)) != 0)||(length(grep("Urs A.; Mueller-Schaerer", cat.index)) != 0)||                            
    (length(grep("James S.; Schlesinger", cat.index)) != 0)||(length(grep("NC 27708 USA;", cat.index)) != 0)||                          
    (length(grep("USGS Ft Collins Sci Ctr", cat.index)) != 0)||(length(grep("Canada;", cat.index)) != 0)||                                     
    (length(grep("Reprint Dept", cat.index)) != 0)||(length(grep("Dept 43", cat.index)) != 0)||                                             
    (length(grep("WI 53706 USA; Old Mill High Sch", cat.index)) != 0)||(length(grep("Attent Reprint Dept", cat.index)) != 0)||                                 
    (length(grep("CH-8952 Schlieren", cat.index)) != 0)||                        
    (length(grep("Marion B.; Schlacher", cat.index)) != 0)||                                
    (length(grep("Thomas A.; Schoeman", cat.index)) != 0)||(length(grep("Anne M.; Schultheis", cat.index)) != 0)||                                 
    (length(grep("MI 49060 USA;", cat.index)) != 0)||                           
    (length(grep("Switzerland;", cat.index)) != 0)||(length(grep("Robert; Schwartz", cat.index)) != 0)||                                   
    (length(grep("Adrianne P.; Schindler", cat.index)) != 0)||                                           
    (length(grep("MA 02481 USA;", cat.index)) != 0)||                                             
    (length(grep("Robert W.; Schmitz", cat.index)) != 0)||(length(grep("Toke T.; Schmidt", cat.index)) != 0)||                                    
    (length(grep("Eva-Maria; Schloter", cat.index)) != 0)||(length(grep("SC 29205 USA;", cat.index)) != 0)||                              
    (length(grep("OR 97201 USA;", cat.index)) != 0)||(length(grep("Germany;", cat.index)) != 0)||                                 
    (length(grep("Grace F.; Schemske", cat.index)) != 0)||(length(grep("MI 48824 USA;", cat.index)) != 0)||                             
    (length(grep("Australia;", cat.index)) != 0)||(length(grep("Leonor; Schnitzer", cat.index)) != 0)||                                   
    (length(grep("WA 98112 USA;", cat.index)) != 0)||                           
    (length(grep("K.; Schindler", cat.index)) != 0)||(length(grep("Bolivia;", cat.index)) != 0)||                                 
    (length(grep("RI 02912 USA;", cat.index)) != 0)||(length(grep("Alicia; Schnitzer", cat.index)) != 0)||                                   
    (length(grep("WI 53210 USA;", cat.index)) != 0)||(length(grep("Alexandra; Schnitzer", cat.index)) != 0)||                                
    (length(grep("NH 03755 USA;", cat.index)) != 0)||                            
    (length(grep("Germany;", cat.index)) != 0)||                               
    (length(grep("Ross; Schwarzkopf", cat.index)) != 0)||(length(grep("Australia;", cat.index)) != 0)||                                
    (length(grep("CA 95616 USA;", cat.index)) != 0)||(length(grep("Israel;", cat.index)) != 0)||                                  
    (length(grep("CA 94720 USA;", cat.index)) != 0)||(length(grep("Joachim; Scheu", cat.index)) != 0)||                                      
    (length(grep("AK 99775 USA;", cat.index)) != 0)||(length(grep("The Faculties", cat.index)) != 0)||                                       
    (length(grep("ZA-7700 RONDEBOSCH", cat.index)) != 0)||(length(grep("BOX 30001 DEPT 3AF", cat.index)) != 0)||                                  
    (length(grep("Schmitz", cat.index)) != 0)||(length(grep("E-15706 SANTIAGO DE COMPOSTELA", cat.index)) != 0)||                      
    (length(grep("Schimmel", cat.index)) != 0)||(length(grep("RONDEBOSCH 7700", cat.index)) != 0)||                                     
    (length(grep("POSTFACH 165", cat.index)) != 0)||(length(grep("SCHLOSSER", cat.index)) != 0)||                                           
    (length(grep("SCHONBEINSTR 6", cat.index)) != 0)||(length(grep("SCHLUTER", cat.index)) != 0)||                                            
    (length(grep("SCHALLENBERG", cat.index)) != 0)||(length(grep("TSCHARNTKE", cat.index)) != 0)||                                          
    (length(grep("POSTFACH 6980", cat.index)) != 0)||(length(grep("SCHEINER", cat.index)) != 0)||                                            
    (length(grep("SCHLESER", cat.index)) != 0)||(length(grep("CLEBSCH", cat.index)) != 0)||                                             
    (length(grep("SCHULTZ", cat.index)) != 0)||(length(grep("COMPTON", cat.index)) != 0)||                                             
    (length(grep("TOWNSVILLE MAIL CTR", cat.index)) != 0)||(length(grep("SCHMITT", cat.index)) != 0)||                                             
    (length(grep("SCHROEDER", cat.index)) != 0)||(length(grep("RISCH", cat.index)) != 0)||                                               
    (length(grep("SCHIMEL", cat.index)) != 0)||(length(grep("SCHOENBERG", cat.index)) != 0)||                                          
    (length(grep("US DEPT INTERIOR", cat.index)) != 0)||(length(grep("SCHUM", cat.index)) != 0)||                                               
    (length(grep("FLORIDA DEPT TRANSPORTAT", cat.index)) != 0)||                            
    (length(grep("SEMLITSCH", cat.index)) != 0)|| (length(grep("ALABACK", cat.index)) != 0)){
      counter = counter + 1
      clean[i,j+2]<-NA
    }
    if (!is.na(as.character(clean[i,j+2]))){
      over.in<-min(which(is.na(leftover[,1])))
      leftover[over.in,1] = as.character(clean[i,j+2])
      leftover[over.in,2] = id.in
    }
  }
}


leftover<-as.data.frame(leftover)
leftover1<-leftover[!is.na(leftover$aff),]
leftover1<-as.data.frame(leftover1)
setwd("/Users/nicoleward/Documents/VirginiaTech/Manuscripts/CS_Collaboration")
write.csv(leftover1,"leftoverV2.csv", row.names = FALSE, quote = FALSE)
write.table(df,"Ecology_records.txt", row.names = FALSE, quote = FALSE,sep="\t")
#categories.1<-categories[rowSums(is.na(categories)) != ncol(categories),]
categories.1<-as.data.frame(categories)
categories.1$sumrow <- rowSums(categories.1, na.rm = TRUE)
categories.1$Year<-clean$V1
categories.1<-categories.1[order(categories.1$Year),]
categories.1$Paper<-seq(1:10319)
clean.pre<-clean.pre[order(clean.pre$V1),]
clean.pre$Paper<-seq(1:10319)
empty.cat<-categories.1[which(categories.1$sumrow==0),]
fill.cat<-categories.1[which(categories.1$sumrow>0),]



fill.cat[is.na(fill.cat)] <- 0

plot(fill.cat$Paper,fill.cat$sumrow, type="p",main="# of categories")
plot(fill.cat$Paper,fill.cat$Computer_Science, type="p",main="# of comp")
plot(fill.cat$Paper,fill.cat$Physics, type="p",main="# of Physics")
plot(fill.cat$Paper,fill.cat$Natural_Resources, type="p",main="# of Nat Res")
plot(fill.cat$Paper,fill.cat$Earth_Systems, type="p",main="# of Earth Sys")
plot(fill.cat$Paper,fill.cat$Math, type="p",main="# of Math")
plot(fill.cat$Paper,fill.cat$Agriculture, type="p",main="# of Ag")
plot(fill.cat$Paper,fill.cat$Social_Science, type="p",main="# of Social")
num.comp<-fill.cat[which(fill.cat$Computer_Science>0),]
