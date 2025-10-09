


###Archive of Code removed from other scripts during October 2025

#Final QC and Org





# 
# 
# #samples removed qc and remove sample with no IS 
# p.q.r.dat <- p.qc.remove %>%
#   left_join(., p.q.dat) %>%
#   select(-Area) %>%
#   filter(!Rep == "221006_Smp_S7_C1_D1_A") #sample to be removed
# 
# #export final dataset 
# write_csv(p.q.r.dat, file = "Intermediates/Particualte_Final_Quant_QCed.csv")
# 




# #d.qc.r <- read_csv(d.qc.file.remove)
# # 
# # d.edgecase.dat <- read_csv(d.edgecase.file) %>%
# #   rename("redone.conc" = EE.adjust.conc) %>%
# #   select(Rep, Compound, redone.conc) %>%
# #   filter(Compound == "Glycine betaine")
# #  mutate(SampID = "KM1906_GBT_F2_T0",
# #         min.area.flag = NA,
# #         blk.lod.flag = NA,
# #         smp.remove = NA
# 
# 
# d.lod.dat <- read_csv(d.lod.file) %>%
#   rename("Compound" = Name)
# 
# ##incorporate blk data imputation and blk subtraction into final dataset:
# d.q.r.dat <- d.qc.r %>%
#   filter(!str_detect(Rep, "Std")) %>%
#   filter(!str_detect(Rep, "Blk")) %>%
#   filter(!str_detect(Rep, "Poo")) %>%
#   left_join(., d.q.dat) %>%
#   left_join(., d.lod.dat)  %>%
#  # left_join(., d.edgecase.dat) %>%             #Add in edgecase dat form weird GBT sample
#   # mutate(EE.adjust.conc = case_when(!is.na(redone.conc) ~ redone.conc,
#   #                                   TRUE ~ EE.adjust.conc)) %>%
#   # select(-redone.conc) %>%
#   mutate(Diss.Conc.nM.noblksub = EE.adjust.conc,
#          Blk.ave.conc.nM = EE.adjust.Blk.Av) %>%
#   mutate(Diss.Conc.nM = case_when(blk.lod.flag == "Flag" ~ EE.adjust.Blk.Av/2,
#                                   TRUE ~ EE.adjust.conc - EE.adjust.Blk.Av)) %>%
#   mutate(Diss.Nmol.C = Diss.Conc.nM*C,
#          Diss.Nmol.N = Diss.Conc.nM*N,
#          Diss.Nmol.S = Diss.Conc.nM*S) %>%
#   rename("LOD.nM" = EE.adjust.lod) %>%
#   mutate(LOD.nM.blk.sub = LOD.nM - EE.adjust.Blk.Av) #%>%
# #  select(Rep, SampID, replicate, Cruise, Compound, min.area.flag, blk.lod.flag, smp.remove,
# #         Diss.Conc.nM.noblksub, Blk.ave.conc.nM, Diss.Conc.nM, Diss.Nmol.C, Diss.Nmol.N, LOD.nM, LOD.nM.blk.sub)
#          
# 
# #Perform secondary QC following blank subtraction:
# d.q.r.dat.qc2 <- d.q.r.dat %>%
#   mutate(LOD.Flag.2 = case_when(Diss.Conc.nM <= LOD.nM.blk.sub ~ "Flag",
#                                 TRUE ~ NA)) %>%
#   mutate(Diss.Conc.nM.adj = case_when(LOD.Flag.2 == "Flag" ~ LOD.nM.blk.sub,
#                                       TRUE ~ Diss.Conc.nM)) %>%
#   mutate(LOD.nM.adj = LOD.nM.blk.sub,
#          Diss.Nmol.C.adj = Diss.Conc.nM.adj*C,
#          Diss.Nmol.N.adj = Diss.Conc.nM.adj*N,
#          Diss.Nmol.S.adj = Diss.Conc.nM.adj*S) %>%
#   filter(!is.na(Diss.Conc.nM.adj)) %>%
#   select(Rep, SampID, replicate, Cruise, Compound, smp.remove, Diss.Conc.nM.adj, Diss.Nmol.C.adj, Diss.Nmol.N.adj, Diss.Nmol.S.adj, LOD.nM.adj, LOD.Flag.2)


#write final dataset to csv:
#write_csv(d.q.r.dat.qc2, file = "Intermediates/Dissolved_Final_Quant_QCed.csv")




# c.q.r.dat <- c.qc.remove %>%
#   left_join(., c.q.dat) %>%
#   select(-Area)# %>%
# filter(!Rep == "221006_Smp_S7_C1_D1_A"). #sample to be removed

#export final dataset 
# write_csv(c.q.r.dat, file = "Intermediates/Culture_Final_Quant_QCed.csv")




#   
#   g2.qc.impute %>%
#   left_join(., g2.dat) %>%
#   mutate(Adjusted_Area = case_when(is.na(Adjusted_Area) ~ Area,
#                                    TRUE ~ Adjusted_Area)) %>%
#   select(Rep, SampID, replicate, Batch, SizeFrac, Compound, min.area.flag, blk.ratio.flag, Adjusted_Area)
# 
# #export final dataset 
# write_csv(g2.q.i.dat, file = "Intermediates/G2_Final_Areas_QCed.csv")
# 
# 



#get particulate sample key
# p.ids <- p.q.r.dat %>%
#   select(Rep, SampID, replicate, Cruise) %>%
#   unique() %>%
#   rename("Part.Rep" = Rep)




# d.ids <- d.q.r.dat %>%
#   select(Rep, SampID, replicate, Cruise) %>%
#   unique() %>%
#   rename("Diss.Rep" = Rep)