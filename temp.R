
Log_specimen_rate_1_data_combos2 <- MSE_calculator_with_alternate_data_sets(data8.03, outcome = "log_specimen_rate_1", V_lagG = 1, M_lagG = 0, S_lagG = 1, my_S1 = S_LSGR1)
Log_specimen_rate_2_data_combos2 <- MSE_calculator_with_alternate_data_sets(data8.03, outcome = "log_specimen_rate_2", V_lagG = 0, M_lagG = 0, S_lagG = 1, my_S1 = S_LSGR2)
Log_specimen_rate_3_data_combos2 <- MSE_calculator_with_alternate_data_sets(data8.03, outcome = "log_specimen_rate_3", V_lagG = 0, M_lagG = 0, S_lagG = 1, my_S1 = S_LSGR3)
write.csv(Log_rate_1_data_combos2, file = "Log_rate_1_data_combos2.csv")
write.csv(Log_rate_2_data_combos2, file = "Log_rate_2_data_combos2.csv")
write.csv(Log_rate_3_data_combos2, file = "Log_rate_3_data_combos2.csv")
write.csv(Log_specimen_rate_1_data_combos2, file = "Log_specimen_rate_1_data_combos2.csv")
write.csv(Log_specimen_rate_2_data_combos2, file = "Log_specimen_rate_2_data_combos2.csv")
write.csv(Log_specimen_rate_3_data_combos2, file = "Log_specimen_rate_3_data_combos2.csv")
##Testing the predictive power of the optimal models by LTLA and week
#The following custom function estimated the MSE for each observed week in the master data set for a given outcome and its optimal model. Unlike previous MSE calculators that returned the MSE for the entire master data set, this function returned a data frame with the MSE for each week tested. This allowed for the MSE for each LTLA and week to be estimated using the by() function, whilst still fitting the model to the entire data set.
MSE_calculator_with_specified_formulas2 <- function(data_LTLA, outcome = "log_rate_1",formulat, V_lagG = 0, M_lagG = 0, S_lagG = 0){
  prep_data <- do.call(rbind, by(data_LTLA,data_LTLA$Map,my_lag,V_lag = V_lagG,M_lag = M_lagG,S_lag = S_lagG))
  all_MSEs <- data.frame()
  for(i in 39:length(unique(prep_data$week_index))){
    print(i)
    formulaP <- as.formula(formulat)
    training_set <- prep_data[prep_data$week_index <= i-1,]
    testing_set <- prep_data[prep_data$week_index == i,]
    LTLA_names <- testing_set[,"Map"]
    week_index <- testing_set[,"week_index"]
    model <- lm(formulaP,training_set)
    pred <- predict(model,testing_set)
    true_value <- prep_data[prep_data$week_index == i, outcome]
    MSE <- cbind(pred,true_value)
    MSE <- as.data.frame(MSE)
    MSE$the_mse <- (MSE$pred-MSE$true_value)^2
    MSE$LTLA_names <- LTLA_names
    MSE$week_index <- week_index
    print(head(MSE))
    all_MSEs <- rbind(all_MSEs, MSE)
  }
  colnames(all_MSEs) <- c("pred","true_value","the_mse","LTLA_names","week_index")
  all_MSEs
}
#For the function to work, the optimal formula for each outcome was specifed
formula_for_LGR1 <- c("log_rate_1~Map+cumVaccinationFirstDoseUptakeByVaccinationDatePercentage+cumVaccinationSecondDoseUptakeByVaccinationDatePercentage+change_symptom.Cough+change_symptom.Fever+change_symptom.Fatigue+change_symptom.Diarrhea+change_symptom.Vomiting+change_symptom.Muscle.weakness+change_symptom.Sputum+change_symptom.Shortness.of.breath+change_symptom.Confusion+change_symptom.Chest.pain+change_symptom.Acne+change_symptom.Adrenal.crisis+change_symptom.Astigmatism+change_symptom.Beau.s.lines+change_symptom.Binge.eating+change_symptom.Bleeding.on.probing+change_symptom.Blepharospasm+change_symptom.Blushing+change_symptom.Boil+change_symptom.Braxton.Hicks.contractions+change_symptom.Breast.pain+change_symptom.Bruxism+change_symptom.Burning.mouth.syndrome+change_symptom.Cataract+change_symptom.Cirrhosis+change_symptom.Compulsive.behavior+change_symptom.Dandruff+change_symptom.Diabetic.ketoacidosis+change_symptom.Dysgeusia+change_symptom.Eczema+change_symptom.Erectile.dysfunction+change_symptom.Erythema.chronicum.migrans+change_symptom.Fecal.incontinence+change_symptom.Genital.wart+change_symptom.Globus.pharyngis+change_symptom.Gout+change_symptom.Heart.murmur+change_symptom.Hematoma+change_symptom.Hemoptysis+change_symptom.Hepatitis+change_symptom.Hip.pain+change_symptom.Hypercholesterolemia+change_symptom.Hyperkalemia+change_symptom.Hyperthyroidism+change_symptom.Hypertrophy+change_symptom.Hypochondriasis+change_symptom.Hypogonadism+change_symptom.Impetigo+change_symptom.Intracranial.pressure+change_symptom.Meningitis+change_symptom.Menorrhagia+change_symptom.Myocardial.infarction+change_symptom.Neck.mass+change_symptom.Night.sweats+change_symptom.Night.terror+change_symptom.Nocturnal.enuresis+change_symptom.Onychorrhexis+change_symptom.Oral.candidiasis+change_symptom.Papule+change_symptom.Paresthesia+change_symptom.Photopsia+change_symptom.Pleurisy+change_symptom.Podalgia+change_symptom.Rectal.pain+change_symptom.Scoliosis+change_symptom.Sexual.dysfunction+change_symptom.Sleep.deprivation+change_symptom.Splenomegaly+change_symptom.Strabismus+change_symptom.Suicidal.ideation+change_symptom.Swollen.lymph.nodes+change_symptom.Testicular.pain+change_symptom.Thrombocytopenia+change_symptom.Tonsillitis+change_symptom.Tumor+change_symptom.Unconsciousness+change_symptom.Vaginal.bleeding+change_symptom.Varicose.veins+change_symptom.pancreatitis")
formula_for_LGR2 <- c("log_rate_2~Map+cumVaccinationFirstDoseUptakeByVaccinationDatePercentage+cumVaccinationSecondDoseUptakeByVaccinationDatePercentage+retail_and_recreation_percent_change_from_baseline+grocery_and_pharmacy_percent_change_from_baseline+parks_percent_change_from_baseline+transit_stations_percent_change_from_baseline+residential_percent_change_from_baseline+workplaces_percent_change_from_baseline+change_symptom.Cough+change_symptom.Fever+change_symptom.Fatigue+change_symptom.Diarrhea+change_symptom.Vomiting+change_symptom.Muscle.weakness+change_symptom.Sputum+change_symptom.Shortness.of.breath+change_symptom.Confusion+change_symptom.Chest.pain+change_symptom.Abdominal.pain+change_symptom.Adrenal.crisis+change_symptom.Amnesia+change_symptom.Anal.fissure+change_symptom.Anaphylaxis+change_symptom.Anemia+change_symptom.Angioedema+change_symptom.Anxiety+change_symptom.Apnea+change_symptom.Arthralgia+change_symptom.Back.pain+change_symptom.Beau.s.lines+change_symptom.Bell.s.palsy+change_symptom.Binge.eating+change_symptom.Blood.in.stool+change_symptom.Bone.tumor+change_symptom.Breast.pain+change_symptom.Bruxism+change_symptom.Candidiasis+change_symptom.Cataract+change_symptom.Cheilitis+change_symptom.Chorea+change_symptom.Chronic.pain+change_symptom.Congenital.heart.defect+change_symptom.Conjunctivitis+change_symptom.Dandruff+change_symptom.Dementia+change_symptom.Dry.eye.syndrome+change_symptom.Dysuria+change_symptom.Esophagitis+change_symptom.Fibrillation+change_symptom.Floater+change_symptom.Food.craving+change_symptom.Generalized.anxiety.disorder+change_symptom.Genital.wart+change_symptom.Halitosis+change_symptom.Heart.murmur+change_symptom.Hematuria+change_symptom.Hemoptysis+change_symptom.Hydrocephalus+change_symptom.Hyperglycemia+change_symptom.Hyperthyroidism+change_symptom.Hypochondriasis+change_symptom.Infection+change_symptom.Inflammation+change_symptom.Intracranial.pressure+change_symptom.Lactose.intolerance+change_symptom.Lymphedema+change_symptom.Male.infertility+change_symptom.Manic.Disorder+change_symptom.Melena+change_symptom.Morning.sickness+change_symptom.Motion.sickness+change_symptom.Mydriasis+change_symptom.Myocardial.infarction+change_symptom.Neck.mass+change_symptom.Nerve.injury+change_symptom.Night.sweats+change_symptom.Onychorrhexis+change_symptom.Oral.candidiasis+change_symptom.Orthostatic.hypotension+change_symptom.Paranoia+change_symptom.Paresthesia+change_symptom.Phlegm+change_symptom.Pleural.effusion+change_symptom.Pleurisy+change_symptom.Pneumonia+change_symptom.Post.nasal.drip+change_symptom.Pus+change_symptom.Red.eye+change_symptom.Renal.colic+change_symptom.Rosacea+change_symptom.Sciatica+change_symptom.Seborrheic.dermatitis+change_symptom.Skin.tag+change_symptom.Splenomegaly+change_symptom.Stomach.rumble+change_symptom.Stretch.marks+change_symptom.Swollen.lymph.nodes+change_symptom.Tonsillitis+change_symptom.Type.2.diabetes+change_symptom.Unconsciousness+change_symptom.Vaginal.bleeding+change_symptom.Vasculitis+change_symptom.Wart")
formula_for_LGR3 <- c("log_rate_3~Map+cumVaccinationFirstDoseUptakeByVaccinationDatePercentage+cumVaccinationSecondDoseUptakeByVaccinationDatePercentage+retail_and_recreation_percent_change_from_baseline+grocery_and_pharmacy_percent_change_from_baseline+parks_percent_change_from_baseline+transit_stations_percent_change_from_baseline+residential_percent_change_from_baseline+workplaces_percent_change_from_baseline+change_symptom.Cough+change_symptom.Fever+change_symptom.Fatigue+change_symptom.Diarrhea+change_symptom.Vomiting+change_symptom.Muscle.weakness+change_symptom.Sputum+change_symptom.Shortness.of.breath+change_symptom.Confusion+change_symptom.Chest.pain+change_symptom.Abdominal.obesity+change_symptom.Actinic.keratosis+change_symptom.Allergy+change_symptom.Apnea+change_symptom.Arthralgia+change_symptom.Astigmatism+change_symptom.Avoidant.personality.disorder+change_symptom.Bacterial.vaginosis+change_symptom.Biliary.colic+change_symptom.Binge.eating+change_symptom.Bleeding.on.probing+change_symptom.Boil+change_symptom.Bone.fracture+change_symptom.Breakthrough.bleeding+change_symptom.Breast.pain+change_symptom.Bronchitis+change_symptom.Bunion+change_symptom.Burning.mouth.syndrome+change_symptom.Canker.sore+change_symptom.Cheilitis+change_symptom.Chills+change_symptom.Compulsive.behavior+change_symptom.Congenital.heart.defect+change_symptom.Conjunctivitis+change_symptom.Dentin.hypersensitivity+change_symptom.Dry.eye.syndrome+change_symptom.Dysmenorrhea+change_symptom.Dysphoria+change_symptom.Ear.pain+change_symptom.Edema+change_symptom.Eye.pain+change_symptom.Fibrocystic.breast.changes+change_symptom.Food.intolerance+change_symptom.Gastroesophageal.reflux.disease+change_symptom.Gingival.recession+change_symptom.Globus.pharyngis+change_symptom.Gout+change_symptom.Grandiosity+change_symptom.Hair.loss+change_symptom.Hay.fever+change_symptom.Heart.murmur+change_symptom.Hemoptysis+change_symptom.Hydrocephalus+change_symptom.Hypercholesterolemia+change_symptom.Hyperthermia+change_symptom.Hyperthyroidism+change_symptom.Hypochondriasis+change_symptom.Impulsivity+change_symptom.Infection+change_symptom.Ingrown.hair+change_symptom.Insomnia+change_symptom.Insulin.resistance+change_symptom.Intermenstrual.bleeding+change_symptom.Irregular.menstruation+change_symptom.Kyphosis+change_symptom.Lactose.intolerance+change_symptom.Lymphedema+change_symptom.Manic.Disorder+change_symptom.Menorrhagia+change_symptom.Mood.swing+change_symptom.Night.sweats+change_symptom.Onychorrhexis+change_symptom.Otitis.externa+change_symptom.Palpitations+change_symptom.Papule+change_symptom.Paranoia+change_symptom.Paresthesia+change_symptom.Pelvic.inflammatory.disease+change_symptom.Pleurisy+change_symptom.Post.nasal.drip+change_symptom.Pulmonary.hypertension+change_symptom.Purpura+change_symptom.Pus+change_symptom.Rectal.pain+change_symptom.Rectal.prolapse+change_symptom.Restless.legs.syndrome+change_symptom.Rhinorrhea+change_symptom.Sciatica+change_symptom.Sensitivity.to.sound+change_symptom.Sharp.pain+change_symptom.Shivering+change_symptom.Skin.tag+change_symptom.Sleep.disorder+change_symptom.Sore.throat+change_symptom.Stomach.rumble+change_symptom.Stretch.marks+change_symptom.Stuttering+change_symptom.Tachycardia+change_symptom.Thrombocytopenia+change_symptom.Tic+change_symptom.Urinary.tract.infection+change_symptom.Uterine.contraction+change_symptom.Vaginal.bleeding+change_symptom.Wart+change_symptom.Weakness+change_symptom.Wheeze+change_symptom.Xerostomia+change_symptom.Yawn+change_symptom.pancreatitis")
formula_for_LSGR1 <- c("log_specimen_rate_1~Map+cumVaccinationFirstDoseUptakeByVaccinationDatePercentage+cumVaccinationSecondDoseUptakeByVaccinationDatePercentage+retail_and_recreation_percent_change_from_baseline+grocery_and_pharmacy_percent_change_from_baseline+parks_percent_change_from_baseline+transit_stations_percent_change_from_baseline+residential_percent_change_from_baseline+workplaces_percent_change_from_baseline+change_symptom.Cough+change_symptom.Fever+change_symptom.Fatigue+change_symptom.Diarrhea+change_symptom.Vomiting+change_symptom.Muscle.weakness+change_symptom.Sputum+change_symptom.Shortness.of.breath+change_symptom.Confusion+change_symptom.Chest.pain+change_symptom.Abdominal.obesity+change_symptom.Adrenal.crisis+change_symptom.Amenorrhea+change_symptom.Anaphylaxis+change_symptom.Ascites+change_symptom.Asthma+change_symptom.Astigmatism+change_symptom.Beau.s.lines+change_symptom.Binge.eating+change_symptom.Bleeding+change_symptom.Bleeding.on.probing+change_symptom.Blepharospasm+change_symptom.Blurred.vision+change_symptom.Boil+change_symptom.Breast.pain+change_symptom.Burning.mouth.syndrome+change_symptom.Cardiac.arrest+change_symptom.Carpal.tunnel.syndrome+change_symptom.Cataract+change_symptom.Chorea+change_symptom.Cirrhosis+change_symptom.Congenital.heart.defect+change_symptom.Convulsion+change_symptom.Depression+change_symptom.Diabetes+change_symptom.Dry.eye.syndrome+change_symptom.Edema+change_symptom.Encephalitis+change_symptom.Epilepsy+change_symptom.Epiphora+change_symptom.Eye.pain+change_symptom.Eye.strain+change_symptom.Fatty.liver.disease+change_symptom.Fibrocystic.breast.changes+change_symptom.Food.intolerance+change_symptom.Genital.wart+change_symptom.Gingival.recession+change_symptom.Globus.pharyngis+change_symptom.Gout+change_symptom.Grandiosity+change_symptom.Headache+change_symptom.Heart.murmur+change_symptom.Heartburn+change_symptom.Hematochezia+change_symptom.Hemoptysis+change_symptom.Hemorrhoids+change_symptom.Hepatitis+change_symptom.Hyperthyroidism+change_symptom.Hypoglycemia+change_symptom.Hypogonadism+change_symptom.Hyponatremia+change_symptom.Impulsivity+change_symptom.Infection+change_symptom.Ingrown.hair+change_symptom.Insomnia+change_symptom.Intracranial.pressure+change_symptom.Jaundice+change_symptom.Lactose.intolerance+change_symptom.Leg.cramps+change_symptom.Lesion+change_symptom.Major.depressive.disorder+change_symptom.Migraine+change_symptom.Mouth.ulcer+change_symptom.Nasal.congestion+change_symptom.Neck.mass+change_symptom.Neonatal.jaundice+change_symptom.Nerve.injury+change_symptom.Night.sweats+change_symptom.Obesity+change_symptom.Onychorrhexis+change_symptom.Orthostatic.hypotension+change_symptom.Osteopenia+change_symptom.Palpitations+change_symptom.Papule+change_symptom.Paranoia+change_symptom.Pericarditis+change_symptom.Periodontal.disease+change_symptom.Pleurisy+change_symptom.Pneumonia+change_symptom.Pus+change_symptom.Red.eye+change_symptom.Self.harm+change_symptom.Sharp.pain+change_symptom.Sinusitis+change_symptom.Sleep.apnea+change_symptom.Sleep.disorder+change_symptom.Sore.throat+change_symptom.Stretch.marks+change_symptom.Swollen.lymph.nodes+change_symptom.Testicular.pain+change_symptom.Thrombocytopenia+change_symptom.Vasculitis+change_symptom.Weakness+change_symptom.Wheeze+change_symptom.hyperhidrosis")
formula_for_LSGR2 <- c("log_specimen_rate_2~Map+cumVaccinationFirstDoseUptakeByVaccinationDatePercentage+cumVaccinationSecondDoseUptakeByVaccinationDatePercentage+retail_and_recreation_percent_change_from_baseline+grocery_and_pharmacy_percent_change_from_baseline+parks_percent_change_from_baseline+transit_stations_percent_change_from_baseline+residential_percent_change_from_baseline+workplaces_percent_change_from_baseline+change_symptom.Cough+change_symptom.Fever+change_symptom.Fatigue+change_symptom.Diarrhea+change_symptom.Vomiting+change_symptom.Muscle.weakness+change_symptom.Sputum+change_symptom.Shortness.of.breath+change_symptom.Confusion+change_symptom.Chest.pain+change_symptom.Abdominal.obesity+change_symptom.Abdominal.pain+change_symptom.Amenorrhea+change_symptom.Angioedema+change_symptom.Apnea+change_symptom.Arthralgia+change_symptom.Arthritis+change_symptom.Asthma+change_symptom.Astigmatism+change_symptom.Biliary.colic+change_symptom.Binge.eating+change_symptom.Bloating+change_symptom.Blurred.vision+change_symptom.Blushing+change_symptom.Boil+change_symptom.Bradycardia+change_symptom.Breakthrough.bleeding+change_symptom.Breast.pain+change_symptom.Canker.sore+change_symptom.Cardiac.arrest+change_symptom.Cheilitis+change_symptom.Chorea+change_symptom.Cirrhosis+change_symptom.Congenital.heart.defect+change_symptom.Dizziness+change_symptom.Dry.eye.syndrome+change_symptom.Dysphagia+change_symptom.Edema+change_symptom.Epilepsy+change_symptom.Erythema.chronicum.migrans+change_symptom.Facial.nerve.paralysis+change_symptom.Fasciculation+change_symptom.Fibrocystic.breast.changes+change_symptom.Fibromyalgia+change_symptom.Folate.deficiency+change_symptom.Frequent.urination+change_symptom.Genital.wart+change_symptom.Gingival.recession+change_symptom.Gingivitis+change_symptom.Gout+change_symptom.Heart.arrhythmia+change_symptom.Heart.murmur+change_symptom.Hemoptysis+change_symptom.Hemorrhoids+change_symptom.Hepatitis+change_symptom.Hypermobility+change_symptom.Hyperpigmentation+change_symptom.Hyperthermia+change_symptom.Hyperthyroidism+change_symptom.Hypoglycemia+change_symptom.Hypogonadism+change_symptom.Impulsivity+change_symptom.Inflammation+change_symptom.Ingrown.hair+change_symptom.Intermenstrual.bleeding+change_symptom.Irregular.menstruation+change_symptom.Itch+change_symptom.Kyphosis+change_symptom.Lesion+change_symptom.Leukorrhea+change_symptom.Lymphedema+change_symptom.Morning.sickness+change_symptom.Nausea+change_symptom.Neck.mass+change_symptom.Neck.pain+change_symptom.Nerve.injury+change_symptom.Orthostatic.hypotension+change_symptom.Osteopenia+change_symptom.Papule+change_symptom.Paresthesia+change_symptom.Pericarditis+change_symptom.Photopsia+change_symptom.Post.nasal.drip+change_symptom.Pus+change_symptom.Rectal.pain+change_symptom.Red.eye+change_symptom.Rosacea+change_symptom.Scar+change_symptom.Scoliosis+change_symptom.Sharp.pain+change_symptom.Shivering+change_symptom.Shyness+change_symptom.Skin.tag+change_symptom.Sleep.apnea+change_symptom.Sore.throat+change_symptom.Tenderness+change_symptom.Testicular.pain+change_symptom.Tic+change_symptom.Upper.respiratory.tract.infection+change_symptom.Vaginal.discharge+change_symptom.Vasculitis+change_symptom.Wart+change_symptom.Wheeze+change_symptom.Xeroderma")
formula_for_LSGR3 <- c("log_specimen_rate_3~Map+cumVaccinationFirstDoseUptakeByVaccinationDatePercentage+cumVaccinationSecondDoseUptakeByVaccinationDatePercentage+retail_and_recreation_percent_change_from_baseline+grocery_and_pharmacy_percent_change_from_baseline+parks_percent_change_from_baseline+transit_stations_percent_change_from_baseline+residential_percent_change_from_baseline+workplaces_percent_change_from_baseline+change_symptom.Cough+change_symptom.Fever+change_symptom.Fatigue+change_symptom.Diarrhea+change_symptom.Vomiting+change_symptom.Muscle.weakness+change_symptom.Sputum+change_symptom.Shortness.of.breath+change_symptom.Confusion+change_symptom.Chest.pain+change_symptom.Abdominal.obesity+change_symptom.Abdominal.pain+change_symptom.Acne+change_symptom.Actinic.keratosis+change_symptom.Adrenal.crisis+change_symptom.Allergy+change_symptom.Anaphylaxis+change_symptom.Aphasia+change_symptom.Arthralgia+change_symptom.Astigmatism+change_symptom.Autoimmune.disease+change_symptom.Bacterial.vaginosis+change_symptom.Binge.eating+change_symptom.Bloating+change_symptom.Blood.in.stool+change_symptom.Boil+change_symptom.Bone.fracture+change_symptom.Bradycardia+change_symptom.Braxton.Hicks.contractions+change_symptom.Breast.pain+change_symptom.Bruxism+change_symptom.Candidiasis+change_symptom.Canker.sore+change_symptom.Chills+change_symptom.Colitis+change_symptom.Coma+change_symptom.Convulsion+change_symptom.Dizziness+change_symptom.Dry.eye.syndrome+change_symptom.Dysphagia+change_symptom.Eczema+change_symptom.Edema+change_symptom.Encephalopathy+change_symptom.Esophagitis+change_symptom.Fasciculation+change_symptom.Fibrocystic.breast.changes+change_symptom.Folate.deficiency+change_symptom.Gingival.recession+change_symptom.Gingivitis+change_symptom.Gout+change_symptom.Hair.loss+change_symptom.Hematochezia+change_symptom.Hematoma+change_symptom.Hemoptysis+change_symptom.Hemorrhoids+change_symptom.Hydrocephalus+change_symptom.Hypercholesterolemia+change_symptom.Hyperglycemia+change_symptom.Hyperthermia+change_symptom.Hyperthyroidism+change_symptom.Hypochondriasis+change_symptom.Hypoglycemia+change_symptom.Hypogonadism+change_symptom.Impulsivity+change_symptom.Infection+change_symptom.Inflammatory.bowel.disease+change_symptom.Ingrown.hair+change_symptom.Insulin.resistance+change_symptom.Irregular.menstruation+change_symptom.Kidney.stone+change_symptom.Lactose.intolerance+change_symptom.Leukorrhea+change_symptom.Lightheadedness+change_symptom.Low.back.pain+change_symptom.Melasma+change_symptom.Menorrhagia+change_symptom.Milium+change_symptom.Myalgia+change_symptom.Mydriasis+change_symptom.Nausea+change_symptom.Neck.pain+change_symptom.Neonatal.jaundice+change_symptom.Nerve.injury+change_symptom.Night.terror+change_symptom.Nocturnal.enuresis+change_symptom.Nosebleed+change_symptom.Onychorrhexis+change_symptom.Oral.candidiasis+change_symptom.Osteopenia+change_symptom.Otitis+change_symptom.Palpitations+change_symptom.Paranoia+change_symptom.Paresthesia+change_symptom.Pelvic.inflammatory.disease+change_symptom.Periorbital.puffiness+change_symptom.Perspiration+change_symptom.Post.nasal.drip+change_symptom.Prediabetes+change_symptom.Pulmonary.hypertension+change_symptom.Purpura+change_symptom.Pus+change_symptom.Radiculopathy+change_symptom.Rectal.pain+change_symptom.Rectal.prolapse+change_symptom.Red.eye+change_symptom.Renal.colic+change_symptom.Rhinitis+change_symptom.Rhinorrhea+change_symptom.Scar+change_symptom.Sharp.pain+change_symptom.Shivering+change_symptom.Skin.tag+change_symptom.Sore.throat+change_symptom.Splenomegaly+change_symptom.Suicidal.ideation+change_symptom.Swollen.lymph.nodes+change_symptom.Tachycardia+change_symptom.Telangiectasia+change_symptom.Testicular.pain+change_symptom.Throat.irritation+change_symptom.Tinnitus+change_symptom.Tonsillitis+change_symptom.Type.2.diabetes+change_symptom.Upper.respiratory.tract.infection+change_symptom.Vertigo+change_symptom.Wheeze+change_symptom.Xeroderma")
#Then the data sets with MSEs per each LTLA and week were prepared
all_MSEs_LGR1 <- MSE_calculator_with_specified_formulas2(data8.03, outcome = "log_rate_1", formulat = formula_for_LGR1, V_lagG = 0, M_lagG = 1, S_lagG = 1)
all_MSEs_LGR2 <- MSE_calculator_with_specified_formulas2(data8.03, outcome = "log_rate_2", formulat = formula_for_LGR2, V_lagG = 1, M_lagG = 2, S_lagG = 0)
all_MSEs_LGR3 <- MSE_calculator_with_specified_formulas2(data8.03, outcome = "log_rate_3", formulat = formula_for_LGR3, V_lagG = 0, M_lagG = 1, S_lagG = 1)
all_MSEs_LSGR1 <- MSE_calculator_with_specified_formulas2(data8.03, outcome = "log_specimen_rate_1", formulat = formula_for_LSGR1, V_lagG = 1, M_lagG = 0, S_lagG = 1)
all_MSEs_LSGR2 <- MSE_calculator_with_specified_formulas2(data8.03, outcome = "log_specimen_rate_2", formulat = formula_for_LSGR2, V_lagG = 0, M_lagG = 0, S_lagG = 1)
all_MSEs_LSGR3 <- MSE_calculator_with_specified_formulas2(data8.03, outcome = "log_specimen_rate_3", formulat = formula_for_LSGR3, V_lagG = 0, M_lagG = 0, S_lagG = 1)
#With these data sets prepared, we could then estimate the MSE by LTLA and week using the by() function, and save the results in a data frame.
MSEs_by_LTLA <- by(all_MSEs_LGR1,all_MSEs_LGR1$LTLA_names,function(x) mean(x$the_mse,na.rm = TRUE))
MSEs_by_week <- by(all_MSEs_LGR1,all_MSEs_LGR1$week_index,function(x) mean(x$the_mse,na.rm = TRUE))
MSE_by_LTLA2 <- as.list(MSEs_by_LTLA)
MSE_by_week2 <- as.list(MSEs_by_week)
MSE_by_LTLA_LGR1 <- data.frame(LTLA = names(MSE_by_LTLA2), MSE = unlist(MSE_by_LTLA2))
MSE_by_week_LGR1 <- data.frame(week = names(MSE_by_week2), MSE = unlist(MSE_by_week2))
MSEs_by_LTLA <- by(all_MSEs_LGR2,all_MSEs_LGR2$LTLA_names,function(x) mean(x$the_mse,na.rm = TRUE))
MSEs_by_week <- by(all_MSEs_LGR2,all_MSEs_LGR2$week_index,function(x) mean(x$the_mse,na.rm = TRUE))
MSE_by_LTLA2 <- as.list(MSEs_by_LTLA)
MSE_by_week2 <- as.list(MSEs_by_week)
MSE_by_LTLA_LGR2 <- data.frame(LTLA = names(MSE_by_LTLA2), MSE = unlist(MSE_by_LTLA2))
MSE_by_week_LGR2 <- data.frame(week = names(MSE_by_week2), MSE = unlist(MSE_by_week2))
MSEs_by_LTLA <- by(all_MSEs_LGR3,all_MSEs_LGR3$LTLA_names,function(x) mean(x$the_mse,na.rm = TRUE))
MSEs_by_week <- by(all_MSEs_LGR3,all_MSEs_LGR3$week_index,function(x) mean(x$the_mse,na.rm = TRUE))
MSE_by_LTLA2 <- as.list(MSEs_by_LTLA)
MSE_by_week2 <- as.list(MSEs_by_week)
MSE_by_LTLA_LGR3 <- data.frame(LTLA = names(MSE_by_LTLA2), MSE = unlist(MSE_by_LTLA2))
MSE_by_week_LGR3 <- data.frame(week = names(MSE_by_week2), MSE = unlist(MSE_by_week2))
MSEs_by_LTLA <- by(all_MSEs_LSGR1,all_MSEs_LSGR1$LTLA_names,function(x) mean(x$the_mse,na.rm = TRUE))
MSEs_by_week <- by(all_MSEs_LSGR1,all_MSEs_LSGR1$week_index,function(x) mean(x$the_mse,na.rm = TRUE))
MSE_by_LTLA2 <- as.list(MSEs_by_LTLA)
MSE_by_week2 <- as.list(MSEs_by_week)
MSE_by_LTLA_LSGR1 <- data.frame(LTLA = names(MSE_by_LTLA2), MSE = unlist(MSE_by_LTLA2))
MSE_by_week_LSGR1 <- data.frame(week = names(MSE_by_week2), MSE = unlist(MSE_by_week2))
MSEs_by_LTLA <- by(all_MSEs_LSGR2,all_MSEs_LSGR2$LTLA_names,function(x) mean(x$the_mse,na.rm = TRUE))
MSEs_by_week <- by(all_MSEs_LSGR2,all_MSEs_LSGR2$week_index,function(x) mean(x$the_mse,na.rm = TRUE))
MSE_by_LTLA2 <- as.list(MSEs_by_LTLA)
MSE_by_week2 <- as.list(MSEs_by_week)
MSE_by_LTLA_LSGR2 <- data.frame(LTLA = names(MSE_by_LTLA2), MSE = unlist(MSE_by_LTLA2))
MSE_by_week_LSGR2 <- data.frame(week = names(MSE_by_week2), MSE = unlist(MSE_by_week2))
MSEs_by_LTLA <- by(all_MSEs_LSGR3,all_MSEs_LSGR3$LTLA_names,function(x) mean(x$the_mse,na.rm = TRUE))
MSEs_by_week <- by(all_MSEs_LSGR3,all_MSEs_LSGR3$week_index,function(x) mean(x$the_mse,na.rm = TRUE))
MSE_by_LTLA2 <- as.list(MSEs_by_LTLA)
MSE_by_week2 <- as.list(MSEs_by_week)
MSE_by_LTLA_LSGR3 <- data.frame(LTLA = names(MSE_by_LTLA2), MSE = unlist(MSE_by_LTLA2))
MSE_by_week_LSGR3 <- data.frame(week = names(MSE_by_week2), MSE = unlist(MSE_by_week2))
#All of these results were then saved as csv files
write.csv(MSE_by_LTLA_LGR1, file = "MSE_by_LTLA_LGR1.csv")
write.csv(MSE_by_LTLA_LGR2, file = "MSE_by_LTLA_LGR2.csv")
write.csv(MSE_by_LTLA_LGR3, file = "MSE_by_LTLA_LGR3.csv")
write.csv(MSE_by_LTLA_LSGR1, file = "MSE_by_LTLA_LSGR1.csv")
write.csv(MSE_by_LTLA_LSGR2, file = "MSE_by_LTLA_LSGR2.csv")
write.csv(MSE_by_LTLA_LSGR3, file = "MSE_by_LTLA_LSGR3.csv")
write.csv(MSE_by_week_LGR1, file = "MSE_by_week_LGR1.csv")
write.csv(MSE_by_week_LGR2, file = "MSE_by_week_LGR2.csv")
write.csv(MSE_by_week_LGR3, file = "MSE_by_week_LGR3.csv")
write.csv(MSE_by_week_LSGR1, file = "MSE_by_week_LSGR1.csv")
write.csv(MSE_by_week_LSGR2, file = "MSE_by_week_LSGR2.csv")
write.csv(MSE_by_week_LSGR3, file = "MSE_by_week_LSGR3.csv")
#Estimating Naive models by LTLA an week.
#As with the optimal model function, this function is a modified form of the Naive model MSE estimator that returns a data frame of the MSEs for each observed week, allowing the MSE by LTLA and week to be estimated.
my_naive_model_MSE2 <- function(data_LTLA, h = 1, y= "log_rate_1"){
  prep_data <- data_LTLA[39:nrow(data_LTLA),]
  true_value <- c()
  pred <- c()
  LTLA_names <- prep_data[,"Map"]
  week_index <- prep_data[,"week_index"]
  for(i in 39:(nrow(data_LTLA))){
    true_value <- c(true_value,as.numeric(data_LTLA[i,y]))
    pred <- c(pred,as.numeric(data_LTLA[i-h,y]))
  }
  MSE <- cbind(pred,true_value)
  MSE <- as.data.frame(MSE)
  MSE$the_mse <- (MSE$pred-MSE$true_value)^2
  MSE$LTLA_names <- LTLA_names
  MSE$week_index <- week_index
  MSE
}
#With this we prepared the data sets for the Naive MSEs by LTLA and week.
Naive_MSEs_LGR1 <- do.call(rbind, by(data8.03,data8.03$Map,my_naive_model_MSE2,h = 2, y = "log_rate_1"))
Naive_MSEs_LGR2 <- do.call(rbind, by(data8.03,data8.03$Map,my_naive_model_MSE2,h = 1, y = "log_rate_2"))
Naive_MSEs_LGR3 <- do.call(rbind, by(data8.03,data8.03$Map,my_naive_model_MSE2,h = 1, y = "log_rate_3"))
Naive_MSEs_LSGR1 <- do.call(rbind, by(data8.03,data8.03$Map,my_naive_model_MSE2,h = 2, y = "log_specimen_rate_1"))
Naive_MSEs_LSGR2 <- do.call(rbind, by(data8.03,data8.03$Map,my_naive_model_MSE2,h = 1, y = "log_specimen_rate_2"))
Naive_MSEs_LSGR3 <- do.call(rbind, by(data8.03,data8.03$Map,my_naive_model_MSE2,h = 1, y = "log_specimen_rate_3"))
#With these data sets prepared, we could then estimate the Naive MSE by LTLA and week using the by() function, and save the results in a data frame.
MSEs_by_LTLA <- by(Naive_MSEs_LGR1,Naive_MSEs_LGR1$LTLA_names, function(x) mean(x$the_mse, na.rm = TRUE))
MSEs_by_week <- by(Naive_MSEs_LGR1,Naive_MSEs_LGR1$week_index, function(x) mean(x$the_mse, na.rm = TRUE))
MSE_by_LTLA2 <- as.list(MSEs_by_LTLA)
Naive_MSE_by_LTLA_LGR1 <- data.frame(LTLA = names(MSE_by_LTLA2), MSE = unlist(MSE_by_LTLA2))
MSE_by_week2 <- as.list(MSEs_by_week)
Naive_MSE_by_week_LGR1 <- data.frame(week = names(MSE_by_week2), MSE = unlist(MSE_by_week2))
MSEs_by_LTLA <- by(Naive_MSEs_LGR2,Naive_MSEs_LGR1$LTLA_names, function(x) mean(x$the_mse, na.rm = TRUE))
MSEs_by_week <- by(Naive_MSEs_LGR2,Naive_MSEs_LGR2$week_index, function(x) mean(x$the_mse, na.rm = TRUE))
MSE_by_LTLA2 <- as.list(MSEs_by_LTLA)
Naive_MSE_by_LTLA_LGR2 <- data.frame(LTLA = names(MSE_by_LTLA2), MSE = unlist(MSE_by_LTLA2))
MSE_by_week2 <- as.list(MSEs_by_week)
Naive_MSE_by_week_LGR2 <- data.frame(week = names(MSE_by_week2), MSE = unlist(MSE_by_week2))
MSEs_by_LTLA <- by(Naive_MSEs_LGR3,Naive_MSEs_LGR3$LTLA_names, function(x) mean(x$the_mse, na.rm = TRUE))
MSEs_by_week <- by(Naive_MSEs_LGR3,Naive_MSEs_LGR3$week_index, function(x) mean(x$the_mse, na.rm = TRUE))
MSE_by_LTLA2 <- as.list(MSEs_by_LTLA)
Naive_MSE_by_LTLA_LGR3 <- data.frame(LTLA = names(MSE_by_LTLA2), MSE = unlist(MSE_by_LTLA2))
MSE_by_week2 <- as.list(MSEs_by_week)
Naive_MSE_by_week_LGR3 <- data.frame(week = names(MSE_by_week2), MSE = unlist(MSE_by_week2))
MSEs_by_LTLA <- by(Naive_MSEs_LSGR1,Naive_MSEs_LSGR1$LTLA_names, function(x) mean(x$the_mse, na.rm = TRUE))
MSEs_by_week <- by(Naive_MSEs_LSGR1,Naive_MSEs_LSGR1$week_index, function(x) mean(x$the_mse, na.rm = TRUE))
MSE_by_LTLA2 <- as.list(MSEs_by_LTLA)
Naive_MSE_by_LTLA_LSGR1 <- data.frame(LTLA = names(MSE_by_LTLA2), MSE = unlist(MSE_by_LTLA2))
MSE_by_week2 <- as.list(MSEs_by_week)
Naive_MSE_by_week_LSGR1 <- data.frame(week = names(MSE_by_week2), MSE = unlist(MSE_by_week2))
MSEs_by_LTLA <- by(Naive_MSEs_LSGR2,Naive_MSEs_LSGR2$LTLA_names, function(x) mean(x$the_mse, na.rm = TRUE))
MSEs_by_week <- by(Naive_MSEs_LSGR2,Naive_MSEs_LSGR2$week_index, function(x) mean(x$the_mse, na.rm = TRUE))
MSE_by_LTLA2 <- as.list(MSEs_by_LTLA)
Naive_MSE_by_LTLA_LSGR2 <- data.frame(LTLA = names(MSE_by_LTLA2), MSE = unlist(MSE_by_LTLA2))
MSE_by_week2 <- as.list(MSEs_by_week)
Naive_MSE_by_week_LSGR2 <- data.frame(week = names(MSE_by_week2), MSE = unlist(MSE_by_week2))
MSEs_by_LTLA <- by(Naive_MSEs_LSGR3,Naive_MSEs_LSGR3$LTLA_names, function(x) mean(x$the_mse, na.rm = TRUE))
MSEs_by_week <- by(Naive_MSEs_LSGR3,Naive_MSEs_LSGR3$week_index, function(x) mean(x$the_mse, na.rm = TRUE))
MSE_by_LTLA2 <- as.list(MSEs_by_LTLA)
Naive_MSE_by_LTLA_LSGR3 <- data.frame(LTLA = names(MSE_by_LTLA2), MSE = unlist(MSE_by_LTLA2))
MSE_by_week2 <- as.list(MSEs_by_week)
Naive_MSE_by_week_LSGR3 <- data.frame(week = names(MSE_by_week2), MSE = unlist(MSE_by_week2))
#The results were then saved as csv files.
write.csv(Naive_MSE_by_LTLA_LGR1, file = "Naive_MSE_by_LTLA_LGR1.csv")
write.csv(Naive_MSE_by_LTLA_LGR2, file = "Naive_MSE_by_LTLA_LGR2.csv")
write.csv(Naive_MSE_by_LTLA_LGR3, file = "Naive_MSE_by_LTLA_LGR3.csv")
write.csv(Naive_MSE_by_LTLA_LSGR1, file = "Naive_MSE_by_LTLA_LSGR1.csv")
write.csv(Naive_MSE_by_LTLA_LSGR2, file = "Naive_MSE_by_LTLA_LSGR2.csv")
write.csv(Naive_MSE_by_LTLA_LSGR3, file = "Naive_MSE_by_LTLA_LSGR3.csv")
write.csv(Naive_MSE_by_week_LGR1, file = "Naive_MSE_by_week_LGR1.csv")
write.csv(Naive_MSE_by_week_LGR2, file = "Naive_MSE_by_week_LGR2.csv")
write.csv(Naive_MSE_by_week_LGR3, file = "Naive_MSE_by_week_LGR3.csv")
write.csv(Naive_MSE_by_week_LSGR1, file = "Naive_MSE_by_week_LSGR1.csv")
write.csv(Naive_MSE_by_week_LSGR2, file = "Naive_MSE_by_week_LSGR2.csv")
write.csv(Naive_MSE_by_week_LSGR3, file = "Naive_MSE_by_week_LSGR3.csv")
#Estimating the optimal model/naive model MSE ratios
#The data sets for the naive model results (by LTLA and week) were merged with the optimal mode results to be able to estimate the ratios.
MSE_ratio_by_LTLA_LGR1 <- left_join(Naive_MSE_by_LTLA_LGR1,MSE_by_LTLA_LGR1, by = c("LTLA"))
MSE_ratio_by_LTLA_LGR2 <- left_join(Naive_MSE_by_LTLA_LGR2,MSE_by_LTLA_LGR2, by = c("LTLA"))
MSE_ratio_by_LTLA_LGR3 <- left_join(Naive_MSE_by_LTLA_LGR3,MSE_by_LTLA_LGR3, by = c("LTLA"))
MSE_ratio_by_LTLA_LSGR1 <- left_join(Naive_MSE_by_LTLA_LSGR1,MSE_by_LTLA_LSGR1, by = c("LTLA"))
MSE_ratio_by_LTLA_LSGR2 <- left_join(Naive_MSE_by_LTLA_LSGR2,MSE_by_LTLA_LSGR2, by = c("LTLA"))
MSE_ratio_by_LTLA_LSGR3 <- left_join(Naive_MSE_by_LTLA_LSGR3,MSE_by_LTLA_LSGR3, by = c("LTLA"))
MSE_ratio_by_week_LGR1 <- left_join(Naive_MSE_by_week_LGR1,MSE_by_week_LGR1, by = c("week"))
MSE_ratio_by_week_LGR2 <- left_join(Naive_MSE_by_week_LGR2,MSE_by_week_LGR2, by = c("week"))
MSE_ratio_by_week_LGR3 <- left_join(Naive_MSE_by_week_LGR3,MSE_by_week_LGR3, by = c("week"))
MSE_ratio_by_week_LSGR1 <- left_join(Naive_MSE_by_week_LSGR1,MSE_by_week_LSGR1, by = c("week"))
MSE_ratio_by_week_LSGR2 <- left_join(Naive_MSE_by_week_LSGR2,MSE_by_week_LSGR2, by = c("week"))
MSE_ratio_by_week_LSGR3 <- left_join(Naive_MSE_by_week_LSGR3,MSE_by_week_LSGR3, by = c("week"))
#With the merged data sets, we then estimated the ratio of the MSE for the optimal model (equal to MSE.y) over the MSE for the naive model (equal to MSE.x), to see the change in predictive power for the optimal model per each LTLA and week.
MSE_ratio_by_LTLA_LGR1$ratio <- MSE_ratio_by_LTLA_LGR1$MSE.y/MSE_ratio_by_LTLA_LGR1$MSE.x
MSE_ratio_by_LTLA_LGR2$ratio <- MSE_ratio_by_LTLA_LGR2$MSE.y/MSE_ratio_by_LTLA_LGR2$MSE.x
MSE_ratio_by_LTLA_LGR3$ratio <- MSE_ratio_by_LTLA_LGR3$MSE.y/MSE_ratio_by_LTLA_LGR3$MSE.x
MSE_ratio_by_LTLA_LSGR1$ratio <- MSE_ratio_by_LTLA_LSGR1$MSE.y/MSE_ratio_by_LTLA_LSGR1$MSE.x
MSE_ratio_by_LTLA_LSGR2$ratio <- MSE_ratio_by_LTLA_LSGR2$MSE.y/MSE_ratio_by_LTLA_LSGR2$MSE.x
MSE_ratio_by_LTLA_LSGR3$ratio <- MSE_ratio_by_LTLA_LSGR3$MSE.y/MSE_ratio_by_LTLA_LSGR3$MSE.x
MSE_ratio_by_week_LGR1$ratio <- MSE_ratio_by_week_LGR1$MSE.y/MSE_ratio_by_week_LGR1$MSE.x
MSE_ratio_by_week_LGR2$ratio <- MSE_ratio_by_week_LGR2$MSE.y/MSE_ratio_by_week_LGR2$MSE.x
MSE_ratio_by_week_LGR3$ratio <- MSE_ratio_by_week_LGR3$MSE.y/MSE_ratio_by_week_LGR3$MSE.x
MSE_ratio_by_week_LSGR1$ratio <- MSE_ratio_by_week_LSGR1$MSE.y/MSE_ratio_by_week_LSGR1$MSE.x
MSE_ratio_by_week_LSGR2$ratio <- MSE_ratio_by_week_LSGR2$MSE.y/MSE_ratio_by_week_LSGR2$MSE.x
MSE_ratio_by_week_LSGR3$ratio <- MSE_ratio_by_week_LSGR3$MSE.y/MSE_ratio_by_week_LSGR3$MSE.x
#The data sets with the MSE ratios were then saved as csv files
write.csv(MSE_ratio_by_LTLA_LGR1, file = "MSE_ratio_by_LTLA_LGR1.csv")
write.csv(MSE_ratio_by_LTLA_LGR2, file = "MSE_ratio_by_LTLA_LGR2.csv")
write.csv(MSE_ratio_by_LTLA_LGR3, file = "MSE_ratio_by_LTLA_LGR3.csv")
write.csv(MSE_ratio_by_LTLA_LSGR1, file = "MSE_ratio_by_LTLA_LSGR1.csv")
write.csv(MSE_ratio_by_LTLA_LSGR2, file = "MSE_ratio_by_LTLA_LSGR2.csv")
write.csv(MSE_ratio_by_LTLA_LSGR3, file = "MSE_ratio_by_LTLA_LSGR3.csv")
write.csv(MSE_ratio_by_week_LGR1, file = "MSE_ratio_by_week_LGR1.csv")
write.csv(MSE_ratio_by_week_LGR2, file = "MSE_ratio_by_week_LGR2.csv")
write.csv(MSE_ratio_by_week_LGR3, file = "MSE_ratio_by_week_LGR3.csv")
write.csv(MSE_ratio_by_week_LSGR1, file = "MSE_ratio_by_week_LSGR1.csv")
write.csv(MSE_ratio_by_week_LSGR2, file = "MSE_ratio_by_week_LSGR2.csv")
write.csv(MSE_ratio_by_week_LSGR3, file = "MSE_ratio_by_week_LSGR3.csv")
##This concludes the analyses done to discover and investigate predictive models for COVID outbreaks using Google search and mobility data.