getInstrumentInfo <- function(event) {
  instruments_to_check <- character(0)
  fields_to_exclude <- character(0)
  
  if (event == "01_selfreport_base_arm_1") {
    instruments_to_check <- c(
      "prime_revised",
      "prodromal_questionnairebrief_pqb",
      "aseba_youth_selfreport_ysr_1117",
      "aseba_adult_selfreport_asr_18",
      "social_responsiveness_scaleshort_form_srssf",
      "suicidal_ideation_questionnairejunior_siqjr",
      "adult_suicidal_ideation_questionnaire_asiq",
      "aadis_adolescent_alcohol_and_drug_involvement_scal",
      "columbia_impairment_scale_cis_selfreport_youth",
      "world_health_organization_disability_assessment_sc",
      "education_attainment_questionnaire_baseline"
    )
    
    fields_to_exclude <- c(
      "prime_revised_notes2",
      "prime_screen_result",
      "pqb_total_sum",
      "ycis_closing",
      "aadis_totalscore",
      "whodas_totalscore",
      "eaq_q4b",
      "eaq_q4c",
      "yrs_working",
      "ysr_sports_a",
      "ysr_sports_b", 
      "ysr_sports_c",
      "ysr_groups_a",
      "ysr_groups_b", 
      "ysr_groups_c",
      "ysr_jobs_a",
      "ysr_jobs_b",
      "ysr_jobs_c",
      "ysr_hobbies_a",
      "ysr_hobbies_b",
      "ysr_hobbies_c",
      "ysr_subject_e",
      "ysr_subject_f",
      "ysr_subject_g",
      "ysr_q113_describe",
      "ysr_otherproblems",
      "ysr_schoolproblems",
      "ysr_mworktype",
      "ysr_bestthing",
      "asr_work_sp",
      "gf_ss_notes3")
    
  } else if (event == "01_interview_basel_arm_1") {
    instruments_to_check <- c(
      "columbia_suicide_severity_rating_scale_cssrs_basel",
      "nonsuicidal_self_injury_nssi_baseline",
      "nonsuicidal_self_injury_nssi_followup",
      "structured_interview_for_prodromal_symptoms_sips",
      "global_functioning_social_scale",
      "global_functioning_role_scale"
    )
    
    fields_to_exclude <- c(
      "nssibase_indirect",
      "nssibase_indirect_fu",
      "nssibase_other1ans",
      "nssibase_other2ans",
      "nssibase_other1ans_fu",
      "nssibase_other2ans_fu",
      "nssibase_other1",
      "nssibase_other2",
      "nssibase_other1_fu",
      "nssibase_other2_fu"
    )
    
  } else if (event %in% c("03_selfreport_12mo_arm_1",
                          "05_selfreport_24mo_arm_1")) {
    instruments_to_check <- c(
      "prime_revised",
      "prodromal_questionnairebrief_pqb",
      "aseba_youth_selfreport_ysr_1117",
      "aseba_adult_selfreport_asr_18",
      "social_responsiveness_scaleshort_form_srssf",
      "suicidal_ideation_questionnairejunior_siqjr",
      "adult_suicidal_ideation_questionnaire_asiq",
      "aadis_adolescent_alcohol_and_drug_involvement_scal",
      "columbia_impairment_scale_cis_selfreport_youth",
      "world_health_organization_disability_assessment_sc"
    )
    
    fields_to_exclude <- c(
      "prime_revised_notes2",
      "prime_screen_result",
      "pqb_total_sum",
      "ycis_closing",
      "aadis_totalscore",
      "whodas_totalscore",     
      "yrs_working",
      "ysr_sports_a",
      "ysr_sports_b", 
      "ysr_sports_c", 
      "ysr_groups_a",
      "ysr_groups_b", 
      "ysr_groups_c",
      "ysr_jobs_a",
      "ysr_jobs_b",
      "ysr_jobs_c",
      "ysr_hobbies_a",
      "ysr_hobbies_b",
      "ysr_hobbies_c",
      "ysr_subject_e",
      "ysr_subject_f",
      "ysr_subject_g",
      "ysr_q113_describe",
      "ysr_otherproblems",
      "ysr_schoolproblems",
      "ysr_mworktype",
      "ysr_bestthing",
      "asr_work_sp",
      "gf_ss_notes3"
    )
    
  } else if (event %in% c("03_interview_12mon_arm_1",
                          "05_interview_24mon_arm_1")) {
    instruments_to_check <- c(
      "columbia_suicide_severity_rating_scale_cssrs_follo",
      "nonsuicidal_self_injury_nssi_followup",
      "structured_interview_for_prodromal_symptoms_sips",
      "global_functioning_social_scale",
      "global_functioning_role_scale"
    )
    
    fields_to_exclude <- c(
      "nssibase_indirect_fu",
      "nssibase_other1ans_fu",
      "nssibase_other2ans_fu",
      "nssibase_other1_fu",
      "nssibase_other2_fu"
    )
  }
  
  return(
    list(
      instruments_to_check = instruments_to_check,
      fields_to_exclude = fields_to_exclude
    )
  )
}
