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
    
  } else if (event == "02_selfreport_6mon_arm_1") {
    instruments_to_check <- c(
      # "health_and_social_service_utilization_6_month",
      # "medication_list",
      # "international_physical_activity_questionnaire",
      "pain_questionnaire",
      "pittsburgh_sleep_quality_index_psqi",
      "ohio_state_university_traumatic_brain_injury_ident",
      "prosocial_scale_from_the_strengths_and_difficultie",
      "metacognition_questionnaire_mcq30",
      "personality_inventory_for_dsm5_faceted_brief_form",
      "life_events_checklist",
      "adverse_life_events_scale",
      "connor_davidson_resilience_scale_2_item_cdrisc2",
      "parenting_monitoring_scale",
      "gender_identity_questionnaire_for_adolescents_and",
      "gender_identity_questionnaire_for_adolescents_4fa5",
      "puberty_development_scale_for_girls",
      "puberty_development_scale_for_boys",
      "personal_attributes_questionnaire_paq",
      "vancouver_index_of_acculturation_via",
      "multigroup_ethnic_identity_measure_revised_meimr",
      "the_mexican_american_cultural_values_scale_macvs",
      "phenx_acculturation_survey",
      "neighborhood_safetycrime_survey",
      "school_risk_and_protective_factors_srpf",
      "everyday_discrimination_scale",
      "adolescent_health_history",
      "conflict_behaviour_questionnaire"
    )
    
    fields_to_exclude <- c(
      "ipaq_q1a_other",
      "ohio_tbi_q6_1",
      "ohio_tbi_q6_2",
      "ohio_tbi_q6_3",
      "ohio_tbi_q6_4", 
      "ohio_tbi_q6_5",
      "ohio_tbi_q8_2",
      "ohio_tbi_q8_3",
      "psqi_005j_oth",
      "ahh_q2b_oth1", 
      "ahh_q2b_oth2", 
      "ahh_q2b_oth3",
      "ahh_q2b_oth4"
    )
    
  }  else if (event == "02_interview_6mont_arm_1") {
    instruments_to_check <- c(
      "height_weight_bmi_and_waist_circumference",
      "attachment_script_assessment_adolescent_version"
    )
    
    fields_to_exclude <- c(
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
    
  }  else if (event == "04_selfreport_18mo_arm_1") {
    instruments_to_check <- c(
      "health_and_social_service_utilization_6_month",
      "personality_inventory_for_dsm5_faceted_brief_form"
    )
    
    fields_to_exclude <- c(
      "nssibase_indirect"
    )
    
  }  else if (event == "04_interview_18mon_arm_1") {
    instruments_to_check <- c(
      "height_weight_bmi_and_waist_circumference"
    )
    
    fields_to_exclude <- c(
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
