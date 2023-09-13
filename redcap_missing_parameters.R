getInstrumentInfo <- function(event) {
  instruments_to_check <- character(0)
  fields_to_exclude <- character(0)
  
  if (event == "01_selfreport_base_arm_1") {
    instruments_to_check <- c(
      "demographics",
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
      "gf_ss_notes3",
      "gf_ss_redcap_id_2",
      "not_listed",
      "not_listed_2",
      "not_listed_3",
      "not_listed_4",
      "not_listed_5",
      "not_listed_6",
      "not_listed_7",
      "not_listed_8",
      "not_listed_9",
      "not_listed_10",
      "not_listed_11",
      "not_listed_12",
      "not_listed_13",
      "not_listed_14",
      "not_listed_15",
      "not_listed_16",
      "not_listed_17",
      "not_listed_18",
      "not_listed_19",
      "not_listed_20",
      "not_listed_21",
      "not_listed_22",
      "not_listed_23",
      "not_listed_24",
      "not_listed_25",
      "rel1",
      "rel2",
      "rel3",
      "rel4",
      "rel5",
      "rel6",
      "rel7",
      "rel8",
      "rel_stat_1",
      "rel_stat_2",
      "rel_stat_3",
      "rel_stat_4",
      "rel_stat_5",
      "rel_stat_6",
      "rel_stat_7",
      "rel_stat_8")
    
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
      "health_and_social_service_utilization_6_month",
      # "medication_list",
      "pain_questionnaire",
      "international_physical_activity_questionnaire",
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
      "hssu2a_psych_vs",
      "hssu2b_psychol_vs",
      "hssu2c_soc_vs",
      "hssu2d_nurse_vs",
      "hssu2e_child_vs",
      "hssu4a_famdr_vs",
      "hssu4b_psych_vsout",
      "hssu_pedia_vs",
      "hssu4g_oth1_vsout",
      "hssu4h_oth2_vsout",
      "hssu5_supp_vs",
      "hssu5_dayp_vs",
      "hssu5_oth1_vs",
      "hssu5_oth2_vs",
      "hssu6_phys_vs",
      "hssu6_psych_vs",
      "hssu6_socw_vs",
      "hssu6_adult_vs",
      "hssu6_family_vs",
      "hssu6_child_vs",
      "hssu6_police_vs",
      "hssu6_supp_vs",
      "hssu6_tell_vs",
      "hssu6_oth_vs",
      "hssu6_oth2_vs",
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
      "attach_hide_score"
    )
    
  } else if (event %in% c("03_selfreport_12mo_arm_1",
                          "05_selfreport_24mo_arm_1")) {
    instruments_to_check <- c(
      "survey_queue",
      "demographics",
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
      "education_attainment_questionnaire_yearly_followup"
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
      "gf_ss_notes3",
      "gf_ss_redcap_id_2",
      "not_listed",
      "not_listed_2",
      "not_listed_3",
      "not_listed_4",
      "not_listed_5",
      "not_listed_6",
      "not_listed_7",
      "not_listed_8",
      "not_listed_9",
      "not_listed_10",
      "not_listed_11",
      "not_listed_12",
      "not_listed_13",
      "not_listed_14",
      "not_listed_15",
      "not_listed_16",
      "not_listed_17",
      "not_listed_18",
      "not_listed_19",
      "not_listed_20",
      "not_listed_21",
      "not_listed_22",
      "not_listed_23",
      "not_listed_24",
      "not_listed_25",
      "rel1",
      "rel2",
      "rel3",
      "rel4",
      "rel5",
      "rel6",
      "rel7",
      "rel8",
      "rel_stat_1",
      "rel_stat_2",
      "rel_stat_3",
      "rel_stat_4",
      "rel_stat_5",
      "rel_stat_6",
      "rel_stat_7",
      "rel_stat_8"
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
      "hssu2a_psych_vs",
      "hssu2b_psychol_vs",
      "hssu2c_soc_vs",
      "hssu2d_nurse_vs",
      "hssu2e_child_vs",
      "hssu4a_famdr_vs",
      "hssu4b_psych_vsout",
      "hssu_pedia_vs",
      "hssu4g_oth1_vsout",
      "hssu4h_oth2_vsout",
      "hssu5_supp_vs",
      "hssu5_dayp_vs",
      "hssu5_oth1_vs",
      "hssu5_oth2_vs",
      "hssu6_phys_vs",
      "hssu6_psych_vs",
      "hssu6_socw_vs",
      "hssu6_adult_vs",
      "hssu6_family_vs",
      "hssu6_child_vs",
      "hssu6_police_vs",
      "hssu6_supp_vs",
      "hssu6_tell_vs",
      "hssu6_oth_vs",
      "hssu6_oth2_vs"
    )
    
  }  else if (event == "04_interview_18mon_arm_1") {
    instruments_to_check <- c(
      "height_weight_bmi_and_waist_circumference"
    )
    
    fields_to_exclude <- c(
    )
    
  } 
  
  
  return(
    list(
      instruments_to_check = instruments_to_check,
      fields_to_exclude = fields_to_exclude
    )
  )
}


getCaregiverInstrumentInfo <- function(event) {
  instruments_to_check <- character(0)
  fields_to_exclude <- character(0)
  
  if (event  %in% c("01_selfreport_base_arm_1",
                    "03_selfreport_12mo_arm_1",
                    "05_selfreport_24mo_arm_1")) {
    
    instruments_to_check <- c(
      "demographics",
      "social_responsiveness_scaleshort_form_srssf_inform",
      "aseba_child_behaviour_checklist_cbcl_caregiver",
      "columbia_impairment_scale_cis_caregiver_report",
      "whodas_20_proxy_caregiver",
      "social_communication_questionnaire_scq"
    )
    
    fields_to_exclude <- c(
      "pcbcl_groups_a",
      "pcbcl_groups_b",
      "pcbcl_groups_c",
      "pcbcl_sports_a",
      "pcbcl_sports_b",
      "pcbcl_sports_c",
      "pcbcl_hobbies_a",
      "pcbcl_hobbies_b",
      "pcbcl_hobbies_c",
      "pcbcl_jobs_a",
      "pcbcl_jobs_b",
      "pcbcl_jobs_c",
      "pcbcl_subject_e",
      "pcbcl_subject_f",
      "pcbcl_subject_g",
      "pcbcl_a113atext", 
      "pcbcl_a113a",
      "pcbcl_a113btext", 
      "pcbcl_a113b",
      "pcbcl_a113ctext", 
      "pcbcl_a113c",
      "ccis_closing",
      "not_listed",
      "not_listed_2",
      "not_listed_3",
      "not_listed_4",
      "not_listed_5",
      "not_listed_6",
      "not_listed_7",
      "not_listed_8",
      "not_listed_9",
      "not_listed_10",
      "not_listed_11",
      "not_listed_12",
      "not_listed_13",
      "not_listed_14",
      "not_listed_15",
      "not_listed_16",
      "not_listed_17",
      "not_listed_18",
      "not_listed_19",
      "not_listed_20",
      "not_listed_21",
      "not_listed_22",
      "not_listed_23",
      "not_listed_24",
      "not_listed_25",
      "rel1",
      "rel2",
      "rel3",
      "rel4",
      "rel5",
      "rel6",
      "rel7",
      "rel8",
      "rel_stat_1",
      "rel_stat_2",
      "rel_stat_3",
      "rel_stat_4",
      "rel_stat_5",
      "rel_stat_6",
      "rel_stat_7",
      "rel_stat_8",
      "demo_youth_relat_nl",
      "demo_id_language_nl",
      "demo_ed_q2_nl",
      "demo_ed_q3_nl",
      "demo_ed_q4_nl",
      "demo_marital_nl")
    
  } else if (event == "02_selfreport_6mon_arm_1") {
    instruments_to_check <- c(
      "developmental_history_questionnaire",
      # "medication_list",
      "adverse_life_events_scale",
      "vancouver_index_of_acculturation_via",
      "multigroup_ethnic_identity_measure_revised_meimr",
      "phenx_acculturation_survey",
      "prosocial_scale_from_the_strengths_and_difficultie_d2f385",
      "neighborhood_safetycrime_survey",
      "adolescent_health_history_caregiver",
      "conflict_behaviour_questionnaire_caregiver"
    )
    
    fields_to_exclude <- c(
      "ahhc_q2b_oth1", 
      "ahhc_q2b_oth2", 
      "ahhc_q2b_oth3",
      "ahhc_q2b_oth4",
      "dhx_pounds",
      "dhx_ounces",
      "dhx_age_mother",
      "dhx_age_father",
      "dhx_weeks_find_pregnant",
      "dhx_presc_freq",
      "dhx_presc_amount",
      "dhx_other_presc_freq",
      "dhx_other_presc_amount",
      "dhx_other_presc_freq_2",
      "dhx_other_presc_amount_2",
      "dhx_tobacco_freq",
      "dhx_alcohol_max",
      "dhx_alcohol_avg",
      "dhx_alcohol_effect",
      "dhx_marijuana_freq",
      "dhx_cocaine_freq",
      "dhx_heroin_freq",
      "dhx_oxycontin_freq",
      "dhx_other_drug_freq_1",
      "dhx_other_drug_amount_1",
      "dhx_other_drug_freq_2",
      "dhx_other_drug_amount_2",
      "dhx_other_drug_freq_3",
      "dhx_other_drug_amount_3",
      "dhx_other_drug_freq_4",
      "dhx_other_drug_amount_4",
      "dhx_other_drug_freq_5",
      "dhx_other_drug_amount_5",
      "dhx_a_presc_freq",
      "dhx_a_presc_amount",
      "dhx_a_other_presc_freq",
      "dhx_a_other_presc_amount",
      "dhx_a_other_presc_freq_2",
      "dhx_a_other_presc_amount_2",
      "dhx_a_other_presc_freq_3",
      "dhx_a_other_presc_amount_3",
      "dhx_a_other_presc_freq_4",
      "dhx_a_other_presc_amount_4",
      "dhx_a_tobacco_freq",
      "dhx_a_alcohol_max",
      "dhx_a_alcohol_avg",
      "dhx_a_alcohol_effect",
      "dhx_a_marijuana_freq",
      "dhx_a_cocaine_freq",
      "dhx_a_heroin_freq",
      "dhx_a_oxycontin_freq",
      "dhx_a_other_drug_freq_1",
      "dhx_a_other_drug_amount_1",
      "dhx_a_other_drug_freq_2",
      "dhx_a_other_drug_amount_2",
      "dhx_a_other_drug_freq_3",
      "dhx_a_other_drug_amount_3",
      "dhx_a_other_drug_freq_4",
      "dhx_a_other_drug_amount_4",
      "dhx_a_other_drug_freq_5",
      "dhx_a_other_drug_amount_5",
      "dhx_caffeine_day",
      "dhx_caffeine_week",
      "dhx_caffeine_month",
      "dhx_doctor_freq",
      "dhx_incubator_days",
      "dhx_fever_days",
      "dhx_infection_days",
      "dhx_breastfed_months_df",
      "dhx_age_rollover",
      "dhx_age_sit",
      "dhx_age_walk",
      "dhx_age_sayword"
    )
    
  }
  
  return(
    list(
      instruments_to_check = instruments_to_check,
      fields_to_exclude = fields_to_exclude
    )
  )
}
