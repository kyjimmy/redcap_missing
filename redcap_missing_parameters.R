# Specify event to check
event <- "01_selfreport_base_arm_1"

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
    "eaq_q4c"
  )
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
} else {
  
}
