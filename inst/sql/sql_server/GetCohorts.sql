{DEFAULT @cdm_version = '5'}

SELECT cast(row_id as int) row_id,
	subject_id,
{@cdm_version == "4"} ? {	
	cohort_concept_id AS target_id,
} : {
	cohort_definition_id AS target_id,
}
	cohort_start_date,
	observation_period_start_date,
	days_from_obs_start,
	days_to_cohort_end,
	observation_period_end_date,
	days_to_obs_end,
	birth_year,
	gender
FROM #cohort_person cohort
ORDER BY subject_id