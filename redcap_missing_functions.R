# Code Sources
# Report: https://gist.github.com/nutterb/501c370418abb58bee78
# redcapAPI: https://github.com/nutterb/redcapAPI-Defunct/blob/c3e53a53211b70803406cd8b6ff5681cd1d792ec/R/parseBranchingLogic.R

parseBranchingLogic <- function(l) {
  l <- tolower(l)
  l <- gsub("\\n", " ", l)
  l <- gsub(" or ", " | ", l)
  l <- gsub(" and ", " & ", l)
  l <- gsub("([a-z,0-9,_])\\((?<=\\()(.*?)(?=\\))\\)",
            "\\1___\\2",
            l,
            perl = TRUE)
  l <- gsub("([[]|[]])", "", l)
  l <- gsub("[=]", " == ", l)
  l <- gsub("[!] [=]", " !", l)
  l <- gsub("[<] [=]", " <", l)
  l <- gsub("[>] [=]", " >", l)
  l <- gsub("[<][>]", "!=", l)
  lapply(l, function(x)
    ifelse(x == "", NA, parse(text = x)))
}


.missingSummary_isMissingInField <- function(records_orig,
                                             meta_data,
                                             logic) {
  records <- records_orig
  
  for (i in seq_along(records)) {
    # Actual field name
    this_field <- names(records)[i]
    # Remove checkbox suffixes. This allows logic to be matched to the field.
    this_field_base <- sub("___.+$", "", this_field)
    # get the logic expression for this iteration of the loop
    this_logic <- logic[[this_field_base]]
    
    # We are only going to look at fields that are informative as missing.
    # we skip fixed fields (see unexported) and the ID variable.
    if (!this_field %in% c(.missingSummary_fixedFields,
                           meta_data$field_name[1]) &
        !is.null(this_logic)) {
      # get the name of the form on which the field is saved
      tmp_form <- meta_data$form_name[meta_data$field_name ==
                                        sub("___[[:print:]]", "", names(records)[i])]
      tmp_form <- paste0(tmp_form, "_complete")
      
      # NOTE: in the result, TRUE means the value is missing
      #                      FALSE means the value is non-missing
      if (tmp_form == "_complete") {
        # If we are here, we didn't find a matching form name. We will
        # assume variables not on a form are always non-missing.
        records[[i]] <- rep(FALSE, nrow(records))
      }
      else if (!tmp_form %in% names(records)) {
        # If we are here, we are evaluating a `[form]_complete` field.
        # We just want to know if it is missing or not.
        records[[i]] <- is.na(records[[i]])
      } else if (!is.expression(this_logic)) {
        # If we are here, there is not branching logic.
        # If the `[form]_complete` field is missing, we return FALSE
        # If the `[form]_complete` is non-missing, we return the missingness of the value
        records[[i]] <-
          ifelse(
            test = is.na(records_orig[[tmp_form]]),
            yes = FALSE,
            no = is.na(records_orig[[i]])
          )
      }
      else
        # Here we have branching logic.
        # If the `[form]_complete` field is missing, we return FALSE
        # If the `[form]_complete` is non-missing:
        #    The branching logic is satisfied: return the missingness of the value
        #    The branchign logic is not satisfied: return FALSE
        records[[i]] <-
        ifelse(
          test = is.na(records_orig[[tmp_form]]),
          yes = FALSE,
          no = ifelse(
            test = with(records_orig, eval(this_logic)),
            yes = is.na(records_orig[[i]]),
            no = FALSE
          )
        )
    }
  }
  records
}


.missingSummary_excludeMissingForm <- function(records,
                                               meta_data,
                                               logic) {
  # Get the `[form]_complete` fields.
  form_names <- unique(meta_data$form_name)
  # form_complete_names <- paste0(form_names, "_complete") #into loop
  
  for (i in seq_len(nrow(records))) {
    # For each record, find the fields associated with the forms
    # where the `[form]_complete` field is missing.
    completeFormMissing <- lapply(form_names,
                                  function(f) {
                                    flds <- meta_data$field_name[meta_data$form_name %in% f]
                                    flds <-
                                      flds[!flds %in% meta_data$field_name[1]]
                                    flds <-
                                      flds[!flds %in% meta_data$field_name[meta_data$field_type == "checkbox"]]
                                    form_complete_status <-
                                      paste0(f, "_complete")
                                    form_complete_name <-
                                      meta_data$field_name[(
                                        meta_data$field_label %in% c("Was this form completed?", "Was the form completed?")
                                      ) &
                                        (meta_data$form_name %in% f)][1]
                                    if (length(flds) == 0) {
                                      return(NULL)
                                    }
                                    else if (is.na(records[i, form_complete_name])) {
                                      return(flds)
                                    }
                                    else if ((records[i, form_complete_status] %in% c("0", "1")) |
                                             (records[i, form_complete_name] == "2")) {
                                      return(flds)
                                    }
                                    else {
                                      return(NULL)
                                    }
                                  })
    # If the `[form]_complete` field is missing, we set the missingness value of the
    # record for fields on that value to FALSE, indicating that they are non-missing
    # That is, we don't consider a value missing unless the form is marked either 'Complete' or 'Incomplete'
    completeFormMissing <- unlist(completeFormMissing)
    if (!is.null(completeFormMissing)) {
      records[i, completeFormMissing] <- FALSE
    }
  }
  
  records
}


.missingSummary_makeResultFrame <- function(records,
                                            meta_data) {
  # These are the identifier fields in the result
  start_field <- c(meta_data$field_name[1],
                   .missingSummary_fixedFields)
  start_field <- start_field[start_field %in% names(records)]
  
  # Make the initial data frame of results. Only the identifiers here
  MissingSummary <- records[start_field]
  
  # Remove the identifier fields from `records`.
  # This makes it easier to run an apply statement on the rows
  records <- records[!names(records) %in% start_field]
  
  # Number of missing values
  MissingSummary$n_missing <- numeric(nrow(records))
  MissingSummary$missing <- character(nrow(records))
  
  for (i in seq_len(nrow(MissingSummary))) {
    missing_this_row <- vapply(records[i, ],
                               FUN = isTRUE,
                               FUN.VALUE = logical(1))
    MissingSummary$n_missing[i] <- sum(missing_this_row)
    MissingSummary$missing[i] <-
      paste0(names(records)[missing_this_row],
             collapse = ", ")
  }
  
  MissingSummary
}


.missingSummary_makeSingleResultFrame <- function(records,
                                                  meta_data) {
  # These are the identifier fields in the result
  start_field <- c(meta_data$field_name[1],
                   .missingSummary_fixedFields)
  start_field <- start_field[start_field %in% names(records)]
  
  # Make the initial data frame of results. Only the identifiers here
  MissingSummary <- records[start_field]
  
  # Remove the identifier fields from `records`.
  # This makes it easier to run an apply statement on the rows
  records <- records[!names(records) %in% start_field]
  
  # Number of missing values
  missing_this_row <- vapply(records[1, ],
                             FUN = isTRUE,
                             FUN.VALUE = logical(1))
  missing_fields <- names(records)[missing_this_row]
  
  missing_fields_df <- data.frame(field_name = missing_fields)
  
  
  missing_fields_df %>% 
    left_join(meta_data, by='field_name') %>%
    select(form_name, field_name, field_label)
}


# The field names listed here are those generated by REDCap. We are
# not interested in if they are missing and so will skip them.
# Their values can make useful labels, so we want to leave them
# untouched.
.missingSummary_fixedFields <- c("redcap_event_name",
                                 "redcap_repeat_instrument",
                                 "redcap_repeat_instance")
