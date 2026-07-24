#' Estimates the model using brms, takes the formula, a filename, and data
#' if use_model != null, then it updates the model provided
run_model <- function(form, filename, dat, force = FALSE, use_model = NULL, refresh = 0, ...) {
  if (!file.exists(filename) || force) {
    # Weakly informative default prior (Gelman et al., 2008; except for the scaling)

    if (!is.null(use_model)) {
      fit <- update(use_model, newdata = dat, formula = form, recompile = FALSE, refresh = refresh)
    } else {
      fit <- brm(form, data = dat, prior = prior(student_t(3, 0, 2.5), class = b), refresh = refresh, ...)
    }
    saveRDS(fit, filename)
  } else {
    fit <- readRDS(filename)
  }

  fit
}

run_all_models <- function(
    dat_final, predictor = 'techno_optimism', filename_ext = '',
    cores = 10, force = FALSE, family = 'bernoulli', sensitivity = FALSE, worry = FALSE
  ) {
  registerDoParallel(cores = cores)
  
  behaviors <- list(
    # Civic behaviors
    'talk_climate' = 'Talked about climate with others',
    'donate_money' = 'Donated to climate organizations',
    'signed_petitions' = 'Signed petitions',
    'advocated_change' = 'Advocated change within institution',
    'engaged_policymakers' = 'Engaged with politicians',
    'engaged_disobedience' = 'Engaged in civil disobedience',
    'engaged_protest' = 'Engaged in protest',
    'engaged_advocacy' = 'Engaged in advocacy',
    'wrote_letters' = 'Wrote letters to politicians',
    
    # Lifestyle behaviors
    'reduced_flying' = 'Reduced flying',
    'reduced_car' = 'Reduced car usage',
    'electric_vehicle' = 'Switched to electric vehicle',
    'energy_home' = 'Switched to renewable energy at home',
    'veggie_diet' = 'Follows a mostly vegetarian or vegan diet',
    'fewer_children' = 'Decided to have fewer or no children'
  )
  
  form <- make_form(
    'talk_climate', predictor = predictor,
    random_intercept = FALSE, random_slope = FALSE,
    marginal = TRUE, worry = FALSE, informed = FALSE,
    sensitivity = sensitivity, control_only = TRUE
  )
  
  form_adj <- make_form(
    'talk_climate', predictor = predictor,
    random_intercept = FALSE, random_slope = FALSE,
    marginal = FALSE, worry = worry, informed = TRUE,
    sensitivity = sensitivity, control_only = FALSE
  )
  
  filename <- paste0('models/marginal_talk_climate', filename_ext, '.RDS')
  filename_adj <- paste0('models/adjusted_talk_climate', filename_ext, '.RDS')
  
  # Run one initial model so below we don't need to recompile them, but can use this one
  fit_init_marginal <- run_model(
    form, filename, dat_final, use_model = NULL,
    cores = 4, chains = 4, family = family, force = force, iter = 2000, warmup = 500
  )
  
  fit_init_adj <- run_model(
    form_adj, filename_adj, dat_final, use_model = NULL,
    cores = 4, chains = 4, family = family, force = force, iter = 2000, warmup = 500
  )
  
  # Condition on nothing
  fit_all_marginal <- foreach(i = seq(length(behaviors))) %dopar% {
    b <- names(behaviors)[i]
    
    filename <- paste0('models/marginal_', b, filename_ext, '.RDS')
    form <- make_form(
      b, predictor = predictor,
      random_intercept = FALSE, random_slope = FALSE,
      marginal = TRUE, worry = FALSE, informed = FALSE,
      sensitivity = sensitivity, control_only = TRUE
    )
    
    fit <- run_model(
      form, filename, dat_final, use_model = fit_init_marginal,
      cores = 1, chains = 4, family = family, force = force, iter = 2000, warmup = 500
    )
    
    print(b)
    res <- list()
    res[[b]] <- fit
    res
  }
  
  # Condition only on background variables
  fit_all_adj <- foreach(i = seq(length(behaviors))) %dopar% {
    b <- names(behaviors)[i]
    
    filename <- paste0('models/adjusted_', b, filename_ext, '.RDS')
    form <- make_form(
      b, predictor = predictor,
      random_intercept = FALSE, random_slope = FALSE,
      marginal = FALSE, worry = worry, informed = TRUE,
      sensitivity = sensitivity, control_only = FALSE
    )
    
    fit <- run_model(
      form, filename, dat_final, use_model = fit_init_adj,
      cores = 1, chains = 4, family = family, force = force, iter = 2000, warmup = 500
    )
    
    res <- list()
    res[[b]] <- fit
    res
  }
  
  list(
    'fit_all_marginal' = fit_all_marginal,
    'fit_all_adj' = fit_all_adj
  )
}

run_techno_optimsm_predictions <- function(
    dat_final, predictor = 'techno_optimism', filename_ext = '',
    cores = 10, force = FALSE, family = 'bernoulli', sensitivity = FALSE) {
  
  registerDoParallel(cores = cores)
  
  behaviors <- list(
    # Civic behaviors
    'talk_climate' = 'Talked about climate with others',
    'donate_money' = 'Donated to climate organizations',
    'signed_petitions' = 'Signed petitions',
    'advocated_change' = 'Advocated change within institution',
    'engaged_policymakers' = 'Engaged with politicians',
    'engaged_disobedience' = 'Engaged in civil disobedience',
    'engaged_protest' = 'Engaged in protest',
    'engaged_advocacy' = 'Engaged in advocacy',
    'wrote_letters' = 'Wrote letters to politicians',
    
    # Lifestyle behaviors
    'reduced_flying' = 'Reduced flying',
    'reduced_car' = 'Reduced car usage',
    'electric_vehicle' = 'Switched to electric vehicle',
    'energy_home' = 'Switched to renewable energy at home',
    'veggie_diet' = 'Follows a mostly vegetarian or vegan diet',
    'fewer_children' = 'Decided to have fewer or no children'
  )
  
  form <- make_form(
    'techno_optimism', predictor = '',
    random_intercept = FALSE, random_slope = FALSE,
    marginal = FALSE, worry = FALSE, informed = FALSE,
    sensitivity = FALSE, control_only = FALSE
  )
  
  
  filename <- paste0('models/marginal_talk_climate', filename_ext, '.RDS')
  filename_adj <- paste0('models/adjusted_talk_climate', filename_ext, '.RDS')
  
  # Run one initial model so below we don't need to recompile them, but can use this one
  fit_init_marginal <- run_model(
    form, filename, dat_final, use_model = NULL,
    cores = 4, chains = 4, family = family, force = force, iter = 2000, warmup = 500
  )
  
  fit_init_adj <- run_model(
    form_adj, filename_adj, dat_final, use_model = NULL,
    cores = 4, chains = 4, family = family, force = force, iter = 2000, warmup = 500
  )
  
  # Condition on nothing
  fit_all_marginal <- foreach(i = seq(length(behaviors))) %dopar% {
    b <- names(behaviors)[i]
    
    filename <- paste0('models/marginal_', b, filename_ext, '.RDS')
    form <- make_form(
      b, predictor = predictor,
      random_intercept = FALSE, random_slope = FALSE,
      marginal = TRUE, worry = FALSE, informed = FALSE,
      sensitivity = sensitivity, control_only = TRUE
    )
    
    fit <- run_model(
      form, filename, dat_final, use_model = fit_init_marginal,
      cores = 1, chains = 4, family = family, force = force, iter = 2000, warmup = 500
    )
    
    print(b)
    res <- list()
    res[[b]] <- fit
    res
  }
  
  # Condition only on background variables
  fit_all_adj <- foreach(i = seq(length(behaviors))) %dopar% {
    b <- names(behaviors)[i]
    
    filename <- paste0('models/adjusted_', b, filename_ext, '.RDS')
    form <- make_form(
      b, predictor = predictor,
      random_intercept = FALSE, random_slope = FALSE,
      marginal = FALSE, worry = FALSE, informed = TRUE,
      sensitivity = sensitivity, control_only = FALSE
    )
    
    fit <- run_model(
      form, filename, dat_final, use_model = fit_init_adj,
      cores = 1, chains = 4, family = family, force = force, iter = 2000, warmup = 500
    )
    
    res <- list()
    res[[b]] <- fit
    res
  }
  
  list(
    'fit_all_marginal' = fit_all_marginal,
    'fit_all_adj' = fit_all_adj
  )
}

estimate_effects <- function(
    all_models, filename,
    predictor = 'techno_optimism',
    type = 'avg_predictions',
    force = FALSE,
    comp = 'difference',
    sensitivity = FALSE
) {
  behaviors <- list(
    # Civic behaviors
    'talk_climate' = 'Talked about climate with others',
    'donate_money' = 'Donated to climate organizations',
    'signed_petitions' = 'Signed petitions',
    'advocated_change' = 'Advocated change within institution',
    'engaged_policymakers' = 'Engaged with politicians',
    'engaged_disobedience' = 'Engaged in civil disobedience',
    'engaged_protest' = 'Engaged in protest',
    'engaged_advocacy' = 'Engaged in advocacy',
    'wrote_letters' = 'Wrote letters to politicians',
    
    # Lifestyle behaviors
    'reduced_flying' = 'Reduced flying',
    'reduced_car' = 'Reduced car usage',
    'electric_vehicle' = 'Switched to electric vehicle',
    'energy_home' = 'Switched to renewable energy at home',
    'veggie_diet' = 'Follows a mostly vegetarian or vegan diet',
    'fewer_children' = 'Decided to have fewer or no children'
  )
  
  behavior_map <- list(
    'Civic action' = unlist(behaviors[seq(9)]),
    'Lifestyle change' = unlist(behaviors[seq(10, 15)])
  )
  
  if (!file.exists(filename) || force) {
    fit_all_marginal <- all_models[['fit_all_marginal']]
    fit_all_adj <- all_models[['fit_all_adj']]
    
    if (!sensitivity) {
      df_marginal <- do.call('rbind', lapply(seq(15), function(i) {
        fit <- fit_all_marginal[[i]]
        behavior <- names(fit)
        
        get_effects_proper(fit[[1]], behavior, variable = 'techno_optimism', type = type, comp = comp)
      }))
    }
    
    df_adj <- do.call('rbind', lapply(seq(15), function(i) {
      fit <- fit_all_adj[[i]]
      behavior <- names(fit)
      
      get_effects_proper(fit[[1]], behavior, variable = 'techno_optimism', type = type, comp = comp)
    }))
    
    if (sensitivity) {
      df_effects <- df_adj %>% mutate(class = 'sensitivity')
      
    } else {
      df_effects <- bind_rows(
        df_marginal %>% mutate(class = 'unadjusted'),
        df_adj %>% mutate(class = 'adjusted')
      )
    }
    
    write.csv(df_effects, filename, row.names = FALSE)
    
  } else {
    df_effects <- read.csv(filename)
  }
  
  df_effects
}


order_effects <- function(df_effects, type = 'absolute') {
  
  if (type == 'absolute') {
    # Order from civic action to lifestyle actions, and then within
    df_effects <- df_effects %>% 
      add_behavior_categories(behavior_map)
    
    # Step 1: Calculate the multiplicative difference within each category and behavior
    behavior_order <- df_effects %>%
      filter(class == 'adjusted') %>%
      group_by(category, behavior) %>% 
      summarize(mult_diff = estimate[combined == 'Disagree'] - estimate[combined == 'Agree']) %>%
      arrange(category, desc(mult_diff)) %>%
      ungroup() %>% 
      mutate(behavior_order = row_number()) %>%
      ungroup()
    
    # Step 2: Reorder the `behavior` factor levels in the original data
    df_effects <- df_effects %>%
      left_join(behavior_order %>% select(behavior, category, behavior_order), by = c("behavior", "category")) %>%
      mutate(behavior = fct_reorder(behavior, behavior_order, .desc = TRUE))
    
    df_effects
    
  } else {
    
    # Order from civic action to lifestyle actions, and then within
    df_effects <- df_effects %>% 
      add_behavior_categories(behavior_map)
    
    # Step 1: Calculate the multiplicative difference within each category and behavior
    behavior_order <- df_effects %>%
      filter(class == 'adjusted') %>%
      select(behavior, estimate, category) %>%
      arrange(category, estimate) %>%
      mutate(behavior_order = row_number()) %>%
      ungroup()
    
    # Step 2: Reorder the `behavior` factor levels in the original data
    df_effects <- df_effects %>%
      left_join(behavior_order %>% select(behavior, category, behavior_order), by = c("behavior", "category")) %>%
      mutate(behavior = fct_reorder(behavior, behavior_order, .desc = TRUE))
    
    df_effects
  }
}


# Add the category of the behaviors to a data frame
add_behavior_categories <- function(df, categories) {
  n <- nrow(df)
  df$category <- seq(n)

  for (i in seq(n)) {
    varname <- as.character(df$behavior[i])

    if (varname %in% categories$Civic) {
      category <- 'Civic action'
    } else if (varname %in% categories$Lifestyle) {
      category <- 'Lifestyle change'
    }

    df$category[i] <- category
  }

  df %>%
    mutate(category = factor(category, levels = c('Civic action', 'Lifestyle change')))
}

# Create a form for the models of the climate actions
make_form <- function(
    outcome, predictor = 'techno_optimism_trin', random_intercept = TRUE,
    random_slope = TRUE, binarize = TRUE, marginal = FALSE,
    worry = FALSE, informed = FALSE, sensitivity = FALSE, control_only = FALSE
) {

  if (marginal) {
    pred <- predictor
  } else {
    pred <- paste0(
      predictor, ifelse(worry, ' + worry', ''), ifelse(informed, ' + informed', ''),
      ' + research_fact + Age_std + political + position + field + continent + is_tenured + is_female + is_gender_other'
    )
  }
  
  # Hack to do the sensitivity analysis for omitted confounding
  if (sensitivity) {
    pred <- paste0(
      ifelse(control_only, '', predictor),
      ifelse(worry, ' + Worry_std', ''), ifelse(informed, ' + informed', ''),
      ' + research_fact + Age_std + political + position + field + continent + is_tenured + is_female + is_gender_other'
    )
  }

  if (random_intercept & !(random_slope)) {
    pred <- paste0(pred, ' + (1 | country)')
  } else if (random_slope) {
    re <- paste0(' + (1 + ', pred, ' | country)')
    pred <- paste0(pred, re)
  }

  as.formula(paste0(outcome, ' ~ ', pred))
}

# Get average comparisons from fitted model
get_effects <- function(
    fit, behavior, type, predictor = 'techno_optimism_trin',
    metric = 'absolute', conf_level = 0.95, ...
  ) {
  
  fn <- ifelse(type == 'avg_predictions', avg_predictions, avg_comparisons)
  df <- data.frame(fn(fit, variable = predictor, conf_level = conf_level, ...))

  df <- df %>%
    rename(
      ci_lo = conf.low,
      ci_hi = conf.high
    ) %>%
    mutate(
      behavior = behavior,
      type = type
    )

  df
}


get_effects_proper <- function(
    fit, behavior, variable = 'techno_optimism',
    type = 'avg_predictions', comp = 'difference'
  ) {
  
  if (type == 'avg_predictions') {
    df_pred <- avg_predictions(fit, variable = 'techno_optimism')
    df_draws <- get_draws(df_pred)
    
    if (comp == 'difference') {
    
      df_effects <- df_draws %>%
        filter(techno_optimism != 'Neutral') %>% 
        mutate(combined = ifelse(techno_optimism %in% c('Strongly agree', 'Agree'), 'Agree', 'Disagree')) %>% 
        # For each draw and combined category
        group_by(drawid, combined) %>%
        # Average the predicted probabilities across combined categories
        summarise(draw = mean(draw), .groups = 'drop') %>%
        group_by(combined) %>%
        summarise(
          estimate = mean(draw),
          ci_lo = quantile(draw, 0.025),
          ci_hi = quantile(draw, 0.975),
          .groups = 'drop'
        ) %>% 
        mutate(behavior = behavior)
    
    } else {
      
      df_effects <- df_draws %>%
        filter(techno_optimism != 'Neutral') %>% 
        mutate(combined = ifelse(techno_optimism %in% c('Strongly agree', 'Agree'), 'Agree', 'Disagree')) %>% 
        group_by(drawid, combined) %>%
        # Average the predicted probabilities across combined categories
        summarise(draw = mean(draw), .groups = 'drop')
      
      df_ratio <- df_effects %>%
        tidyr::pivot_wider(names_from = combined, values_from = draw) %>%  # 6000 rows
        mutate(ratio = Disagree / pmax(Agree, 1e-9))
      
      # summarise across draws
      df_effects <- df_ratio %>%
        summarise(
          estimate = mean(ratio, na.rm = TRUE),
          ci_lo    = quantile(ratio, 0.025, na.rm = TRUE),
          ci_hi    = quantile(ratio, 0.975, na.rm = TRUE)
        ) %>% 
        mutate(behavior = behavior)
      
    }
    
  } else {
    
    # Get average difference between techno-optimism categories
    df_pred <- avg_comparisons(fit, variables = list(techno_optimism = 'sequential'))
    df_draws <- get_draws(df_pred)
    
    df_effects <- df_draws %>%
      # For each draw, compute the mean across pairwise differences
      group_by(drawid) %>%
      summarise(
        estimate = mean(draw),
        .groups = 'drop'
      ) %>% 
      mutate(group = 'group') %>%
      group_by(group) %>%
      summarise(
        ci_lo = quantile(estimate, 0.025),
        ci_hi = quantile(estimate, 0.975),
        estimate = mean(estimate)
      ) %>% 
      mutate(behavior = behavior)
  }
  
  df_effects
}
