

# a function to combine the goodies of ms_sensitivity into a handy
# decomp wrapper. It's hard-coded to the data characteristics in this
# particular study: ages 40+, quarter-year time intervals, etc
do_dec_gender <- function(data){
  prev <- data |> 
    filter(age == 40)
  trans <- data |> 
    filter(age > 40) 
  
  init_pars <- 
    prev |> 
    fselect(gender, H = HH, U = UU) |> 
    dt_pivot_longer(H:U, names_to = "state", values_to = "p") |> 
    dt_pivot_wider(names_from = gender, values_from = p) |> 
    fmutate(delta = women - men,
            p = (women + men) / 2)
  
  init <- init_pars$p
  names(init) <- init_pars$state
  
  decomp_data <- 
    trans |> 
    select(gender, age, c(HH,HU,HD,UU,UH,UD)) |> 
    dt_pivot_longer(c(HH,HU,HD,UU,UH,UD), names_to = "transition", values_to = "p") |> 
    dt_pivot_wider(names_from = gender, values_from = p) |> 
    fmutate(delta = women - men,
            p = (women + men) / 2) |> 
    fselect(-men, -women) |> 
    arrange(transition, age) |> 
    fmutate(age = age - min(age))
  
  
  sen <-
    decomp_data |> 
    s2t(init = init, expectancy = "h", interval = .25) |> 
    fmutate(effect = effect) # ? what's the missing thought here?
  
  
  dec <- 
    init_pars |> 
    filter(state == "H") |> 
    fselect(-state,-men,-women) |> 
    fmutate(transition = "init", age = 0) |> 
    bind_rows(decomp_data) |> 
    filter(! transition %in% c("UU","HH","UH","UD")) |> 
    left_join(sen, by = join_by("transition", "age")) |> 
    fmutate(cc = delta * effect) |> 
    fmutate(age = age + 40)
  
  dec
}

# This is the same as the above, with just a few cosmetic
# changes to pick out period columns rather than sex columns.
# It could be more elegant/general in this respect, but good
# enough for now.
do_dec_time <- function(data){
  prev <- data |> 
    filter(age == 40)
  trans <- data |> 
    filter(age > 40) 
  
  init_pars <- 
    prev |> 
    fselect(period, H = HH, U = UU) |> 
    dt_pivot_longer(H:U, names_to = "state", values_to = "p") |> 
    dt_pivot_wider(names_from = period, values_from = p) |> 
    fmutate(delta = `2016-2020` - `2000-2004`,
            p = (`2016-2020` + `2000-2004`) / 2)
  
  init <- init_pars$p
  names(init) <- init_pars$state
  
  decomp_data <- 
    trans |> 
    fselect(period, age, c(HH, HU, HD, UH, UU, UD)) |> 
    dt_pivot_longer(HH:UD, names_to = "transition", values_to = "p") |> 
    dt_pivot_wider(names_from = period, values_from = p) |> 
    fmutate(delta = `2016-2020` - `2000-2004`,
            p = (`2016-2020` + `2000-2004`) / 2) |> 
    fselect(-`2016-2020`, -`2000-2004`) |> 
    arrange(transition, age) |> 
    fmutate(age = age - min(age))
  
  
  sen <-
    decomp_data |> 
    s2t(init = init, expectancy = "h", interval = .25)
  
  
  dec <- 
    init_pars |> 
    filter(state == "H") |> 
    fselect(-state, -`2000-2004`, -`2016-2020`) |> 
    fmutate(transition = "init", age = 0) |> 
    bind_rows(decomp_data) |> 
    filter(! transition %in% c("UU","HH","UH","UD")) |> 
    left_join(sen, by = join_by("transition", "age")) |> 
    fmutate(cc = delta * effect) |> 
    fmutate(age = age + 40)
  
  dec
}


# an expectancy wrapper, also for our data
# characteristics
calc_expectancy <- function(ptibble,
                            expectancy = "h",
                            interval = .25){
  init <- c(H = ptibble$HH[1], U = ptibble$UU[1])
  init <- init * 1/sum(init)
  
  # we could have used f2() or f3() and
  # got identical results (assuming these got
  # interval args integrated)
  f1(hh = ptibble$HH[-1], 
     hu = ptibble$HU[-1], 
     uu = ptibble$UU[-1], 
     uh = ptibble$UH[-1], 
     init = init, 
     expectancy = expectancy,
     interval = interval)
}


# used in 02_smooth_probabilities.R
# does scam p-spline, used to create p_mpi
mono_pspline_scam_chunk <- function(chunk, k = 6){
  newdata <- tibble(age = seq(40,100,by=.25), denom = 1)
  chunk |> 
    mutate(y = round(EMP * denom)) |> 
    filter(!is.na(y),
           !is.nan(y),
           denom > 0) %>% 
    scam(y ~ s(age, bs = "mpi", k = k) + offset(log(denom)),
         data = .,
         family = poisson) |> 
    predict(newdata = newdata) |> 
    as_tibble() |> 
    rename(logp = value) |> 
    bind_cols(newdata) |> 
    mutate(mpi_fit = exp(logp)) |> 
    select(-logp,-denom) 
  
}

# used in 02_smooth_probabilities.R
# does scam p-spline, used to create p_ps
pspline_gam_chunk <- function(chunk, k = 6){
  newdata <- tibble(age = seq(40,100,by=.25), denom = 1)
  chunk |> 
    mutate(y = round(EMP * denom)) |> 
    filter(!is.na(y),
           !is.nan(y),
           denom > 0) %>% 
    gam(y ~ s(age, bs = "ps", k = k) + offset(log(denom)),
        data = .,
        family = poisson) |> 
    predict(newdata = newdata) |> 
    as_tibble() |> 
    rename(logp = value) |> 
    bind_cols(newdata) |> 
    mutate(ps_fit = exp(logp)) |> 
    select(-logp,-denom) 
}

# used in 02_smooth_probabilities.R
# used to create p_hybrid
logistic_weights <- function(x, scale, pivot_age){
  1 / (1+exp(-scale* (x - pivot_age)))
}