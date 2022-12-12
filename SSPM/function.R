PM_func <- function(Bt, r, K, n, Ft, pe){
  B_next <- (Bt + r/(n-1)*Bt*(1-(Bt/K)^(n-1)) - Ft*Bt)*exp(pe)
  return(max(B_next, 100))
}


plot_barbiomass <- function(res, with_biomass=TRUE){
  
  Bmsy <- get.par("Bmsy",res)[2]
  res3 <- get_factor_value(res)    
  
  cols <- c("Surplus_Production"=2, "Catch"=3, "Process_error"=4)
  gg1 <- res3 %>% dplyr::filter(!is.na(process_error)) %>%
    ggplot() +
    geom_area(aes(x=year,y=biomass), fill="gray")+
    scale_color_manual(name="Arrow",values=cols)+
    geom_point(aes(x=year,y=biomass),col=1) +
    geom_segment(aes(x=year,xend=year,y=biomass,yend=biomass1,color="Surplus_Production"),
                 arrow=arrow(type="closed",length=unit(0.20,"cm")),lwd=1)+
    geom_segment(aes(x=year+0.3 ,xend=year+0.3 ,y=biomass1,yend=biomass2,color="Catch"),
                 arrow=arrow(type="closed",length=unit(0.20,"cm")),lwd=1)+
    geom_segment(aes(x=year+0.6,xend=year+0.6,y=biomass2,yend=biomass3,color="Process_error"),
                 arrow=arrow(type="closed",length=unit(0.20,"cm")),lwd=1)+
    theme_bw(base_size=14) + coord_cartesian(expand=0.2) +
    geom_hline(aes(yintercept=0)) + theme(legend.position="top") +
    geom_hline(aes(yintercept=Bmsy),col=2,lty=2)
  
  gg2 <- res3 %>% dplyr::filter(!is.na(process_error)) %>%
    ggplot() +
    scale_color_manual(name="Arrow",values=cols)+
    geom_segment(aes(x=year,xend=year,y=0,yend=sp,color="Surplus_Production"),
                 arrow=arrow(type="closed",length=unit(0.20,"cm")),lwd=1)+
    geom_segment(aes(x=year+0.3 ,xend=year+0.3 ,y=0,yend=-catch,color="Catch"),
                 arrow=arrow(type="closed",length=unit(0.20,"cm")),lwd=1)+
    geom_segment(aes(x=year+0.6,xend=year+0.6,y=0,yend=process_error,color="Process_error"),
                 arrow=arrow(type="closed",length=unit(0.20,"cm")),lwd=1)+
    theme_bw(base_size=14) + coord_cartesian(expand=0.2) +
    geom_hline(aes(yintercept=0)) + theme(legend.position="top")  +
    xlab("Year") + ylab("Diff")
  
  list(with_biomass=gg1, only_diff=gg2)
  
}



get_factor_value <- function(res){
  K <- get.par("logK",res,exp=TRUE)[2]
  r <- get.par("logr",res,exp=TRUE)[2]
  n <- get.par("logn",res,exp=TRUE)[2]
  Bmsy <- get.par("Bmsy",res)[2]
  calc_sp <- function(B, K, r, n){
    r/(n-1) * B * (1-(B/K)^(n-1))
  }
  
  res3 <- get_spict_res(res) %>%
    dplyr::filter(stat=="B") %>%
    select(est, year) %>%
    rename(biomass=est) %>%
    mutate(year=as.numeric(year)) %>%
    left_join(tibble(catch=res$inp$obsC, year=as.numeric(res$inp$timeC))) %>%
    mutate(sp=calc_sp(biomass,K,r,n)) %>%
    mutate(biomass0=biomass,
           biomass1=biomass+sp,
           biomass2=biomass+sp-catch)
  res3$biomass3 <- c(res3$biomass[-1],NA)
  res3 <- res3 %>% mutate(process_error=biomass3-biomass2)
  return(res3)
}


get_spict_res <- function(res_spict, CI=0.95){
  
  get_stat_ <- function(stat_name){
    get.par(stat_name, res_spict, exp = TRUE, CI=CI) %>%
      as.data.frame() %>%
      rownames_to_column(var="year") %>%
      mutate(season=as.numeric(year)-floor(as.numeric(year))) %>%
      dplyr::filter(season==0)  %>%
      dplyr::select(-season)
  }
  
  #distinguish dat or prediction
  yr_last <- max(res_spict$inp$timeC)
  
  # biomass time series
  FBdata <- bind_rows(get_stat_("logB") %>% mutate(stat="B"),
                      get_stat_("logF") %>% mutate(stat="F"),
                      get_stat_("logBBmsy") %>% mutate(stat="BBmsy"),
                      get_stat_("logFFmsy") %>% mutate(stat="FFmsy")) %>%
    mutate(obs_pred = case_when(year <= yr_last ~ "obs",
                                year > yr_last ~ "pred"))
  
  # important parameter
  pardata <- sumspict.parest(res_spict, CI=CI) %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    dplyr::mutate(parameter=str_replace_all(rowname," ","")) %>%
    dplyr::select(-rowname, -log.est) %>%
    dplyr::rename(est=estimate, ll=cilow, ul=ciupp, stat=parameter)
  
  otherdata <- dplyr::bind_rows(dplyr::tibble(stat="convergence",est=res_spict$opt$convergence),
                                dplyr::tibble(stat="number_se_nan",est=sum(is.nan(res_spict$sd))),
                                dplyr::tibble(stat="number_se_inf",est=sum(is.infinite(res_spict$sd))))
  
  otherdata2 <- res_spict %>% spict::get.par("logbkfrac",.,exp=TRUE) %>% dplyr::as_tibble() %>% dplyr::mutate(stat="bkfrac")
  
  dplyr::bind_rows(pardata,otherdata2, FBdata,otherdata) %>% dplyr::mutate(model="spict") %>% dplyr::mutate(year=as.numeric(year))
  
}


