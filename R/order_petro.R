#' Order petrographic ordinal variables
#'
#' Order the values (levels) of petrographic ordinal variables (factors) that use the cerUB naming system.
#'
#' @param data Data frame containing petrographic ordinal variables that use the cerUB naming system.
#'
order_petro <- function(data){

  ### put values in proper order

  ord_yni<-c("no","yes")
  ord_fabprot<-c("crystalline","shell-sedimentary","sedimentary")
  ord_clay<-c("Ca-rich", "Fe to Ca-rich", "Fe-rich")
  ord_formtech<-c("hand-made clay colid", "mould", "wheel")

  ord_inclorient<-c("unparallel", "slightly parallel", "parallel", "none")
  ord_incldistrib<-c("poorly", "poorly to moderately", "moderately", "moderately to well", "well", "none")
  ord_temp<-c("unfired", "700-800ºC", "800-900ºC", "900-1000ºC", "1000-1100ºC")
  ord_comp<-c("none", "few", "common", "frequent", "dominant", "predominant")
  ord_comp2<-c("none", "few", "frequent", "predominant")
  ord_freq<-c("none", "very few", "few", "common", "abundant", "very abundant")
  ord_grain<-c("none","very fine","very fine to fine","fine","fine to medium","medium","medium to coarse","coarse","coarse to very coarse","very coarse")
  ord_round<-c("angular","angular to subangular", "subangular","subangular to subrounded","subrounded","subrounded to rounded","rounded","none")
  ord_form<-c("elongate","elongate to equidimensional","equidimensional","equidimensional to laminar","laminar", "none")
  ord_spac<-c("single-spaced","single to double-spaced","double-spaced","double to open-spaced","open-spaced","none")
  ord_sort<-c("poorly-sorted","poorly to moderately-sorted","moderately-sorted","moderately to well-sorted","well-sorted","none")
  ord_grain2<-c("none","very fine silt","very fine to fine silt","fine silt","fine to medium silt","medium silt","medium to coarse silt","coarse silt","coarse silt to very fine sand")

  if ("FABRIC_PROTOTYPE" %in% names(data)){
    data$CLAY <-factor(data$CLAY, levels=ord_clay)
  }
  if ("CLAY" %in% names(data)){
    data$CLAY <-factor(data$CLAY, levels=ord_clay)
  }
  if ("TEMPER" %in% names(data)){
    data$TEMPER <-factor(data$TEMPER, levels=ord_yni)
  }
  if ("INCLUSIONS" %in% names(data)){
    data$INCLUSIONS <-factor(data$INCLUSIONS, levels=ord_yni)
  }
  if ("CLAY_MIX" %in% names(data)){
    data$CLAY_MIX <-factor(data$CLAY_MIX, levels=ord_yni)
  }
  if ("CLAY_TEXT_FEAT" %in% names(data)){
    data$CLAY_TEXT_FEAT <-factor(data$CLAY_TEXT_FEAT, levels=ord_yni)
  }

  if ("INCLUS_DISTRIB" %in% names(data)){
    data$INCLUS_DISTRIB <- factor(data$INCLUS_DISTRIB, levels=ord_incldistrib)
  }

  if ("INCLUS_ORIENT" %in% names(data)){
    data$INCLUS_ORIENT <- factor(data$INCLUS_ORIENT, levels=ord_inclorient)
  }

  if ("TEMP" %in% names(data)){
    data$TEMP <- factor(data$TEMP, levels=ord_temp)
  }

  if ("VOID_OVERALL" %in% names(data)){
    data$VOID_OVERALL <- factor(data$VOID_OVERALL, levels=ord_freq)
  }
  if ("VOID_VESIC_MEGA" %in% names(data)){
    data$VOID_VESIC_MEGA <-factor(data$VOID_VESIC_MEGA, levels=ord_comp2)
  }
  if ("VOID_VESIC_MACRO" %in% names(data)){
    data$VOID_VESIC_MACRO <-factor(data$VOID_VESIC_MACRO, levels=ord_comp2)
  }
  if ("VOID_VESIC_MESO" %in% names(data)){
    data$VOID_VESIC_MESO <-factor(data$VOID_VESIC_MESO, levels=ord_comp2)
  }
  if ("VOID_VESIC_MICRO" %in% names(data)){
    data$VOID_VESIC_MICRO <-factor(data$VOID_VESIC_MICRO, levels=ord_comp2)
  }
  if ("VOID_VUGH_MEGA" %in% names(data)){
    data$VOID_VUGH_MEGA <-factor(data$VOID_VUGH_MEGA, levels=ord_comp2)
  }
  if ("VOID_VUGH_MACRO" %in% names(data)){
    data$VOID_VUGH_MACRO <-factor(data$VOID_VUGH_MACRO, levels=ord_comp2)
  }
  if ("VOID_VUGH_MESO" %in% names(data)){
    data$VOID_VUGH_MESO <-factor(data$VOID_VUGH_MESO, levels=ord_comp2)
  }
  if ("VOID_VUGH_MICRO" %in% names(data)){
    data$VOID_VUGH_MICRO <-factor(data$VOID_VUGH_MICRO, levels=ord_comp2)
  }
  if ("VOID_CHAN_MEGA" %in% names(data)){
    data$VOID_CHAN_MEGA <-factor(data$VOID_CHAN_MEGA, levels=ord_comp2)
  }
  if ("VOID_CHAN_MACRO" %in% names(data)){
    data$VOID_CHAN_MACRO <-factor(data$VOID_CHAN_MACRO, levels=ord_comp2)
  }
  if ("VOID_CHAN_MESO" %in% names(data)){
    data$VOID_CHAN_MESO <-factor(data$VOID_CHAN_MESO, levels=ord_comp2)
  }
  if ("VOID_CHAN_MICRO" %in% names(data)){
    data$VOID_CHAN_MICRO <-factor(data$VOID_CHAN_MICRO, levels=ord_comp2)
  }
  if ("VOID_PLAN_MEGA" %in% names(data)){
    data$VOID_PLAN_MEGA <-factor(data$VOID_PLAN_MEGA, levels=ord_comp2)
  }
  if ("VOID_PLAN_MACRO" %in% names(data)){
    data$VOID_PLAN_MACRO <-factor(data$VOID_PLAN_MACRO, levels=ord_comp2)
  }
  if ("VOID_PLAN_MESO" %in% names(data)){
    data$VOID_PLAN_MESO <-factor(data$VOID_PLAN_MESO, levels=ord_comp2)
  }
  if ("VOID_PLAN_MICRO" %in% names(data)){
    data$VOID_PLAN_MICRO <-factor(data$VOID_PLAN_MICRO, levels=ord_comp2)
  }

  if ("COAR_GRAINSIZE" %in% names(data)){
    data$COAR_GRAINSIZE <- factor(data$COAR_GRAINSIZE, levels=ord_grain)
  }
  if ("COAR_FREQ" %in% names(data)){
    data$COAR_FREQ <- factor(data$COAR_FREQ, levels=ord_freq)
  }
  if ("COAR_ROUNDNESS" %in% names(data)){
    data$COAR_ROUNDNESS <- factor(data$COAR_ROUNDNESS, levels=ord_round)
  }
  if ("COAR_FORM" %in% names(data)){
    data$COAR_FORM <- factor(data$COAR_FORM, levels=ord_form)
  }
  if ("COAR_SPACING" %in% names(data)){
    data$COAR_SPACING <- factor(data$COAR_SPACING, levels=ord_spac)
  }
  if ("COAR_SORTING" %in% names(data)){
    data$COAR_SORTING <- factor(data$COAR_SORTING, levels=ord_sort)
  }

  if ("COAR_R_GRANIT" %in% names(data)){
    data$COAR_R_GRANIT <-factor(data$COAR_R_GRANIT, levels=ord_comp)
  }
  if ("COAR_R_RHYOL" %in% names(data)){
    data$COAR_R_RHYOL <-factor(data$COAR_R_RHYOL, levels=ord_comp)
  }
  if ("COAR_R_DIOR" %in% names(data)){
    data$COAR_R_DIOR <-factor(data$COAR_R_DIOR, levels=ord_comp)
  }
  if ("COAR_R_DAC_AND" %in% names(data)){
    data$COAR_R_DAC_AND <-factor(data$COAR_R_DAC_AND, levels=ord_comp)
  }
  if ("COAR_R_GABBRO" %in% names(data)){
    data$COAR_R_GABBRO <-factor(data$COAR_R_GABBRO, levels=ord_comp)
  }
  if ("COAR_R_BASALT" %in% names(data)){
    data$COAR_R_BASALT <-factor(data$COAR_R_BASALT, levels=ord_comp)
  }
  if ("COAR_R_SYEN" %in% names(data)){
    data$COAR_R_SYEN <-factor(data$COAR_R_SYEN, levels=ord_comp)
  }
  if ("COAR_R_TRACHY" %in% names(data)){
    data$COAR_R_TRACHY <-factor(data$COAR_R_TRACHY, levels=ord_comp)
  }
  if ("COAR_R_CONGBREC" %in% names(data)){
    data$COAR_R_CONGBREC <-factor(data$COAR_R_CONGBREC, levels=ord_comp)
  }
  if ("COAR_R_QTZSANDST" %in% names(data)){
    data$COAR_R_QTZSANDST <-factor(data$COAR_R_QTZSANDST, levels=ord_comp)
  }
  if ("COAR_R_FELDSANDST" %in% names(data)){
    data$COAR_R_FELDSANDST <-factor(data$COAR_R_FELDSANDST, levels=ord_comp)
  }
  if ("COAR_R_LITSANDST" %in% names(data)){
    data$COAR_R_LITSANDST <-factor(data$COAR_R_LITSANDST, levels=ord_comp)
  }
  if ("COAR_R_CASILTST" %in% names(data)){
    data$COAR_R_CASILTST <-factor(data$COAR_R_CASILTST, levels=ord_comp)
  }
  if ("COAR_R_FESILTST" %in% names(data)){
    data$COAR_R_FESILTST <-factor(data$COAR_R_FESILTST, levels=ord_comp)
  }
  if ("COAR_R_CAMUDST" %in% names(data)){
    data$COAR_R_CAMUDST <-factor(data$COAR_R_CAMUDST, levels=ord_comp)
  }
  if ("COAR_R_FEMUDST" %in% names(data)){
    data$COAR_R_FEMUDST <-factor(data$COAR_R_FEMUDST, levels=ord_comp)
  }
  if ("COAR_R_CLAYST" %in% names(data)){
    data$COAR_R_CLAYST <-factor(data$COAR_R_CLAYST, levels=ord_comp)
  }
  if ("COAR_R_LIMEST" %in% names(data)){
    data$COAR_R_LIMEST <-factor(data$COAR_R_LIMEST, levels=ord_comp)
  }
  if ("COAR_R_CALS" %in% names(data)){
    data$COAR_R_CALS <-factor(data$COAR_R_CALS, levels=ord_comp)
  }
  if ("COAR_R_DOLOM" %in% names(data)){
    data$COAR_R_DOLOM <-factor(data$COAR_R_DOLOM, levels=ord_comp)
  }
  if ("COAR_R_CALM" %in% names(data)){
    data$COAR_R_CALM <-factor(data$COAR_R_CALM, levels=ord_comp)
  }
  if ("COAR_R_SPELEO" %in% names(data)){
    data$COAR_R_SPELEO <-factor(data$COAR_R_SPELEO, levels=ord_comp)
  }
  if ("COAR_R_CALFOS" %in% names(data)){
    data$COAR_R_CALFOS <-factor(data$COAR_R_CALFOS, levels=ord_comp)
  }
  if ("COAR_R_BIVAL" %in% names(data)){
    data$COAR_R_BIVAL <-factor(data$COAR_R_BIVAL, levels=ord_comp)
  }
  if ("COAR_R_TRAV" %in% names(data)){
    data$COAR_R_TRAV <-factor(data$COAR_R_TRAV, levels=ord_comp)
  }
  if ("COAR_R_EVAP" %in% names(data)){
    data$COAR_R_EVAP <-factor(data$COAR_R_EVAP, levels=ord_comp)
  }
  if ("COAR_R_CHERT" %in% names(data)){
    data$COAR_R_CHERT <-factor(data$COAR_R_CHERT, levels=ord_comp)
  }
  if ("COAR_R_RADIO" %in% names(data)){
    data$COAR_R_RADIO <-factor(data$COAR_R_RADIO, levels=ord_comp)
  }
  if ("COAR_R_SLATE" %in% names(data)){
    data$COAR_R_SLATE <-factor(data$COAR_R_SLATE, levels=ord_comp)
  }
  if ("COAR_R_PHYLL" %in% names(data)){
    data$COAR_R_PHYLL <-factor(data$COAR_R_PHYLL, levels=ord_comp)
  }
  if ("COAR_R_SCHIST" %in% names(data)){
    data$COAR_R_SCHIST <-factor(data$COAR_R_SCHIST, levels=ord_comp)
  }
  if ("COAR_R_GNEISS" %in% names(data)){
    data$COAR_R_GNEISS <-factor(data$COAR_R_GNEISS, levels=ord_comp)
  }
  if ("COAR_R_QUARTZ" %in% names(data)){
    data$COAR_R_QUARTZ <-factor(data$COAR_R_QUARTZ, levels=ord_comp)
  }
  if ("COAR_R_MARBLE" %in% names(data)){
    data$COAR_R_MARBLE <-factor(data$COAR_R_MARBLE, levels=ord_comp)
  }
  if ("COAR_R_AMP" %in% names(data)){
    data$COAR_R_AMP <-factor(data$COAR_R_AMP, levels=ord_comp)
  }
  if ("COAR_R_SERP" %in% names(data)){
    data$COAR_R_SERP <-factor(data$COAR_R_SERP, levels=ord_comp)
  }
  if ("COAR_C_QTZ" %in% names(data)){
    data$COAR_C_QTZ <-factor(data$COAR_C_QTZ, levels=ord_comp)
  }
  if ("COAR_C_PL" %in% names(data)){
    data$COAR_C_PL <-factor(data$COAR_C_PL, levels=ord_comp)
  }
  if ("COAR_C_KFS" %in% names(data)){
    data$COAR_C_KFS <-factor(data$COAR_C_KFS, levels=ord_comp)
  }
  if ("COAR_C_SA" %in% names(data)){
    data$COAR_C_SA <-factor(data$COAR_C_SA, levels=ord_comp)
  }
  if ("COAR_C_MS" %in% names(data)){
    data$COAR_C_MS <-factor(data$COAR_C_MS, levels=ord_comp)
  }
  if ("COAR_C_BT" %in% names(data)){
    data$COAR_C_BT <-factor(data$COAR_C_BT, levels=ord_comp)
  }
  if ("COAR_C_SRP" %in% names(data)){
    data$COAR_C_SRP <-factor(data$COAR_C_SRP, levels=ord_comp)
  }
  if ("COAR_C_OP" %in% names(data)){
    data$COAR_C_OP <-factor(data$COAR_C_OP, levels=ord_comp)
  }
  if ("COAR_C_RT" %in% names(data)){
    data$COAR_C_RT <-factor(data$COAR_C_RT, levels=ord_comp)
  }
  if ("COAR_C_SPL" %in% names(data)){
    data$COAR_C_SPL <-factor(data$COAR_C_SPL, levels=ord_comp)
  }
  if ("COAR_C_EP" %in% names(data)){
    data$COAR_C_EP <-factor(data$COAR_C_EP, levels=ord_comp)
  }
  if ("COAR_C_AM" %in% names(data)){
    data$COAR_C_AM <-factor(data$COAR_C_AM, levels=ord_comp)
  }
  if ("COAR_C_CPX" %in% names(data)){
    data$COAR_C_CPX <-factor(data$COAR_C_CPX, levels=ord_comp)
  }
  if ("COAR_C_OPX" %in% names(data)){
    data$COAR_C_OPX <-factor(data$COAR_C_OPX, levels=ord_comp)
  }
  if ("COAR_C_OL" %in% names(data)){
    data$COAR_C_OL <-factor(data$COAR_C_OL, levels=ord_comp)
  }
  if ("COAR_C_GRT" %in% names(data)){
    data$COAR_C_GRT <-factor(data$COAR_C_GRT, levels=ord_comp)
  }
  if ("COAR_C_SIL" %in% names(data)){
    data$COAR_C_SIL <-factor(data$COAR_C_SIL, levels=ord_comp)
  }
  if ("COAR_C_ST" %in% names(data)){
    data$COAR_C_ST <-factor(data$COAR_C_ST, levels=ord_comp)
  }
  if ("COAR_C_TTN" %in% names(data)){
    data$COAR_C_TTN <-factor(data$COAR_C_TTN, levels=ord_comp)
  }
  if ("COAR_C_ZRN" %in% names(data)){
    data$COAR_C_ZRN <-factor(data$COAR_C_ZRN, levels=ord_comp)
  }
  if ("COAR_C_AP" %in% names(data)){
    data$COAR_C_AP <-factor(data$COAR_C_AP, levels=ord_comp)
  }
  if ("COAR_C_PY" %in% names(data)){
    data$COAR_C_PY <-factor(data$COAR_C_PY, levels=ord_comp)
  }

  if ("FINE_FREQ" %in% names(data)){
    data$FINE_FREQ <- factor(data$FINE_FREQ, levels=ord_freq)
  }
  if ("FINE_GRAINSIZE" %in% names(data)){
    data$FINE_GRAINSIZE <- factor(data$FINE_GRAINSIZE, levels=ord_grain2)
  }
  if ("FINE_FORM" %in% names(data)){
    data$FINE_FORM <- factor(data$FINE_FORM, levels=ord_form)
  }

  if ("FINE_C_CAL" %in% names(data)){
    data$FINE_C_CAL <- factor(data$FINE_C_CAL, levels=ord_comp2)
  }
  if ("FINE_C_CAL_FOS" %in% names(data)){
    data$FINE_C_CAL_FOS <- factor(data$FINE_C_CAL_FOS, levels=ord_comp2)
  }
  if ("FINE_C_QTZ" %in% names(data)){
    data$FINE_C_QTZ <- factor(data$FINE_C_QTZ, levels=ord_comp2)
  }
  if ("FINE_C_PL" %in% names(data)){
    data$FINE_C_PL <- factor(data$FINE_C_PL, levels=ord_comp2)
  }
  if ("FINE_C_KFS" %in% names(data)){
    data$FINE_C_KFS <- factor(data$FINE_C_KFS, levels=ord_comp2)
  }
  if ("FINE_C_SA" %in% names(data)){
    data$FINE_C_SA <- factor(data$FINE_C_SA, levels=ord_comp2)
  }
  if ("FINE_C_MS" %in% names(data)){
    data$FINE_C_MS <- factor(data$FINE_C_MS, levels=ord_comp2)
  }
  if ("FINE_C_BT" %in% names(data)){
    data$FINE_C_BT <- factor(data$FINE_C_BT, levels=ord_comp2)
  }
  if ("FINE_C_SRP" %in% names(data)){
    data$FINE_C_SRP <- factor(data$FINE_C_SRP, levels=ord_comp2)
  }
  if ("FINE_C_OP" %in% names(data)){
    data$FINE_C_OP <- factor(data$FINE_C_OP, levels=ord_comp2)
  }
  if ("FINE_C_RT" %in% names(data)){
    data$FINE_C_RT <- factor(data$FINE_C_RT, levels=ord_comp2)
  }
  if ("FINE_C_EP" %in% names(data)){
    data$FINE_C_EP <- factor(data$FINE_C_EP, levels=ord_comp2)
  }
  if ("FINE_C_AM" %in% names(data)){
    data$FINE_C_AM <- factor(data$FINE_C_AM, levels=ord_comp2)
  }
  if ("FINE_C_CPX" %in% names(data)){
    data$FINE_C_CPX <- factor(data$FINE_C_CPX, levels=ord_comp2)
  }
  if ("FINE_C_OPX" %in% names(data)){
    data$FINE_C_OPX <- factor(data$FINE_C_OPX, levels=ord_comp2)
  }
  if ("FINE_C_OL" %in% names(data)){
    data$FINE_C_OL <- factor(data$FINE_C_OL, levels=ord_comp2)
  }
  if ("FINE_C_GRT" %in% names(data)){
    data$FINE_C_GRT <- factor(data$FINE_C_GRT, levels=ord_comp2)
  }
  if ("FINE_C_ZRN" %in% names(data)){
    data$FINE_C_ZRN <- factor(data$FINE_C_ZRN, levels=ord_comp2)
  }

  return(data)
}
