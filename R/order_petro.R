#'
#'
#'
order_petro <- function(dt) {

  ### put values in proper order

  ord_yni <- c("no", "yes")
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

  if ("FABRIC_PROTOTYPE" %in% names(dt)){
    dt$CLAY <-factor(dt$CLAY, levels=ord_clay)
  }
  if ("CLAY" %in% names(dt)){
    dt$CLAY <-factor(dt$CLAY, levels=ord_clay)
  }
  if ("TEMPER" %in% names(dt)){
    dt$TEMPER <-factor(dt$TEMPER, levels=ord_yni)
  }
  if ("INCLUSIONS" %in% names(dt)){
    dt$INCLUSIONS <-factor(dt$INCLUSIONS, levels=ord_yni)
  }
  if ("CLAY_MIX" %in% names(dt)){
    dt$CLAY_MIX <-factor(dt$CLAY_MIX, levels=ord_yni)
  }
  if ("CLAY_TEXT_FEAT" %in% names(dt)){
    dt$CLAY_TEXT_FEAT <-factor(dt$CLAY_TEXT_FEAT, levels=ord_yni)
  }

  if ("INCLUS_DISTRIB" %in% names(dt)){
    dt$INCLUS_DISTRIB <- factor(dt$INCLUS_DISTRIB, levels=ord_incldistrib)
  }

  if ("INCLUS_ORIENT" %in% names(dt)){
    dt$INCLUS_ORIENT <- factor(dt$INCLUS_ORIENT, levels=ord_inclorient)
  }

  if ("TEMP" %in% names(dt)){
    dt$TEMP <- factor(dt$TEMP, levels=ord_temp)
  }

  if ("VOID_OVERALL" %in% names(dt)){
    dt$VOID_OVERALL <- factor(dt$VOID_OVERALL, levels=ord_freq)
  }
  if ("VOID_VESIC_MEGA" %in% names(dt)){
    dt$VOID_VESIC_MEGA <-factor(dt$VOID_VESIC_MEGA, levels=ord_comp2)
  }
  if ("VOID_VESIC_MACRO" %in% names(dt)){
    dt$VOID_VESIC_MACRO <-factor(dt$VOID_VESIC_MACRO, levels=ord_comp2)
  }
  if ("VOID_VESIC_MESO" %in% names(dt)){
    dt$VOID_VESIC_MESO <-factor(dt$VOID_VESIC_MESO, levels=ord_comp2)
  }
  if ("VOID_VESIC_MICRO" %in% names(dt)){
    dt$VOID_VESIC_MICRO <-factor(dt$VOID_VESIC_MICRO, levels=ord_comp2)
  }
  if ("VOID_VUGH_MEGA" %in% names(dt)){
    dt$VOID_VUGH_MEGA <-factor(dt$VOID_VUGH_MEGA, levels=ord_comp2)
  }
  if ("VOID_VUGH_MACRO" %in% names(dt)){
    dt$VOID_VUGH_MACRO <-factor(dt$VOID_VUGH_MACRO, levels=ord_comp2)
  }
  if ("VOID_VUGH_MESO" %in% names(dt)){
    dt$VOID_VUGH_MESO <-factor(dt$VOID_VUGH_MESO, levels=ord_comp2)
  }
  if ("VOID_VUGH_MICRO" %in% names(dt)){
    dt$VOID_VUGH_MICRO <-factor(dt$VOID_VUGH_MICRO, levels=ord_comp2)
  }
  if ("VOID_CHAN_MEGA" %in% names(dt)){
    dt$VOID_CHAN_MEGA <-factor(dt$VOID_CHAN_MEGA, levels=ord_comp2)
  }
  if ("VOID_CHAN_MACRO" %in% names(dt)){
    dt$VOID_CHAN_MACRO <-factor(dt$VOID_CHAN_MACRO, levels=ord_comp2)
  }
  if ("VOID_CHAN_MESO" %in% names(dt)){
    dt$VOID_CHAN_MESO <-factor(dt$VOID_CHAN_MESO, levels=ord_comp2)
  }
  if ("VOID_CHAN_MICRO" %in% names(dt)){
    dt$VOID_CHAN_MICRO <-factor(dt$VOID_CHAN_MICRO, levels=ord_comp2)
  }
  if ("VOID_PLAN_MEGA" %in% names(dt)){
    dt$VOID_PLAN_MEGA <-factor(dt$VOID_PLAN_MEGA, levels=ord_comp2)
  }
  if ("VOID_PLAN_MACRO" %in% names(dt)){
    dt$VOID_PLAN_MACRO <-factor(dt$VOID_PLAN_MACRO, levels=ord_comp2)
  }
  if ("VOID_PLAN_MESO" %in% names(dt)){
    dt$VOID_PLAN_MESO <-factor(dt$VOID_PLAN_MESO, levels=ord_comp2)
  }
  if ("VOID_PLAN_MICRO" %in% names(dt)){
    dt$VOID_PLAN_MICRO <-factor(dt$VOID_PLAN_MICRO, levels=ord_comp2)
  }

  if ("COAR_GRAINSIZE" %in% names(dt)){
    dt$COAR_GRAINSIZE <- factor(dt$COAR_GRAINSIZE, levels=ord_grain)
  }
  if ("COAR_FREQ" %in% names(dt)){
    dt$COAR_FREQ <- factor(dt$COAR_FREQ, levels=ord_freq)
  }
  if ("COAR_ROUNDNESS" %in% names(dt)){
    dt$COAR_ROUNDNESS <- factor(dt$COAR_ROUNDNESS, levels=ord_round)
  }
  if ("COAR_FORM" %in% names(dt)){
    dt$COAR_FORM <- factor(dt$COAR_FORM, levels=ord_form)
  }
  if ("COAR_SPACING" %in% names(dt)){
    dt$COAR_SPACING <- factor(dt$COAR_SPACING, levels=ord_spac)
  }
  if ("COAR_SORTING" %in% names(dt)){
    dt$COAR_SORTING <- factor(dt$COAR_SORTING, levels=ord_sort)
  }

  if ("COAR_R_GRANIT" %in% names(dt)){
    dt$COAR_R_GRANIT <-factor(dt$COAR_R_GRANIT, levels=ord_comp)
  }
  if ("COAR_R_RHYOL" %in% names(dt)){
    dt$COAR_R_RHYOL <-factor(dt$COAR_R_RHYOL, levels=ord_comp)
  }
  if ("COAR_R_DIOR" %in% names(dt)){
    dt$COAR_R_DIOR <-factor(dt$COAR_R_DIOR, levels=ord_comp)
  }
  if ("COAR_R_DAC_AND" %in% names(dt)){
    dt$COAR_R_DAC_AND <-factor(dt$COAR_R_DAC_AND, levels=ord_comp)
  }
  if ("COAR_R_GABBRO" %in% names(dt)){
    dt$COAR_R_GABBRO <-factor(dt$COAR_R_GABBRO, levels=ord_comp)
  }
  if ("COAR_R_BASALT" %in% names(dt)){
    dt$COAR_R_BASALT <-factor(dt$COAR_R_BASALT, levels=ord_comp)
  }
  if ("COAR_R_SYEN" %in% names(dt)){
    dt$COAR_R_SYEN <-factor(dt$COAR_R_SYEN, levels=ord_comp)
  }
  if ("COAR_R_TRACHY" %in% names(dt)){
    dt$COAR_R_TRACHY <-factor(dt$COAR_R_TRACHY, levels=ord_comp)
  }
  if ("COAR_R_CONGBREC" %in% names(dt)){
    dt$COAR_R_CONGBREC <-factor(dt$COAR_R_CONGBREC, levels=ord_comp)
  }
  if ("COAR_R_QTZSANDST" %in% names(dt)){
    dt$COAR_R_QTZSANDST <-factor(dt$COAR_R_QTZSANDST, levels=ord_comp)
  }
  if ("COAR_R_FELDSANDST" %in% names(dt)){
    dt$COAR_R_FELDSANDST <-factor(dt$COAR_R_FELDSANDST, levels=ord_comp)
  }
  if ("COAR_R_LITSANDST" %in% names(dt)){
    dt$COAR_R_LITSANDST <-factor(dt$COAR_R_LITSANDST, levels=ord_comp)
  }
  if ("COAR_R_CASILTST" %in% names(dt)){
    dt$COAR_R_CASILTST <-factor(dt$COAR_R_CASILTST, levels=ord_comp)
  }
  if ("COAR_R_FESILTST" %in% names(dt)){
    dt$COAR_R_FESILTST <-factor(dt$COAR_R_FESILTST, levels=ord_comp)
  }
  if ("COAR_R_CAMUDST" %in% names(dt)){
    dt$COAR_R_CAMUDST <-factor(dt$COAR_R_CAMUDST, levels=ord_comp)
  }
  if ("COAR_R_FEMUDST" %in% names(dt)){
    dt$COAR_R_FEMUDST <-factor(dt$COAR_R_FEMUDST, levels=ord_comp)
  }
  if ("COAR_R_CLAYST" %in% names(dt)){
    dt$COAR_R_CLAYST <-factor(dt$COAR_R_CLAYST, levels=ord_comp)
  }
  if ("COAR_R_LIMEST" %in% names(dt)){
    dt$COAR_R_LIMEST <-factor(dt$COAR_R_LIMEST, levels=ord_comp)
  }
  if ("COAR_R_CALS" %in% names(dt)){
    dt$COAR_R_CALS <-factor(dt$COAR_R_CALS, levels=ord_comp)
  }
  if ("COAR_R_DOLOM" %in% names(dt)){
    dt$COAR_R_DOLOM <-factor(dt$COAR_R_DOLOM, levels=ord_comp)
  }
  if ("COAR_R_CALM" %in% names(dt)){
    dt$COAR_R_CALM <-factor(dt$COAR_R_CALM, levels=ord_comp)
  }
  if ("COAR_R_SPELEO" %in% names(dt)){
    dt$COAR_R_SPELEO <-factor(dt$COAR_R_SPELEO, levels=ord_comp)
  }
  if ("COAR_R_CALFOS" %in% names(dt)){
    dt$COAR_R_CALFOS <-factor(dt$COAR_R_CALFOS, levels=ord_comp)
  }
  if ("COAR_R_BIVAL" %in% names(dt)){
    dt$COAR_R_BIVAL <-factor(dt$COAR_R_BIVAL, levels=ord_comp)
  }
  if ("COAR_R_TRAV" %in% names(dt)){
    dt$COAR_R_TRAV <-factor(dt$COAR_R_TRAV, levels=ord_comp)
  }
  if ("COAR_R_EVAP" %in% names(dt)){
    dt$COAR_R_EVAP <-factor(dt$COAR_R_EVAP, levels=ord_comp)
  }
  if ("COAR_R_CHERT" %in% names(dt)){
    dt$COAR_R_CHERT <-factor(dt$COAR_R_CHERT, levels=ord_comp)
  }
  if ("COAR_R_RADIO" %in% names(dt)){
    dt$COAR_R_RADIO <-factor(dt$COAR_R_RADIO, levels=ord_comp)
  }
  if ("COAR_R_SLATE" %in% names(dt)){
    dt$COAR_R_SLATE <-factor(dt$COAR_R_SLATE, levels=ord_comp)
  }
  if ("COAR_R_PHYLL" %in% names(dt)){
    dt$COAR_R_PHYLL <-factor(dt$COAR_R_PHYLL, levels=ord_comp)
  }
  if ("COAR_R_SCHIST" %in% names(dt)){
    dt$COAR_R_SCHIST <-factor(dt$COAR_R_SCHIST, levels=ord_comp)
  }
  if ("COAR_R_GNEISS" %in% names(dt)){
    dt$COAR_R_GNEISS <-factor(dt$COAR_R_GNEISS, levels=ord_comp)
  }
  if ("COAR_R_QUARTZ" %in% names(dt)){
    dt$COAR_R_QUARTZ <-factor(dt$COAR_R_QUARTZ, levels=ord_comp)
  }
  if ("COAR_R_MARBLE" %in% names(dt)){
    dt$COAR_R_MARBLE <-factor(dt$COAR_R_MARBLE, levels=ord_comp)
  }
  if ("COAR_R_AMP" %in% names(dt)){
    dt$COAR_R_AMP <-factor(dt$COAR_R_AMP, levels=ord_comp)
  }
  if ("COAR_R_SERP" %in% names(dt)){
    dt$COAR_R_SERP <-factor(dt$COAR_R_SERP, levels=ord_comp)
  }
  if ("COAR_C_QTZ" %in% names(dt)){
    dt$COAR_C_QTZ <-factor(dt$COAR_C_QTZ, levels=ord_comp)
  }
  if ("COAR_C_PL" %in% names(dt)){
    dt$COAR_C_PL <-factor(dt$COAR_C_PL, levels=ord_comp)
  }
  if ("COAR_C_KFS" %in% names(dt)){
    dt$COAR_C_KFS <-factor(dt$COAR_C_KFS, levels=ord_comp)
  }
  if ("COAR_C_SA" %in% names(dt)){
    dt$COAR_C_SA <-factor(dt$COAR_C_SA, levels=ord_comp)
  }
  if ("COAR_C_MS" %in% names(dt)){
    dt$COAR_C_MS <-factor(dt$COAR_C_MS, levels=ord_comp)
  }
  if ("COAR_C_BT" %in% names(dt)){
    dt$COAR_C_BT <-factor(dt$COAR_C_BT, levels=ord_comp)
  }
  if ("COAR_C_SRP" %in% names(dt)){
    dt$COAR_C_SRP <-factor(dt$COAR_C_SRP, levels=ord_comp)
  }
  if ("COAR_C_OP" %in% names(dt)){
    dt$COAR_C_OP <-factor(dt$COAR_C_OP, levels=ord_comp)
  }
  if ("COAR_C_RT" %in% names(dt)){
    dt$COAR_C_RT <-factor(dt$COAR_C_RT, levels=ord_comp)
  }
  if ("COAR_C_SPL" %in% names(dt)){
    dt$COAR_C_SPL <-factor(dt$COAR_C_SPL, levels=ord_comp)
  }
  if ("COAR_C_EP" %in% names(dt)){
    dt$COAR_C_EP <-factor(dt$COAR_C_EP, levels=ord_comp)
  }
  if ("COAR_C_AM" %in% names(dt)){
    dt$COAR_C_AM <-factor(dt$COAR_C_AM, levels=ord_comp)
  }
  if ("COAR_C_CPX" %in% names(dt)){
    dt$COAR_C_CPX <-factor(dt$COAR_C_CPX, levels=ord_comp)
  }
  if ("COAR_C_OPX" %in% names(dt)){
    dt$COAR_C_OPX <-factor(dt$COAR_C_OPX, levels=ord_comp)
  }
  if ("COAR_C_OL" %in% names(dt)){
    dt$COAR_C_OL <-factor(dt$COAR_C_OL, levels=ord_comp)
  }
  if ("COAR_C_GRT" %in% names(dt)){
    dt$COAR_C_GRT <-factor(dt$COAR_C_GRT, levels=ord_comp)
  }
  if ("COAR_C_SIL" %in% names(dt)){
    dt$COAR_C_SIL <-factor(dt$COAR_C_SIL, levels=ord_comp)
  }
  if ("COAR_C_ST" %in% names(dt)){
    dt$COAR_C_ST <-factor(dt$COAR_C_ST, levels=ord_comp)
  }
  if ("COAR_C_TTN" %in% names(dt)){
    dt$COAR_C_TTN <-factor(dt$COAR_C_TTN, levels=ord_comp)
  }
  if ("COAR_C_ZRN" %in% names(dt)){
    dt$COAR_C_ZRN <-factor(dt$COAR_C_ZRN, levels=ord_comp)
  }
  if ("COAR_C_AP" %in% names(dt)){
    dt$COAR_C_AP <-factor(dt$COAR_C_AP, levels=ord_comp)
  }
  if ("COAR_C_PY" %in% names(dt)){
    dt$COAR_C_PY <-factor(dt$COAR_C_PY, levels=ord_comp)
  }

  if ("FINE_FREQ" %in% names(dt)){
    dt$FINE_FREQ <- factor(dt$FINE_FREQ, levels=ord_freq)
  }
  if ("FINE_GRAINSIZE" %in% names(dt)){
    dt$FINE_GRAINSIZE <- factor(dt$FINE_GRAINSIZE, levels=ord_grain2)
  }
  if ("FINE_FORM" %in% names(dt)){
    dt$FINE_FORM <- factor(dt$FINE_FORM, levels=ord_form)
  }

  if ("FINE_C_CAL" %in% names(dt)){
    dt$FINE_C_CAL <- factor(dt$FINE_C_CAL, levels=ord_comp2)
  }
  if ("FINE_C_CAL_FOS" %in% names(dt)){
    dt$FINE_C_CAL_FOS <- factor(dt$FINE_C_CAL_FOS, levels=ord_comp2)
  }
  if ("FINE_C_QTZ" %in% names(dt)){
    dt$FINE_C_QTZ <- factor(dt$FINE_C_QTZ, levels=ord_comp2)
  }
  if ("FINE_C_PL" %in% names(dt)){
    dt$FINE_C_PL <- factor(dt$FINE_C_PL, levels=ord_comp2)
  }
  if ("FINE_C_KFS" %in% names(dt)){
    dt$FINE_C_KFS <- factor(dt$FINE_C_KFS, levels=ord_comp2)
  }
  if ("FINE_C_SA" %in% names(dt)){
    dt$FINE_C_SA <- factor(dt$FINE_C_SA, levels=ord_comp2)
  }
  if ("FINE_C_MS" %in% names(dt)){
    dt$FINE_C_MS <- factor(dt$FINE_C_MS, levels=ord_comp2)
  }
  if ("FINE_C_BT" %in% names(dt)){
    dt$FINE_C_BT <- factor(dt$FINE_C_BT, levels=ord_comp2)
  }
  if ("FINE_C_SRP" %in% names(dt)){
    dt$FINE_C_SRP <- factor(dt$FINE_C_SRP, levels=ord_comp2)
  }
  if ("FINE_C_OP" %in% names(dt)){
    dt$FINE_C_OP <- factor(dt$FINE_C_OP, levels=ord_comp2)
  }
  if ("FINE_C_RT" %in% names(dt)){
    dt$FINE_C_RT <- factor(dt$FINE_C_RT, levels=ord_comp2)
  }
  if ("FINE_C_EP" %in% names(dt)){
    dt$FINE_C_EP <- factor(dt$FINE_C_EP, levels=ord_comp2)
  }
  if ("FINE_C_AM" %in% names(dt)){
    dt$FINE_C_AM <- factor(dt$FINE_C_AM, levels=ord_comp2)
  }
  if ("FINE_C_CPX" %in% names(dt)){
    dt$FINE_C_CPX <- factor(dt$FINE_C_CPX, levels=ord_comp2)
  }
  if ("FINE_C_OPX" %in% names(dt)){
    dt$FINE_C_OPX <- factor(dt$FINE_C_OPX, levels=ord_comp2)
  }
  if ("FINE_C_OL" %in% names(dt)){
    dt$FINE_C_OL <- factor(dt$FINE_C_OL, levels=ord_comp2)
  }
  if ("FINE_C_GRT" %in% names(dt)){
    dt$FINE_C_GRT <- factor(dt$FINE_C_GRT, levels=ord_comp2)
  }
  if ("FINE_C_ZRN" %in% names(dt)){
    dt$FINE_C_ZRN <- factor(dt$FINE_C_ZRN, levels=ord_comp2)
  }

  return(dt)
}
