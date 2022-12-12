endoR_devel_grt<-function (TA = 20, TAREF = TA, TGRD = TA, TSKY = TA, VEL = 0.1, 
                           RH = 5, QSOLR = 0, Z = 20, ELEV = 0, ABSSB = 0.8, FLTYPE = 0, 
                           TCONDSB = TGRD, KSUB = 2.79, TBUSH = TA, BP = -1, O2GAS = 20.95, 
                           N2GAS = 79.02, CO2GAS = 0.0412, R_PCO2 = CO2GAS/100, PDIF = 0.15, 
                           SHADE = 0, FLYHR = 0, UNCURL = 0.1, TC_INC = 0.1, PCTWET_INC = 0.1, 
                           PCTWET_MAX = 100, AK1_INC = 0.1, AK1_MAX = 2.8, PANT = 1, 
                           PANT_INC = 0.1, PANT_MULT = 1.05, AMASS = 65, ANDENS = 1000, 
                           SUBQFAT = 0, FATPCT = 20, SHAPE = 4, SHAPE_B = 1.1, SHAPE_B_MAX = 5, 
                           SHAPE_C = SHAPE_B, PVEN = 0.5, PCOND = 0, SAMODE = 0, ORIENT = 0, 
                           FURTHRMK = 0, DHAIRD = 3e-05, DHAIRV = 3e-05, LHAIRD = 0.0239, 
                           LHAIRV = 0.0239, ZFURD = 0.002, ZFURV = 0.002, RHOD = 3e+07, 
                           RHOV = 3e+07, REFLD = 0.2, REFLV = 0.2, ZFURCOMP = ZFURV, 
                           KHAIR = 0.209, XR = 1, EMISAN = 0.99, FABUSH = 0, FGDREF = 0.5, 
                           FSKREF = 0.5, TC = 37, TC_MAX = 39, AK1 = 0.9, AK2 = 0.23, 
                           PCTWET = 0.5, FURWET = 0, PCTBAREVAP = 0, PCTEYES = 0, DELTAR = 0, 
                           RELXIT = 100, QBASAL = (70 * AMASS^0.75) * (4.185/(24 * 
                                                                                3.6)), RQ = 0.8, EXTREF = 20, PANT_MAX = 5, Q10 = 2, 
                           TS = TC - 3, TFA = TA, DIFTOL = 0.001, THERMOREG = 1, RESPIRE = 1) 
{
  if (SHAPE_B <= 1 & SHAPE == 4) {
    SHAPE_B <- 1.01
    message("warning: SHAPE_B must be greater than 1 for ellipsoids, resetting to 1.01 \n")
  }
  if (SHAPE_B_MAX <= 1 & SHAPE == 4) {
    SHAPE_B_MAX <- 1.01
    message("warning: SHAPE_B_MAX must be greater than 1 for ellipsoids, resetting to 1.01 \n")
  }
  if (SHAPE_B_MAX < SHAPE_B) {
    message("warning: SHAPE_B_MAX must greater than than or equal to SHAPE_B, resetting to SHAPE_B \n")
    SHAPE_B_MAX <- SHAPE_B
  }
  if (PANT_INC == 0) {
    PANT_MAX <- PANT
  }
  if (PCTWET_INC == 0) {
    PCTWET_MAX <- PCTWET
  }
  if (TC_INC == 0) {
    TC_MAX <- TC
  }
  if (AK1_INC == 0) {
    AK1_MAX <- AK1
  }
  if (UNCURL == 0) {
    SHAPE_B_MAX <- SHAPE_B
  }
  TSKINMAX <- TC
  Q10mult <- 1
  PANT_COST <- 0
  QGEN <- 0
  QRESP <- 0
  TC_REF <- TC
  QBASREF <- QBASAL
  
  
  # NB edit green ringtails - allow PVEN to change with posture
  # Assume that min SHAPE_B = 1.1, max = 6, specify these so that if limit extent can uncurl fur property calcs are correct
  # save input as reference value, this should specify the max proportion of ventral fur
  PVEN_REF <- PVEN 
  PVEN = (SHAPE_B - 1.1)/(6 - 1.1) * PVEN_REF #initial value
  
  #Also use PCOND to set the max PCOND for the hour since no longer have a variable for this
  PCOND_MAX = PCOND
  PCOND = 0
  
  
  while (QGEN < QBASAL) {
    IRPROP.out <- IRPROP((0.7 * TS + 0.3 * TFA), DHAIRD, 
                         DHAIRV, LHAIRD, LHAIRV, ZFURD, ZFURV, RHOD, RHOV, 
                         REFLD, REFLV, ZFURCOMP, PVEN, KHAIR)
    KEFARA <- IRPROP.out[1:3]
    BETARA <- IRPROP.out[4:6]
    B1ARA <- IRPROP.out[7:9]
    DHAR <- IRPROP.out[10:12]
    LHAR <- IRPROP.out[13:15]
    RHOAR <- IRPROP.out[16:18]
    ZZFUR <- IRPROP.out[19:21]
    REFLFR <- IRPROP.out[22:24]
    FURTST <- IRPROP.out[25]
    KFURCMPRS <- IRPROP.out[26]
    DHARA <- DHAR[1]
    RHOARA <- RHOAR[1]
    ZFUR <- ZZFUR[1]
    GEOM.out <- GEOM_ENDO(AMASS, ANDENS, FATPCT, SHAPE, 
                          ZFUR, SUBQFAT, SHAPE_B, SHAPE_C, DHARA, RHOARA, 
                          PCOND, SAMODE, ORIENT, Z)
    VOL <- GEOM.out[1]
    D <- GEOM.out[2]
    MASFAT <- GEOM.out[3]
    VOLFAT <- GEOM.out[4]
    ALENTH <- GEOM.out[5]
    AWIDTH <- GEOM.out[6]
    AHEIT <- GEOM.out[7]
    ATOT <- GEOM.out[8]
    ASIL <- GEOM.out[9]
    ASILN <- GEOM.out[10]
    ASILP <- GEOM.out[11]
    GMASS <- GEOM.out[12]
    AREASKIN <- GEOM.out[13]
    FLSHVL <- GEOM.out[14]
    FATTHK <- GEOM.out[15]
    ASEMAJ <- GEOM.out[16]
    BSEMIN <- GEOM.out[17]
    CSEMIN <- GEOM.out[18]
    CONVSK <- GEOM.out[19]
    CONVAR <- GEOM.out[20]
    R1 <- GEOM.out[21]
    R2 <- GEOM.out[22]
    ZEN <- pi/180 * Z
    if (Z < 90) {
      CZ <- cos(ZEN)
      QNORM <- QSOLR/CZ
    }
    else {
      QNORM <- QSOLR
    }
    ABSAND <- 1 - REFLFR[2]
    ABSANV <- 1 - REFLFR[3]
    FAVEG <- FSKREF * (SHADE/100)
    FASKY <- FSKREF - FAVEG
    FAGRD <- FGDREF
    SOLAR.out <- SOLAR_ENDO(ATOT, ABSAND, ABSANV, ABSSB, 
                            ASIL, PDIF, QNORM, SHADE, QSOLR, FASKY, FAVEG)
    QSOLAR <- SOLAR.out[1]
    QSDIR <- SOLAR.out[2]
    QSSKY <- SOLAR.out[3]
    QSRSB <- SOLAR.out[4]
    QSDIFF <- SOLAR.out[5]
    QDORSL <- SOLAR.out[6]
    QVENTR <- SOLAR.out[7]
    SURFAR <- CONVAR
    TENV <- TA
    CONV.out <- CONV_ENDO(TS, TENV, SHAPE, SURFAR, FLTYPE, 
                          FURTST, D, TFA, VEL, ZFUR, BP, ELEV)
    QCONV <- CONV.out[1]
    HC <- CONV.out[2]
    HCFREE <- CONV.out[3]
    HCFOR <- CONV.out[4]
    HD <- CONV.out[5]
    HDFREE <- CONV.out[6]
    HDFORC <- CONV.out[7]
    ANU <- CONV.out[8]
    RE <- CONV.out[9]
    GR <- CONV.out[10]
    PR <- CONV.out[11]
    RA <- CONV.out[12]
    SC <- CONV.out[13]
    BP <- CONV.out[14]
    FABUSHREF <- FABUSH
    FASKYREF <- FASKY
    FAGRDREF <- FAGRD
    FAVEGREF <- FAVEG
    SIMULSOL.out <- matrix(data = 0, nrow = 2, ncol = 16)
    for (S in 1:2) {
      TVEG <- TAREF
      TLOWER <- TGRD
      if (QSOLAR > 0) {
        if (S == 1) {
          FASKY <- FASKYREF/(FASKYREF + FAVEGREF)
          FAVEG <- FAVEGREF/(FASKYREF + FAVEGREF)
          FAGRD <- 0
          FABUSH <- 0
          QSLR <- 2 * QSDIR + ((QSSKY/FASKYREF) * FASKY)
        }
        else {
          FASKY <- 0
          FAVEG <- 0
          FAGRD <- FAGRDREF/(FAGRDREF + FABUSHREF)
          FABUSH <- FABUSHREF/(FAGRDREF + FABUSHREF)
          QSLR <- (QVENTR/(1 - FASKYREF - FAVEGREF)) * 
            (1 - (2 * PCOND))
        }
      }
      else {
        QSLR <- 0
        if (S == 1) {
          FASKY <- FASKYREF/(FASKYREF + FAVEGREF)
          FAVEG <- FAVEGREF/(FASKYREF + FAVEGREF)
          FAGRD <- 0
          FABUSH <- 0
        }
        else {
          FASKY <- 0
          FAVEG <- 0
          FAGRD <- FAGRDREF/(FAGRDREF + FABUSHREF)
          FABUSH <- FABUSHREF/(FAGRDREF + FABUSHREF)
        }
      }
      if (QSOLR > 0 | ZZFUR[2] != ZZFUR[3]) {
        if (S == 1) {
          ZL <- ZZFUR[2]
          KEFF <- KEFARA[2]
        }
        else {
          ZL <- ZZFUR[3]
          KEFF <- KEFARA[3]
        }
      }
      else {
        ZL <- ZZFUR[1]
        KEFF <- KEFARA[1]
      }
      RSKIN <- R1
      RFLESH <- R1 - FATTHK
      RFUR <- R1 + ZL
      D <- 2 * RFUR
      RRAD <- RSKIN + (XR * ZL)
      LEN <- ALENTH
      if (SHAPE != 4) {
        RFURCMP <- RSKIN + ZFURCOMP
      }
      else {
        RFURCMP <- RFUR
      }
      if (SHAPE == 4) {
        BLCMP <- BSEMIN + ZFURCOMP
      }
      else {
        BLCMP <- RFUR
      }
      if (SUBQFAT == 1 & FATTHK > 0) {
        VOL <- FLSHVL
      }
      if (S == 2) {
        AREACND <- ATOT * PCOND * 2
        CD <- AREACND * KSUB/0.025
      }
      else {
        AREACND <- 0
        CD <- 0
      }
      FURVARS <- c(LEN, ZFUR, FURTHRMK, KEFF, BETARA, 
                   FURTST, ZL, LHAR[S + 1], DHAR[S + 1], RHOAR[S + 
                                                                 1], REFLFR[S + 1], KHAIR, S)
      GEOMVARS <- c(SHAPE, SUBQFAT, CONVAR, VOL, D, CONVAR, 
                    CONVSK, RFUR, RFLESH, RSKIN, XR, RRAD, ASEMAJ, 
                    BSEMIN, CSEMIN, CD, PCOND, RFURCMP, BLCMP, KFURCMPRS)
      ENVVARS <- c(FLTYPE, TA, TS, TBUSH, TVEG, TLOWER, 
                   TSKY, TCONDSB, RH, VEL, BP, ELEV, FASKY, FABUSH, 
                   FAVEG, FAGRD, QSLR)
      TRAITS <- c(TC, AK1, AK2, EMISAN, FATTHK, FLYHR, 
                  FURWET, PCTBAREVAP, PCTEYES)
      if (SHAPE %in% c(1, 3)) {
        IPT <- 1
      }
      if (SHAPE == 2) {
        IPT <- 2
      }
      if (SHAPE == 4) {
        IPT <- 3
      }
      SIMULSOL.out[S, ] <- SIMULSOL(DIFTOL, IPT, FURVARS, 
                                    GEOMVARS, ENVVARS, TRAITS, TFA, PCTWET, TS)
    }
    TSKINMAX <- max(SIMULSOL.out[1, 2], SIMULSOL.out[2, 
                                                     2])
    GEND <- SIMULSOL.out[1, 5]
    GENV <- SIMULSOL.out[2, 5]
    DMULT <- FASKYREF + FAVEGREF
    VMULT <- 1 - DMULT
    X <- GEND * DMULT + GENV * VMULT
    QSUM <- X
    FABUSH <- FABUSHREF
    FASKY <- FASKYREF
    FAGRD <- FAGRDREF
    FAVEG <- FAVEGREF
    TS <- (SIMULSOL.out[1, 2] + SIMULSOL.out[2, 2]) * 0.5
    TFA <- (SIMULSOL.out[1, 1] + SIMULSOL.out[2, 1]) * 0.5
    TLUNG <- (TC + TS) * 0.5
    TAEXIT <- min(TA + DELTAR, TLUNG)
    if (RESPIRE == 1) {
      QMIN <- QBASAL
      if (TA < TC & TSKINMAX < TC) {
        QM1 <- QBASAL * 2 * -1
        QM2 <- QBASAL * 50
      }
      else {
        QM1 <- QBASAL * 50 * -1
        QM2 <- QBASAL * 2
      }
      TOL <- AMASS * 0.01
      ZBRENT.in <- c(TA, O2GAS, N2GAS, CO2GAS, BP, QMIN, 
                     RQ, TLUNG, GMASS, EXTREF, RH, RELXIT, 1, TAEXIT, 
                     QSUM, PANT, R_PCO2)
      ZBRENT.out <- ZBRENT_ENDO(QM1, QM2, TOL, ZBRENT.in)
      colnames(ZBRENT.out) <- c("RESPFN", "QRESP", "GEVAP", 
                                "PCTO2", "PCTN2", "PCTCO2", "RESPGEN", "O2STP", 
                                "O2MOL1", "N2MOL1", "AIRML1", "O2MOL2", "N2MOL2", 
                                "AIRML2", "AIRVOL")
      QGEN <- ZBRENT.out[7]
    }
    else {
      QGEN <- QSUM
      ZBRENT.out <- matrix(data = 0, nrow = 1, ncol = 15)
      colnames(ZBRENT.out) <- c("RESPFN", "QRESP", "GEVAP", 
                                "PCTO2", "PCTN2", "PCTCO2", "RESPGEN", "O2STP", 
                                "O2MOL1", "N2MOL1", "AIRML1", "O2MOL2", "N2MOL2", 
                                "AIRML2", "AIRVOL")
    }
    SHAPE_B_LAST <- SHAPE_B
    AK1_LAST <- AK1
    TC_LAST <- TC
    PANT_LAST <- PANT
    PCTWET_LAST <- PCTWET
    
    ##THERMOREG SEQUENCE####
    
    TSKINV <- SIMULSOL.out[2,2] #NB edit - Add this so can check for whether should use conduction
    
    if(THERMOREG != 0){
      
      #Thermoreg sequence for green possums
      # seek shade (select best from separate sims)
      # start to increase TC
      # increase flesh cond
      # uncurl
      # pant
      # may lick (?)
      
      if(TC < (TC_MAX - 4)){ # start to raise core temp
        TC <- TC + TC_INC
        Q10mult <- Q10 ^ ((TC - TC_REF) / 10)
        QBASAL <- QBASREF * Q10mult 
      }else{ # raise core temp and increase flesh conductivity
        if((AK1 < AK1_MAX)| (TC < (TC_MAX - 3))){
          AK1 <- AK1 + AK1_INC
          AK1[AK1 > AK1_MAX] <- AK1_MAX #Add check so don't overshoot
          TC <- TC + TC_INC
          TC[TC > (TC_MAX-3)] <- (TC_MAX-3) #Add check so don't overshoot
          Q10mult <- Q10 ^ ((TC - TC_REF) / 10)
          QBASAL <- QBASREF * Q10mult + PANT_COST
        }else{
          if((SHAPE_B < SHAPE_B_MAX) & (UNCURL > 0)){ #Uncurl - also run set of simulations with uncurl = 0 as not always beneficial
            SHAPE_B <- SHAPE_B + UNCURL
            #NB edit -Also need to adjust PVEN - the proportion of ventral fur exposed
            PVEN <- (SHAPE_B - 1.1)/(6 - 1.1) * PVEN_REF
          }else{
            SHAPE_B <- SHAPE_B_MAX
            PVEN <- (SHAPE_B - 1.1)/(6 - 1.1) * PVEN_REF
            if((PANT < PANT_MAX)| (TC < TC_MAX)){
              # increase panting
              PANT <- PANT + PANT_INC
              if(PANT > PANT_MAX){
                PANT <- PANT_MAX
              }
              PANT_COST <- ((PANT - 1) / (PANT_MAX - 1) * PANT_MULT * QBASREF)
              # increase core temp
              TC <- TC + TC_INC # TC = 37.28+0.2 = 37.48
              if(TC > TC_MAX){
                TC <- TC_MAX
              }
              # Adjust QBASAL for panting and TC
              Q10mult <- Q10 ^ ((TC - TC_REF) / 10)
              QBASAL <- QBASREF * Q10mult + PANT_COST
              
            }else{
              if(PCTWET < PCTWET_MAX){
                PCTWET <- PCTWET + PCTWET_INC
                if(PCTWET >= PCTWET_MAX){
                  PCTWET = PCTWET_MAX
                }
              }
              break
            }
          }
        }
      }
    }
  }
  
    ##OLD CODE####################
  #   if (THERMOREG != 0) {
  #     if (SHAPE_B < SHAPE_B_MAX) {
  #       SHAPE_B <- SHAPE_B + UNCURL
  #     }
  #     else {
  #       SHAPE_B <- SHAPE_B_MAX
  #       if (AK1 < AK1_MAX) {
  #         AK1 <- AK1 + AK1_INC
  #       }
  #       else {
  #         AK1 <- AK1_MAX
  #         if (TC < TC_MAX) {
  #           TC <- TC + TC_INC
  #           Q10mult <- Q10^((TC - TC_REF)/10)
  #           QBASAL <- QBASREF * Q10mult
  #         }
  #         else {
  #           TC <- TC_MAX
  #           Q10mult <- Q10^((TC - TC_REF)/10)
  #           if (PANT < PANT_MAX) {
  #             PANT <- PANT + PANT_INC
  #             PANT_COST <- ((PANT - 1)/(PANT_MAX - 1) * 
  #                             (PANT_MULT - 1) * QBASREF)
  #             QBASAL <- QBASREF * Q10mult + PANT_COST
  #           }
  #           else {
  #             PANT <- PANT_MAX
  #             PANT_COST <- ((PANT - 1)/(PANT_MAX - 1) * 
  #                             (PANT_MULT - 1) * QBASREF)
  #             QBASAL <- QBASREF * Q10mult + PANT_COST
  #             PCTWET <- PCTWET + PCTWET_INC
  #             if (PCTWET > PCTWET_MAX | PCTWET_INC == 
  #                 0) {
  #               PCTWET <- PCTWET_MAX
  #               break
  #             }
  #           }
  #         }
  #       }
  #     }
  #   }
  #   else {
  #     break
  #   }
  # }
  
  TFA.D <- SIMULSOL.out[1, 1]
  TSKCALCAV.D <- SIMULSOL.out[1, 2]
  QCONV.D <- SIMULSOL.out[1, 3]
  QCOND.D <- SIMULSOL.out[1, 4]
  QGENNET.D <- SIMULSOL.out[1, 5]
  QSEVAP.D <- SIMULSOL.out[1, 6]
  QRAD.D <- SIMULSOL.out[1, 7]
  QSLR.D <- SIMULSOL.out[1, 8]
  QRSKY.D <- SIMULSOL.out[1, 9]
  QRBSH.D <- SIMULSOL.out[1, 10]
  QRVEG.D <- SIMULSOL.out[1, 11]
  QRGRD.D <- SIMULSOL.out[1, 12]
  QFSEVAP.D <- SIMULSOL.out[1, 13]
  NTRY.D <- SIMULSOL.out[1, 14]
  SUCCESS.D <- SIMULSOL.out[1, 15]
  TFA.V <- SIMULSOL.out[2, 1]
  TSKCALCAV.V <- SIMULSOL.out[2, 2]
  QCONV.V <- SIMULSOL.out[2, 3]
  QCOND.V <- SIMULSOL.out[2, 4]
  QGENNET.V <- SIMULSOL.out[2, 5]
  QSEVAP.V <- SIMULSOL.out[2, 6]
  QRAD.V <- SIMULSOL.out[2, 7]
  QSLR.V <- SIMULSOL.out[2, 8]
  QRSKY.V <- SIMULSOL.out[2, 9]
  QRBSH.V <- SIMULSOL.out[2, 10]
  QRVEG.V <- SIMULSOL.out[2, 11]
  QRGRD.V <- SIMULSOL.out[2, 12]
  QFSEVAP.V <- SIMULSOL.out[2, 13]
  NTRY.V <- SIMULSOL.out[2, 14]
  SUCCESS.V <- SIMULSOL.out[2, 15]
  RESPFN <- ZBRENT.out[1]
  QRESP <- ZBRENT.out[2]
  GEVAP <- ZBRENT.out[3]
  PCTO2 <- ZBRENT.out[4]
  PCTN2 <- ZBRENT.out[5]
  PCTCO2 <- ZBRENT.out[6]
  RESPGEN <- ZBRENT.out[7]
  O2STP <- ZBRENT.out[8]
  O2MOL1 <- ZBRENT.out[9]
  N2MOL1 <- ZBRENT.out[10]
  AIRML1 <- ZBRENT.out[11]
  O2MOL2 <- ZBRENT.out[12]
  N2MOL2 <- ZBRENT.out[13]
  AIRML2 <- ZBRENT.out[14]
  AIRVOL <- ZBRENT.out[15]
  HTOVPR <- 2501200 - 2378.7 * TA
  SWEAT.G.S <- (QSEVAP.D + QSEVAP.V) * 0.5/HTOVPR * 1000
  EVAP.G.S <- GEVAP + SWEAT.G.S
  sigma <- 5.6697e-08
  QIROUT.D <- sigma * EMISAN * AREASKIN * (TSKCALCAV.D + 273.15)^4
  QIRIN.D <- QRAD.D * -1 + QIROUT.D
  QIROUT.V <- sigma * EMISAN * AREASKIN * (TSKCALCAV.D + 273.15)^4
  QIRIN.V <- QRAD.V * -1 + QIROUT.V
  QSOL <- QSLR.D * DMULT + QSLR.V * VMULT
  QIRIN <- QIRIN.D * DMULT + QIRIN.V * VMULT
  if (RESPIRE == 1) {
    QGEN <- RESPGEN
  }
  else {
    QGEN <- QSUM
  }
  QEVAP <- QSEVAP.D * DMULT + QSEVAP.V * VMULT + QFSEVAP.D * 
    DMULT + QFSEVAP.V * VMULT + QRESP
  QIROUT <- QIROUT.D * DMULT + QIROUT.V * VMULT
  QCONV <- QCONV.D * DMULT + QCONV.V * VMULT
  QCOND <- QCOND.D * DMULT + QCOND.V * VMULT
  treg1 <- c(TC_LAST, TLUNG, TSKCALCAV.D, TSKCALCAV.V, TFA.D, 
             TFA.V, SHAPE_B_LAST, PANT_LAST, PCTWET_LAST, AK1_LAST, 
             KEFARA[1], KEFARA[2], KEFARA[3], KFURCMPRS, Q10mult)
  morph1 <- c(ATOT, VOL, D, MASFAT, FATTHK, FLSHVL, ALENTH, 
              AWIDTH, AHEIT, R1, R2, ASIL, ASILN, ASILP, AREASKIN, 
              CONVSK, CONVAR, AREACND/2, FASKY, FAGRD)
  enbal1 <- c(QSOL, QIRIN, QGEN, QEVAP, QIROUT, QCONV, QCOND, 
              RESPFN, max(NTRY.D, NTRY.V), min(SUCCESS.D, SUCCESS.V))
  masbal1 <- c(AIRVOL, O2STP, GEVAP, SWEAT.G.S, O2MOL1, O2MOL2, 
               N2MOL1, N2MOL2, AIRML1, AIRML2) * 3600
  treg <- matrix(data = treg1, nrow = 1, ncol = 15)
  morph <- matrix(data = morph1, nrow = 1, ncol = 20)
  masbal <- matrix(data = masbal1, nrow = 1, ncol = 10)
  enbal <- matrix(data = enbal1, nrow = 1, ncol = 10)
  treg.names <- c("TC", "TLUNG", "TSKIN_D", "TSKIN_V", "TFA_D", 
                  "TFA_V", "SHAPE_B", "PANT", "PCTWET", "K_FLESH", "K_FUR", 
                  "K_FUR_D", "K_FUR_V", "K_COMPFUR", "Q10")
  morph.names <- c("AREA", "VOLUME", "CHAR_DIM", "MASS_FAT", 
                   "FAT_THICK", "FLESH_VOL", "LENGTH", "WIDTH", "HEIGHT", 
                   "DIAM_FLESH", "DIAM_FUR", "AREA_SIL", "AREA_SILN", "AREA_ASILP", 
                   "AREA_SKIN", "AREA_SKIN_EVAP", "AREA_CONV", "AREA_COND", 
                   "F_SKY", "F_GROUND")
  enbal.names <- c("QSOL", "QIRIN", "QGEN", "QEVAP", "QIROUT", 
                   "QCONV", "QCOND", "ENB", "NTRY", "SUCCESS")
  masbal.names <- c("AIR_L", "O2_L", "H2OResp_g", "H2OCut_g", 
                    "O2_mol_in", "O2_mol_out", "N2_mol_in", "N2_mol_out", 
                    "AIR_mol_in", "AIR_mol_out")
  colnames(treg) <- treg.names
  colnames(morph) <- morph.names
  colnames(enbal) <- enbal.names
  colnames(masbal) <- masbal.names
  if (max(treg) == 0) {
    warning("A solution could not be found and panting/'sweating' options are exhausted; try allowing greater evaporation or allowing higher body maximum body temperature")
  }
  endo.out <- list(treg = treg, morph = morph, enbal = enbal, 
                   masbal = masbal)
  return(endo.out)
}
