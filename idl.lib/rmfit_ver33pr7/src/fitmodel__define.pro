; ----------------------------------------------------------------------------
;+
; NAME:
;
;     FitModel
;
; PURPOSE:
;
;     This class encapsulates results from a photon model fit.
;
; INPUTS:
;
;     NONE
;
; KEYWORDS:
;
;     NONE
;
; INHERITS:
;     NONE
;
; RESTRICTIONS:
;
; DEPENDENCIES:
;
; METHODS:
;    
; MODIFICATION HISTORY:
;
;     Written, 2000 March, Robert.Mallozzi@msfc.nasa.gov
;
;-
; ----------------------------------------------------------------------------


; ----------------------------------------------------------------------------
; Constructor
; ----------------------------------------------------------------------------
FUNCTION FitModel::init

    self.data.haveModel = 0L
          
    RETURN, 1

END


; ----------------------------------------------------------------------------
; Destructor
; ----------------------------------------------------------------------------
PRO FitModel::cleanup

    STRUCT_FREE, self.data
        
END    


; ----------------------------------------------------------------------------
; Return the current model status and selected variables
; ----------------------------------------------------------------------------
FUNCTION FitModel::haveModel & RETURN, self.data.haveModel & END
FUNCTION FitModel::haveFit   & RETURN, self.data.haveFit   & END


; ----------------------------------------------------------------------------
; The individual accessor functions
; ----------------------------------------------------------------------------
FUNCTION FitModel::get_model_chisq            & RETURN, self.data.chisq                & END
FUNCTION FitModel::get_dof                    & RETURN, self.data.dof                  & END
FUNCTION FitModel::get_fit_err                & RETURN, self.data.fitErr               & END
FUNCTION FitModel::get_use_det                & RETURN, *self.data.useDet              & END 
FUNCTION FitModel::get_term_used              & RETURN, *self.data.term_used           & END
FUNCTION FitModel::get_fitChan                & RETURN, *self.data.fitChannels         & END
FUNCTION FitModel::get_numChan                & RETURN, *self.data.numChan             & END
FUNCTION FitModel::get_chanEnergy             & RETURN, *self.data.chanEnergy          & END
FUNCTION FitModel::get_chanWidth              & RETURN, *self.data.chanWidth           & END
FUNCTION FitModel::get_obsCrate               & RETURN, *self.data.obsCrate            & END
FUNCTION FitModel::get_backCrate              & RETURN, *self.data.backCrate           & END
FUNCTION FitModel::get_backCsig               & RETURN, *self.data.backCsig            & END
FUNCTION FitModel::get_liveTime               & RETURN, *self.data.liveTime            & END
FUNCTION FitModel::get_model_cnt_rate         & RETURN, *self.data.modelCntRate        & END
FUNCTION FitModel::get_phot_obs_rate          & RETURN, *self.data.photObsRate         & END
FUNCTION FitModel::get_phot_obs_sig           & RETURN, *self.data.photObsSig          & END
FUNCTION FitModel::get_nu_f_nu_data           & RETURN, *self.data.nuFnuData           & END
FUNCTION FitModel::get_nu_f_nu_sig            & RETURN, *self.data.nuFnuSig            & END
FUNCTION FitModel::get_nu_f_nu_model          & RETURN, *self.data.nuFnuModel          & END
FUNCTION FitModel::get_model_energy           & RETURN, *self.data.modelEnergy         & END
FUNCTION FitModel::get_model_phot_rate        & RETURN, *self.data.modelPhotRate       & END
FUNCTION FitModel::get_model_phot_rate_bychan & RETURN, *self.data.modelPhotRateBychan & END
FUNCTION FitModel::get_model_cr_vari          & RETURN, *self.data.modelCrVari         & END
FUNCTION FitModel::get_data_cr_vari           & RETURN, *self.data.dataCrVari          & END
FUNCTION FitModel::get_names                  & RETURN, *self.data.names               & END
FUNCTION FitModel::get_param                  & RETURN, *self.data.param               & END
FUNCTION FitModel::get_param_uncer            & RETURN, *self.data.param_uncer         & END


; ----------------------------------------------------------------------------
; Set the availablity of the current model
; ----------------------------------------------------------------------------
PRO FitModel::setHaveFit, tf

    self.data.haveFit = tf

END


; ----------------------------------------------------------------------------
; Set the current model
; ----------------------------------------------------------------------------
PRO FitModel::setModel, $

    numDet,        have_model,   have_fit,        model_chisq,            $
    dof,           fit_err,      use_det,         term_used,   fitChan,   $
    numChan,       chanEnergy,   chanWidth,       obsCrate,               $
    backCrate,     backCsig,     liveTime,        model_cnt_rate,         $
    phot_obs_rate, phot_obs_sig, nu_f_nu_data,    nu_f_nu_sig,            $
    nu_f_nu_model, model_energy, model_phot_rate, model_phot_rate_bychan, $
    model_cr_vari, data_cr_vari, names,           param,                  $
    param_uncer


    STRUCT_FREE, self.data
    self.data.haveModel = 0L
        
    self.data.numDet              = numDet
    self.data.haveModel           = have_model
    self.data.haveFit             = have_fit
    self.data.chisq               = model_chisq
    self.data.dof                 = dof
    self.data.fitErr              = fit_err

    self.data.useDet              = PTR_NEW (use_det)
    self.data.term_used           = PTR_NEW (term_used)
    self.data.fitChannels         = PTR_NEW (fitChan)
    self.data.numChan             = PTR_NEW (numChan)
    self.data.chanEnergy          = PTR_NEW (chanEnergy)
    self.data.chanWidth           = PTR_NEW (chanWidth)
    self.data.obsCrate            = PTR_NEW (obsCrate)
    self.data.backCrate           = PTR_NEW (backCrate)
    self.data.backCsig            = PTR_NEW (backCsig)
    self.data.liveTime            = PTR_NEW (liveTime)
    self.data.modelCntRate        = PTR_NEW (model_cnt_rate)
    self.data.photObsRate         = PTR_NEW (phot_obs_rate)
    self.data.photObsSig          = PTR_NEW (phot_obs_sig)
    self.data.nuFnuData           = PTR_NEW (nu_f_nu_data)
    self.data.nuFnuSig            = PTR_NEW (nu_f_nu_sig)
    self.data.nuFnuModel          = PTR_NEW (nu_f_nu_model)
    self.data.modelEnergy         = PTR_NEW (model_energy)
    self.data.modelPhotRate       = PTR_NEW (model_phot_rate)
    self.data.modelPhotRateBychan = PTR_NEW (model_phot_rate_bychan)
    self.data.modelCrVari         = PTR_NEW (model_cr_vari)
    self.data.dataCrVari          = PTR_NEW (data_cr_vari)
    self.data.names               = PTR_NEW (names)
    self.data.param               = PTR_NEW (param)
    self.data.param_uncer         = PTR_NEW (param_uncer)

END


; ----------------------------------------------------------------------------
; Return the current model
; ----------------------------------------------------------------------------
FUNCTION FitModel::model, ERROR = error

    error = 0

    IF (self.data.haveModel) THEN BEGIN
            
       model = { $

           haveModel           : self.data.haveModel,            $
           haveFit             : self.data.haveFit,              $
           chisq               : self.data.chisq,                $
           dof                 : self.data.dof,                  $
           fitErr              : self.data.fitErr,               $
           numDet              : self.data.numDet,               $
           useDet              : *self.data.useDet,              $
           term_used           : *self.data.term_used,           $
           fitChannels         : *self.data.fitChannels,         $
           numChan             : *self.data.numChan,             $
           chanEnergy          : *self.data.chanEnergy,          $
           chanWidth           : *self.data.chanWidth,           $
           obsCrate            : *self.data.obsCrate,            $
           backCrate           : *self.data.backCrate,           $
           backCsig            : *self.data.backCsig,            $
           liveTime            : *self.data.liveTime,            $
           modelCntRate        : *self.data.modelCntRate,        $
           photObsRate         : *self.data.photObsRate,         $
           photObsSig          : *self.data.photObsSig,          $
           nuFnuData           : *self.data.nuFnuData,           $
           nuFnuSig            : *self.data.nuFnuSig,            $
           nuFnuModel          : *self.data.nuFnuModel,          $
           modelEnergy         : *self.data.modelEnergy,         $
           modelPhotRate       : *self.data.modelPhotRate,       $
           modelPhotRateBychan : *self.data.modelPhotRateBychan, $
           modelCrVari         : *self.data.modelCrVari,         $
           dataCrVari          : *self.data.dataCrVari,          $
           names               : *self.data.names,               $
           param               : *self.data.param,               $
           param_uncer         : *self.data.param_uncer          $

       }
    
    ENDIF ELSE BEGIN
    
       error = 1
       model = 0
    
    ENDELSE
    
    RETURN, model   

END


; ----------------------------------------------------------------------------
; Object definition
; ----------------------------------------------------------------------------
PRO FitModel__define

    tmpl = { FIT_MODEL, $

        haveModel           : 0L,         $
        haveFit             : 0L,         $
        chisq               : 0.0,        $
        dof                 : 0L,         $
        fitErr              : 0L,         $
        numDet              : 0L,         $
        useDet              : PTR_NEW (), $
        term_used           : PTR_NEW (), $
        fitChannels         : PTR_NEW (), $
        numChan             : PTR_NEW (), $
        chanEnergy          : PTR_NEW (), $
        chanWidth           : PTR_NEW (), $
        obsCrate            : PTR_NEW (), $
        backCrate           : PTR_NEW (), $
        backCsig            : PTR_NEW (), $
        liveTime            : PTR_NEW (), $
        modelCntRate        : PTR_NEW (), $
        photObsRate         : PTR_NEW (), $
        photObsSig          : PTR_NEW (), $
        nuFnuData           : PTR_NEW (), $
        nuFnuSig            : PTR_NEW (), $
        nuFnuModel          : PTR_NEW (), $
        modelEnergy         : PTR_NEW (), $
        modelPhotRate       : PTR_NEW (), $
        modelPhotRateBychan : PTR_NEW (), $
        modelCrVari         : PTR_NEW (), $
        dataCrVari          : PTR_NEW (), $
        names               : PTR_NEW (), $
        param               : PTR_NEW (), $
        param_uncer         : PTR_NEW ()  $
       
    }


    obj = { FITMODEL, $
        
        data : { FIT_MODEL } $

    }


END
