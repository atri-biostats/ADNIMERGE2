#' @title Generate ADNI modified versions of the Preclinical Alzheimer's Cognitive Composite (PACC)
#'
#' @description The Preclinical Alzheimer's Cognitive Composite (\href{https://doi.org/10.1001/jamaneurol.2014.803}{PACC}) 
#' is a baseline standardized composite of
#'
#' @details 
#' \itemize{
#'   \item Free and Cued Selective Reminding Test (FCSRT)
#'   \item Logical Memory IIa Delayed Recall (LM)
#'   \item Digit Symbol Substitituion Test (DSST)
#'   \item Mini-Mental State Examination (MMSE)
#' }
#'
#' This function generates two modified versions of PACC scores based on ADNI data. FCSRT is not used in ADNI, so we use the
#' Delayed Recall portion of the Alzheimer's Disease Assessment Scale (ADAS) as a proxy. Score \code{mPACCdigit} uses the
#' DSST when available (ADNI1). \code{mPACCtrailsB} uses (log transformed) Trails B as a proxy for DSST. Raw component scores 
#' standardized according to the mean and standard deviation of baseline scores of ADNI subjects with normal cognition 
#' to create \code{Z} scores for each component \code{(Z=(raw - mean(raw.bl))/sd(raw.bl))}. 
#' The Z scores are reoriented if necessary so that greater scores reflect better performance. 
#' The composite is the sum of these Z scores. 
#'
#' \strong{Missing components:} At least two components must be present to produce a score. If more than two components are missing, 
#' the PACC will be \code{NA}.
#'
#' @param dd data frame to score with variables ADASQ4, LDELTOTAL, DIGITSCOR, MMSE, TRABSCOR, DX.bl, and VISCODE
#' @param keepComponents should components and z scores be returned? Defaults to FALSE so that just the composite scores are returned.
#'
#' @return The \code{data.frame} with appended columns for \code{mPACCdigit} and \code{mPACCtrailsB}
#' @references 
#' \itemize{
#'   \item Donohue MC, et al. The Preclinical Alzheimer Cognitive Composite: Measuring Amyloid-Related Decline. \emph{JAMA Neurol}. 2014;71(8):961–970. doi:10.1001/jamaneurol.2014.803 \url{http://dx.doi.org/10.1001/jamaneurol.2014.803}
#'   \item Donohue MC, Sperling RA, Petersen R, Sun C, Weiner MW, Aisen PS, for the Alzheimer’s Disease Neuroimaging Initiative. Association Between Elevated Brain Amyloid and Subsequent Cognitive Decline Among Cognitively Normal Persons. \emph{JAMA}. 2017;317(22):2305–2316. \url{http://dx.doi.org/10.1001/jama.2017.6669}
#' }
#' @author Michael Donohue \email{mdonohue@@usc.edu}
#' @seealso \code{\link{adnimerger}}, \code{\link{adnimerge}}
#' @examples
#' library(nlme)
#' library(dplyr)
#' library(multcomp)
#' library(Hmisc)
#' 
#' csf2numeric <- function(x){
#'   as.numeric(gsub("<", "", gsub(">", "", x)))
#' }
#' 
#' dd <- subset(adnimerge, DX.bl %in% c('CN', 'SMC') & !is.na(mPACCtrailsB)) %>%
#'   mutate(
#'     ABETA = csf2numeric(ABETA)
#'   )
#'
#' # identify those with elevated PIB PET at ANY visit OR 
#' # elevated BASELINE AV45 PET OR elevated BASELINE FBB PET OR
#' # low BASELINE CSF Abeta
#' # AV45 ~ PIB regression from Landau et al 2012:
#' elevatedAmyloid <- unique(c(
#'   subset(dd, 0.67*PIB + 0.15 > 1.11)$RID,
#'   subset(dd, VISCODE == 'bl' & AV45 > 1.11)$RID,
#'   subset(dd, VISCODE == 'bl' & FBB > 1.08)$RID,
#'   subset(dd, VISCODE == 'bl' & ABETA < 900)$RID
#' ))
#' anyAmyloid <- unique(subset(dd, !is.na(AV45.bl) | !is.na(PIB) | 
#'  | !is.na(FBB.bl) | !is.na(ABETA.bl))$RID)
#' dd <- mutate(dd,
#'   ElevatedAmyloid =
#'     ifelse(RID %in% elevatedAmyloid, 1,
#'     ifelse(RID %in% anyAmyloid, 0, NA)),
#'   m = Month.bl, m2 = Month.bl^2,
#'   APOEe4 = APOE4>0)
#'
#' summary(ElevatedAmyloid ~ APOEe4 + AGE + PTEDUCAT, data = dd,
#'   subset=VISCODE=='bl', method='reverse', overall=TRUE)
#'
#' # Quadratic time model:
#' fit <- lme(mPACCtrailsB ~ mPACCtrailsB.bl + APOEe4 + AGE + PTEDUCAT + m + m2 + (m + m2):ElevatedAmyloid,
#'   random = ~m|RID, data = dd, na.action=na.omit)
#'
#' Months <- seq(12,96,12)
#' elevated.design <- model.matrix(
#'   mPACCtrailsB ~ mPACCtrailsB.bl + APOEe4 + AGE + PTEDUCAT + m + m2 + (m + m2):ElevatedAmyloid,
#'   data = data.frame(mPACCtrailsB = 0, mPACCtrailsB.bl=0, APOEe4=TRUE, AGE=75, PTEDUCAT=12, ElevatedAmyloid=1, m=Months, m2=Months^2)
#' )
#'
#' normal.design <- model.matrix(
#'   mPACCtrailsB ~ mPACCtrailsB.bl + APOEe4 + AGE + PTEDUCAT + m + m2 + (m + m2):ElevatedAmyloid,
#'   data = data.frame(mPACCtrailsB = 0, mPACCtrailsB.bl=0, APOEe4=TRUE, AGE=75, PTEDUCAT=12, ElevatedAmyloid=0, m=Months, m2=Months^2)
#' )
#'
#' contrast.data <- elevated.design - normal.design
#' summary(multcomp::glht(fit, linfct = contrast.data))
#' @export
pacc <- function(dd, keepComponents = FALSE)
{
  require(dplyr)
  require(tidyr)
  holdNames <- colnames(dd)
  dd <- mutate(dd, log.TRABSCOR = log(TRABSCOR+1))
  
  bl.summary <- filter(dd, VISCODE == 'bl') %>% select(DX.bl, ADASQ4, LDELTOTAL, DIGITSCOR, log.TRABSCOR, MMSE) %>%
    gather(VARIABLE, SCORE, -DX.bl) %>%
    filter(!is.na(SCORE)) %>%
    group_by(DX.bl, VARIABLE) %>%
    dplyr::summarize(
      N=n(),
      mean = mean(SCORE),
      sd = sd(SCORE))
  
  zscore <- function(x, var){ 
    (x - filter(bl.summary, DX.bl=='CN' & VARIABLE==var)$mean)/
      filter(bl.summary, DX.bl=='CN' & VARIABLE==var)$sd
  }
  
  dd <- mutate(dd, 
    ADASQ4.z       = -zscore(ADASQ4, 'ADASQ4'),
    LDELTOTAL.z    =  zscore(LDELTOTAL, 'LDELTOTAL'),
    DIGITSCOR.z    =  zscore(DIGITSCOR, 'DIGITSCOR'),
    log.TRABSCOR.z = -zscore(log.TRABSCOR, 'log.TRABSCOR'),
    MMSE.z         =  zscore(MMSE, 'MMSE'))
  
  # check that all measure are positively correlated:
  corTest <- cor(select(dd, ADASQ4.z, LDELTOTAL.z, DIGITSCOR.z, log.TRABSCOR.z, MMSE.z), use='pairwise.complete.obs')
  if(any(corTest<0)) stop('Some PACC z scores are negatively correlated!')
  # GGally::ggpairs(select(dd, ADASQ4.z, LDELTOTAL.z, DIGITSCOR.z, log.TRABSCOR.z, MMSE.z))
  
  compscore <- function(x, n.components=4, n.missing=2){
    ifelse(sum(is.na(x))>n.missing, NA, mean(x, na.rm=TRUE))*n.components
  }
  
  dd$mPACCdigit <- apply(dd[, c('ADASQ4.z', 'LDELTOTAL.z', 'DIGITSCOR.z', 'MMSE.z')], 1, compscore)
  dd$mPACCtrailsB <- apply(dd[, c('ADASQ4.z', 'LDELTOTAL.z', 'log.TRABSCOR.z', 'MMSE.z')], 1, compscore)
  if(!keepComponents) dd <- dd[, c(holdNames, 'mPACCdigit', 'mPACCtrailsB')]  
  as.data.frame(dd)
}
