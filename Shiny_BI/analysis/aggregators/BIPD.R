val_funcs(list(
  LOSS=quote(sum(LOSS_CPD_DEV_TRND_INCR)),
  ECY=quote(sum(BIPD_ECY)),
  CLM=quote(sum(CLM_CNT_INCR)),
  FREQ=quote(sum(CLM_CNT_INCR)/sum(BIPD_ECY)),
  SEV=quote(sum(LOSS_CPD_DEV_TRND_INCR)/sum(CLM_CNT_INCR)),
  PP=quote(sum(LOSS_CPD_DEV_TRND_INCR)/sum(BIPD_ECY))
))

val_fmts(list(
  LOSS=function(dt){hot_col(dt, col='LOSS', format='$2a')},
  ECY=function(dt){hot_col(dt, col='ECY', format='2a')},
  CLM=function(dt){hot_col(dt, col='CLM', format='2a')},
  FREQ=function(dt){hot_col(dt, col='FREQ', format='0.00%')},
  SEV=function(dt){hot_col(dt, col='SEV', format='$2a')},
  PP=function(dt){hot_col(dt, col='PP', format='$2a')}
))

val_vars(c(
  'LOSS_CPD_DEV_TRND_INCR',
  'BIPD_ECY',
  'CLM_CNT_INCR'
))
