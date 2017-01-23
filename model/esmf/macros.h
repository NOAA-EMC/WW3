/* -------------------------------------------------------------------------- */
/* Macros for ESMF logging                                                    */
/* -------------------------------------------------------------------------- */
#ifndef FILENAME
#define FILENAME __FILE__
#endif
#define CONTEXT  line=__LINE__,file=FILENAME
#define PASSTHRU msg=ESMF_LOGERR_PASSTHRU,CONTEXT


/* -------------------------------------------------------------------------- */
/* Define real kind for data passed through ESMF interface                    */
/* -------------------------------------------------------------------------- */
#if defined(ESMF_R8)
#define _ESMF_KIND_RX _ESMF_KIND_R8
#define ESMF_KIND_RX ESMF_KIND_R8
#define ESMF_TYPEKIND_RX ESMF_TYPEKIND_R8
#else
#define _ESMF_KIND_RX _ESMF_KIND_R4
#define ESMF_KIND_RX ESMF_KIND_R4
#define ESMF_TYPEKIND_RX ESMF_TYPEKIND_R4
#endif


/* -------------------------------------------------------------------------- */
/* Define macros for mask values                                              */
/* -------------------------------------------------------------------------- */
#define MASK_INLAND_WATER -1
#define MASK_WATER         0
#define MASK_LAND          1
#define MASK_FROZEN_WATER  2
#define MASK_FROZEN_LAND   3
