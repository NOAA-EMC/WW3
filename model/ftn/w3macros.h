/* ------------------------------------------------------------------ */
/*    C-Preprocessor macros                                           */
/*                                                                    */
/*                  +-----------------------------------+             */
/*                  | WAVEWATCH III           NOAA/NCEP |             */
/*                  |        T. J. Campbell, NRL        |             */
/*                  |                                 C |             */
/*                  | Last update :         10-Dec-2014 |             */
/*                  +-----------------------------------+             */
/*                                                                    */
/*    10-Dec-2014 : Origination.                     ( version 5.04 ) */
/*                                                                    */
/* 1. Purpose :                                                       */
/*                                                                    */
/*    Define C-preprocessor macros for WW3 source code.               */
/*                                                                    */
/* 2. Method :                                                        */
/*                                                                    */
/* 3. Parameters :                                                    */
/*                                                                    */
/* 4. Subroutines used :                                              */
/*                                                                    */
/* 5. Called by :                                                     */
/*                                                                    */
/* 6. Error messages :                                                */
/*                                                                    */
/* 7. Remarks :                                                       */
/*                                                                    */
/*    The __FILE__ and __LINE__ macros are defined by CPP.            */
/*                                                                    */
/* 8. Structure :                                                     */
/*                                                                    */
/*    See source code.                                                */
/*                                                                    */
/* 9. Source code :                                                   */
/*                                                                    */
/* ------------------------------------------------------------------ */

/*
 * Macros to wrap checking allocate/deallocate status
 */
#define CHECK_ALLOC_STATUS( STAT ) \
   IF ( STAT .NE. 0 ) \
   CALL EXTCDE ( 99, MSG="ALLOCATE FAILED", FILE=__FILE__, LINE=__LINE__ )
#define CHECK_DEALLOC_STATUS( STAT ) \
   IF ( STAT .NE. 0 ) \
   CALL EXTCDE ( 99, MSG="DEALLOCATE FAILED", FILE=__FILE__, LINE=__LINE__ )

/* End of w3macros.h ------------------------------------------------ */
