MODULE mod_oasis_getput_interface
!---------------------------------------------------------------------

    use mod_oasis_kinds
    use mod_oasis_data
    use mod_oasis_parameters
    use mod_oasis_advance
    use mod_oasis_var
    use mod_oasis_sys
    use mct_mod

    implicit none
    private

    public oasis_put
    public oasis_get

#include "oasis_os.h"

    integer(kind=ip_i4_p)     istatus(MPI_STATUS_SIZE)

  interface oasis_put
#ifndef __NO_4BYTE_REALS
     module procedure oasis_put_r14
     module procedure oasis_put_r24
#endif
     module procedure oasis_put_r18
     module procedure oasis_put_r28
  end interface

  interface oasis_get
#ifndef __NO_4BYTE_REALS
     module procedure oasis_get_r14
     module procedure oasis_get_r24
#endif
     module procedure oasis_get_r18
     module procedure oasis_get_r28
  end interface

!---------------------------------------------------------------------
contains
!---------------------------------------------------------------------
  SUBROUTINE oasis_put_r14(id_port_id,kstep,fld1,kinfo, &
    fld2, fld3, fld4, fld5)

    IMPLICIT none
    !-------------------------------------
    integer(kind=ip_i4_p) , intent(in) :: id_port_id,kstep
    real(kind=ip_single_p) :: fld1(:)
    integer(kind=ip_i4_p) , intent(out), optional :: kinfo
    real(kind=ip_single_p), optional :: fld2(:)
    real(kind=ip_single_p), optional :: fld3(:)
    real(kind=ip_single_p), optional :: fld4(:)
    real(kind=ip_single_p), optional :: fld5(:)
    !-------------------------------------
    integer(kind=ip_i4_p) :: nfld,ncpl
    integer(kind=ip_i4_p) :: ns,nsx
    integer(kind=ip_i4_p) :: n
    logical :: a2on, a3on, a4on, a5on
    character(len=*),parameter :: subname = 'oasis_put_r14'
    !-------------------------------------

    call oasis_debug_enter(subname)

    kinfo = OASIS_OK

    if (id_port_id == OASIS_Var_Uncpl) then
       if (OASIS_debug >= 1) write(nulprt,*) subname, &
          ' Routine oasis_put is called for a variable not in namcouple: it will not be sent'
       call oasis_abort_noarg()
       return
    endif

    nfld = id_port_id
    ncpl  = prism_var(nfld)%ncpl

    if (ncpl <= 0) then
       if (OASIS_debug >= 15) write(nulprt,*) subname,' variable not coupled ',&
                              trim(prism_var(nfld)%name)
       call oasis_debug_exit(subname)
       return
    endif

    ns = size(fld1,dim=1)

    a2on = .false.
    a3on = .false.
    a4on = .false.
    a5on = .false.

    if (present(fld2)) then
       a2on = .true.
       nsx = size(fld2,dim=1)
       if (nsx /= ns) then
          write(nulprt,*) subname,' ERROR fld2 size does not match fld1 ', &
                          trim(prism_var(nfld)%name)
          CALL oasis_flush(nulprt)
          CALL oasis_abort_noarg()
       endif
    endif

    if (present(fld3)) then
       a3on = .true.
       nsx = size(fld3,dim=1)
       if (nsx /= ns) then
          write(nulprt,*) subname,' ERROR fld3 size does not match fld1 ', &
                          trim(prism_var(nfld)%name)
          CALL oasis_flush(nulprt)
          CALL oasis_abort_noarg()
       endif
    endif

    if (present(fld4)) then
       a4on = .true.
       nsx = size(fld4,dim=1)
       if (nsx /= ns) then
          write(nulprt,*) subname,' ERROR array4 size does not match fld1 ', &
                          trim(prism_var(nfld)%name)
          CALL oasis_flush(nulprt)
          CALL oasis_abort_noarg()
       endif
    endif

    if (present(fld5)) then
       a5on = .true.
       nsx = size(fld5,dim=1)
       if (nsx /= ns) then
          write(nulprt,*) subname,' ERROR fld5 size does not match fld1 ', &
                          trim(prism_var(nfld)%name)
          CALL oasis_flush(nulprt)
          CALL oasis_abort_noarg()
       endif
    endif


    IF ((.NOT. a2on) .AND. (.NOT. a3on) .AND. (.NOT. a4on) .AND. (.NOT. a5on)) THEN
        CALL oasis_advance_run(OASIS_Out,nfld,kstep,kinfo,&
                               array1din= DBLE(fld1),readrest=.FALSE.)
    ELSE IF (a2on .AND. (.NOT. a3on) .AND. (.NOT. a4on) .AND. (.NOT. a5on)) THEN
        CALL oasis_advance_run(OASIS_Out,nfld,kstep,kinfo,&
                               array1din= DBLE(fld1),readrest=.FALSE.,&
                               a2on=a2on,array2=DBLE(fld2))
    ELSE IF (a2on .AND. a3on .AND. (.NOT. a4on) .AND. (.NOT. a5on)) THEN
        CALL oasis_advance_run(OASIS_Out,nfld,kstep,kinfo,&
                               array1din= DBLE(fld1),readrest=.FALSE.,&
                               a2on=a2on,array2=DBLE(fld2),&
                               a3on=a3on,array3=DBLE(fld3))
    ELSE IF (a2on .AND. a3on .AND. a4on .AND. (.NOT. a5on)) THEN
        CALL oasis_advance_run(OASIS_Out,nfld,kstep,kinfo,&
                               array1din= DBLE(fld1),readrest=.FALSE.,&
                               a2on=a2on,array2=DBLE(fld2),&
                               a3on=a3on,array3=DBLE(fld3),&
                               a4on=a4on,array4=DBLE(fld4))
    ELSE IF (a2on .AND. a3on .AND. a4on .AND. a5on) THEN
        CALL oasis_advance_run(OASIS_Out,nfld,kstep,kinfo,&
                               array1din= DBLE(fld1),readrest=.FALSE.,&
                               a2on=a2on,array2=DBLE(fld2),&
                               a3on=a3on,array3=DBLE(fld3),&
                               a4on=a4on,array4=DBLE(fld4),&
                               a5on=a5on,array5=DBLE(fld5))
    ELSE
        WRITE(nulprt,*) 'Wrong field array argument list in oasis_put'
        CALL oasis_flush(nulprt)
    ENDIF

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_put_r14

!-------------------------------------------------------------------
!---------------------------------------------------------------------
  SUBROUTINE oasis_put_r18(id_port_id,kstep,fld1,kinfo, &
    fld2, fld3, fld4, fld5)

    IMPLICIT none
    !-------------------------------------
    integer(kind=ip_i4_p) , intent(in) :: id_port_id,kstep
    real(kind=ip_double_p)             :: fld1(:)
    integer(kind=ip_i4_p) , intent(out), optional :: kinfo
    real(kind=ip_double_p), optional :: fld2(:)
    real(kind=ip_double_p), optional :: fld3(:)
    real(kind=ip_double_p), optional :: fld4(:)
    real(kind=ip_double_p), optional :: fld5(:)
    !-------------------------------------
    integer(kind=ip_i4_p) :: nfld,ncpl
    integer(kind=ip_i4_p) :: ns,nsx
    integer(kind=ip_i4_p) :: n
    logical :: a2on, a3on, a4on, a5on
    character(len=*),parameter :: subname = 'oasis_put_r18'
    !-------------------------------------

    call oasis_debug_enter(subname)

    kinfo = OASIS_OK

    if (id_port_id == OASIS_Var_Uncpl) then
       if (OASIS_debug >= 1) write(nulprt,*) subname, &
          ' Routine oasis_put is called for a variable not in namcouple: it will not be sent'
       call oasis_abort_noarg()
       return
    endif

    nfld = id_port_id
    ncpl  = prism_var(nfld)%ncpl

    if (ncpl <= 0) then
       if (OASIS_debug >= 15) write(nulprt,*) subname,' variable not coupled ',&
                              trim(prism_var(nfld)%name)
       call oasis_debug_exit(subname)
       return
    endif

    ns = size(fld1,dim=1)

    a2on = .false.
    a3on = .false.
    a4on = .false.
    a5on = .false.

    if (present(fld2)) then
       a2on = .true.
       nsx = size(fld2,dim=1)
       if (nsx /= ns) then
          write(nulprt,*) subname,' ERROR fld2 size does not match fld ', &
                          trim(prism_var(nfld)%name)
          CALL oasis_flush(nulprt)
          CALL oasis_abort_noarg()
       endif
    endif

    if (present(fld3)) then
       a3on = .true.
       nsx = size(fld3,dim=1)
       if (nsx /= ns) then
          write(nulprt,*) subname,' ERROR fld3 size does not match fld ', &
                          trim(prism_var(nfld)%name)
          CALL oasis_flush(nulprt)
          CALL oasis_abort_noarg()
       endif
    endif

    if (present(fld4)) then
       a4on = .true.
       nsx = size(fld4,dim=1)
       if (nsx /= ns) then
          write(nulprt,*) subname,' ERROR fld4 size does not match fld ', &
                          trim(prism_var(nfld)%name)
          CALL oasis_flush(nulprt)
          CALL oasis_abort_noarg()
       endif
    endif

    if (present(fld5)) then
       a5on = .true.
       nsx = size(fld5,dim=1)
       if (nsx /= ns) then
          write(nulprt,*) subname,' ERROR fld5 size does not match fld ', &
                          trim(prism_var(nfld)%name)
          CALL oasis_flush(nulprt)
          CALL oasis_abort_noarg()
       endif
    endif

    IF ((.NOT. a2on) .AND. (.NOT. a3on) .AND. (.NOT. a4on) .AND. (.NOT. a5on)) THEN
        CALL oasis_advance_run(OASIS_Out,nfld,kstep,kinfo,&
                               array1din=fld1,readrest=.FALSE.)
    ELSE IF (a2on .AND. (.NOT. a3on) .AND. (.NOT. a4on) .AND. (.NOT. a5on)) THEN
        CALL oasis_advance_run(OASIS_Out,nfld,kstep,kinfo,&
                               array1din=fld1,readrest=.FALSE.,&
                               a2on=a2on,array2=fld2)
    ELSE IF (a2on .AND. a3on .AND. (.NOT. a4on) .AND. (.NOT. a5on)) THEN
        CALL oasis_advance_run(OASIS_Out,nfld,kstep,kinfo,&
                               array1din= fld1,readrest=.FALSE.,&
                               a2on=a2on,array2=fld2,&
                               a3on=a3on,array3=fld3)
    ELSE IF (a2on .AND. a3on .AND. a4on .AND. (.NOT. a5on)) THEN
        CALL oasis_advance_run(OASIS_Out,nfld,kstep,kinfo,&
                               array1din=fld1,readrest=.FALSE.,&
                               a2on=a2on,array2=fld2,&
                               a3on=a3on,array3=fld3,&
                               a4on=a4on,array4=fld4)
    ELSE IF (a2on .AND. a3on .AND. a4on .AND. a5on) THEN
        CALL oasis_advance_run(OASIS_Out,nfld,kstep,kinfo,&
                               array1din=fld1,readrest=.FALSE.,&
                               a2on=a2on,array2=fld2,&
                               a3on=a3on,array3=fld3,&
                               a4on=a4on,array4=fld4,&
                               a5on=a5on,array5=fld5)
    ELSE
        WRITE(nulprt,*) 'Wrong field array argument list in oasis_put'
        CALL oasis_flush(nulprt)
    ENDIF

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_put_r18

!-------------------------------------------------------------------
!---------------------------------------------------------------------
  SUBROUTINE oasis_put_r24(id_port_id,kstep,fld1,kinfo, &
    fld2, fld3, fld4, fld5)

    IMPLICIT none
    !-------------------------------------
    integer(kind=ip_i4_p) , intent(in) :: id_port_id,kstep
    real(kind=ip_single_p) :: fld1(:,:)
    integer(kind=ip_i4_p) , intent(out), optional :: kinfo
    real(kind=ip_single_p), optional :: fld2(:,:)
    real(kind=ip_single_p), optional :: fld3(:,:)
    real(kind=ip_single_p), optional :: fld4(:,:)
    real(kind=ip_single_p), optional :: fld5(:,:)
    !-------------------------------------
    integer(kind=ip_i4_p) :: nfld,ncpl
    integer(kind=ip_i4_p) :: ns,nis,njs,nisx,njsx
    integer(kind=ip_i4_p) :: n,ni,nj
    logical :: a2on, a3on, a4on, a5on
    character(len=*),parameter :: subname = 'oasis_put_r24'
    !-------------------------------------

    call oasis_debug_enter(subname)

    kinfo = OASIS_OK

    if (id_port_id == OASIS_Var_Uncpl) then
       if (OASIS_debug >= 1) write(nulprt,*) subname, &
          ' Routine oasis_put is called for a variable not in namcouple: it will not be sent'
       call oasis_abort_noarg()
       return
    endif

    nfld = id_port_id
    ncpl  = prism_var(nfld)%ncpl

    if (ncpl <= 0) then
       if (OASIS_debug >= 15) write(nulprt,*) subname,' variable not coupled ',&
                              trim(prism_var(nfld)%name)
       call oasis_debug_exit(subname)
       return
    endif

    nis = size(fld1,dim=1)
    njs = size(fld1,dim=2)
    ns = nis*njs

    a2on = .false.
    a3on = .false.
    a4on = .false.
    a5on = .false.

    if (present(fld2)) then
       a2on = .true.
       nisx = size(fld2,dim=1)
       njsx = size(fld2,dim=2)
       if (nisx /= nis .or. njsx /= njs) then
          write(nulprt,*) subname,' ERROR fld2 size does not match fld ', &
                          trim(prism_var(nfld)%name)
          CALL oasis_flush(nulprt)
          CALL oasis_abort_noarg()
       endif
    endif

    if (present(fld3)) then
       a3on = .true.
       nisx = size(fld3,dim=1)
       njsx = size(fld3,dim=2)
       if (nisx /= nis .or. njsx /= njs) then
          write(nulprt,*) subname,' ERROR fld3 size does not match fld ', &
                          trim(prism_var(nfld)%name)
          CALL oasis_flush(nulprt)
          CALL oasis_abort_noarg()
       endif
    endif

    if (present(fld4)) then
       a4on = .true.
       nisx = size(fld4,dim=1)
       njsx = size(fld4,dim=2)
       if (nisx /= nis .or. njsx /= njs) then
          write(nulprt,*) subname,' ERROR fld4 size does not match fld ', &
                          trim(prism_var(nfld)%name)
          CALL oasis_flush(nulprt)
          CALL oasis_abort_noarg()
       endif
    endif

    if (present(fld5)) then
       a5on = .true.
       nisx = size(fld5,dim=1)
       njsx = size(fld5,dim=2)
       if (nisx /= nis .or. njsx /= njs) then
          write(nulprt,*) subname,' ERROR fld5 size does not match fld ', &
                          trim(prism_var(nfld)%name)
          CALL oasis_flush(nulprt)
          CALL oasis_abort_noarg()
       endif
    endif


    IF ((.NOT. a2on) .AND. (.NOT. a3on) .AND. (.NOT. a4on) .AND. (.NOT. a5on)) THEN
        CALL oasis_advance_run(OASIS_Out,nfld,kstep,kinfo,&
                               array1din= DBLE(PACK(fld1, mask= .true.)),readrest=.FALSE.)
    ELSE IF (a2on .AND. (.NOT. a3on) .AND. (.NOT. a4on) .AND. (.NOT. a5on)) THEN
        CALL oasis_advance_run(OASIS_Out,nfld,kstep,kinfo,&
                               array1din= DBLE(PACK(fld1, mask= .TRUE.)),readrest=.FALSE.,&
                               a2on=a2on,array2=DBLE(PACK(fld2, mask= .true.)))
    ELSE IF (a2on .AND. a3on .AND. (.NOT. a4on) .AND. (.NOT. a5on)) THEN
        CALL oasis_advance_run(OASIS_Out,nfld,kstep,kinfo,&
                               array1din= DBLE(PACK(fld1, mask= .TRUE.)),readrest=.FALSE.,&
                               a2on=a2on,array2=DBLE(PACK(fld2, mask= .TRUE.)),&
                               a3on=a3on,array3=DBLE(PACK(fld3, mask= .TRUE.)))
    ELSE IF (a2on .AND. a3on .AND. a4on .AND. (.NOT. a5on)) THEN
        CALL oasis_advance_run(OASIS_Out,nfld,kstep,kinfo,&
                               array1din= DBLE(PACK(fld1, mask= .TRUE.)),readrest=.FALSE.,&
                               a2on=a2on,array2=DBLE(PACK(fld2, mask= .TRUE.)),&
                               a3on=a3on,array3=DBLE(PACK(fld3, mask= .TRUE.)),&
                               a4on=a4on,array4=DBLE(PACK(fld4, mask= .TRUE.)))
    ELSE IF (a2on .AND. a3on .AND. a4on .AND. a5on) THEN
        CALL oasis_advance_run(OASIS_Out,nfld,kstep,kinfo,&
                               array1din= DBLE(PACK(fld1, mask= .TRUE.)),readrest=.FALSE.,&
                               a2on=a2on,array2=DBLE(PACK(fld2, mask= .TRUE.)),&
                               a3on=a3on,array3=DBLE(PACK(fld3, mask= .TRUE.)),&
                               a4on=a4on,array4=DBLE(PACK(fld4, mask= .TRUE.)),&
                               a5on=a5on,array5=DBLE(PACK(fld5, mask= .TRUE.)))
    ELSE
        WRITE(nulprt,*) 'Wrong field array argument list in oasis_put'
        CALL oasis_flush(nulprt)
    ENDIF

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_put_r24

!-------------------------------------------------------------------
!---------------------------------------------------------------------
  SUBROUTINE oasis_put_r28(id_port_id,kstep,fld1,kinfo, &
    fld2, fld3, fld4, fld5)

    IMPLICIT none
    !-------------------------------------
    integer(kind=ip_i4_p) , intent(in) :: id_port_id,kstep
    real(kind=ip_double_p) :: fld1(:,:)
    integer(kind=ip_i4_p) , intent(out), optional :: kinfo
    real(kind=ip_double_p), optional :: fld2(:,:)
    real(kind=ip_double_p), optional :: fld3(:,:)
    real(kind=ip_double_p), optional :: fld4(:,:)
    real(kind=ip_double_p), optional :: fld5(:,:)
    !-------------------------------------
    integer(kind=ip_i4_p) :: nfld,ncpl
    integer(kind=ip_i4_p) :: ns,nis,njs,nisx,njsx
    integer(kind=ip_i4_p) :: n,ni,nj
    logical :: a2on, a3on, a4on, a5on
    character(len=*),parameter :: subname = 'oasis_put_r28'
    !-------------------------------------

    call oasis_debug_enter(subname)

    kinfo = OASIS_OK

    if (id_port_id == OASIS_Var_Uncpl) then
       if (OASIS_debug >= 1) write(nulprt,*) subname, &
          ' Routine oasis_put is called for a variable not in namcouple: it will not be sent'
       call oasis_abort_noarg()
       return
    endif

    nfld = id_port_id
    ncpl  = prism_var(nfld)%ncpl

    if (ncpl <= 0) then
       if (OASIS_debug >= 15) write(nulprt,*) subname,' variable not coupled ',&
                              trim(prism_var(nfld)%name)
       call oasis_debug_exit(subname)
       return
    endif

    nis = size(fld1,dim=1)
    njs = size(fld1,dim=2)
    ns = nis*njs

    a2on = .false.
    a3on = .false.
    a4on = .false.
    a5on = .false.

    if (present(fld2)) then
       a2on = .true.
       nisx = size(fld2,dim=1)
       njsx = size(fld2,dim=2)
       if (nisx /= nis .or. njsx /= njs) then
          write(nulprt,*) subname,' ERROR fld2 size does not match fld ', &
                          trim(prism_var(nfld)%name)
          CALL oasis_flush(nulprt)
          CALL oasis_abort_noarg()
       endif
    endif

    if (present(fld3)) then
       a3on = .true.
       nisx = size(fld3,dim=1)
       njsx = size(fld3,dim=2)
       if (nisx /= nis .or. njsx /= njs) then
          write(nulprt,*) subname,' ERROR fld3 size does not match fld ', &
                          trim(prism_var(nfld)%name)
          CALL oasis_flush(nulprt)
          CALL oasis_abort_noarg()
       endif
    endif

    if (present(fld4)) then
       a4on = .true.
       nisx = size(fld4,dim=1)
       njsx = size(fld4,dim=2)
       if (nisx /= nis .or. njsx /= njs) then
          write(nulprt,*) subname,' ERROR fld4 size does not match fld ', &
                          trim(prism_var(nfld)%name)
          CALL oasis_flush(nulprt)
          CALL oasis_abort_noarg()
       endif
    endif

    if (present(fld5)) then
       a5on = .true.
       nisx = size(fld5,dim=1)
       njsx = size(fld5,dim=2)
       if (nisx /= nis .or. njsx /= njs) then
          write(nulprt,*) subname,' ERROR fld5 size does not match fld ', &
                          trim(prism_var(nfld)%name)
          CALL oasis_flush(nulprt)
          CALL oasis_abort_noarg()
       endif
    endif


    IF ((.NOT. a2on) .AND. (.NOT. a3on) .AND. (.NOT. a4on) .AND. (.NOT. a5on)) THEN
        CALL oasis_advance_run(OASIS_Out,nfld,kstep,kinfo, &
                               array1din= (PACK(fld1, mask= .true.)),readrest=.FALSE.)
    ELSE IF (a2on .AND. (.NOT. a3on) .AND. (.NOT. a4on) .AND. (.NOT. a5on)) THEN
        CALL oasis_advance_run(OASIS_Out,nfld,kstep,kinfo,&
                               array1din= (PACK(fld1, mask= .TRUE.)),readrest=.FALSE.,&
                               a2on=a2on,array2=(PACK(fld2, mask= .true.)))
    ELSE IF (a2on .AND. a3on .AND. (.NOT. a4on) .AND. (.NOT. a5on)) THEN
        CALL oasis_advance_run(OASIS_Out,nfld,kstep,kinfo,&
                               array1din= (PACK(fld1, mask= .TRUE.)),readrest=.FALSE.,&
                               a2on=a2on,array2=(PACK(fld2, mask= .TRUE.)),&
                               a3on=a3on,array3=(PACK(fld3, mask= .TRUE.)))
    ELSE IF (a2on .AND. a3on .AND. a4on .AND. (.NOT. a5on)) THEN
        CALL oasis_advance_run(OASIS_Out,nfld,kstep,kinfo,&
                               array1din= (PACK(fld1, mask= .TRUE.)),readrest=.FALSE.,&
                               a2on=a2on,array2=(PACK(fld2, mask= .TRUE.)),&
                               a3on=a3on,array3=(PACK(fld3, mask= .TRUE.)),&
                               a4on=a4on,array4=(PACK(fld4, mask= .TRUE.)))
    ELSE IF (a2on .AND. a3on .AND. a4on .AND. a5on) THEN
        CALL oasis_advance_run(OASIS_Out,nfld,kstep,kinfo,&
                               array1din= (PACK(fld1, mask= .TRUE.)),readrest=.FALSE.,&
                               a2on=a2on,array2=(PACK(fld2, mask= .TRUE.)),&
                               a3on=a3on,array3=(PACK(fld3, mask= .TRUE.)),&
                               a4on=a4on,array4=(PACK(fld4, mask= .TRUE.)),&
                               a5on=a5on,array5=(PACK(fld5, mask= .TRUE.)))
    ELSE
        WRITE(nulprt,*) 'Wrong field array argument list in oasis_put'
        CALL oasis_flush(nulprt)
    ENDIF

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_put_r28

!-------------------------------------------------------------------
!---------------------------------------------------------------------
  SUBROUTINE oasis_get_r14(id_port_id,kstep,rd_field,kinfo)

    IMPLICIT none
    !-------------------------------------
    integer(kind=ip_i4_p) , intent(in) :: id_port_id,kstep
    real(kind=ip_single_p), intent(inout) :: rd_field(:)
    integer(kind=ip_i4_p) , intent(out), optional :: kinfo
    !-------------------------------------
    integer(kind=ip_i4_p) :: nfld,ncpl
    integer(kind=ip_i4_p) :: ns,nis,njs
    integer(kind=ip_i4_p) :: n,ni,nj
    real(kind=ip_r8_p), allocatable :: array(:)
    character(len=*),parameter :: subname = 'oasis_get_r14'
    !-------------------------------------

    call oasis_debug_enter(subname)

    kinfo = OASIS_OK

    if (id_port_id == OASIS_Var_Uncpl) then
       if (OASIS_debug >= 1) write(nulprt,*) subname, &
          ' Routine oasis_get is called for variable not in namcouple; it will not be received'
       if (OASIS_debug >= 1) write(nulprt,*) subname,' BE CAREFUL NOT TO USE IT !!!!!'
       call oasis_abort_noarg()
       return
    endif

    nfld = id_port_id
    ncpl  = prism_var(nfld)%ncpl

    if (ncpl <= 0) then
       if (OASIS_debug >= 15) write(nulprt,*) subname,' variable not coupled ',&
                              trim(prism_var(nfld)%name)
       call oasis_debug_exit(subname)
       return
    endif

    ns = size(rd_field,dim=1)

    allocate(array(ns))

    CALL oasis_advance_run(OASIS_In,nfld,kstep,kinfo,array1dout=array,readrest=.FALSE.)

    IF (kinfo /= OASIS_Ok) THEN
        rd_field(:) = REAL(array(:))
    ENDIF

    deallocate(array)
    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_get_r14

!---------------------------------------------------------------------
  SUBROUTINE oasis_get_r18(id_port_id,kstep,rd_field,kinfo)

    IMPLICIT none
    !-------------------------------------
    integer(kind=ip_i4_p) , intent(in) :: id_port_id,kstep
    real(kind=ip_double_p), intent(inout) :: rd_field(:)
    integer(kind=ip_i4_p) , intent(out), optional :: kinfo
    !-------------------------------------
    integer(kind=ip_i4_p) :: nfld,ncpl
    integer(kind=ip_i4_p) :: ns,nis,njs
    integer(kind=ip_i4_p) :: n,ni,nj
    character(len=*),parameter :: subname = 'oasis_get_r18'
    !-------------------------------------

    call oasis_debug_enter(subname)

    kinfo = OASIS_OK

    if (id_port_id == OASIS_Var_Uncpl) then
       if (OASIS_debug >= 1) write(nulprt,*) subname, &
          ' Routine oasis_get is called for variable not in namcouple; it will not be received'
       if (OASIS_debug >= 1) write(nulprt,*) subname,' BE CAREFUL NOT TO USE IT !!!!!'
       call oasis_abort_noarg()
       return
    endif

    nfld = id_port_id
    ncpl  = prism_var(nfld)%ncpl

    if (ncpl <= 0) then
       if (OASIS_debug >= 15) write(nulprt,*) subname,' variable not coupled ',&
                              trim(prism_var(nfld)%name)
       call oasis_debug_exit(subname)
       return
    endif

    CALL oasis_advance_run(OASIS_In,nfld,kstep,kinfo,array1dout=rd_field,readrest=.FALSE.)

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_get_r18

!---------------------------------------------------------------------
  SUBROUTINE oasis_get_r24(id_port_id,kstep,rd_field,kinfo)

    IMPLICIT none
    !-------------------------------------
    integer(kind=ip_i4_p) , intent(in) :: id_port_id,kstep
    real(kind=ip_single_p), intent(inout) :: rd_field(:,:)
    integer(kind=ip_i4_p) , intent(out), optional :: kinfo
    !-------------------------------------
    integer(kind=ip_i4_p) :: nfld,ncpl
    integer(kind=ip_i4_p) :: ns,nis,njs
    integer(kind=ip_i4_p) :: n,ni,nj
    REAL(kind=ip_r8_p), ALLOCATABLE :: array(:,:)
    character(len=*),parameter :: subname = 'oasis_get_r24'
    !-------------------------------------

    call oasis_debug_enter(subname)

    kinfo = OASIS_OK

    if (id_port_id == OASIS_Var_Uncpl) then
       if (OASIS_debug >= 1) write(nulprt,*) subname, &
          ' Routine oasis_get is called for variable not in namcouple; it will not be received'
       if (OASIS_debug >= 1) write(nulprt,*) subname,' BE CAREFUL NOT TO USE IT !!!!!'
       call oasis_abort_noarg()
       return
    endif

    nfld = id_port_id
    ncpl  = prism_var(nfld)%ncpl

    if (ncpl <= 0) then
       if (OASIS_debug >= 15) write(nulprt,*) subname,' variable not coupled ',&
                                              trim(prism_var(nfld)%name)
       call oasis_debug_exit(subname)
       return
    endif

    nis = size(rd_field,dim=1)
    njs = size(rd_field,dim=2)
    ns = nis*njs

    ALLOCATE(array(nis,njs))

    CALL oasis_advance_run(OASIS_In,nfld,kstep,kinfo,array2dout=array,readrest=.FALSE.)

    IF (kinfo /= OASIS_Ok) THEN
        rd_field(:,:) = REAL(array(:,:))
    ENDIF

    deallocate(array)
    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_get_r24

!---------------------------------------------------------------------
  SUBROUTINE oasis_get_r28(id_port_id,kstep,rd_field,kinfo)

    IMPLICIT none
    !-------------------------------------
    integer(kind=ip_i4_p) , intent(in) :: id_port_id,kstep
    real(kind=ip_double_p), intent(inout) :: rd_field(:,:)
    integer(kind=ip_i4_p) , intent(out), optional :: kinfo
    !-------------------------------------
    integer(kind=ip_i4_p) :: nfld,ncpl
    integer(kind=ip_i4_p) :: ns,nis,njs
    integer(kind=ip_i4_p) :: n,ni,nj
    character(len=*),parameter :: subname = 'oasis_get_r28'
    !-------------------------------------

    call oasis_debug_enter(subname)

    kinfo = OASIS_OK

    if (id_port_id == OASIS_Var_Uncpl) then
       if (OASIS_debug >= 1) write(nulprt,*) subname, &
          ' Routine oasis_get is called for variable not in namcouple; it will not be received'
       if (OASIS_debug >= 1) write(nulprt,*) subname,' BE CAREFUL NOT TO USE IT !!!!!'
       call oasis_abort_noarg()
       return
    endif

    nfld = id_port_id
    ncpl  = prism_var(nfld)%ncpl

    if (ncpl <= 0) then
       if (OASIS_debug >= 15) write(nulprt,*) subname,' variable not coupled ',&
                                              trim(prism_var(nfld)%name)
       call oasis_debug_exit(subname)
       return
    endif

    CALL oasis_advance_run(OASIS_In,nfld,kstep,kinfo,array2dout=rd_field,readrest=.FALSE.)

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_get_r28

!-------------------------------------------------------------------

END MODULE mod_oasis_getput_interface

