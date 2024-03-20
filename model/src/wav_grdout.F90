module wav_grdout

  use w3odatmd    , only: nogrp, ngrpp

  implicit none

  integer, parameter   :: maxvars = 25         ! maximum number of variables/group

  private ! except

  public :: varatts
  public :: outvars
  public :: wavinit_grdout

  ! tag read from inp file and is used to set flogrd flags
  ! var_name is the name of the variable
  type :: varatts
    character(len= 5) :: tag
    character(len=10) :: var_name
    character(len=48) :: long_name
    character(len=10) :: unit_name
    character(len= 2) :: dims
    logical           :: validout
  end type varatts

  type(varatts), dimension(nogrp,maxvars) :: gridoutdefs

  type(varatts), dimension(:), allocatable :: outvars

  !===============================================================================
contains
  !===============================================================================

  !====================================================================================
  subroutine wavinit_grdout

    use w3gdatmd    , only: e3df, p2msf, us3df, usspf
    use w3odatmd    , only: nds, iaproc, napout
    use w3iogomd    , only: fldout
    use w3servmd    , only: strsplit

    ! local variables
    character(len=100)      :: inptags(100) = ''
    integer                 :: j,k,n,nout
    character(len= 12)      :: ttag

    ! obtain all possible output variable tags and attributes
    call initialize_gridout

    ! obtain the tags for the requested output variables
    call strsplit(fldout,inptags)

    ! determine which variables are tagged for output
    do k = 1,nogrp
      do j = 1,maxvars
        if (len_trim(gridoutdefs(k,j)%tag) > 0) then
          do n = 1,len(inptags)
            if (len_trim(inptags(n)) > 0) then
              if (trim(inptags(n)) == trim(gridoutdefs(k,j)%tag)) gridoutdefs(k,j)%validout = .true.
            end if
          end do
        end if
      end do
    end do

    ! remove requested variables which are only allocated if specific
    ! options are set in mod_def (see w3adatmd, '3D arrays')
    do k = 1,nogrp
      do j = 1,maxvars
        if (gridoutdefs(k,j)%validout) then
          ttag = trim(gridoutdefs(k,j)%tag)
          if (ttag == 'EF'    .and. e3df(1,1) == 0) gridoutdefs(k,j)%validout = .false.
          if (ttag == 'TH1M'  .and. e3df(1,2) == 0) gridoutdefs(k,j)%validout = .false.
          if (ttag == 'STH1M' .and. e3df(1,3) == 0) gridoutdefs(k,j)%validout = .false.
          if (ttag == 'TH2M'  .and. e3df(1,4) == 0) gridoutdefs(k,j)%validout = .false.
          if (ttag == 'STH2M' .and. e3df(1,5) == 0) gridoutdefs(k,j)%validout = .false.

          if (ttag == 'P2L' .and. p2msf(1) == 0) gridoutdefs(k,j)%validout = .false.
          if (ttag == 'USF' .and. us3df(1) == 0) gridoutdefs(k,j)%validout = .false.
          if (ttag == 'USP' .and. usspf(1) == 0) gridoutdefs(k,j)%validout = .false.
        end if
      end do
    end do

    ! determine number of output variables (not the same as the number of tags)
    n = 0
    do k = 1,nogrp
      do j = 1,maxvars
        if (gridoutdefs(k,j)%validout) n = n+1
      end do
    end do
    nout = n
    allocate(outvars(1:nout))

    ! subset variables requested
    n = 0
    do k = 1,nogrp
      do j = 1,maxvars
        if (gridoutdefs(k,j)%validout) then
          n = n+1
          outvars(n) = gridoutdefs(k,j)
        end if
      enddo
    end do

    ! check
    if ( iaproc == napout ) then
      write(nds(1),*)
      write(nds(1),'(a)')' --------------------------------------------------'
      write(nds(1),'(a)')'  Requested gridded output variables : '
      write(nds(1),'(a)')' --------------------------------------------------'
      write(nds(1),*)
      do n = 1,nout
        write(nds(1),'(i5,2a12,a50)')n,'  '//trim(outvars(n)%tag), &
             '  '//trim(outvars(n)%var_name), &
             '  '//trim(outvars(n)%long_name)
      end do
      write(nds(1),*)
    end if

  end subroutine wavinit_grdout

  !====================================================================================
  subroutine initialize_gridout

    gridoutdefs(:,:)%tag = ""
    gridoutdefs(:,:)%var_name = ""
    gridoutdefs(:,:)%long_name = ""
    gridoutdefs(:,:)%unit_name = ""
    gridoutdefs(:,:)%dims = ""
    gridoutdefs(:,:)%validout = .false.

    ! TODO: confirm unit values
    !  1   Forcing Fields
    gridoutdefs(1,1:14) = [ &
         varatts( "DPT  ", "DW        ", "Water depth                                     ", "m         ", "  ", .false.) , &
         varatts( "CUR  ", "CX        ", "Mean current, x-component                       ", "m s-1     ", "  ", .false.) , &
         varatts( "CUR  ", "CY        ", "Mean current, y-component                       ", "m s-1     ", "  ", .false.) , &
         varatts( "WND  ", "UAX       ", "Mean wind, x-component                          ", "m s-1     ", "  ", .false.) , &
         varatts( "WND  ", "UAY       ", "Mean wind, y-component                          ", "m s-1     ", "  ", .false.) , &
         varatts( "AST  ", "AS        ", "Air-sea temperature difference                  ", "K         ", "  ", .false.) , &
         varatts( "WLV  ", "WLV       ", "Water levels                                    ", "m         ", "  ", .false.) , &
         varatts( "ICE  ", "ICE       ", "Ice coverage                                    ", "nd        ", "  ", .false.) , &
         varatts( "IBG  ", "BERG      ", "Iceberg-induced damping                         ", "km-1      ", "  ", .false.) , &
         varatts( "TAUA ", "TAUAX     ", "Atm momentum x                                  ", "Pa        ", "  ", .false.) , &
         varatts( "TAUA ", "TAUAY     ", "Atm momentum y                                  ", "Pa        ", "  ", .false.) , &
         varatts( "RHO  ", "RHOAIR    ", "Air density                                     ", "kg m-3    ", "  ", .false.) , &
         varatts( "IC1  ", "ICEH      ", "Ice thickness                                   ", "m         ", "  ", .false.) , &
         varatts( "IC5  ", "ICEF      ", "Ice floe diameter                               ", "m         ", "  ", .false.)   &
         ]

    !  2   Standard mean wave Parameters
    gridoutdefs(2,1:18) = [ &
         varatts( "HS   ", "HS        ", "Significant wave height                         ", "m         ", "  ", .false.) , &
         varatts( "LM   ", "WLM       ", "Mean wave length                                ", "m         ", "  ", .false.) , &
         varatts( "T02  ", "T02       ", "Mean wave period (Tm0,2)                        ", "s         ", "  ", .false.) , &
         varatts( "T0M1 ", "T0M1      ", "Mean wave period (Tm0,-1)                       ", "s         ", "  ", .false.) , &
         varatts( "T01  ", "T01       ", "Mean wave period (Tm0,1)                        ", "s         ", "  ", .false.) , &
         varatts( "FP   ", "FP0       ", "Peak frequency                                  ", "s-1       ", "  ", .false.) , &
         varatts( "DIR  ", "THM       ", "Mean wave direction                             ", "rad       ", "  ", .false.) , &
         varatts( "SPR  ", "THS       ", "Mean directional spread                         ", "rad       ", "  ", .false.) , &
         varatts( "DP   ", "THP0      ", "Peak direction                                  ", "rad       ", "  ", .false.) , &
         varatts( "HIG  ", "HSIG      ", "Infragravity height                             ", "m         ", "  ", .false.) , &
         varatts( "MXE  ", "STMAXE    ", "Max surface elev (STE)                          ", "m         ", "  ", .false.) , &
         varatts( "MXES ", "STMAXD    ", "St Dev Max surface elev (STE)                   ", "m         ", "  ", .false.) , &
         varatts( "MXH  ", "HMAXE     ", "Max wave height (S.)                            ", "m         ", "  ", .false.) , &
         varatts( "MXHC ", "HCMAXE    ", "Max wave height from crest (STE)                ", "m         ", "  ", .false.) , &
         varatts( "SDMH ", "HMAXD     ", "St Dev of MXC (STE)                             ", "m         ", "  ", .false.) , &
         varatts( "SDMHC", "HCMAXD    ", "St Dev of MXHC (STE)                            ", "m         ", "  ", .false.) , &
         varatts( "WBT  ", "WBT       ", "Dominant wave breaking probability (b_T)        ", "nd        ", "  ", .false.) , &
         varatts( "WNM  ", "WNMEAN    ", "Mean wave number                                ", "m-1       ", "  ", .false.)   &
         ]

    !  3   Spectral Parameters
    gridoutdefs(3,1:6) = [ &
         varatts( "EF   ", "EF        ", "1D spectral density                             ", "m2 s      ", "k ", .false.) , &
         varatts( "TH1M ", "TH1M      ", "Mean wave direction from a1,b2                  ", "deg       ", "k ", .false.) , &
         varatts( "STH1M", "STH1M     ", "Directional spreading from a1,b2                ", "deg       ", "k ", .false.) , &
         varatts( "TH2M ", "TH2M      ", "Mean wave direction from a2,b2                  ", "deg       ", "k ", .false.) , &
         varatts( "STH2M", "STH2M     ", "Directional spreading from a2,b2                ", "deg       ", "k ", .false.) , &
                                !TODO: has reverse indices (nk,nsea)
         varatts( "WN   ", "WN        ", "Wavenumber array                                ", "m-1       ", "k ", .false.)   &
         ]

    !  4   Spectral Partition Parameters
    gridoutdefs(4,1:17) = [ &
         varatts( "PHS  ", "PHS       ", "Partitioned wave heights                        ", "m         ", "s ", .false.) , &
         varatts( "PTP  ", "PTP       ", "Partitioned peak period                         ", "s         ", "s ", .false.) , &
         varatts( "PLP  ", "PLP       ", "Partitioned peak wave length                    ", "m         ", "s ", .false.) , &
         varatts( "PDIR ", "PDIR      ", "Partitioned mean direction                      ", "deg       ", "s ", .false.) , &
         varatts( "PSPR ", "PSI       ", "Partitioned mean directional spread             ", "deg       ", "s ", .false.) , &
         varatts( "PWS  ", "PWS       ", "Partitioned wind sea fraction                   ", "nd        ", "s ", .false.) , &
         varatts( "PDP  ", "PTHP0     ", "Peak wave direction of partition                ", "deg       ", "s ", .false.) , &
         varatts( "PQP  ", "PQP       ", "Goda peakdedness parameter of partition         ", "nd        ", "s ", .false.) , &
         varatts( "PPE  ", "PPE       ", "JONSWAP peak enhancement factor of partition    ", "s-1       ", "s ", .false.) , &
         varatts( "PGW  ", "PGW       ", "Gaussian frequency width of partition           ", "nd        ", "s ", .false.) , &
         varatts( "PSW  ", "PSW       ", "Spectral width of partition                     ", "nd        ", "s ", .false.) , &
         varatts( "PTM10", "PTM1      ", "Mean wave period (m-1,0) of partition           ", "s         ", "s ", .false.) , &
         varatts( "PT01 ", "PT1       ", "Mean wave period (m0,1) of partition            ", "s         ", "s ", .false.) , &
         varatts( "PT02 ", "PT2       ", "Mean wave period (m0,2) of partition            ", "s         ", "s ", .false.) , &
         varatts( "PEP  ", "PEP       ", "Peak spectral density of partition              ", "m2 s rad-1", "s ", .false.) , &
         varatts( "TWS  ", "PWST      ", "Total wind sea fraction                         ", "nd        ", "  ", .false.) , &
         varatts( "PNR  ", "PNR       ", "Number of partitions                            ", "nd        ", "  ", .false.)   &
         ]

    !  5   Atmosphere-waves layer
    gridoutdefs(5,1:14) = [ &
         varatts( "UST  ", "USTX      ", "Friction velocity x                             ", "m s-1     ", "  ", .false.) , &
         varatts( "UST  ", "USTY      ", "Friction velocity y                             ", "m s-1     ", "  ", .false.) , &
         varatts( "CHA  ", "CHARN     ", "Charnock parameter                              ", "nd        ", "  ", .false.) , &
         varatts( "CGE  ", "CGE       ", "Energy flux                                     ", "kW m-1    ", "  ", .false.) , &
         varatts( "FAW  ", "PHIAW     ", "Air-sea energy flux                             ", "W m-2     ", "  ", .false.) , &
         varatts( "TAW  ", "TAUWIX    ", "Net wave-supported stress x                     ", "m2 s-2    ", "  ", .false.) , &
         varatts( "TAW  ", "TAUWIY    ", "Net wave-supported stress y                     ", "m2 s-2    ", "  ", .false.) , &
         varatts( "TWA  ", "TAUWNX    ", "Negative part of the wave-supported stress x    ", "m2 s-2    ", "  ", .false.) , &
         varatts( "TWA  ", "TAUWNY    ", "Negative part of the wave-supported stress y    ", "m2 s-2    ", "  ", .false.) , &
         varatts( "WCC  ", "WCC       ", "Whitecap coverage                               ", "nd        ", "  ", .false.) , &
         varatts( "WCF  ", "WCF       ", "Whitecap foam thickness                         ", "m         ", "  ", .false.) , &
         varatts( "WCH  ", "WCH       ", "Mean breaking wave heigh                        ", "m         ", "  ", .false.) , &
         varatts( "WCM  ", "WCM       ", "Whitecap moment                                 ", "nd        ", "  ", .false.) , &
         varatts( "FWS  ", "TWS       ", "Wind sea mean period                            ", "s         ", "  ", .false.)   &
         ]

    !  6   Wave-ocean layer
    gridoutdefs(6,1:25) = [ &
         varatts( "SXY  ", "SXX       ", "Radiation stresses xx                           ", "N m-1     ", "  ", .false.) , &
         varatts( "SXY  ", "SYY       ", "Radiation stresses yy                           ", "N m-1     ", "  ", .false.) , &
         varatts( "SXY  ", "SXY       ", "Radiation stresses xy                           ", "N m-1     ", "  ", .false.) , &
         varatts( "TWO  ", "TAUOX     ", "Wave to ocean momentum flux x                   ", "m2 s-2    ", "  ", .false.) , &
         varatts( "TWO  ", "TAUOY     ", "Wave to ocean momentum flux y                   ", "m2 s-2    ", "  ", .false.) , &
         varatts( "BHD  ", "BHD       ", "Bernoulli head (J term)                         ", "m2 s-2    ", "  ", .false.) , &
         varatts( "FOC  ", "PHIOC     ", "Wave to ocean energy flux                       ", "W m-2     ", "  ", .false.) , &
         varatts( "TUS  ", "TUSX      ", "Stokes transport x                              ", "m2 s-1    ", "  ", .false.) , &
         varatts( "TUS  ", "TUSY      ", "Stokes transport y                              ", "m2 s-1    ", "  ", .false.) , &
         varatts( "USS  ", "USSX      ", "Surface Stokes drift x                          ", "m s-1     ", "  ", .false.) , &
         varatts( "USS  ", "USSY      ", "Surface Stokes drift y                          ", "m s-1     ", "  ", .false.) , &
         varatts( "P2S  ", "PRMS      ", "Second-order sum pressure                       ", "m4        ", "  ", .false.) , &
         varatts( "P2S  ", "TPMS      ", "Second-order sum pressure                       ", "s-1       ", "  ", .false.) , &
         varatts( "USF  ", "US3DX     ", "Spectrum of surface Stokes drift x              ", "m s-1 Hz-1", "k ", .false.) , &
         varatts( "USF  ", "US3DY     ", "Spectrum of surface Stokes drift y              ", "m s-1 Hz-1", "k ", .false.) , &
         varatts( "P2L  ", "P2SMS     ", "Micro seism  source term                        ", "Pa2 m2 s  ", "m ", .false.) , &
         varatts( "TWI  ", "TAUICEX   ", "Wave to sea ice stress x                        ", "m2 s-2    ", "  ", .false.) , &
         varatts( "TWI  ", "TAUICEY   ", "Wave to sea ice stress y                        ", "m2 s-2    ", "  ", .false.) , &
         varatts( "FIC  ", "PHICE     ", "Wave to sea ice energy flux                     ", "W m-2     ", "  ", .false.) , &
         varatts( "USP  ", "USSPX     ", "Partitioned surface Stokes drift x              ", "m s-1     ", "p ", .false.) , &
         varatts( "USP  ", "USSPY     ", "Partitioned surface Stokes drift y              ", "m s-1     ", "p ", .false.) , &
         varatts( "TWC  ", "TAUOCX    ", "Total wave to ocean stress x                    ", "Pa        ", "  ", .false.) , &
         varatts( "TWC  ", "TAUOCY    ", "Total wave to ocean stress y                    ", "Pa        ", "  ", .false.) , &
         varatts( "USSH ", "USSHX     ", "Surface layer averaged Stokes drift x           ", "m s-1     ", "  ", .false.) , &
         varatts( "USSH ", "USSHY     ", "Surface layer averaged Stokes drift y           ", "m s-1     ", "  ", .false.)   &
         ]

    !  7   Wave-bottom layer
    gridoutdefs(7,1:10) = [ &
         varatts( "ABR  ", "ABAX      ", "Near bottom rms wave excursion amplitudes x     ", "m         ", "  ", .false.) , &
         varatts( "ABR  ", "ABAY      ", "Near bottom rms wave excursion amplitudes y     ", "m         ", "  ", .false.) , &
         varatts( "UBR  ", "UBAX      ", "Near bottom rms wave velocities x               ", "m s-1     ", "  ", .false.) , &
         varatts( "UBR  ", "UBAY      ", "Near bottom rms wave velocities y               ", "m s-1     ", "  ", .false.) , &
         varatts( "BED  ", "BED       ", "Bottom roughness                                ", "m         ", "  ", .false.) , &
         varatts( "BED  ", "RIPPLEX   ", "Sea bottom ripple wavelength x                  ", "m         ", "  ", .false.) , &
         varatts( "BED  ", "RIPPLEY   ", "Sea bottom ripple wavelength y                  ", "m         ", "  ", .false.) , &
         varatts( "FBB  ", "PHIBBL    ", "Energy flux due to bottom friction              ", "W m-2     ", "  ", .false.) , &
         varatts( "TBB  ", "TAUBBLX   ", "Momentum flux due to bottom friction x          ", "m2 s-2    ", "  ", .false.) , &
         varatts( "TBB  ", "TAUBBLY   ", "Momentum flux due to bottom friction y          ", "m2 s-2    ", "  ", .false.)   &
         ]

    !  8   Spectrum parameters
    gridoutdefs(8,1:9) = [ &
         varatts( "MSS  ", "MSSX      ", "Surface mean square slope x                     ", "nd        ", "  ", .false.) , &
         varatts( "MSS  ", "MSSY      ", "Surface mean square slope y                     ", "nd        ", "  ", .false.) , &
         varatts( "MSC  ", "MSCX      ", "Spectral level at high frequency tail x         ", "nd        ", "  ", .false.) , &
         varatts( "MSC  ", "MSCY      ", "Spectral level at high frequency tail y         ", "nd        ", "  ", .false.) , &
         varatts( "WL02 ", "WL02X     ", "East/X North/Y mean wavelength component        ", "nd        ", "  ", .false.) , &
         varatts( "WL02 ", "WL02Y     ", "East/X North/Y mean wavelength component        ", "nd        ", "  ", .false.) , &
         varatts( "AXT  ", "ALPXT     ", "Correl sea surface gradients (x,t)              ", "nd        ", "  ", .false.) , &
         varatts( "AYT  ", "ALPYT     ", "Correl sea surface gradients (y,t)              ", "nd        ", "  ", .false.) , &
         varatts( "AXY  ", "ALPXY     ", "Correl sea surface gradients (x,y)              ", "nd        ", "  ", .false.)   &
         ]

    !  9   Numerical diagnostics
    gridoutdefs(9,1:5) = [ &
         varatts( "DTD  ", "DTDYN     ", "Average time step in integration                ", "min       ", "  ", .false.) , &
         varatts( "FC   ", "FCUT      ", "Cut-off frequency                               ", "s-1       ", "  ", .false.) , &
         varatts( "CFX  ", "CFLXYMAX  ", "Max. CFL number for spatial advection           ", "nd        ", "  ", .false.) , &
         varatts( "CFD  ", "CFLTHMAX  ", "Max. CFL number for theta-advection             ", "nd        ", "  ", .false.) , &
         varatts( "CFK  ", "CFLKMAX   ", "Max. CFL number for k-advection                 ", "nd        ", "  ", .false.)  &
         ]

    !  10   User defined
    gridoutdefs(10,1:2) = [ &
         varatts( "U1   ", "U1        ", "User defined 1                                  ", "nd        ", "  ", .false.) , &
         varatts( "U2   ", "U2        ", "User defined 2                                  ", "nd        ", "  ", .false.)  &
         ]
  end subroutine initialize_gridout
end module wav_grdout
