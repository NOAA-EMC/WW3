module wav_grdout

  use w3odatmd    , only: nogrp, ngrpp

  implicit none

  integer, parameter   :: maxvars = 24         ! maximum number of variables/group

  private ! except

  public :: varatts
  public :: outvars
  public :: wavinit_grdout

  type :: varatts
     character(len= 5) :: tag
     character(len=10) :: var_name
     character(len=48) :: long_name
     character(len= 5) :: unit_name
     character(len= 6) :: dims
     logical           :: validout
  end type

  type(varatts), dimension(nogrp,maxvars) :: gridoutdefs

  type(varatts), dimension(:), allocatable :: outvars

!===============================================================================
contains
!===============================================================================

  !====================================================================================
  subroutine wavinit_grdout

    use w3odatmd    , only: nds, iaproc, napout
    use w3iogomd    , only: fldout
    use w3servmd    , only: strsplit

    ! local variables
    character(len=100)      :: inptags(100) = ''
    integer                 :: j,k,n,nout

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
          write(nds(1),'(i5,a)')n,'  '//trim(outvars(n)%tag)//' '//trim(outvars(n)%var_name)//'  '// &
             trim(outvars(n)%long_name)
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

    ! TODO: replace nd units w/ correct values
    gridoutdefs(1,1:15) = [ &
    varatts( "DPT  ", "DW        ", "Water depth                                     ", "m    ", "x y   ", .false.) , &
    varatts( "CUR  ", "CX        ", "Mean current, x-component                       ", "m/s  ", "x y   ", .false.) , &
    varatts( "CUR  ", "CY        ", "Mean current, y-component                       ", "m/s  ", "x y   ", .false.) , &
    varatts( "WND  ", "UAX       ", "Mean wind, x-component                          ", "m/s  ", "x y   ", .false.) , &
    varatts( "WND  ", "UAY       ", "Mean wind, y-component                          ", "m/s  ", "x y   ", .false.) , &
    varatts( "AST  ", "AS        ", "Air-sea temperature difference                  ", "deg C", "x y   ", .false.) , &
    varatts( "WLV  ", "WLV       ", "Water levels                                    ", "m    ", "x y   ", .false.) , &
    varatts( "ICE  ", "ICE       ", "Ice coverage                                    ", "nd   ", "x y   ", .false.) , &
    varatts( "IBG  ", "BERG      ", "Iceberg-induced damping                         ", "nd   ", "x y   ", .false.) , &
    varatts( "TAU  ", "TAUAX     ", "Atm momentum x                                  ", "nd   ", "x y   ", .false.) , &
    varatts( "TAU  ", "TAUAY     ", "Atm momentum y                                  ", "nd   ", "x y   ", .false.) , &
    varatts( "RHO  ", "RHOAIR    ", "Air density                                     ", "nd   ", "x y   ", .false.) , &
    varatts( "D50  ", "SED_D50   ", "Median sediment grain size                      ", "nd   ", "x y   ", .false.) , &
    varatts( "IC1  ", "ICH       ", "Ice thickness                                   ", "nd   ", "x y   ", .false.) , &
    varatts( "IC5  ", "ICF       ", "Ice floe diameter                               ", "nd   ", "x y   ", .false.)   &
                       ]

    gridoutdefs(2,1:18) = [ &
    varatts( "HS   ", "HS        ", "Significant wave height                         ", "m    ", "x y   ", .false.) , &
    varatts( "LM   ", "WLM       ", "Mean wave length                                ", "m    ", "x y   ", .false.) , &
    varatts( "T02  ", "T02       ", "Mean wave period (Tm0,2)                        ", "s    ", "x y   ", .false.) , &
    varatts( "T0M1 ", "T0M1      ", "Mean wave period (Tm0,-1)                       ", "s    ", "x y   ", .false.) , &
    varatts( "T01  ", "T01       ", "Mean wave period (Tm0,1)                        ", "s    ", "x y   ", .false.) , &
    varatts( "FP   ", "FP0       ", "Peak frequency                                  ", "Hz   ", "x y   ", .false.) , &
    varatts( "DIR  ", "THM       ", "Mean wave direction                             ", "rad  ", "x y   ", .false.) , &
    varatts( "SPR  ", "THS       ", "Mean directional spread                         ", "rad  ", "x y   ", .false.) , &
    varatts( "DP   ", "THP0      ", "Peak direction                                  ", "rad  ", "x y   ", .false.) , &
    varatts( "HSIG ", "HSIG      ", "Infragravity height                             ", "nd   ", "x y   ", .false.) , &
    varatts( "MXE  ", "STMAXE    ", "Max surface elev (STE)                          ", "m    ", "x y   ", .false.) , &
    varatts( "MXES ", "STMAXD    ", "St Dev Max surface elev (STE)                   ", "m    ", "x y   ", .false.) , &
    varatts( "MXH  ", "HMAXE     ", "Max wave height (S.)                            ", "m    ", "x y   ", .false.) , &
    varatts( "MXHC ", "HCMAXE    ", "Max wave height from crest (STE)                ", "m    ", "x y   ", .false.) , &
    varatts( "SDMH ", "HMAXD     ", "St Dev of MXC (STE)                             ", "m    ", "x y   ", .false.) , &
    varatts( "SDMHC", "HCMAXD    ", "St Dev of MXHC (STE)                            ", "m    ", "x y   ", .false.) , &
    varatts( "WBT  ", "WBT       ", "Dominant wave breaking probability bT           ", "m    ", "x y   ", .false.) , &
    varatts( "TP   ", "FP0p      ", "Peak period (from peak freq)                    ", "Hz   ", "x y   ", .false.)   &
                       ]

    gridoutdefs(3,1:6) = [ &
    varatts( "EF   ", "EF        ", "1D spectral density                             ", "nd   ", "x y k ", .false.) , &
    varatts( "TH1M ", "TH1M      ", "Mean wave direction from a1,b2                  ", "nd   ", "x y k ", .false.) , &
    varatts( "STH1M", "STH1M     ", "Directional spreading from a1,b2                ", "nd   ", "x y k ", .false.) , &
    varatts( "TH2M ", "TH2M      ", "Mean wave direction from a2,b2                  ", "nd   ", "x y k ", .false.) , &
    varatts( "STH2M", "STH2M     ", "Directional spreading from a2,b2                ", "nd   ", "x y k ", .false.) , &
    varatts( "WN   ", "WN        ", "Wavenumber array                                ", "nd   ", "x y k ", .false.)   &
                       ]

    gridoutdefs(4,1:17) = [ &
    varatts( "PHS  ", "PHS       ", "Partitioned wave heights                        ", "m    ", "x y s ", .false.) , &
    varatts( "PTP  ", "PTP       ", "Partitioned peak period                         ", "s    ", "x y s ", .false.) , &
    varatts( "PLP  ", "PLP       ", "Partitioned peak wave length                    ", "m    ", "x y s ", .false.) , &
    varatts( "PDIR ", "PDIR      ", "Partitioned mean direction                      ", "nd   ", "x y s ", .false.) , &
    varatts( "PSPR ", "PSI       ", "Partitioned mean directional spread             ", "nd   ", "x y s ", .false.) , &
    varatts( "PWS  ", "PWS       ", "Partitioned wind sea fraction                   ", "nd   ", "x y s ", .false.) , &
    varatts( "PDP  ", "PDP       ", "Peak wave direction of partition                ", "nd   ", "x y s ", .false.) , &
    varatts( "PQP  ", "PQP       ", "Goda peakdedness parameter of partition         ", "nd   ", "x y s ", .false.) , &
    varatts( "PPE  ", "PPE       ", "JONSWAP peak enhancement factor of partition    ", "nd   ", "x y s ", .false.) , &
    varatts( "PGW  ", "PGW       ", "Gaussian frequency width of partition           ", "nd   ", "x y s ", .false.) , &
    varatts( "PSW  ", "PSW       ", "Spectral width of partition                     ", "nd   ", "x y s ", .false.) , &
    varatts( "PTM10", "PTM1      ", "Mean wave period (m-1,0) of partition           ", "nd   ", "x y s ", .false.) , &
    varatts( "PT01 ", "PT1       ", "Mean wave period (m0,1) of partition            ", "nd   ", "x y s ", .false.) , &
    varatts( "PT02 ", "PT2       ", "Mean wave period (m0,2) of partition            ", "nd   ", "x y s ", .false.) , &
    varatts( "PEP  ", "PEP       ", "Peak spectral density of partition              ", "nd   ", "x y s ", .false.) , &
    varatts( "TWS  ", "PWST      ", "Total wind sea fraction                         ", "nd   ", "x y   ", .false.) , &
    varatts( "PNR  ", "PNR       ", "Number of partitions                            ", "nd   ", "x y   ", .false.)   &
                       ]

    gridoutdefs(5,1:14) = [ &
    varatts( "UST  ", "USTX      ", "Friction velocity x                             ", "m/s  ", "x y   ", .false.) , &
    varatts( "UST  ", "USTY      ", "Friction velocity y                             ", "m/s  ", "x y   ", .false.) , &
    varatts( "CHA  ", "CHA       ", "Charnock parameter                              ", "nd   ", "x y   ", .false.) , &
    varatts( "CGE  ", "CGE       ", "Energy flux                                     ", "nd   ", "x y   ", .false.) , &
    varatts( "FAW  ", "PHIAW     ", "Air-sea energy flux                             ", "nd   ", "x y   ", .false.) , &
    varatts( "TAW  ", "TAUWIX    ", "Net wave-supported stress x                     ", "nd   ", "x y   ", .false.) , &
    varatts( "TAW  ", "TAUWIY    ", "Net wave-supported stress y                     ", "nd   ", "x y   ", .false.) , &
    varatts( "TWA  ", "TAUWNX    ", "Negative part of the wave-supported stress x    ", "nd   ", "x y   ", .false.) , &
    varatts( "TWA  ", "TAUWNY    ", "Negative part of the wave-supported stress y    ", "nd   ", "x y   ", .false.) , &
    varatts( "WCC  ", "WCC       ", "Whitecap coverage                               ", "nd   ", "x y   ", .false.) , &
    varatts( "WCF  ", "WCF       ", "Whitecap thickness                              ", "nd   ", "x y   ", .false.) , &
    varatts( "WCH  ", "WCH       ", "Mean breaking height                            ", "nd   ", "x y   ", .false.) , &
    varatts( "WCM  ", "WCM       ", "Whitecap moment                                 ", "nd   ", "x y   ", .false.) , &
    varatts( "FW   ", "FWS       ", "Wind sea mean period                            ", "nd   ", "x y   ", .false.)   &
                       ]

    gridoutdefs(6,1:24) = [ &
    varatts( "SXY  ", "SXX       ", "Radiation stresses xx                           ", "nd   ", "x y   ", .false.) , &
    varatts( "SXY  ", "SYY       ", "Radiation stresses yy                           ", "nd   ", "x y   ", .false.) , &
    varatts( "SXY  ", "SXY       ", "Radiation stresses xy                           ", "nd   ", "x y   ", .false.) , &
    varatts( "TWO  ", "TAUOX     ", "Wave to ocean momentum flux x                   ", "nd   ", "x y   ", .false.) , &
    varatts( "TWO  ", "TAUOY     ", "Wave to ocean momentum flux y                   ", "nd   ", "x y   ", .false.) , &
    varatts( "BHD  ", "BHD       ", "Bernoulli head (J term)                         ", "nd   ", "x y   ", .false.) , &
    varatts( "FOC  ", "PHIOC     ", "Wave to ocean energy flux                       ", "nd   ", "x y   ", .false.) , &
    varatts( "TUS  ", "TUSX      ", "Stokes transport x                              ", "nd   ", "x y   ", .false.) , &
    varatts( "TUS  ", "TUSY      ", "Stokes transport y                              ", "nd   ", "x y   ", .false.) , &
    varatts( "USS  ", "USSX      ", "Surface Stokes drift x                          ", "m/s  ", "x y   ", .false.) , &
    varatts( "USS  ", "USSY      ", "Surface Stokes drift y                          ", "m/s  ", "x y   ", .false.) , &
    varatts( "P2S  ", "PRMS      ", "Second-order sum pressure                       ", "nd   ", "x y   ", .false.) , &
    varatts( "P2S  ", "TPMS      ", "Second-order sum pressure                       ", "nd   ", "x y   ", .false.) , &
    varatts( "USF  ", "US3DX     ", "Spectrum of surface Stokes drift x              ", "nd   ", "x y k ", .false.) , &
    varatts( "USF  ", "US3DY     ", "Spectrum of surface Stokes drift y              ", "nd   ", "x y k ", .false.) , &
    varatts( "P2L  ", "P2SMS     ", "Micro seism  source term                        ", "nd   ", "x y m ", .false.) , &
    varatts( "TWI  ", "TAUICEX   ", "Wave to sea ice stress x                        ", "nd   ", "x y   ", .false.) , &
    varatts( "TWI  ", "TAUICEY   ", "Wave to sea ice stress y                        ", "nd   ", "x y   ", .false.) , &
    varatts( "FIC  ", "PHICE     ", "Wave to sea ice energy flux                     ", "nd   ", "x y   ", .false.) , &
    varatts( "USP  ", "USSPX     ", "Partitioned surface Stokes drift x              ", "nd   ", "x y p ", .false.) , &
    varatts( "USP  ", "USSPY     ", "Partitioned surface Stokes drift y              ", "nd   ", "x y p ", .false.) , &
    varatts( "TWC  ", "TAUOCX    ", "Wave to ?? stress x                             ", "nd   ", "x y   ", .false.) , &
    varatts( "TWC  ", "TAUOCY    ", "Wave to ?? stress y                             ", "nd   ", "x y   ", .false.) , &
    varatts( "LAN  ", "LANGMT    ", "Turbulent Langmuir number (La_t)                ", "nd   ", "x y   ", .false.)   &
                       ]

    gridoutdefs(7,1:8) = [ &
    varatts( "ABR  ", "ABAX      ", "Near bottom rms wave excursion amplitudes x     ", "m    ", "x y   ", .false.) , &
    varatts( "ABR  ", "ABAY      ", "Near bottom rms wave excursion amplitudes y     ", "m    ", "x y   ", .false.) , &
    varatts( "UBR  ", "UBAX      ", "Near bottom rms wave velocities x               ", "m/s  ", "x y   ", .false.) , &
    varatts( "UBR  ", "UBAY      ", "Near bottom rms wave velocities y               ", "m/s  ", "x y   ", .false.) , &
    varatts( "BED  ", "Bedforms  ", "Bedforms                                        ", "nd   ", "x y b ", .false.) , &
    varatts( "FBB  ", "PHIBBL    ", "Energy flux due to bottom friction              ", "nd   ", "x y   ", .false.) , &
    varatts( "TBB  ", "TAUBBLX   ", "Momentum flux due to bottom friction x          ", "nd   ", "x y   ", .false.) , &
    varatts( "TBB  ", "TAUBBLY   ", "Momentum flux due to bottom friction y          ", "nd   ", "x y   ", .false.)   &
                       ]

    gridoutdefs(8,1:9) = [ &
    varatts( "MSS  ", "MSSX      ", "Mean square slope x                             ", "nd   ", "x y   ", .false.) , &
    varatts( "MSS  ", "MSSY      ", "Mean square slope y                             ", "nd   ", "x y   ", .false.) , &
    varatts( "MSC  ", "MSCX      ", "Spectral level at high frequency tail x         ", "nd   ", "x y   ", .false.) , &
    varatts( "MSC  ", "MSCY      ", "Spectral level at high frequency tail y         ", "nd   ", "x y   ", .false.) , &
    varatts( "WL02 ", "WL02X     ", "East/X North/Y mean wavelength component        ", "nd   ", "x y   ", .false.) , &
    varatts( "WL02 ", "WL02Y     ", "East/X North/Y mean wavelength component        ", "nd   ", "x y   ", .false.) , &
    varatts( "AXT  ", "ALPXT     ", "Correl sea surface gradients (x,t)              ", "nd   ", "x y   ", .false.) , &
    varatts( "AYT  ", "ALPYT     ", "Correl sea surface gradients (y,t)              ", "nd   ", "x y   ", .false.) , &
    varatts( "AXY  ", "ALPXY     ", "Correl sea surface gradients (x,y)              ", "nd   ", "x y   ", .false.)   &
                       ]

    gridoutdefs(9,1:5) = [ &
    varatts( "DTD  ", "DTDYN     ", "Average time step in integration                ", "nd   ", "x y   ", .false.) , &
    varatts( "FC   ", "FCUT      ", "Cut-off frequency                               ", "nd   ", "x y   ", .false.) , &
    varatts( "CFX  ", "CFLXYMAX  ", "Max. CFL number for spatial advection           ", "nd   ", "x y   ", .false.) , &
    varatts( "CFD  ", "CFLTHMAX  ", "Max. CFL number for theta-advection             ", "nd   ", "x y   ", .false.) , &
    varatts( "CFK  ", "CFLKMAX   ", "Max. CFL number for k-advection                 ", "nd   ", "x y   ", .false.)  &
                       ]

    gridoutdefs(10,1:2) = [ &
    varatts( "U1   ", "U1        ", "User defined 1                                  ", "nd   ", "x y   ", .false.) , &
    varatts( "U2   ", "U2        ", "User defined 2                                  ", "nd   ", "x y   ", .false.)  &
                        ]
  end subroutine initialize_gridout
end module wav_grdout
