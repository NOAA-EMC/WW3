;/
;/                  +-----------------------------------+
;/                  | TWIST                Ifremer-SHOM |
;/                  |           F. Ardhuin              |
;/                  |              IDL command language |
;/                  | Last update :         17-Oct-2013 |
;/                  +-----------------------------------+
;/
;/ Licence information: This code is distributed under the CeCILL license
;/                      generally compatible with the Gnu Public Licence (GPL) 
;/                      http://www.cecill.info/index.en.html
;/
;/    17-Oct-2013 : Clean up                            ( version 2.00 )
;----------------------------------------------------------------------------
PRO parametros, anterior_root
;----------------------------------------------------------------------------
; This procedure generates the widgets of the parameters subwindow
; for the TWIST visualization tool
;----------------------------------------------------------------------------
COMMON AXISFRAME,axis_orient,frametype,outx1lab,outx2lab,outy1lab,outy2lab
COMMON CONTOURPARAM,numlevels,c_repart,lev,maxval,fixrange
COMMON COLORBARPAR,cbnticks,cbexrange,cbtrx,cbtry,cbblx,cbbly, $
   filloutofrange,addmini,addmaxi
COMMON DRAWING, Navailcolor,colorind,rangex,rangey,xtoy,filltype,logplot
COMMON DRAWSIZE,winx,winy,mwinx,mwiny,blx,bly,trx,try
COMMON FLAGS,   eqscale,cbar,clickflag,subwin
COMMON OVERLAY2,adbathydot,adcontour
COMMON POSTSCRIPT, filep,pspath,prcoul,psor,pstype, $
                pwinx,pwiny,papierx,papiery,xoffset,yoffset, $
                facpolice,fontrescale,basefontsize,pssizex,pssizey,psfont
COMMON THREED,  Ax3D,Az3D,smoothing
COMMON TIMESERIES, modspec,obs,obs2,om,ts_filetype, nbins, bulktype,scatvar
COMMON TITLES,  font,outtit,outxtit,outytit,outcbtit,textx,texty,textdx,textdy
COMMON WPARAM,WParray,root,WParam5,WParam6

	pbase=LONG(0)
	vecbase=LONG(0)
	Line1=LONG(0)
   WPbox=lonarr(8)
	WPline=LONARR(15)
   WParray=LONARR(15,16)

; Guardar valores en variables temporales
	root=anterior_root
;	WIDGET_CONTROL, root, SENSITIVE=0


   pbase = WIDGET_BASE(TITLE='Display and Print parameters', /COLUMN)
   WIDGET_CONTROL, pbase, GROUP_LEADER=root
      i=0
      WPbox(i)=WIDGET_BASE(pbase, /FRAME, /COLUMN)
      j=0
      WPline(j)=WIDGET_BASE(WPbox(i), /ROW)
         WParray(j,0)=WIDGET_LABEL(WPline(j),VALUE='CONTOUR Num. Levels: ')
         WParray(j,1)=WIDGET_TEXT(WPline(j),/EDITABLE, XSIZE=5, YSIZE=1, $
            VALUE=STRCOMPRESS(String(numlevels)))
         WParray(j,2)= WIDGET_DROPLIST(WPline(j), $
            VALUE=['linear','log4','log6','duck shelf','Shields Number'])
         WParray(j,3)= WIDGET_DROPLIST(WPline(j), $
            VALUE=['no contour','solid','dotted','dashed','solid/dashed'])
      j=1
      WPline(j)=WIDGET_BASE(WPbox(i), /ROW)
         WParray(j,0)= WIDGET_DROPLIST(WPline(j), $
            VALUE=['Plot data','Plot log10(data)'])
         WParray(j,1)= WIDGET_DROPLIST(WPline(j), $
            VALUE=['Fit range to data','Fixed range'])
         WParray(j,2)= WIDGET_DROPLIST(WPline(j), $
            VALUE=['Fill out of range','Blank out of range'])

     i=1
     WPbox(i)=WIDGET_BASE(pbase, /FRAME, /COLUMN)
     j=2
     WPline(j)=WIDGET_BASE(WPbox(i), /ROW)
        WParray(j,0)=WIDGET_LABEL(WPline(j),VALUE='3D views:')
        WParray(j,1)=WIDGET_SLIDER(WPline(j), MAXIMUM=180, MINIMUM=0, value=ax3D, $
             TITLE='Ax')
        WParray(j,2)=WIDGET_SLIDER(WPline(j), MAXIMUM=360, MINIMUM=0, value=az3D, $
             TITLE='Az')
        WParray(j,3)=WIDGET_LABEL(WPline(j),VALUE='Smooth:')
        WParray(j,4)=WIDGET_TEXT(WPline(j),/EDITABLE, $
            XSIZE=4,YSIZE=1,VALUE=strcompress(string(smoothing)))
        WParray(j,5)=WIDGET_LABEL(WPline(j),VALUE='N. bins:')
        WParray(j,6)=WIDGET_TEXT(WPline(j),/EDITABLE, $
            XSIZE=4,YSIZE=1,VALUE=strcompress(string(nbins)))

     i=i+1
     WPbox(i)=WIDGET_BASE(pbase, /FRAME, /COLUMN)
     j=3
     WPline(j)=WIDGET_BASE(WPbox(i), /ROW)
	      WParray(j,0)=WIDGET_LABEL(WPline(j),VALUE='Title:')
	      WParray(j,1)=WIDGET_DROPLIST(WPline(j),VALUE=['YES','NO'])
	      WParray(j,2)=WIDGET_LABEL(WPline(j),VALUE='xTitle:')
	      WParray(j,3)=WIDGET_DROPLIST(WPline(j),VALUE=['YES','NO'])
	      WParray(j,4)=WIDGET_LABEL(WPline(j),VALUE='yTitle:')
	      WParray(j,5)=WIDGET_DROPLIST(WPline(j),VALUE=['YES','NO'])
	      WParray(j,6)=WIDGET_LABEL(WPline(j),VALUE='cbTitle:')
	      WParray(j,7)=WIDGET_DROPLIST(WPline(j),VALUE=['YES','NO'])
         WParray(j,8)=WIDGET_BUTTON(WPline(j),VALUE='Font')
     j=4
     WPline(j)=WIDGET_BASE(WPbox(i), /ROW)
         WParray(j,0)=WIDGET_LABEL(WPline(j),VALUE='Color bar:')
	      WParray(j,1)=WIDGET_DROPLIST(WPline(j),VALUE=['on','off'])
	      WParray(j,2)=WIDGET_DROPLIST(WPline(j),VALUE=['exact range','rounded range'])
	      WParray(j,3)=WIDGET_LABEL(WPline(j),VALUE='N ticks:')
         WParray(j,4)=WIDGET_TEXT(WPline(j),/EDITABLE, $
            XSIZE=4,YSIZE=1,VALUE=strcompress(string(cbnticks)))

      i=i+1
      WPbox(i)=WIDGET_BASE(pbase, /FRAME, /COLUMN)
      j=5
      WPline(j)=WIDGET_BASE(WPbox(i), /ROW)
	      WParray(j,0)=WIDGET_LABEL(WPline(j),VALUE='blx:')
         WParray(j,1)=WIDGET_TEXT(WPline(j), $
            XSIZE=5,VALUE=STRING(blx,FORMAT='(F5.3)'),/EDITABLE)
         WParray(j,2)=WIDGET_LABEL(WPline(j),VALUE='bly:')
         WParray(j,3)=WIDGET_TEXT(WPline(j), $
            XSIZE=5,VALUE=STRING(bly,FORMAT='(F5.3)'),/EDITABLE)
         WParray(j,4)=WIDGET_LABEL(WPline(j),VALUE='trx:')
         WParray(j,5)=WIDGET_TEXT(WPline(j), $
            XSIZE=5,VALUE=STRING(trx,FORMAT='(F5.3)'),/EDITABLE)
         WParray(j,6)=WIDGET_LABEL(WPline(j),VALUE='try:')
         WParray(j,7)=WIDGET_TEXT(WPline(j), $
            XSIZE=5,VALUE=STRING(try,FORMAT='(F5.3)'),/EDITABLE)
         WParray(j,8)=WIDGET_LABEL(WPline(j),VALUE='tx:')
         WParray(j,9)=WIDGET_TEXT(WPline(j), $
            XSIZE=5,VALUE=STRING(textx,FORMAT='(F5.3)'),/EDITABLE)
         WParray(j,10)=WIDGET_LABEL(WPline(j),VALUE='ty:')
         WParray(j,11)=WIDGET_TEXT(WPline(j), $
            XSIZE=5,VALUE=STRING(texty,FORMAT='(F5.3)'),/EDITABLE)
         WParray(j,12)=WIDGET_LABEL(WPline(j),VALUE='tdy:')
         WParray(j,13)=WIDGET_TEXT(WPline(j), $
            XSIZE=5,VALUE=STRING(textdx,FORMAT='(F5.3)'),/EDITABLE)
         WParray(j,14)=WIDGET_LABEL(WPline(j),VALUE='tdy:')
         WParray(j,15)=WIDGET_TEXT(WPline(j), $
            XSIZE=5,VALUE=STRING(textdy,FORMAT='(F5.3)'),/EDITABLE)
      j=6
      WPline(j)=WIDGET_BASE(WPbox(i), /ROW)
	      WParray(j,0)=WIDGET_LABEL(WPline(j),VALUE='cbblx:')
         WParray(j,1)=WIDGET_TEXT(WPline(j), $
            XSIZE=5,VALUE=STRING(cbblx,FORMAT='(F5.3)'),/EDITABLE)
         WParray(j,2)=WIDGET_LABEL(WPline(j),VALUE='cbbly:')
         WParray(j,3)=WIDGET_TEXT(WPline(j), $
            XSIZE=5,VALUE=STRING(cbbly,FORMAT='(F5.3)'),/EDITABLE)
         WParray(j,4)=WIDGET_LABEL(WPline(j),VALUE='cbtrx:')
         WParray(j,5)=WIDGET_TEXT(WPline(j), $
            XSIZE=5,VALUE=STRING(cbtrx,FORMAT='(F5.3)'),/EDITABLE)
         WParray(j,6)=WIDGET_LABEL(WPline(j),VALUE='cbtry:')
         WParray(j,7)=WIDGET_TEXT(WPline(j), $
            XSIZE=5,VALUE=STRING(cbtry,FORMAT='(F5.3)'),/EDITABLE)
         WParray(j,8)=WIDGET_LABEL(WPline(j),VALUE='Orientation:')
         WParray(j,9)=WIDGET_DROPLIST(WPline(j),VALUE=['Portrait','Landscape'])

      i=i+1
      WPbox(i)=WIDGET_BASE(pbase, /FRAME, /COLUMN)
      j=j+1
      WPline(j)=WIDGET_BASE(WPbox(i), /ROW)
         WParray(j,0)=WIDGET_LABEL(WPline(j),VALUE='Print settings:')
         WParray(j,1)=WIDGET_DROPLIST(WPline(j),VALUE=['Portrait','Landscape'])
         WParray(j,2)=WIDGET_LABEL(WPline(j),VALUE='font:')
         WParray(j,3)=WIDGET_DROPLIST(WPline(j),VALUE=['Helvetica','Times'])
      j=8
      WPline(j)=WIDGET_BASE(WPbox(i), /ROW)
         WParray(j,0)=WIDGET_TEXT(WPline(j), $
            VALUE='Print dimensions in cm (Max:'+ $
	         strcompress(string(papierx-2*xoffset))+'*'+ $
	         strcompress(string(papiery-2*yoffset))+'):')
	      WParray(j,1)=WIDGET_TEXT(WPline(j),/EDITABLE, $
	         VALUE=strcompress(string(pwinx)),YSIZE=1,XSIZE=5)
	      WParray(j,2)=WIDGET_LABEL(WPline(j),VALUE='*')
	      WParray(j,3)=WIDGET_TEXT(WPline(j),/EDITABLE,VALUE= $
            strcompress(string(pwiny)),YSIZE=1,XSIZE=5)
      j=j+1
      WPline(j)=WIDGET_BASE(WPbox(i), /ROW)
	      WParray(j,0)=WIDGET_LABEL(WPline(j),VALUE='Font size:')
	      WParray(j,1)=WIDGET_TEXT(WPline(j),/EDITABLE, $
	         VALUE=strcompress(string(facpolice)),YSIZE=1,XSIZE=5)
         WParray(j,6)=WIDGET_DROPLIST(WPline(j), $
         VALUE=['Rescale font','no rescale'])
	      WParray(j,7)=WIDGET_LABEL(WPline(j),VALUE='Base size(pt):')
	      WParray(j,8)=WIDGET_TEXT(WPline(j),/EDITABLE, $
	         VALUE=string(basefontsize,FORMAT='(I2)'),YSIZE=1,XSIZE=2)
         WParray(j,2)=WIDGET_LABEL(WPline(j),VALUE='xoffset(cm):')
	      WParray(j,3)=WIDGET_TEXT(WPline(j),/EDITABLE, $
	         VALUE=strcompress(string(xoffset)),YSIZE=1,XSIZE=5)
	      WParray(j,4)=WIDGET_LABEL(WPline(j),VALUE='yoffset(cm):')
	      WParray(j,5)=WIDGET_TEXT(WPline(j),/EDITABLE, $
	         VALUE=strcompress(string(yoffset)),YSIZE=1,XSIZE=5)
      i=i+1
      WPbox(i)=WIDGET_BASE(pbase, /FRAME, /COLUMN)
      j=j+1
      WPline(j)=WIDGET_BASE(WPbox(i), /ROW)
	      WParray(j,0)=WIDGET_LABEL(WPline(j),VALUE='Frame:')
	      WParray(j,1)=WIDGET_DROPLIST(WPline(j),VALUE=['X-Y','X,Lon-Y,Lat'])
	      WParray(j,2)=WIDGET_LABEL(WPline(j),VALUE='x1lab:')
	      WParray(j,3)=WIDGET_DROPLIST(WPline(j),VALUE=['YES','NO'])
	      WParray(j,4)=WIDGET_LABEL(WPline(j),VALUE='x2lab:')
	      WParray(j,5)=WIDGET_DROPLIST(WPline(j),VALUE=['YES','NO'])
         WParray(j,6)=WIDGET_LABEL(WPline(j),VALUE='y1lab:')
	      WParray(j,7)=WIDGET_DROPLIST(WPline(j),VALUE=['YES','NO'])
	      WParray(j,8)=WIDGET_LABEL(WPline(j),VALUE='y2lab:')
	      WParray(j,9)=WIDGET_DROPLIST(WPline(j),VALUE=['YES','NO'])



      j=11
      WPline(j)=WIDGET_BASE(WPbox(i), /ROW)
         WParray(j,0)=WIDGET_BUTTON(pbase, VALUE='CLOSE')

	WIDGET_CONTROL, /REALIZE, pbase
   XMANAGER, 'PARAMETER', pbase
   subwin(0)=1
RETURN
END


;-----------------------------------------------------------------------------
PRO parameter_event, ev
COMMON AXISFRAME,axis_orient,frametype,outx1lab,outx2lab,outy1lab,outy2lab
COMMON CONTOURPARAM,numlevels,c_repart,lev,maxval,fixrange
COMMON COLORBARPAR,cbnticks,cbexrange,cbtrx,cbtry,cbblx,cbbly, $
   filloutofrange,addmini,addmaxi
COMMON DRAWING, Navailcolor,colorind,rangex,rangey,xtoy,filltype,logplot
COMMON DRAWSIZE,winx,winy,mwinx,mwiny,blx,bly,trx,try
COMMON FILES,   filestatus,datastatus,paths,filters,filenames,raypath
COMMON FLAGS,   eqscale,cbar,clickflag,subwin
COMMON THREED,  Ax3D,Az3D,smoothing
COMMON TITLES,  font,outtit,outxtit,outytit,outcbtit,textx,texty,textdx,textdy
COMMON POSTSCRIPT, filep,pspath,prcoul,psor,pstype, $
                pwinx,pwiny,papierx,papiery,xoffset,yoffset, $
                facpolice,fontrescale,basefontsize,pssizex,pssizey,psfont
COMMON WIDGETS, Wtoprow,Wright,Wdraw,Wdraw_value_update,Wsimple
COMMON WPARAM,WParray,root,WParam5,WParam6
COMMON TIMESERIES, modspec,obs,obs2,om,ts_filetype, nbins, bulktype,scatvar
COMMON WW3,     ww3date,ww3time,ww3fieldname,ww3scale,ww3miss,ww3unit,ww3lon, $
               ww3lat,ww3matrix,ww3dir,ww3path
COMMON OVERLAY, addir,adsyms,adbathy,adcoast,psyms,psymsizes,adtr,adtri
COMMON OVERLAY2,adbathydot,adcontour
COMMON BATHY,   gd,nx,ny,dx,dy,sx,sy,rlonmax,rlonmin,rlatmin,rlatmax
COMMON FREQ,    freq,nfband,findex,findex2
COMMON DIR,     nabin,nabint,aindex,THETA
COMMON TIME,    timestep,tindex,tindex2,ntime,dtime,time0,day0,dtindex,nstep, $
                timezone_plot,timezone_string,months,time
COMMON ZOOM,    nxzmax,nyzmax,nxzmin,nyzmin,maxdepth,mindepth
COMMON CURRENT, datatype,plottype,line,column,c_numlev,output,plotncvar,normvec
COMMON SPECIALS,nspecgp,specmat,specname,c_spec,ispec1,ispec2
common colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr
COMMON HScalc,Hsc
;*******END OF COMMON BLOCKS*******************************

	type = TAG_NAMES(ev, /STRUCTURE)
	CASE type OF
   'WIDGET_BUTTON': BEGIN
      WIDGET_CONTROL, ev.id, GET_VALUE=value
      CASE value OF
      'CLOSE': BEGIN
         WIDGET_CONTROL, /DESTROY, ev.top
		   RETURN
         END
      'Font': BEGIN
            font=XFONT(GROUP=ev.top,/PRESERVE)
            print,font
            WIDGET_CONTROL, GET_VALUE=drawval, Wdraw
            WSET, drawval
            END
      ENDCASE
      END

   'WIDGET_TEXT_CH': BEGIN
      WIDGET_CONTROL, ev.id, GET_VALUE=value
      print,ev.id
      CASE ev.id OF
      WParray(0,1): numlevels=FIX(value(0))
      WParray(2,4): smoothing=FIX(value(0))
      WParray(2,6): nbins=FIX(value(0))
      WParray(4,4): cbnticks=FIX(value(0))
      WParray(8,1): pwinx=FLOAT(value(0))
      WParray(8,3): pwiny=FLOAT(value(0))
      WParray(9,1): facpolice=FLOAT(value(0))
      WParray(9,8): basefontsize=FIX(value(0))
      WParray(9,3): xoffset=FLOAT(value(0))
      WParray(9,5): yoffset=FLOAT(value(0))
      WParray(5,1): blx=FLOAT(value(0))
      WParray(5,3): bly=FLOAT(value(0))
      WParray(5,5): trx=FLOAT(value(0))
      WParray(5,7): try=FLOAT(value(0))
      WParray(5,9): textx=FLOAT(value(0))
      WParray(5,11): texty=FLOAT(value(0))
      WParray(5,13): textdx=FLOAT(value(0))
      WParray(5,15): textdy=FLOAT(value(0))
      WParray(6,1): cbblx=FLOAT(value(0))
      WParray(6,3): cbbly=FLOAT(value(0))
      WParray(6,5): cbtrx=FLOAT(value(0))
      WParray(6,7): cbtry=FLOAT(value(0))
      ENDCASE
      END
	'WIDGET_DROPLIST':BEGIN
      CASE ev.id of
      WParray(0,2): c_repart=ev.index
      WParray(0,3): adcontour=ev.index
      WParray(1,0): logplot=ev.index
      WParray(1,1): fixrange=ev.index
      WParray(1,2): filloutofrange=1-ev.index
      WParray(3,1): outtit=1-ev.index
      WParray(3,3): outxtit=1-ev.index
      WParray(3,5): outytit=1-ev.index
      WParray(3,7): outcbtit=1-ev.index
      WParray(4,1): cbar=1-ev.index
      WParray(4,2): cbexrange=1-ev.index
      WParray(6,9): axis_orient=ev.index
      WParray(7,1): psor=ev.index
      WParray(7,3): psfont=ev.index
      WParray(9,6):fontrescale=1-ev.index
      WParray(10,1): frametype=ev.index
      WParray(10,3): outx1lab=1-ev.index
      WParray(10,5): outx2lab=1-ev.index
      WParray(10,7): outy1lab=1-ev.index
      WParray(10,9): outy2lab=1-ev.index
      ENDCASE
      print,'TS:', ts_filetype
      END
   'WIDGET_SLIDER': BEGIN
      WIDGET_CONTROL, ev.id, GET_VALUE=value
      CASE (ev.id) OF
         WParray(2,1): Ax3D=FLOAT(value)
         WParray(2,2): Az3D=FLOAT(value)
      ENDCASE
      END
   ENDCASE
RETURN
END


