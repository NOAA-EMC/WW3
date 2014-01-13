PRO Mapping, anterior_root
; This procedure creates the widged for the "mapping parameters" subwindow
;*******COMMON BLOCKS**************************************
;** 1 ** Display parameters
COMMON FLAGS,   eqscale,cbar,clickflag,subwin
COMMON MAP,     MAPFLAG, MAPPROJ, MAPLONGCENTER, MAPLATCENTER, MAPCONTINENT,  $
                MAPCOUNTRIES, MAPLONLAT
;** 4 ** subwindow widgets widgets
COMMON WMap,WMapping,rootmtool
;*******END OF COMMON BLOCKS*******************************
   pbase=LONG(0)
	vecbase=LONG(0)
	WMLines=LONARR(10)
	WMapping=LONARR(10,10)

; Guardar valores en variables temporales
	rootmtool=anterior_root
;	WIDGET_CONTROL, root, SENSITIVE=0


   pbase = WIDGET_BASE(TITLE='Mapping parameters', /COLUMN)

        i=0
        WMLines(i)=WIDGET_BASE(pbase, /FRAME, /ROW)
        WMapping(i,0)=WIDGET_DROPLIST(WMLines(i), $
            VALUE=['No map projection','Use map projection'])
        WMapping(i,1)=WIDGET_DROPLIST(WMLines(i), $
            VALUE=['Orthographic','Mollweide','Mercator','Azimuthal'])
        WMapping(i,2)=WIDGET_DROPLIST(WMLines(i), $
            VALUE=['No continents','Continents outline','Filled continents'])
        i=1
        WMLines(i)=WIDGET_BASE(pbase, /FRAME, /ROW)
        WMapping(i,0) = WIDGET_SLIDER(WMlines(I), MAXIMUM=180, MINIMUM=-180, $
         value=MAPLONGCENTER,TITLE='Longitude of center')
        WMapping(i,1) = WIDGET_SLIDER(WMlines(I), MAXIMUM=90, MINIMUM=-90, $
         value=MAPLATCENTER,TITLE='Latitude of center')
        i=i+1
        WMLines(i)= WIDGET_BUTTON(pbase, VALUE='OK')
	WIDGET_CONTROL, /REALIZE, pbase
        XMANAGER, 'Mappingg', pbase
        subwin(6)=1
RETURN
END


;----------------------------------------------------------------------------
PRO Mappingg_event, ev
;*******COMMON BLOCKS**************************************
;** 1 ** Display parameters
COMMON FLAGS,   eqscale,cbar,clickflag,subwin
COMMON MAP,     MAPFLAG, MAPPROJ, MAPLONGCENTER, MAPLATCENTER, MAPCONTINENT,  $
                MAPCOUNTRIES, MAPLONLAT
;** 4 ** subwindow widgets widgets
COMMON WMap,WMapping,rootmtool
;*******END OF COMMON BLOCKS*******************************

	type = TAG_NAMES(ev, /STRUCTURE)
   ;print,'type:',type
   CASE type OF
   'WIDGET_BUTTON': BEGIN
      WIDGET_CONTROL, ev.id, GET_VALUE=value
      CASE value OF
      'OK': BEGIN
            WIDGET_CONTROL, /DESTROY, ev.top
		   WIDGET_CONTROL,rootmtool, SENSITIVE=1
            subwin(6)=0
            RETURN
	    END
      ENDCASE
      END
   'WIDGET_SLIDER': BEGIN
      WIDGET_CONTROL, ev.id, GET_VALUE=value
      CASE (ev.id) OF
         WMapping(1,0): MAPLONGCENTER=FLOAT(value)
         WMapping(1,1): MAPLATCENTER=FLOAT(value)
      ENDCASE
      doplot
      END
    'WIDGET_DROPLIST':BEGIN
      CASE ev.id of
      WMapping(0,0): BEGIN
         MAPFLAG=ev.index
         doplot
         END
      WMapping(0,1): BEGIN
         MAPPROJ=ev.index
         doplot
         END
      WMapping(0,2): BEGIN
         MAPCONTINENT=ev.index
         doplot
         END
      ENDCASE
      END
   ENDCASE
RETURN
END


