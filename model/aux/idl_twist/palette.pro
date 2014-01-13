; $Id: xpalette.pro,v 1.18 1998/08/20 15:02:24 alan Exp $
;
; Copyright (c) 1992-1998, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;       Modified by Fabrice Ardhuin, August 1999
;+
; NAME:
;       XPALETTE
;
; PURPOSE:
;       Interactively create color tables using the RGB, CMY, HSV, and
;       HLS color systems using the mouse, three sliders, and a cell
;       for each color index. Single colors can be defined or multiple
;       color indices between two endpoints can be interpolated.
;
; CATEGORY:
;       Color tables, widgets.
;
; CALLING SEQUENCE:
;       XPALETTE
;
; INPUTS:
;       No explicit inputs.  The current color table is used as a starting
;       point.
;
; KEYWORD PARAMETERS:
;	BLOCK:  Set this keyword to have XMANAGER block when this
;		application is registered.  By default the Xmanager
;               keyword NO_BLOCK is set to 1 to provide access to the
;               command line if active command 	line processing is available.
;               Note that setting BLOCK for this application will cause
;		all widget applications to block, not only this
;		application.  For more information see the NO_BLOCK keyword
;		to XMANAGER.
;       UPDATECALLBACK: Set this keyword to a string containing the name of
;               a user-supplied procedure that will be called when the color
;               table is updated by XLOADCT.  The procedure may optionally
;               accept a keyword called DATA, which will be automatically
;               set to the value specified by the optional UPDATECBDATA
;               keyword.
;       UPDATECBDATA: Set this keyword to a value of any type. It will be
;               passed via the DATA keyword to the user-supplied procedure
;               specified via the UPDATECALLBACK keyword, if any. If the
;               UPDATECBDATA keyword is not set the value accepted by the
;               DATA keyword to the procedure specified by UPDATECALLBACK
;               will be undefined.
;
; OUTPUTS:
;       None.
;
; COMMON BLOCKS:
;       COLORS: Contains the current RGB color tables.
;       XP_COM: Private to this module.
;
; SIDE EFFECTS:
;       XPALETTE uses two colors from the current color table as
;       drawing foreground and background colors. These are used
;       for the RGB plots on the left, and the current index marker on
;       the right. This means that if the user set these two colors
;       to the same value, the XPALETTE display could become unreadable
;       (like writing on black paper with black ink). XPALETTE minimizes
;       this possibility by noting changes to the color map and always
;       using the brightest available color for the foreground color
;       and the darkest for the background. Thus, the only way
;       to make XPALETTE's display unreadable is to set the entire color
;       map to a single color, which is highly unlikely. The only side
;       effect of this policy is that you may notice XPALETTE redrawing
;       the entire display after you've modified the current color.
;       This simply means that the change has made XPALETTE pick new
;       drawing colors.
;
;       The new color tables are saved in the COLORS common and loaded
;       to the display.
;
; PROCEDURE:
;       The XPALETTE widget has the following controls:
;
;       Left:   Three plots showing the current Red, Green, and Blue vectors.
;
;       Center: A status region containing:
;               1) The total number of colors.
;               2) The current color. XPALETTE allows changing
;                  one color at a time. This color is known as
;                  the "current color" and is indicated in the
;                  color spectrum display with a special marker.
;               3) The current mark index. The mark is used to
;                  remember a color index. It is established by
;                  pressing the "Set Mark Button" while the current
;                  color index is the desired mark index.
;               4) The current color. The special marker used in
;                  color spectrum display prevents the user from seeing
;                  the color of the current index, but it is visible
;                  here.
;
;               A panel of control buttons, which do the following when
;               pressed:
;
;               Done:   Exits XPALETTE.
;
;         Predefined:   Starts XLOADCT to allow selection of one of the
;                       predefined color tables.
;
;               Help:   Supplies help information similar to this header.
;
;               Redraw: Completely redraws the display using the current
;                       state of the color map.
;
;             Set Mark: Set the value of the mark index to the
;                       current index.
;
;          Switch Mark: Exchange the mark and the current index.
;
;         Copy Current: Every color lying between the current
;                       index and the mark index (inclusive) is given
;                       the current color.
;
;          Interpolate: The colors lying between the current
;                       index and the mark index are interpolated linearly
;                       to lie between the colors of two endpoints.
;
;       Three sliders (R, G, and B) that allow the user to modify the
;       current color.
;
;       Right:  A display which shows the current color map as a series of
;               squares. Color index 0 is at the upper left. The color index
;               increases monotonically by rows going left to right and top
;               to bottom.  The current color index is indicated by a special
;               marker symbol. There are 4 ways to change the current color:
;                       1) Press any mouse button while the mouse
;                          pointer is over the color map display.
;                       2) Use the "By Index" slider to move to
;                          the desired color index.
;                       3) Use the "Row" Slider to move the marker
;                          vertically.
;                       4) Use the "Column" Slider to move the marker
;                          horizontally.
;
; MODIFICATION HISTORY:
;       July 1990, AB.          Based on the PALETTE procedure, which does
;                               similar things using only basic IDL graphics
;                               commands.
;
;       7 January 1991, Re-written for general use.
;       1 April 1992, Modified to use the CW_RGBSLIDER and CW_COLORSEL
;               compound widgets. The use of color systems other than
;               RGB is now supported.
;       15 June 1992, Modified to use the CW_FIELD and CW_BGROUP compound
;               widgets.
;       7 April 1993, Removed state caching. Fixed a bug where switching
;		the current index and the mark would fail to update the
;		current index label.
;       10 March 1997, Added !X.TYPE and !Y.TYPE to saved state.
;       8/20/98, ACY - Added UPDATECALLBACK and UPDATECBDATA keywords.
;       4/21/99 added writing file
;-

function XP_NEW_COLORS
; Choose the best foreground and background colors for the current
; color maps and set !P appropriately. Returns 1 if the colors changed,
; 0 otherwise.
  common xp_com, xpw, state

  res = 0
  junk = CT_LUMINANCE(dark=dark_col, bright=bright_col)

  if (bright_col ne !p.color) then begin
    !p.color = bright_col
    res = 1
  endif

  if (dark_col ne !p.background) then begin
    !p.background = dark_col
    res = 1
  endif

  return, res
end

pro XP_ALERT_CALLER

  common xp_com, xpw, state

  ErrorStatus = 0
  CATCH, ErrorStatus
  if (ErrorStatus NE 0) then begin
    CATCH, /CANCEL
    v = DIALOG_MESSAGE(['Unexpected error in XPALETTE:', $
                        '!ERR_STRING = ' + !ERR_STRING], $
                        /ERROR)
    return
  endif
  if (STRLEN(state.updt_callback) gt 0) then begin
    if (PTR_VALID(state.p_updt_cb_data)) then begin
      CALL_PROCEDURE, state.updt_callback, DATA=*(state.p_updt_cb_data)
    endif else begin
      CALL_PROCEDURE, state.updt_callback
    endelse
  endif
end

pro XP_XLCTCALLBACK
  ; For visuals with static colormaps, update the graphics
  ; after a change by XLOADCT.
  if ((COLORMAP_APPLICABLE(redrawRequired) GT 0) and $
        (redrawRequired GT 0)) then begin
    XP_REDRAW
  endif

end

pro XP_REDRAW

  common xp_com, xpw, state

  junk = XP_NEW_COLORS()
  WIDGET_CONTROL, xpw.colorsel, set_value=-1
  XP_REPLOT, !p.color, 'F'        ; Update the plots of RGB
  ; Let the caller of XPALETTE know that the color table was modified
  XP_ALERT_CALLER
end



pro XP_REPLOT, color_index, type
; Re-draw the RGB plots. Type has the following possible values.
;;      - 'D': Draw the data part of all three plots
;       - 'F': draw all three plots
;       - 'R': Draw the data part of the Red plot
;       - 'G': Draw the data part of the Green plot
;       - 'B': Draw the data part of the Blue plot

  common xp_com, xpw, state
  common colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr
  common pscale, r_x_s, r_y_s, g_x_s, g_y_s, b_x_s, b_y_s

  ; Update the plots of RGB
  save_win = !D.WINDOW
  wset, state.plot_win
  save_p_region = !p.region
  save_x_margin = !x.margin
  save_y_margin = !y.margin
  save_x_s = !x.s
  save_y_s = !y.s
  save_x_type = !x.type
  save_y_type = !y.type

  !y.margin= [2, 2]
  !x.margin= [6, 2]

  if (type eq 'F') then begin
    !p.region = [0,.6667, 1, 1]
    plot,xstyle=2, ystyle=3, yrange=[0, 260], r_curr, title='Red'
    r_x_s = !x.s
    r_y_s = !y.s

    !p.region = [0,.333, 1, .6667]
    plot,/noerase, xstyle=2,ystyle=3, yrange=[0, 260], g_curr, title='Green'
    g_x_s = !x.s
    g_y_s = !y.s

    !p.region = [0,0, 1, .333]
    plot,/noerase, xstyle=2,ystyle=3, yrange=[0, 260], b_curr, title='Blue'

    b_x_s = !x.s
    b_y_s = !y.s
  endif else begin
    if ((type eq 'D') or (type eq 'R')) then begin
      !p.region = [0,.6667, 1, 1]
      !x.s = r_x_s
      !y.s = r_y_s
      oplot, r_curr, color=color_index
    endif
    if ((type eq 'D') or (type eq 'G')) then begin
      !p.region = [0,.333, 1, .6667]
      !x.s = g_x_s
      !y.s = g_y_s
      oplot, g_curr, color=color_index
    endif
    if ((type eq 'D') or (type eq 'B')) then begin
      !p.region = [0,0, 1, .333]
      !x.s = b_x_s
      !y.s = b_y_s
      oplot, b_curr, color=color_index
    endif
  endelse

  empty
  WSET, save_win
  !p.region = save_p_region
  !x.margin = save_x_margin
  !y.margin = save_y_margin
  !x.s = save_x_s
  !y.s = save_y_s
  !x.type = save_x_type
  !y.type = save_y_type

end




pro XP_CHANGE_COLOR, type, value
; Change current color. Type has the following possible values.
;       - 'R': Change the R part of the current color
;       - 'G': ...
;       - 'B': ...
  common xp_com, xpw, state
  common colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr


  cur_idx = state.cur_idx

  XP_REPLOT, !p.background, type

  if (type eq 'R') then r_curr[cur_idx] = value;
  if (type eq 'G') then g_curr[cur_idx] = value;
  if (type eq 'B') then b_curr[cur_idx] = value;

  tvlct, r_curr[cur_idx], g_curr[cur_idx], b_curr[cur_idx], cur_idx

  if (XP_NEW_COLORS()) then begin
    ; Highlight the current position using the marker
    WIDGET_CONTROL, xpw.colorsel, set_value=-1  ; Re-initialize
    XP_REPLOT, !p.color, 'F'
  endif else begin
    XP_REPLOT, !p.color, type
  endelse

  ; For visuals with static colormaps, update the graphics
  ; of the current color.
  if ((COLORMAP_APPLICABLE(redrawRequired) GT 0) and $
        (redrawRequired GT 0)) then begin
    ; Mark new square
    tmp = !D.WINDOW
    wset, state.cur_color_win
    erase, color=state.cur_idx
    wset, tmp
  endif

  ; Let the caller of XPALETTE know that the color table was modified
  xp_alert_caller

end







pro XP_BUTTON_EVENT, event

  common xp_com, xpw, state
  common colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr

  ; NOTE: The value of these tags depend on the order of the buttons
  ;     in the base.
  case (event.value) of

    ; DONE
    0: begin
	empty
	r_orig = r_curr & g_orig = g_curr & b_orig = b_curr ;new orig color tbl
	WIDGET_CONTROL, /DESTROY, event.top
	!p = state.old_p
      end

    ; PREDEFINED
    1: xloadpalette, /silent, group=xpw.base, UPDATECALLBACK='XP_XLCTCALLBACK'

    ; HELP
    2: XDisplayFile, FILEPATH("xpalette.txt", subdir=['help', 'widget']), $
	TITLE = "XPalette Help", GROUP = event.top, WIDTH = 55, HEIGHT = 16

    ; REDRAW
    3: XP_REDRAW

    ; SET MARK
    4: begin
      state.mark_idx = state.cur_idx
      WIDGET_CONTROL, xpw.mark_label, $
		  set_value=strcompress(state.mark_idx, /REMOVE)
    end

    ; SWITCH MARK
    5 : if (state.mark_idx ne state.cur_idx) then begin
      tmp = state.mark_idx
      state.mark_idx = state.cur_idx
      state.cur_idx = tmp
      WIDGET_CONTROL, xpw.colorsel, set_value=tmp
      WIDGET_CONTROL, xpw.idx_label, $
		    set_value=strcompress(state.cur_idx, /REMOVE)
      WIDGET_CONTROL, xpw.mark_label, $
		    set_value=strcompress(state.mark_idx, /REMOVE)
      endif

    ; COPY CURRENT
    6 : begin
      do_copy:
	cur_idx = state.cur_idx
	if (state.mark_idx le cur_idx) then begin
	  s = state.mark_idx
	  e = cur_idx
	endif else begin
	  s = cur_idx
	  e = state.mark_idx
	endelse
	n = e-s+1
	XP_REPLOT, !p.background, 'D'
	if (event.value eq 6) then begin
	  r_curr[s:e] = r_curr[cur_idx]
	  g_curr[s:e] = g_curr[cur_idx]
	  b_curr[s:e] = b_curr[cur_idx]
	endif else begin                        ; Interpolate
	  scale = findgen(n)/float(n-1)
	  r_curr[s:e] = r_curr[s] + (fix(r_curr[e]) - fix(r_curr[s])) * scale
	  g_curr[s:e] = g_curr[s] + (fix(g_curr[e]) - fix(g_curr[s])) * scale
	  b_curr[s:e] = b_curr[s] + (fix(b_curr[e]) - fix(b_curr[s])) * scale
	endelse
	tvlct, r_curr[s:e], g_curr[s:e], b_curr[s:e], s
	if (XP_NEW_COLORS()) then begin
	  WIDGET_CONTROL, xpw.colorsel, SET_VALUE=-1
	  XP_REPLOT, !p.color, 'F'
	endif else begin
	  XP_REPLOT, !p.color, 'D'
	endelse
        ; Let the caller of XPALETTE know that the color table was modified
        xp_alert_caller
	end

    7: goto, do_copy
    8: BEGIN
         file=DIALOG_PICKFILE(/READ, FILTER = '*.ct')
         IF file NE '' THEN BEGIN
            unitct=100
            OPENR,unitct,file
		      READF,unitct,nbclu
		      table0=intarr(nbclu,3)
		      READF,unitct,table0
		      ;Interpolevels2,r_curr,table0(*,0),nbc
		      ;Interpolevels2,g_curr,table0(*,1),nbc
		      ;Interpolevels2,b_curr,table0(*,2),nbc
		      tvlct, r_curr, g_curr,b_curr

         END
      END
    9: BEGIN
         file=DIALOG_PICKFILE(/WRITE, FILTER = '*.ct')
         IF file NE '' THEN BEGIN
            unitct=100
            OPENW,unitct,file
         END
       END
    else:
  endcase

end







pro XP_EVENT, event

  common xp_com, xpw, state
  common colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr

  case (event.id) of

  xpw.button_base: XP_BUTTON_EVENT, event

  xpw.rgb_base: begin
	cur_idx = state.cur_idx
	if (event.r ne r_curr[cur_idx]) then XP_CHANGE_COLOR, "R", event.r
	if (event.g ne g_curr[cur_idx]) then XP_CHANGE_COLOR, "G", event.g
	if (event.b ne b_curr[cur_idx]) then XP_CHANGE_COLOR, "B", event.b
	end

  xpw.colorsel: begin
	cur_idx = state.cur_idx
	new_pos = event.value ne cur_idx
	; Update the RBG sliders
	if (event.value ne cur_idx) then begin
	  state.cur_idx = (cur_idx = event.value)
	  WIDGET_CONTROL, xpw.idx_label,  $
			  set_value=strcompress(cur_idx, /REMOVE_ALL)
	  ; Mark new square
	  tmp = !D.WINDOW
	  wset, state.cur_color_win
	  erase, color=cur_idx
          wset, tmp

	  WIDGET_CONTROL, xpw.rgb_base, $
		  set_value=[r_curr[cur_idx], g_curr[cur_idx], b_curr[cur_idx]]
	  endif
	end

    else:
    endcase

end







pro PALETTE, group=group, BLOCK=block, UPDATECALLBACK=updt_cb_name, $
        UPDATECBDATA=updt_cb_data


  common xp_com, xpw, state
  common colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr

  IF N_ELEMENTS(updt_cb_name) EQ 0 THEN updt_callback="" $
                                 ELSE updt_callback=updt_cb_name
  IF N_ELEMENTS(updt_cb_data) GT 0 THEN p_updt_cb_data=PTR_NEW(updt_cb_data) $
                                 ELSE p_updt_cb_data=PTR_NEW()

  xpw = { xp_widgets, base:0L, $
	colorsel:0L, mark_label:0L, idx_label:0L, button_base:0L, rgb_base:0L}

  state = {old_p:!p, $                     ; Original value of !P
	   mark_idx:0, $                   ; Current mark index
	   cur_idx:0, $                    ; Current index
	   cur_color_win:0, $              ; Current Color draw window index
	   plot_win:0, $                   ; RGB plot draw window index
	   updt_callback: updt_callback, $ ; user-defined callback (optional)
	   p_updt_cb_data:p_updt_cb_data}  ; data for callback (optional)

  if (XREGISTERED('XPALETTE')) then return      ; Only one copy at a time

  IF N_ELEMENTS(block) EQ 0 THEN block=0

  on_error,2              ;Return to caller if an error occurs

  nc = !d.table_size            ;# of colors avail
  if nc eq 0 then message, "Device has static color tables.  Can't modify."
  if (nc eq 2) then message, 'Unable to work with monochrome system.'

  state.old_p = !p              ;Save !p
  !p.noclip = 1                 ;No clipping
  !p.color = nc -1              ;Foreground color
  !p.font = 0                   ;Hdw font
  save_win = !d.window  ;Previous window

  IF N_ELEMENTS(r_orig) LE 0 THEN BEGIN	;If no common, use current colors
	TVLCT, r_orig, g_orig, b_orig, /GET
	r_curr = r_orig
	b_curr = b_orig
	g_curr = g_orig
	ENDIF

  ; Create widgets
  xpw.base=WIDGET_BASE(title='XPalette', /ROW, space=30)
  ; This is a little tricky. Setting the managed attribute indicates
  ; our intention to put this app under the control of XMANAGER, and
  ; prevents our draw widgets from becoming candidates for becoming
  ; the default window on WSET, -1. XMANAGER sets this, but doing it here
  ; prevents our own WSETs at startup from having that problem.
  WIDGET_CONTROL, /MANAGED, xpw.base

  version = WIDGET_INFO(/VERSION)
  if (version.style='Motif') then junk=510 else junk = 580
  plot_frame = WIDGET_DRAW(xpw.base, xsize=200, ysize=junk)

  c1 = WIDGET_BASE(xpw.base, /COLUMN, space=20)
    status = WIDGET_BASE(c1, /COLUMN, /FRAME)
      ncw = WIDGET_LABEL(WIDGET_BASE(status), /DYNAMIC_RESIZE)
      xpw.idx_label = CW_FIELD(status, title='Current Index: ', value='0', $
			       xsize=20, /STRING)
      xpw.mark_label = CW_FIELD(status, title='Mark Index:    ', value='0', $
				xsize=20, /STRING)
      c1_1 = widget_base(status, /ROW)
	junk = WIDGET_LABEL(c1_1, value="Current Color: ")
	  cur_color = WIDGET_DRAW(c1_1, xsize = 125, ysize=50, /frame)
    names = [ 'Done', 'Predefined', 'Help', 'Redraw', 'Set Mark', $
		'Switch Mark', 'Copy Current', 'Interpolate']
    xpw.button_base = CW_BGROUP(c1, names, COLUMN=3, /FRAME)
    xpw.rgb_base = CW_RGBSLIDER(c1, /FRAME, /DRAG)

    junk = WIDGET_BASE(xpw.base)        ; Responds to YOFFSET
    if (version.style='Motif') then junk2=30 else junk2 = 50
    xpw.colorsel = CW_COLORSEL(junk, yoffset=junk2)


  state.cur_idx = 0
  state.mark_idx = 0

  ; Position RGB slider appropriately
  WIDGET_CONTROL, xpw.rgb_base, SET_VALUE=[r_curr[0], g_curr[0], b_curr[0]]
  WIDGET_CONTROL, /REALIZE, xpw.base

  WIDGET_CONTROL, ncw, $
	set_value='Number Of Colors: ' + strcompress(!d.n_colors, /REMOVE_ALL)
  WIDGET_CONTROL, get_value=tmp, cur_color
  state.cur_color_win = tmp
  WIDGET_CONTROL, get_value=tmp, plot_frame
  state.plot_win = tmp


  ; Update the plots of RGB
  junk = XP_NEW_COLORS()
  XP_REPLOT, !p.color, 'F'

  WSET, save_win

  XMANAGER, 'XPalette', xpw.base, event_handler='XP_EVENT', group=group, $
	NO_BLOCK=(NOT(FLOAT(block)))
end

