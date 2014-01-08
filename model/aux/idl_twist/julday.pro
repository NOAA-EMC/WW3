; $Id: //depot/idl/releases/IDL_80/idldir/lib/julday.pro#1 $
;
; Copyright (c) 1988-2010, ITT Visual Information Solutions. All
;       rights reserved. Unauthorized reproduction is prohibited.

;+
; NAME:
;	JULDAY
;
; PURPOSE:
;	Calculate the Julian Day Number for a given month, day, and year.
;	This is the inverse of the library function CALDAT.
;	See also caldat, the inverse of this function.
;
; CATEGORY:
;	Misc.
;
; CALLING SEQUENCE:
;	Result = JULDAY([[[[Month, Day, Year], Hour], Minute], Second])
;
; INPUTS:
;	MONTH:	Number of the desired month (1 = January, ..., 12 = December).
;
;	DAY:	Number of day of the month.
;
;	YEAR:	Number of the desired year.Year parameters must be valid
;               values from the civil calendar.  Years B.C.E. are represented
;               as negative integers.  Years in the common era are represented
;               as positive integers.  In particular, note that there is no
;               year 0 in the civil calendar.  1 B.C.E. (-1) is followed by
;               1 C.E. (1).
;
;	HOUR:	Number of the hour of the day.
;
;	MINUTE:	Number of the minute of the hour.
;
;	SECOND:	Number of the second of the minute.
;
;   Note: Month, Day, Year, Hour, Minute, and Second can all be arrays.
;         The Result will have the same dimensions as the smallest array, or
;         will be a scalar if all arguments are scalars.
;
; OPTIONAL INPUT PARAMETERS:
;	Hour, Minute, Second = optional time of day.
;
; OUTPUTS:
;	JULDAY returns the Julian Day Number (which begins at noon) of the
;	specified calendar date.  If Hour, Minute, and Second are not specified,
;	then the result will be a long integer, otherwise the result is a
;	double precision floating point number.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	None.
;
; RESTRICTIONS:
;	Accuracy using IEEE double precision numbers is approximately
;   1/10000th of a second, with higher accuracy for smaller (earlier)
;   Julian dates.
;
; MODIFICATION HISTORY:
;	Translated from "Numerical Recipies in C", by William H. Press,
;	Brian P. Flannery, Saul A. Teukolsky, and William T. Vetterling.
;	Cambridge University Press, 1988 (second printing).
;
;	AB, September, 1988
;	DMS, April, 1995, Added time of day.
;   CT, April 2000, Now accepts vectors or scalars.
;-
;
function JULDAY, MONTH, DAY, YEAR, Hour, Minute, Second

COMPILE_OPT idl2

ON_ERROR, 2		; Return to caller if errors

; Gregorian Calander was adopted on Oct. 15, 1582
; skipping from Oct. 4, 1582 to Oct. 15, 1582
GREG = 2299171L  ; incorrect Julian day for Oct. 25, 1582

; Process the input, if all are missing, use todays date.
NP = n_params()
IF (np EQ 0) THEN RETURN, SYSTIME(/JULIAN)
IF (np LT 3) THEN MESSAGE, 'Incorrect number of arguments.'

; Find the dimensions of the Result:
;  1. Find all of the input arguments that are arrays (ignore scalars)
;  2. Out of the arrays, find the smallest number of elements
;  3. Find the dimensions of the smallest array

; Step 1: find all array arguments
nDims = [SIZE(month,/N_DIMENSIONS), SIZE(day,/N_DIMENSIONS), $
	SIZE(year,/N_DIMENSIONS), SIZE(hour,/N_DIMENSIONS), $
	SIZE(minute,/N_DIMENSIONS), SIZE(second,/N_DIMENSIONS)]
arrays = WHERE(nDims GE 1)

nJulian = 1L    ; assume everything is a scalar
IF (arrays[0] GE 0) THEN BEGIN
	; Step 2: find the smallest number of elements
	nElement = [N_ELEMENTS(month), N_ELEMENTS(day), $
		N_ELEMENTS(year), N_ELEMENTS(hour), $
		N_ELEMENTS(minute), N_ELEMENTS(second)]
	nJulian = MIN(nElement[arrays], whichVar)
	; step 3: find dimensions of the smallest array
	CASE arrays[whichVar] OF
	0: julianDims = SIZE(month,/DIMENSIONS)
	1: julianDims = SIZE(day,/DIMENSIONS)
	2: julianDims = SIZE(year,/DIMENSIONS)
	3: julianDims = SIZE(hour,/DIMENSIONS)
	4: julianDims = SIZE(minute,/DIMENSIONS)
	5: julianDims = SIZE(second,/DIMENSIONS)
	ENDCASE
ENDIF

d_Second = 0d  ; defaults
d_Minute = 0d
d_Hour = 0d
; convert all Arguments to appropriate array size & type
SWITCH np OF  ; use switch so we fall thru all arguments...
6: d_Second = (SIZE(second,/N_DIMENSIONS) GT 0) ? $
	second[0:nJulian-1] : second
5: d_Minute = (SIZE(minute,/N_DIMENSIONS) GT 0) ? $
	minute[0:nJulian-1] : minute
4: d_Hour = (SIZE(hour,/N_DIMENSIONS) GT 0) ? $
	hour[0:nJulian-1] : hour
3: BEGIN ; convert m,d,y to type LONG
	L_MONTH = (SIZE(month,/N_DIMENSIONS) GT 0) ? $
		LONG(month[0:nJulian-1]) : LONG(month)
	L_DAY = (SIZE(day,/N_DIMENSIONS) GT 0) ? $
		LONG(day[0:nJulian-1]) : LONG(day)
	L_YEAR = (SIZE(year,/N_DIMENSIONS) GT 0) ? $
		LONG(year[0:nJulian-1]) : LONG(year)
	END
ENDSWITCH


min_calendar = -4716
max_calendar = 5000000
minn = MIN(l_year, MAX=maxx)
IF (minn LT min_calendar) OR (maxx GT max_calendar) THEN MESSAGE, $
	'Value of Julian date is out of allowed range.'
if (MAX(L_YEAR eq 0) NE 0) then message, $
	'There is no year zero in the civil calendar.'


bc = (L_YEAR LT 0)
L_YEAR = TEMPORARY(L_YEAR) + TEMPORARY(bc)
inJanFeb = (L_MONTH LE 2)
JY = L_YEAR - inJanFeb
JM = L_MONTH + (1b + 12b*TEMPORARY(inJanFeb))


JUL = floor(365.25d * JY) + floor(30.6001d*TEMPORARY(JM)) + L_DAY + 1720995L


; Test whether to change to Gregorian Calandar.
IF (MIN(JUL) GE GREG) THEN BEGIN  ; change all dates
	JA = long(0.01d * TEMPORARY(JY))
	JUL = TEMPORARY(JUL) + 2L - JA + long(0.25d * JA)
ENDIF ELSE BEGIN
	gregChange = WHERE(JUL ge GREG, ngreg)
	IF (ngreg GT 0) THEN BEGIN
		JA = long(0.01d * JY[gregChange])
		JUL[gregChange] = JUL[gregChange] + 2L - JA + long(0.25d * JA)
	ENDIF
ENDELSE


; hour,minute,second?
IF (np GT 3) THEN BEGIN ; yes, compute the fractional Julian date
; Add a small offset so we get the hours, minutes, & seconds back correctly
; if we convert the Julian dates back. This offset is proportional to the
; Julian date, so small dates (a long, long time ago) will be "more" accurate.
	eps = (MACHAR(/DOUBLE)).eps
	eps = eps*ABS(jul) > eps
; For Hours, divide by 24, then subtract 0.5, in case we have unsigned ints.
	jul = TEMPORARY(JUL) + ( (TEMPORARY(d_Hour)/24d - 0.5d) + $
		TEMPORARY(d_Minute)/1440d + TEMPORARY(d_Second)/86400d + eps )
ENDIF

; check to see if we need to reform vector to array of correct dimensions
IF (N_ELEMENTS(julianDims) GT 1) THEN $
	JUL = REFORM(TEMPORARY(JUL), julianDims)

RETURN, jul

END
