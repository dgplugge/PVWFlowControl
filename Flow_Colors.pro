;
; $Id: tek_color.pro,v 1.3 1997/02/11 23:15:49 landers Exp $
;
PRO custom_colors
;+
; NAME:		CUSTOM_COLOR
; PURPOSE:	Load a custom color table
; CATEGORY:	Graphics
; CALLING SEQUENCE:
;	CUSTOM_COLOR
; INPUTS:
;	None.
; KEYWORD PARAMETERS:
;	None
; OUTPUTS:
;	No explicit outputs
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	The first 32 elements of the color table are loaded with
;	custom colors
; RESTRICTIONS:
;	None.
; PROCEDURE:
;	Just copy the 32 colors.  This table is useful for the
;	display of graphics in that the color are distinctive.
;
; MODIFICATION HISTORY:
;	DMS, Jan, 1989.
;       GH,  August, 1995     - added Colors common block
; 	DPG, Nov, 2005        - modified from TEK_COLOR procedure
;-
on_error,2                      ;Return to caller if an error occurs
COMMON Colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr
;
; Initialize the Colors common block if necessary
;
IF (N_ELEMENTS(r_orig) EQ 0) THEN LOADCT, 0, /silent

rgb_info = 'rgb.info'
found = findfile(rgb_info, count=cnt)
if (cnt ne 0) then begin
  status = DC_READ_FIXED(rgb_info,r,g,b,/col,format='(I4)')
endif else begin
;
; Load default color table
;
r = BYTSCL([ 0,100,100,0,0,0,100,100,100,60,0,0,55,100,33,67, $
	100,75,45,17,25,50,75,100,67,40,17,17,17,45,75,90])
g = BYTSCL([ 0,100,0,100,0,100,0,100,50,83,100,50,0,0,33,67, $
	100,100,100,100,83,67,55,33,90,90,90,67,50,33,17,9])
b = BYTSCL([ 0,100,0,0,100,100,83,0,0,0,60,100,83,55,33,67, $
	33,45,60,75,83,83,83,90,45,55,67,90,100,100,100,100])
  status = DC_WRITE_FIXED(rgb_info, fix(r),fix(g),fix(b),/col,format='(I4)')
endelse
;
TVLCT, r, g, b

; Update the Colors common block
;
n = ( N_ELEMENTS(r) < N_ELEMENTS(r_orig) ) - 1
r_orig(0:n) = r(0:n)
g_orig(0:n) = g(0:n)
b_orig(0:n) = b(0:n)

END

pro _pr_ct_debug

  common ct_names, orig, closest, names

  stop ; debug

end

function _ct_get_name, num

  common ct_names, orig, closest, names

  if (size(names,/type) ne 0) then begin

    last = n_elements(names)
    if (num lt last) then begin
      return, names(num)
    endif else begin
      return, names(last-1)
    endelse

  endif else begin

    print, "No names"
    return, 'black'

  endelse

end

pro _ct_init

common ct_names, orig, closest, names

  names = make_array(256,/string)
  closest = make_array(3,256,/int)

end

pro _ct_read_table

COMMON color_info, countin, rgb, name

;rgbname = 'rgb.txt' 
rgbname = !Dir + '/lib/std/windows/rgb.txt' 
alive = 0
namein = ' '
rin=0	&	gin=0	&	bin=0

status = DC_READ_FREE(rgbname, rin, gin, bin, namein, /Column, $
        Resize=[1,2,3,4], Delim=[','],Ignore=['!']) 

IF status NE 0 THEN BEGIN
    MESSAGE, 'GuiColor is trying to read: ' + rgbname, /Continue
    MESSAGE, 'but an error has occurred. Check to see if this file', /Continue
    MESSAGE, 'exists. To point to a different rgb.txt file, pass the', /Continue
    MESSAGE, 'filename and path. For example:', /Continue
    MESSAGE, '  WAVE> guicolor, "/usr/lib/X11/rgb.txt"', /Continue
    RETURN
ENDIF


countin = N_ELEMENTS(rin)

trgb = LONARR(3,countin)	&	name = STRARR(countin)
count = 0
skip = 0
dups = make_array(256,256,256,/int)

; get rid of duplicate RGBs, spaces in namein(), keep second match
FOR i = 0,countin-1 DO BEGIN
    IF (dups(rin(i),bin(i),gin(i)) eq 0) THEN BEGIN
        dups(rin(i),bin(i),gin(i)) = 1
        trgb(0,count) = rin(i)
        trgb(1,count) = gin(i)
        trgb(2,count) = bin(i)
        name(count) = STRSUBST(namein(i),'"','',/Global)
        name(count) = STRSUBST(name(count),STRING(09B),'',/Global)
        name(count) = STRLOWCASE(name(count))
        count = count + 1
    ENDIF else begin
        skip = skip + 1
    endelse
ENDFOR

countin = count

rgb = trgb(*,0:countin-1)

end

pro show_names

  common ct_names, orig, closest, names

  if (size(names,/type) ne 0) then begin

    num = n_elements(names)
    for i=0,num-1 do begin

      print, names(i), closest(*,i)
      ;a = GET_KBRD(1)

    endfor

  endif else begin

    print, "No names"

  endelse

end

PRO flow_colors

  COMMON color_info, countin, rgb, name
  common ct_names, orig, closest, names
  Common colour, Wht, Blk


  Wht=WoColorConvert(1, /IndexToColor)
  Blk=WoColorConvert(0, /IndexToColor)
          

;  loadct, 6
  custom_colors

  _ct_read_table
  tvlct, r, g, b, /get

  _ct_init

  num = n_elements(r)
  for index=0,num-1 do begin
  
    seed = [r(index),g(index),b(index)]
    
    ;print, "Parameter 1: ",index
    ;print, "Parameter 2: ",seed
  
    ;info, index
    ;info, names
; create centroid array
    centroid = rebin(seed,3,countin)
 
; calculate euclidean distance
    sdist = (rgb-centroid)
    ssdist = sdist*sdist
      
    dif  = sum(ssdist,0)

; sort closest to furthest
    ord = sort(dif)

; reorder      
    orgb  = rgb(*,ord)
    oname = name(ord)
 
    ;print, "Set name ",index," to ",oname(0)
    names(index) = oname(0)
    closest(0:2,index) = orgb(0:2,0)
  
  endfor
  
  tvlct, closest(0,*), closest(1,*), closest(2,*), 0
  
  ;show_names

  
END