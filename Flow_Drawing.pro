;
; $Id: DGPDrawing.pro,v 1.12 1996/10/17 22:14:33 jennifer Exp $
;             (c) Copyright Visual Numerics, Inc., 1995
;                     Boulder, Colorado, U.S.A.
;
;                        All Rights Reserved.
;
;+
; NAME:	DGPDrawing
; PURPOSE:
; Drawing allows users to display graphical contents of data processed in 
; WAVE CL.
; Implementation:
;	- creates drawingArea/drawArea(stub) in scrolledWindow
;		- name: draw
;	- sets callback	
; CALLING SEQUENCE:
; wid = DGPDrawing(parent, windex, drawCallback,wsize,dsize
;                          [,FOREGROUND=color,BACKGROUND=color, BORDER=num
;                           ,RIGHT[=wid],BOTTOM[=wid],LEFT[=wid],TOP[=wid]
;                           ,Layout_name=<string>, Name=<string>
;                           ,POSITION=<x,y>,/NOSCROLL,AREA=<wid>])
; INPUT:
;	- parent
;	- WAVE CL window index
;	- draw callback
;	- size of the window drawn (width, height)
;	- size of the draw area displayed (width, height)
; OUTPUT:
;	- scrolling window widget
; KEYWORDS:
;	- colors: FOREGROUND,BACKGROUND=<colorname>
;	- container and children border width: BORDER=<number>
;	- attachments: RIGHT[=wid],BOTTOM[=wid],LEFT[=wid],TOP[=wid]
;       - position: POSITION=<x,y>
;	- do not use scrollbars use dsize to size the area, /NOSCROLL
;	- returns drawing area widget AREA=wid
;       - layout widget name: LAYOUT_NAME=<string>
;       - drawing area widget name: NAME=<string>
;   - do not use metafiles, /NOMETA (for animations)
;
; CALLBACK PARAMETERS:
;	- drawing area widget id
;	- WAVE CL window index
;
; RESTRICTIONS:
;-
;
; Internal expose callback, calls user's redraw callback
;
PRO DGPDrawingExpose, wid, index
@wtxlib

   COMMON Wwdrawcomm, wwdrawind, wwdrawclbk, wwdrawwidth, wwdrawheight, $
          wwdrawnometa
   on_error, 2, /continue
   ;
   ; Initialize WAVE CL window on first expose action
   ;
   IF wwdrawind(index) LT 0 THEN BEGIN
      wwdrawind(index) = -(wwdrawind(index)+1)
      winid = WtGet(wid, /WINDOW)
      IF winid EQ 0 THEN BEGIN
         WwError, "Unable to get drawing area window", /Cont
         RETURN
      ENDIF
      
      ;
      ; Associate X window with WAVE CL window structure
      ;
      IF wwdrawnometa EQ 1 THEN BEGIN
        window, wwdrawind(index), SET_XWIN_ID = winid, /NOMETA
      ENDIF ELSE $
        window, wwdrawind(index), SET_XWIN_ID = winid      

   ENDIF
   ;
   ; Call user redraw callback
   ;
   width = WtGet(wid, 'width')
   height = WtGet(wid, 'height')

   IF strlen(strtrim(wwdrawclbk(index), 2)) GT 0 THEN BEGIN
      IF ((width NE wwdrawwidth(index)) OR $
           (height NE wwdrawheight(index))) THEN BEGIN
         wset, wwdrawind(index)
         status = EXECUTE(wwdrawclbk(index) + ', wid, wwdrawind(index)')
      ENDIF
   ENDIF
   ;
   ; Save maximum dimensions
   ;
   wwdrawwidth(index) = width
   wwdrawheight(index) = height
END
;
; Delete the window from internal data structs for reuse
;
PRO DGPDrawingDestroy, wid, index 
   COMMON Wwdrawcomm, wwdrawind, wwdrawclbk, wwdrawwidth, wwdrawheight, $
          wwdrawnometa

   IF wwdrawind(index) LT 999999L AND wwdrawind(index) GE 0 THEN BEGIN
      device, WINDOW_STATE = wins
      wwdrawclbk(wwdrawind(index)) = ""
      IF wins(wwdrawind(index)) NE 0 THEN wdelete, wwdrawind(index)
   ENDIF
   wwdrawind(index) = 999999L
END
;
; Resize the internal data structures
;
PRO DGPDrawingResizeData, max_wins

   COMMON Wwdrawcomm, wwdrawind, wwdrawclbk, wwdrawwidth, wwdrawheight, $
          wwdrawnometa

   IF max_wins GT N_ELEMENTS(wwdrawind) THEN BEGIN
      app_size = max_wins - N_ELEMENTS(wwdrawind)
      
      wwdrawind    = [wwdrawind, REPLICATE(999999L, app_size)]
      wwdrawclbk   = [wwdrawclbk, REPLICATE("", app_size)]
      wwdrawwidth  = [wwdrawwidth, REPLICATE(0L, app_size)]
      wwdrawheight = [wwdrawheight, REPLICATE(0L, app_size)]
   ENDIF ELSE IF max_wins LT N_ELEMENTS(wwdrawind) THEN BEGIN
      wwdrawind    = wwdrawind(0:max_wins-1)
      wwdrawclbk   = wwdrawclbk(0:max_wins-1)
      wwdrawwidth  = wwdrawwidth(0:max_wins-1)
      wwdrawheight = wwdrawheight(0:max_wins-1)
   ENDIF
END
;
; The main procedure: Creates the drawing area in container
;
FUNCTION DGPDrawing, parent, windex, drawCallback, $
                    Wsize, Dsize, Border=Border, $
                    Foreground=Foreground, Background=Background, $
                    Left=Left, Top=Top, $
                    Right=Right, Bottom=Bottom, $
                    Layout_name=Layout_name, Name=Name, $
                    Position=Position, Noscroll=Noscroll, Area=Area,$
                    NoMeta=NoMeta

   ON_ERROR_GOTO, DGPDrawing_Fail
   
@wtwacclasses
@wtwacconsts
   
   COMMON Wwdrawcomm, wwdrawind, wwdrawclbk, wwdrawwidth, wwdrawheight, $
          wwdrawnometa
   ;
   ; Check the parent, size
   ;
   IF N_ELEMENTS(parent) EQ 0 THEN BEGIN
      WwError, 'Null Parent', /Fatal
   ENDIF ELSE IF parent EQ 0 THEN BEGIN
      WwError, 'Null Parent', /Fatal
   ENDIF
   IF (N_ELEMENTS(Wsize) NE 2) OR (N_ELEMENTS(Dsize) NE 2) THEN BEGIN
      WwError, 'Wrong window/area sizes', /Cont
      RETURN, 0
   ENDIF
   ;
   ; Get the windows available
   ;
   device, WINDOW_STATE = wins
   max_wins = N_ELEMENTS(wins)
   ;
   ; Initialize the data structs
   ;
   IF N_ELEMENTS(wwdrawind) EQ 0 THEN BEGIN
      wwdrawind    = REPLICATE(999999L, max_wins)
      wwdrawclbk   = REPLICATE("", max_wins)
      wwdrawwidth  = REPLICATE(0L, max_wins)
      wwdrawheight = REPLICATE(0L, max_wins)
   ENDIF
   ;
   ; Update the wwdrawind's with open windows
   ;
   used_wins = WHERE(wins EQ 1)
   free_wins = WHERE(wins EQ 0)
   tempwinid = wwdrawind
   known_wins = WHERE(tempwinid LT 0L)

   wwdrawind(0:max_wins-1) = LINDGEN(N_ELEMENTS(wwdrawind(0:max_wins-1)))

   ; If there are some available windows
   IF free_wins(0) NE -1 THEN BEGIN
      wwdrawind(free_wins) = 999999L
   ENDIF 

   ; If there are some unexposed windows
   IF known_wins(0) NE -1 THEN BEGIN
      wwdrawind(known_wins) = tempwinid(known_wins) 
   ENDIF ELSE $
      ;
      ; Increase the current size of structures if needed
      ;
      IF max_wins NE N_ELEMENTS(wwdrawind) THEN $
         DGPDrawingResizeData, max_wins
   ;
   ; Get the available window number
   ;
   IF N_ELEMENTS(windex) EQ 0 THEN BEGIN
      FOR i = 0, N_ELEMENTS(wwdrawind)-1 DO BEGIN
         IF wwdrawind(i) EQ 999999L THEN GOTO, found
      ENDFOR
      DGPDrawingResizeData, max_wins + 32
      i = max_wins
      GOTO, found
   ENDIF ELSE BEGIN
      IF (windex LT 0) OR (windex GE max_wins) THEN BEGIN
         FOR i = 0, N_ELEMENTS(wwdrawind)-1 DO BEGIN
            IF wwdrawind(i) EQ 999999L THEN GOTO, found
         ENDFOR
         DGPDrawingResizeData, max_wins + 32
         i = max_wins

Found:
         windex = i
      ENDIF ELSE BEGIN
         IF wins(windex) NE 0 THEN BEGIN
            WwError, 'DGPDrawing, window index already used', /Cont
            RETURN, 0
         ENDIF
      ENDELSE
   ENDELSE
   ;
   ; Store window id, as negative number, to indicate, that window
   ; was not initialized yet.
   ;
   wwdrawind(windex) = -(windex+1)
   ;
   ; Set drawing area resources
   ;
   IF KEYWORD_SET(NoMeta) THEN wwdrawnometa = 1 ELSE wwdrawnometa = 0

   IF KEYWORD_SET(Border) THEN bwidth = Border ELSE bwidth = 0

   IF KEYWORD_SET(Layout_name) THEN l_name = Layout_name $
   ELSE l_name = 'drawindow'
   IF KEYWORD_SET(Name) THEN d_name = Name ELSE d_name = 'draw'
   
   IF (N_ELEMENTS(Wsize) EQ 2) AND (N_ELEMENTS(Dsize) EQ 2) THEN BEGIN
      ww = Wsize(0) & wh = Wsize(1) & dw = Dsize(0) & dh = Dsize(1)
   ENDIF ELSE IF N_ELEMENTS(Wsize) EQ 2 THEN BEGIN
      ww = Wsize(0) & wh = Wsize(1) & dw = Wsize(0) & dh = Wsize(1)
   ENDIF ELSE IF N_ELEMENTS(Dsize) EQ 2 THEN BEGIN
      ww = Dsize(0) & wh = Dsize(1) & dw = Dsize(0) & dh = Dsize(1)
   ENDIF
   ;
   ; Create the container and drawing area widget
   ;
   IF KEYWORD_SET(Noscroll) THEN BEGIN
      Sargs = 'args = {, borderWidth: bwidth'
      IF WtGet(parent, /Class) EQ arrayWidgetClass THEN BEGIN
         Sargs = Sargs + ',horizontalJustify:XtJustifyFill,verticalJustify:XtVJustifyFill'
      ENDIF
      Sargs = Sargs + '}'
      status = EXECUTE(Sargs)
      
      w = WtCreate(l_name, formWidgetClass, parent, args)

      IF w EQ 0 THEN WwError, "Unable to create drawing area container", /Fatal
   ENDIF ELSE BEGIN
      Sargs = 'args = {, borderWidth: bwidth'
      Sargs = Sargs + ',width:ww,height:wh,vScrollAmount:1,hScrollAmount:1'
      IF WtGet(parent, /Class) EQ arrayWidgetClass THEN BEGIN
         Sargs = Sargs + ',horizontalJustify:XtJustifyFill,verticalJustify:XtVJustifyFill'
      ENDIF
      Sargs = Sargs + '}'
      status = EXECUTE(Sargs)
      
      w = WtCreate(l_name, scrolledWindowWidgetClass, parent, args)
      IF w EQ 0 THEN WwError, "Unable to create drawing area container", /Fatal
      
      IF KEYWORD_SET(Background) THEN BEGIN
         args = {, background:Background}
         status = WtSet(w, args)
      ENDIF
      IF KEYWORD_SET(Foreground) THEN BEGIN
         args = {, foreground:Foreground}
         status = WtSet(w, args)
      ENDIF
   ENDELSE
   
   args = {, width: dw, height: dh, $
            bottomAttachment:WacATTACH_FORM, $
            topAttachment:WacATTACH_FORM, $
            leftAttachment:WacATTACH_FORM, $
            rightAttachment:WacATTACH_FORM}
   draw = WtCreate(d_name, drawingAreaWidgetClass, w, args)
   IF draw EQ 0 THEN WwError, "Unable to create drawing area", /Fatal
   
   Area = draw
   wwdrawwidth(windex) = 0
   wwdrawheight(windex) = 0
   
   IF KEYWORD_SET(Background) THEN BEGIN
      args = {, background:Background}
      status = WtSet(draw, args)
   ENDIF
   IF KEYWORD_SET(Foreground) THEN BEGIN
      args = {, foreground:Foreground}
      status = WtSet(draw, args)
   ENDIF
   ;
   ; Set drawing area callbacks
   ;
   status = WtAddCallback(draw, 'destroyCallback', 'DGPDrawingDestroy', $
                          windex, /NOPARAMS)

   status = WtAddCallback(draw, 'exposeCallback', 'DGPDrawingExpose', $
                          windex, /NOPARAMS)
   status = WtAddCallback(draw, 'resizeCallback', 'DGPDrawingExpose', $
                          windex, /NOPARAMS)
   ;
   ; Register callback proc to be called from StructureNotify handler
   ;
   IF N_ELEMENTS(drawCallback) GT 0 THEN $
      wwdrawclbk(windex) = drawCallback
   ;
   ; Attach or position the widget
   ;
   WwAttachWidget, parent, w, Left = Left, Top = Top, $
     Right = Right, Bottom = Bottom, Position = Position
   
   RETURN, w
   
DGPDrawing_Fail:
   RETURN, 0L
END


; dgp rev 1/25/10 error handling
pro _pr_dac_error

  DEVICE, Window_State = winarr 
  info, trace=tree
  idx = where(winarr ne 0,cnt)
  
  print, "% error in WwDrawing"
  pm, tree
  print, cnt, " windows used"

end

pro Flow_Drawing

  print, "Initialize rewrite of WwDrawing routines"

end
