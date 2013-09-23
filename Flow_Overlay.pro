declare func, _gen_unique_name
declare func, _sense_mod
declare func, _wid_modify
declare func, _wid_get_parent
declare func, _mgs_get_sort_names
declare func, _ct_get_name
declare func, fn_dmf_calc_res
declare func, fn_dmf_get
declare func, _wms_get_main
declare func, _dsp_get_norm
declare func, fn_win_get_tmp
declare func, fn_dmf_par_id
declare func, fn_dmf_single
declare func, fn_data_get_cells
declare func, _par_cur_names
declare func, _dmf_get_lvl_arr
declare func, _cdr_get_pars
declare func, _hist
declare func, _dsp_get_pos
declare func, _wms_rebuild
declare func, fn_wid_exists
declare func, fn_gen_get
declare func, fn_gen_getstr
declare func, fn_par_name
declare func, _wid_get_pos
declare func, fn_wid_parent_name
declare func, fn_calc_win_size
declare func, fn_calc_dsp_size
declare func, fn_lgln_get
declare func, dgpGetValue
declare func, fn_dmf_xmin_ratio
declare func, fn_dmf_xmax_ratio
declare func, fn_dmf_ymin_ratio
declare func, fn_dmf_ymax_ratio

; color_rgb is the reference number of the rgb color
; color_idx is the internal index of the color
; color_name is the name of the color for widget purposes

; color_rgb = WoColorConvert(color_idx,/IndextoColor)

; display the enabled overlays.

; dgp rev 8/16/05 how many overlays?


function fn_olay_count

  common cm_olay_info, cv_olay_key, cv_olay_hash
  
  if (size(cv_olay_hash,/type) ne 11) then return, 0

  return, n_elements(cv_olay_hash('overlays'))

end

; dgp rev 8/16/05 does a key exist
function fn_olay_exists, key

  common cm_olay_info, cv_olay_key, cv_olay_hash
  
  if (size(cv_olay_hash,/type) ne 11) then return, 0

  return, isaskey(cv_olay_hash('overlays'),key)

end

; dgp rev 8/16/05 return active keys
function fn_olay_active

  common cm_olay_info, cv_olay_key, cv_olay_hash
  
  if (size(cv_olay_hash,/type) ne 11) then return, [""]

  if (isaskey(cv_olay_hash,'overlays')) then begin
    keys = askeys(cv_olay_hash('overlays'))
    return, keys
  endif
  
  return, [""]
  
end

; dgp rev 8/16/05 return list of layer keys
function fn_olay_list

  common cm_olay_info, cv_olay_key, cv_olay_hash

  tmp_arr = askeys(cv_olay_hash('overlays',cv_olay_key,'layers'))
  idx = sort(tmp_arr)
  return, tmp_arr(idx)

end

; dgp rev 8/16/05 change overlay keys
function fn_olay_switch, key

  common cm_olay_info, cv_olay_key, cv_olay_hash

  if (isaskey(cv_olay_hash('overlays'),key)) then begin
    print, "Overlay set to ",key
    cv_olay_key = key
    return, 1
  endif

  print, "No overlay found -- ",key
  return, 0

end

; dgp rev 8/16/05 retreive a global overlay variable
function fn_olay_glob_get, label

  common cm_olay_info, cv_olay_key, cv_olay_hash

  if (size(cv_olay_hash,/type) ne 11) then return, 0

  if (isaskey(cv_olay_hash,label)) then return, cv_olay_hash(label)

  return, 0

end

; dgp rev 8/16/05 return layer info
function fn_olay_layer_get, label

  common cm_olay_info, cv_olay_key, cv_olay_hash

  layer = cv_olay_hash('overlays',cv_olay_key,'last layer')
  if (isaskey(cv_olay_hash('overlays',cv_olay_key,'layers',layer),label)) then begin
    return, cv_olay_hash('overlays',cv_olay_key,'layers',layer,label)
  endif

end


; dgp rev 5/23/2012 Does the last layer contain a specific label?
function fn_olay_layer_exists, label

  common cm_olay_info, cv_olay_key, cv_olay_hash
  
  if (size(cv_olay_hash,/type) ne 11) then return, 0
  if (isaskey(cv_olay_hash,'overlays')) then begin
    ; key exists
    if (isaskey(cv_olay_hash('overlays'),cv_olay_key)) then begin
      ; wid id exists
      if (isaskey(cv_olay_hash('overlays',cv_olay_key),'last layer')) then begin
        layer = cv_olay_hash('overlays',cv_olay_key,'last layer')
        return, isaskey(cv_olay_hash('overlays',cv_olay_key,'layers',layer),label)
      endif
    endif
  endif    


  return, 0

end

; dgp rev 8/16/05 get a local overlay variable
function fn_olay_cur_get, label

  common cm_olay_info, cv_olay_key, cv_olay_hash
  
;  print, "Current olay ",cv_olay_key, label

  if (size(cv_olay_hash,/type) ne 11) then return, 0

  if (isaskey(cv_olay_hash('overlays'),cv_olay_key)) then begin
    if (isaskey(cv_olay_hash('overlays',cv_olay_key),label)) then return, cv_olay_hash('overlays',cv_olay_key,label)
  endif
  return, 0

end

; dgp rev 8/18/05 set the current overlay variable
pro _pr_olay_cur_set, label, val

  common cm_olay_info, cv_olay_key, cv_olay_hash

  if (size(cv_olay_hash,/type) eq 11) then begin  
    cv_olay_hash('overlays',cv_olay_key) = [cv_olay_hash('overlays',cv_olay_key),asarr(label,val)]
  endif
  
end

; dgp rev 8/16/05 determine current overlay key
function fn_olay_glob_get_current

  common cm_olay_info, cv_olay_key, cv_olay_hash
  
  if (size(cv_olay_key,/type) eq 0) then cv_olay_key = "Overlay 1"

  return, cv_olay_key

end

; dgp rev 8/15/05 display position, shift display if new
function fn_calc_olay_pos

     cur_pos = fn_olay_cur_get('position')

     if (fn_olay_glob_get('state') ne 'reopen') then begin
       ; increment old_pos
       max_x = !display_size.x
       sze = fn_olay_cur_get('size')
       cur_pos(0) = cur_pos(0) + sze(0)
       if (cur_pos(0) ge max_x-sze(0)) then begin
         cur_pos(0) = 0
         cur_pos(1) = cur_pos(1) + (float(sze(1))/2.0)
       endif
     endif
     
     return, cur_pos

end

; dgp rev 8/16/05 create a new overlay display and call the 
; overlay drawing routine - dcb_olay_draw
pro _pr_olay_create

   common dspCells, dspCells_Pos

     new_pos = fn_calc_olay_pos()

     ; Get the new overlay name
     olay_name = fn_olay_glob_get_current()

     list_item = olay_name
     disp_name = olay_name
     lo_name = 'Olay Layout'
     title = olay_name
     
     top  = _wms_get_main('top')

     main = WwMainWindow(top, lo_main, 'ccb_olay_close', shell_name=disp_name, /ver, $
                         position=new_pos, layout_name=lo_name, title=title)
                         
     lo_buttons = WwLayout(lo_main,/hor,/top,/left,/right)
     lo_draw    = WwLayout(lo_main,/form,top=lo_buttons,/Left, /Right,/Bottom)
     
     draw_name = 'Olay Area'
     layout_name = 'Olay Window'
     
     perc = fn_olay_cur_get('pix perc')
     
     resize_txt = wwtext(lo_buttons,'cb_olay_resize',text=strtrim(perc,2))

     rb = WwRadioBox(lo_buttons, 'Overlay'  ,'cb_olay_set' ,name='Overlay Toggle', toggle=olay_tog, /nofmany)
     s = WwSetValue(olay_tog(0),fn_olay_cur_get('active')) 

     olay_but = WwButtonBox(lo_buttons, 'Export', 'cb_olay_export')
     olay_but = WwButtonBox(lo_buttons, 'X', 'cb_olay_x_layer')
    
     win = fn_calc_win_size(perc)
     dsp = fn_calc_dsp_size(perc)

     draw   = WwDrawing(lo_draw, new_win, 'dcb_olay_draw', $
                        win, dsp, layout=layout_name, $
                        background=Wht, foreground=Blk, name=draw_name, /noscroll, area=_draw_area, $
                        /top, /left,/right,/bottom)
     if (draw eq 0) then _pr_dac_error

                                  
     ; record the new window
     
     _pr_olay_cur_set, 'olay win', new_win
     _pr_olay_cur_set, 'main widget', main
     
     s = WwHandler(main,'fchd_olay_focus','FocusChangeMask')

     status = WwSetValue(main,/display)
                  
     _pr_olay_cur_set, 'position', dgpGetValue(main,/position)
     _pr_olay_cur_set, 'size', dgpGetValue(main,/size)

end

; dgp rev 8/16/05 routine to reopen overlay
pro _pr_olay_reopen

  parent = fn_olay_cur_get('main widget')
  _pr_olay_cur_set, 'position', dgpGetValue(parent,/position)
  _pr_olay_cur_set, 'size', dgpGetValue(parent,/size)
  s      = WwSetValue(parent,/close)

  _pr_olay_create

end

; dgp rev 8/16/05 draw the entire overlay
pro _pr_olay_draw, olay_name

  if (fn_olay_switch(olay_name)) then begin

    _pr_olay_cur_set, 'previous', ""

    single = fn_olay_glob_get('single')
    ; single or dual
    basis = fn_gen_get('pixel count')
    if (single) then begin
      !p.position = [.2,.5,.95,.9]

      _pr_olay_single_frame
      !p.position = [.2,.33,.95,.9]
    endif else begin

      _pr_olay_dual_frame

    endelse

    ; get the list of layers
    olay_list = fn_olay_list()
    _pr_olay_cur_set, 'count', n_elements(olay_list)
    _pr_dsp_olay_title

    lines = reverse(fn_olay_cur_get('lines'))

    xtitle = ["X Lable"]
    ytitle = ["Y Lable"]
    xcolor = [0]
; loop thru each overlay
    for i=0,n_elements(olay_list)-1 do begin
      ; switch to the first layer
      _pr_olay_layer_switch, olay_list(i)
      ; overlay index
      xtitle = [xtitle,fn_olay_layer_get('xtitle')]
      if (not single) then ytitle = [ytitle,fn_olay_layer_get('ytitle')]
      xcolor = [xcolor,fn_olay_layer_get('color')]
      _pr_olay_cur_set, 'index', i
      _pr_olay_cur_set, 'y label', lines(i+1)
      ; display that layer
      _pr_olay_layer
    endfor

    _pr_dsp_olay_xtitle, xtitle(1:*), xcolor(1:*)
    if (not single) then _pr_dsp_olay_ytitle, ytitle(1:*), xcolor(1:*)
    
  endif

end

; dgp rev 8/16/05 remove layer from key
pro _pr_olay_remove_layer, key

  common cm_olay_info, cv_olay_key, cv_olay_hash

  keys = askeys(cv_olay_hash('overlays',key,'layers'))
  
  keys = keys(sort(keys))
  
  cnt = n_elements(keys)
  
  last_layer = 'Layer ' + strtrim(cnt,2)
  
  print, "Remove ",last_layer

  tmp = asarr(keys(0),cv_olay_hash('overlays',key,'layers',keys(0)))
  for i=1,cnt-1 do begin
    if (keys(i) ne last_layer) then tmp = [tmp,asarr(keys(i),cv_olay_hash('overlays',key,'layers',keys(i)))]
  endfor
  cv_olay_hash('overlays',key,'layers') = tmp
  cv_olay_hash('overlays',key,'count') = cnt - 1  
  wset, cv_olay_hash('overlays',key,'olay win')

  _pr_olay_draw, key

end

; dgp rev 8/16/05 remove last layer
pro cb_olay_x_layer, wid, value

  common cm_olay_info, cv_olay_key, cv_olay_hash

  key = fn_wid_parent_name(wid)
  parent = _wid_get_parent(wid)

  if (isaskey(cv_olay_hash('overlays'),key)) then begin
    if (isaskey(cv_olay_hash('overlays',key),'count')) then begin
      cnt = cv_olay_hash('overlays',key,'count')
      print, "Remove layer ",cnt," from overlay ",key
      if (cnt eq 1) then begin
      ; xyzzy
        _pr_olay_glob_set, 'state', 'create'
        s = WwSetValue(parent,/close)
      endif else begin
        _pr_olay_remove_layer, key
      endelse
    endif
  endif

end

; dgp rev 8/16/05 create a new overlay display and call the 
pro fchd_olay_focus, wid, shell, event

  if (event.type ne 9) then return
  ; from the callback id, determine the window number
  
  parent = _wid_get_parent(wid)

  name   = WtGet(parent,/name)

  print, "Focus ",name

  _pr_olay_switch, name

end

; dgp rev 8/16/05
; drawing callback for overlays
pro dcb_olay_draw, wid, value

  Common colour, Wht, Blk

  parent = _wid_get_parent(wid)
  olay_name = WtGet(parent,/name)

  _pr_olay_draw, olay_name

end

; dgp rev 8/15/05 relative display size based upon percentage
function fn_calc_olay_dsp

  pixel_count = float(fn_olay_cur_get('pix perc'))/100.0 * !display_size.y

  win_text   = float(pixel_count)*.35
  win_margin = float(pixel_count)*.05

  xdsp = long(pixel_count+win_margin)
  ydsp = long(pixel_count+win_text+win_margin)

  return, [xdsp,ydsp]

end
; dgp rev 8/15/05 relative window size based upon percentage
function fn_calc_olay_win

  pixel_count = float(fn_olay_cur_get('pix perc'))/100.0 * !display_size.y

  win_buffer = float(pixel_count)*.025
  win_text   = float(pixel_count)*.35
  win_margin = float(pixel_count)*.05

  xwin = long(pixel_count+win_margin+win_buffer)
  ywin = long(pixel_count+win_text+win_margin+win_buffer)

  return, [xwin,ywin]

end
; dgp rev 8/18/05 switch the current overlay key
pro _pr_olay_switch, key

  common cm_olay_info, cv_olay_key, cv_olay_hash

  if (key eq cv_olay_key) then return
  
  if (isaskey(cv_olay_hash,'overlays')) then begin
    if (isaskey(cv_olay_hash('overlays'),key)) then begin
      print, "olay switch from ",cv_olay_key," to ",key
      cv_olay_key = key
      return
    endif
  endif  

  print, "Switch problem"

end
; dgp rev 8/18/05 call window export routine based upon overlay key
; dgp rev 1/9/06 determine if single parameter before export
pro _pr_olay_export, key

  _pr_olay_switch, key

  win = fn_olay_cur_get('olay win')
  _pr_gen_set, 'single', fn_olay_glob_get('single')

  _pr_gen_set, 'unique name', _gen_unique_name()
  _pr_ppt_export, win

end

; dgp rev 8/18/05 determine overlay key to export
pro cb_olay_export, wid, value

  key = WtGet(_wid_get_parent(wid),/name)

  _pr_olay_export, key

end

pro _pr_olay_refresh, wid

     parent = _wid_get_parent(wid)
     _pr_olay_cur_set, 'position', dgpGetValue(parent,/position)
     _pr_olay_cur_set, 'size', dgpGetValue(parent,/size)
     _pr_olay_glob_set, 'state', 'reopen'
     _pr_olay_reopen
     
end

pro cb_olay_resize, wid, value
  
  percentage = dgpGetValue(wid)

  _pr_olay_cur_set, 'pix perc', percentage
  
  _pr_olay_refresh, wid

end

pro _pr_olay_debug

  common cm_olay_info, cv_olay_key, cv_olay_hash

  stop ;debug
  
end

; dgp rev 8/16/05 remove an overlay from the olay structure
pro _pr_olay_remove, key

  common cm_olay_info, cv_olay_key, cv_olay_hash

  keys = askeys(cv_olay_hash('overlays'))
  cnt = n_elements(keys)
  print, "Remove key ",key

  for i=0,cnt-1 do begin
    if (key ne keys(i)) then begin
      if (size(tmp,/type) eq 11) then begin
        tmp = [tmp,asarr(keys(i),cv_olay_hash('overlays',keys(i)))]
      endif else begin
        tmp = asarr(keys(i),cv_olay_hash('overlays',keys(i)))
      endelse
    endif
  endfor
  
  cv_olay_hash('overlays') = tmp

end

pro _pr_olay_find_current

  common cm_olay_info, cv_olay_key, cv_olay_hash

  arr = fn_olay_active()
  _pr_olay_switch, arr(0)

  print, "Failover olay is ",arr(0)

end

; dgp rev 8/18/05 close the overlay
pro _pr_olay_close, parent

; keep the overlay structure if simply a reopen
  if (fn_olay_glob_get('state') eq 'reopen') then return

  if (parent ne 0) then begin
    par_name = WtGet(parent,/name)
    _pr_gen_set, 'closing', par_name
    _pr_olay_remove, par_name
  endif

; may need to set to a new overlay
  _pr_olay_find_current

end

; dgp rev 8/16/05 destroy routine for any overlay
pro ccb_olay_close, shell, wid

  common main_info, state

; if program exit, then forget the bookkeeping
  if (fn_gen_getstr('state') eq 'exit') then return

; determine the parent
  parent = _wid_get_parent(wid)

  _pr_olay_close, parent

end

; dgp rev 9/05/05 create a template for the overlays
pro _pr_olay_init

  common cm_olay_info, cv_olay_key, cv_olay_hash

  cv_olay_key = "default"
  cv_olay_hash = asarr('overlays',asarr(cv_olay_key,asarr('ident',"")))

  ; flag create for a new window
  cv_olay_hash = [cv_olay_hash,asarr('state','create')]

end
; dgp rev 9/06/05 create a new overlay and the first layer.
pro _pr_olay_layer_new, par_ident

  common cm_olay_info, cv_olay_key, cv_olay_hash

  ; create new overlay
  okeys = askeys(cv_olay_hash('overlays'))
  okeys = okeys(sort(okeys))
  cv_olay_key = 'Overlay ' + strtrim(string(n_elements(okeys)),2)
  for i=0,n_elements(okeys)-1 do begin
    key = 'Overlay ' + strtrim(string(i+1),2)
    if (not isaskey(cv_olay_hash('overlays'),key)) then begin
      print, "No key for ",key
      cv_olay_key = key
    endif
  endfor
  layer_key = "Layer 1"
  cv_olay_hash('overlays') = $
        [cv_olay_hash('overlays'),asarr(cv_olay_key,asarr('layers',asarr(layer_key,fn_dmf_get('quick plot'))))]

  cv_olay_hash('overlays',cv_olay_key) = [cv_olay_hash('overlays',cv_olay_key),asarr('active',1)]
  print, "New overlay ",cv_olay_key

end

; dgp rev 9/06/05 append a new 'quick plot' layer to the current overlay
pro _pr_olay_layer_append

  common cm_olay_info, cv_olay_key, cv_olay_hash

  ; add to current overlay - get next layer
  lkeys = askeys(cv_olay_hash('overlays',cv_olay_key,'layers'))
  layer_key = 'Layer ' + strtrim(string(n_elements(lkeys)+1),2)
  cv_olay_hash('overlays',cv_olay_key,'layers') = $
        [cv_olay_hash('overlays',cv_olay_key,'layers'),asarr(layer_key,fn_dmf_get('quick plot'))]
        
  print, "Append to overlay ",cv_olay_key
  print, layer_key

end

pro _pr_olay_set_layer, label, val

  common cm_olay_info, cv_olay_key, cv_olay_hash

  if (size(cv_olay_hash('overlays',cv_olay_key,'last layer'),/type) eq 0) then return

  key = cv_olay_hash('overlays',cv_olay_key,'last layer')
  
  if (isaskey(cv_olay_hash('overlays',cv_olay_key,'layers',key),label)) then begin
    cv_olay_hash('overlays',cv_olay_key,'layers',key,label) = val
  endif else begin
    cv_olay_hash('overlays',cv_olay_key,'layers',key) = $
      [cv_olay_hash('overlays',cv_olay_key,'layers',key),asarr(label,val)]
  endelse
    
end

pro _pr_olay_glob_set, label, val

  common cm_olay_info, cv_olay_key, cv_olay_hash

  cv_olay_hash = [cv_olay_hash,asarr(label,val)]
  
end

pro _pr_olay_layer_switch, key

  common cm_olay_info, cv_olay_key, cv_olay_hash

  if (isaskey(cv_olay_hash('overlays',cv_olay_key,'layers'),key)) then begin
    cv_olay_hash('overlays',cv_olay_key) = [cv_olay_hash('overlays',cv_olay_key),asarr('last layer',key)]
  endif

end

pro _pr_odp_set_cols, col_cnt

  _pr_olay_cur_set, 'col idx', 0
  _pr_olay_cur_set, 'col cnt', col_cnt

  col_right = fn_olay_cur_get('col right')
  col_left  = fn_olay_cur_get('col left')
  chunk = (col_right-col_left)/(col_cnt-1)
  col_arr = indgen(col_cnt)*chunk + col_left
  _pr_olay_cur_set, 'col arr', col_arr
  _pr_olay_cur_set, 'col width', chunk

end

pro _pr_olay_single_frame

  Common colour, Wht, Blk
  common _lgln1, lgln_mx, lgln_min, lgln_lbls, lgln_blanks, lgln_state   
  
  disp_res = fn_dmf_calc_res()
  real_res = _dsp_get_norm()
  res_ratio = float(disp_res)/float(real_res)

  lbl_x = fn_lgln_get('x labels')
  tk_max_x = fn_lgln_get('x max ticks')*res_ratio
  blnk_x = fn_lgln_get('x blanks')

  par = max(_cdr_get_pars())

  ymax = fn_olay_cur_get('data ymax')

  xlow = (disp_res-1) * fn_dmf_xmin_ratio()
  xhi = (disp_res-1) * fn_dmf_xmax_ratio()

  x_rng = [xlow,xhi]

  xidx = where(tk_max_x ge xlow and tk_max_x le xhi,xcnt) 

  frame_com = "plot, x_rng, [0, ymax], /nodata, xs=1, ys=1, "
  if xcnt ne 0 then frame_com = frame_com + "xtickname=lbl_x(xidx), xticks = xcnt, xtickv=tk_max_x(xidx), "
  frame_com = frame_com + "background=wht, color=blk"

;  frame_com = "plot, [0,disp_res-1], [0, ymax], /nodata, xs=1, ys=1, background=wht, color=blk, "
;  frame_com = frame_com + "xtickname=lbl_x, xticks = n_elements(tk_max_x)-1, xtickv=tk_max_x "

  ; dgp rev 3/14/08 debug statement to deactivate error handling on PLOT command
  ON_IOERROR, problem
  results = execute(frame_com)
  ON_IOERROR, null

  _pr_olay_cur_set, 'width', !D.X_Size
  _pr_olay_cur_set, 'height', !D.Y_Size

  dev_coors = convert_coord(!x.crange,!y.crange,/data,/to_device)
  ymax = dev_coors(1,1)
  orig = dev_coors(1,0)
  col_left = dev_coors(0,0)
  col_right = dev_coors(0,1)

  dev_coors = convert_coord(!x.region,!y.region,/norm,/to_device)
  ano_top = dev_coors(1,1)
  ano_bot = dev_coors(1,0)
  ano_left = dev_coors(0,0)
  ano_right = dev_coors(0,1)

  _pr_olay_cur_set, 'ano top',  ano_top  
  _pr_olay_cur_set, 'ano bot',  ano_bot  
  _pr_olay_cur_set, 'ano right',  ano_right 
  _pr_olay_cur_set, 'ano left',  ano_left 

  _pr_olay_cur_set, 'row ymax',  ymax
  _pr_olay_cur_set, 'row orig',  orig
  row_spacing = !d.y_ch_size*!p.charsize
  _pr_olay_cur_set, 'row spacing', row_spacing
  
  lpp = long((ymax-orig)/row_spacing)
  lines = (findgen(lpp)*row_spacing)+orig
  _pr_olay_cur_set, 'lines', lines

  row_pos = ymax - (row_spacing*1.5)
  _pr_olay_cur_set, 'cur y', row_pos
  _pr_olay_cur_set, 'cur y', row_pos
  _pr_olay_cur_set, 'col left', col_left
  _pr_olay_cur_set, 'cur x', col_left
  _pr_olay_cur_set, 'col right', col_right
  
  _pr_odp_set_cols, 3
 
  return

   problem:
   print, !err
   print, !err_string
 
end

; dgp rev 8/22/2012 
pro _pr_olay_dual_frame

  Common colour, Wht, Blk
  common _lgln1, lgln_mx, lgln_min, lgln_lbls, lgln_blanks, lgln_state   

  disp_res = fn_dmf_calc_res()
  real_res = _dsp_get_norm()
  res_ratio = float(disp_res)/float(real_res)

  lbl_x = fn_lgln_get('x labels')
  tk_max_x = fn_lgln_get('x max ticks')*res_ratio
  blnk_x = fn_lgln_get('x blanks')
  lbl_y = fn_lgln_get('y labels')
  tk_max_y = fn_lgln_get('y max ticks')*res_ratio
  blnk_y = fn_lgln_get('y blanks')

  pars = _cdr_get_pars()

   xlow = (disp_res-1) * fn_dmf_xmin_ratio()
   xhi = (disp_res-1) * fn_dmf_xmax_ratio()

   ylow = (disp_res-1) * fn_dmf_ymin_ratio()
   yhi = (disp_res-1) * fn_dmf_ymax_ratio()

   x_rng = [xlow,xhi]
   y_rng = [ylow,yhi]

  xidx = where(tk_max_x ge xlow and tk_max_x le xhi,xcnt) 
  yidx = where(tk_max_y ge ylow and tk_max_y le yhi,ycnt) 

  frame_com = "plot, x_rng, y_rng, /nodata, xs=1, ys=1, "
  if xcnt ne 0 then frame_com = frame_com + "xtickname=lbl_x(xidx), xticks = xcnt, xtickv=tk_max_x(xidx), "
  if ycnt ne 0 then frame_com = frame_com + "ytickname=lbl_y(yidx), yticks = ycnt, ytickv=tk_max_y(yidx), "
  frame_com = frame_com + "background=wht, color=blk "
;  frame_com = frame_com + "xtitle=xlab, ytitle=ylab "

;  frame_com = "plot, [0,disp_res-1], [0, disp_res-1], /nodata, xs=1, ys=1, background=wht, color=blk, "
;  frame_com = frame_com + "xtickname=lbl_x, xticks = n_elements(tk_max_x)-1, xtickv=tk_max_x, "
;  frame_com = frame_com + "ytickname=lbl_y, yticks = n_elements(tk_max_y)-1, ytickv=tk_max_y "
;  frame_com = frame_com + "xtitle=fn_par_name(pars(0)-1), ytitle=fn_par_name(pars(1)-1) "
  ..locals 6 0
  results = execute(frame_com)

  _pr_olay_cur_set, 'width', !D.X_Size
  _pr_olay_cur_set, 'height', !D.Y_Size

  dev_coors = convert_coord(!x.crange,!y.crange,/data,/to_device)
  ymax = dev_coors(1,1)
  orig = dev_coors(1,0)
  col_left = dev_coors(0,0)
  col_right = dev_coors(0,1)

  dev_coors = convert_coord(!x.region,!y.region,/norm,/to_device)
  ano_top = dev_coors(1,1)
  ano_bot = dev_coors(1,0)
  ano_left = dev_coors(0,0)
  ano_right = dev_coors(0,1)

  _pr_olay_cur_set, 'ano top',  ano_top  
  _pr_olay_cur_set, 'ano bot',  ano_bot  
  _pr_olay_cur_set, 'ano right',  ano_right 
  _pr_olay_cur_set, 'ano left',  ano_left 
  
  _pr_olay_cur_set, 'row ymax',  ymax
  _pr_olay_cur_set, 'row orig',  orig

  row_spacing = !d.y_ch_size*!p.charsize
  _pr_olay_cur_set, 'row spacing', row_spacing

  lpp = long((ymax-orig)/row_spacing)
  lines = (findgen(lpp)*row_spacing)+orig
  _pr_olay_cur_set, 'lines', lines

  row_pos = dev_coors(1,0) - (row_spacing*1.5)
  _pr_olay_cur_set, 'cur y', row_pos
  _pr_olay_cur_set, 'cur y', row_pos
  _pr_olay_cur_set, 'cur x', col_left
  _pr_olay_cur_set, 'col left', col_left
  _pr_olay_cur_set, 'col right', col_right

  _pr_odp_set_cols, 3

end

pro _pr_olay_crhflf

  _pr_olay_cur_set, 'col idx', 0
  
  half = float(fn_olay_cur_get('row spacing'))/2.0

  _pr_olay_cur_set, 'cur y', (fn_olay_cur_get('cur y') - half)
  
end

pro _pr_subt_type_cr

  _pr_olay_cur_set, 'col idx', 0
  
end

pro _pr_olay_crlf

  _pr_olay_cur_set, 'col idx', 0

  _pr_olay_cur_set, 'cur y', (fn_olay_cur_get('cur y') - fn_olay_cur_get('row spacing')) 
  
end

pro _pr_olay_layer_subtitle

  Common colour, Wht, Blk

  _pr_olay_crhflf

  applied = fn_olay_layer_get('applied')
;  only = string(fn_olay_layer_get('1only'))
;  if (only eq '0') then only = ""
  only = ""
 
  filename = fn_olay_layer_get('filename')

  line = fn_olay_layer_get('line style')
  color_idx = fn_olay_layer_get('color')
  color_rgb = WoColorConvert(color_idx,/IndextoColor)

  row_pos = fn_olay_cur_get('cur y')
  col_left = fn_olay_cur_get('col left')
  col_right = fn_olay_cur_get('col right')
  previous = fn_olay_cur_get('previous')
  
  plots, [col_left, col_right], [row_pos,row_pos], /device, color=color_rgb, linestyle=line
  _pr_olay_crlf

  if (previous ne filename) then begin
    _pr_olay_type_row, filename, color_rgb
  endif

  _pr_olay_cur_set, 'color', blk
  for i=0,n_elements(applied)-1 do begin
    if (applied(i) eq only) then begin
      _pr_olay_cur_set, 'color', color_rgb
      _pr_olay_type_col, applied(i)
      _pr_olay_cur_set, 'color', blk
    endif else begin
      _pr_olay_type_col, applied(i)
    endelse
  endfor

  _pr_olay_cur_set, 'previous', filename

end

pro _pr_olay_layer
  
  plot_type = fn_olay_layer_get('type')  
  
  if (size(plot_type,/type) eq 7) then begin
    if (plot_type eq 'contour') then _olay_layer_contour
    if (plot_type eq 'histo') then _olay_layer_histo
    if (plot_type eq 'dot') then _olay_layer_dot
  endif

  _pr_olay_layer_subtitle
  
end


pro _pr_dsp_olay_xtitle, xtitle, xcolor

    row_axis = fn_olay_cur_get('row orig')
    ano_bot = fn_olay_cur_get('ano bot') 

    row_pos = (row_axis+ano_bot)/2

    col_left  = fn_olay_cur_get('col left') 
    col_right = fn_olay_cur_get('col right') 

    cnt = n_elements(xtitle)
    ycenter = (col_left+col_right)/2
    chunk    = (col_right-col_left)/(cnt+1)
    cols     = indgen(cnt+2)*chunk + col_left    
    
    for i=0,cnt-1 do begin
      xyouts, cols(i+1), row_pos, xtitle(i), color=WoColorConvert(xcolor(i)), /device, align=.5
    endfor    

end

pro _pr_dsp_olay_ytitle, ytitle, ycolor

    row_axis = fn_olay_cur_get('col left')
    ano_bot = fn_olay_cur_get('ano left') 

    x_pos = (row_axis+ano_bot)/2

    top  = fn_olay_cur_get('row ymax') 
    bot = fn_olay_cur_get('row orig') 

    cnt = n_elements(ytitle)
    chunk    = (top-bot)/(cnt+1)
    y_cols     = indgen(cnt+2)*chunk + bot    
    
    for i=0,cnt-1 do begin
      xyouts, x_pos, y_cols(i+1), ytitle(i), color=WoColorConvert(ycolor(i)), /device, align=.5, orient=90
    endfor    

end
; dgp rev 1/5/06 dot size can be modified
pro _olay_layer_dot

  Common colour, Wht, Blk

  hist1 = fn_olay_layer_get('hist1')
  hist2 = fn_olay_layer_get('hist2')
  color_idx = fn_olay_layer_get('color')
  color_rgb = WoColorConvert(color_idx,/IndextoColor)
  dotsize = float(fn_dmf_get('dot size'))
  plot_com  = "oplot, hist1, hist2, psym=2, symsize=dotsize, color=color_rgb, background=Wht, xs=5, ys=5"
        
  ..locals 6 0
  results = execute(plot_com)

end

pro _pr_olay_ano_single, color_rgb

  Common colour, Wht, Blk

  full_ano  = fn_olay_layer_get('annotate')
  ; annotate overlay

  if (size(full_ano,/type) eq 11) then begin  
    if (isaskey(full_ano,'on')) then begin

      gate_names = fn_gen_get('gate names')

      ano   = full_ano('on')
      index = fn_olay_cur_get('index')
      cnt   = fn_olay_cur_get('count')
      data_ymax   = fn_olay_cur_get('data ymax')
      keys  = askeys(ano)
      top   = fn_olay_cur_get('top')
      ymax  = fn_olay_cur_get('row ymax')
      chk   = (top-ymax)/cnt
      ytop  = top-(chk*4/5)
      ypos  = ytop - chk*(index+1)

      for i=0,n_elements(ano)-1 do begin
        roi  = ano(keys(i),'roi')
        name = ano(keys(i),'name')
        perc = ano(keys(i),'perc')
        mask = ano(keys(i),'mask')

        results = where(name eq gate_names,cnt)
        if (cnt eq 0) then begin
          first = 1
          gate_names = [gate_names,name]
          _pr_gen_set, 'gate names', gate_names
        endif else begin
          first = 0
        endelse
     
        dev_roi = convert_coord(roi,[0,data_ymax],/data,/to_device)
        
        xb = dev_roi(0,0)
        xe = dev_roi(0,1)
        yb = dev_roi(1,0)
        ye = dev_roi(1,1)

      ; draw gate
        if (((mask and 1) ne 0)  and first) then begin

          mgs_color_idx = fn_gen_get('mgs color')
  
          mgs_color_rgb = WoColorConvert(mgs_color_idx,/IndextoColor)
  
          plots, [xb,xb], [yb,ye] ,/device, color=mgs_color_rgb
          plots, [xe,xe], [yb,ye] ,/device, color=mgs_color_rgb
  
        endif
      ; annotate label
        ypos = ye - fn_olay_cur_get('row spacing')

        if (((mask and 2) ne 0)  and first) then begin
            XYOUTS, xe, ypos, name, color=blk, align=1, /device
        endif
;        ypos = ypos - fn_olay_cur_get('row spacing')

        ypos = fn_olay_cur_get('y label')

    ; annotate the percentage  
        if ((mask and 4) ne 0) then begin
          fperc = string(perc,format="(F6.2)")
          XYOUTS, xe, ypos, fperc, color=color_rgb, align=1, /device
        endif
      
      endfor
    endif
  endif

end

pro _pr_olay_ano_dual, reso, color_rgb

  Common colour, Wht, Blk

  full_ano  = fn_olay_layer_get('annotate')
  ; annotate overlay
  if (size(full_ano,/type) eq 11) then begin  
    if (isaskey(full_ano,'on')) then begin  

    gate_names = fn_gen_get('gate names')

    ano = full_ano('on')
    index = fn_olay_cur_get('index')
    cnt = fn_olay_cur_get('count')
    if (cnt lt 3) then cnt = 3
    keys = askeys(ano)

    for i=0,n_elements(ano)-1 do begin
      roi = ano(keys(i),'roi')
      name = ano(keys(i),'name')
      perc = ano(keys(i),'perc')
      mask = ano(keys(i),'mask')

      results = where(name eq gate_names,cnt)
      if (cnt eq 0) then begin
        first = 1
        gate_names = [gate_names,name]
        _pr_gen_set, 'gate names', gate_names
      endif else begin
        first = 0
      endelse
      
      dev_roi = convert_coord(roi(0:1),roi(2:3),/data,/to_device)
        
      xb = dev_roi(0,0)
      xe = dev_roi(0,1)
      yb = dev_roi(1,0)
      ye = dev_roi(1,1)


      xmid = (xe+xb)/2

    ; outline gates
      if (((mask and 1) ne 0) and first) then begin

        mgs_color_idx = fn_gen_get('mgs color')
        mgs_color_rgb = WoColorConvert(mgs_color_idx,/IndextoColor)
  
        plots, [xb,xb], [yb,ye], /device,color=mgs_color_rgb
        plots, [xe,xe], [yb,ye], /device,color=mgs_color_rgb
        plots, [xb,xe], [ye,ye], /device,color=mgs_color_rgb
        plots, [xb,xe], [yb,yb], /device,color=mgs_color_rgb

      endif
  ; label gates
      ypos = yb
      if (((mask and 2) ne 0) and first) then begin
        XYOUTS, xmid, ypos, name, color=blk, align=.5, /device
      endif

    ; annotate the percentage  
      if ((mask and 4) ne 0) then begin

        ypos = yb - fn_olay_cur_get('row spacing')*(index+1)
        fperc = string(perc,format="(F6.2)")
        XYOUTS, xe, ypos, fperc, color=color_rgb, align=1, /device

      endif
      
    endfor
    endif
  endif

end


pro _olay_layer_histo

  Common colour, Wht, Blk
  
  data = fn_olay_layer_get('data')
  line = fn_olay_layer_get('line style')
  thick = fn_olay_layer_get('thick')
  ymax = fn_olay_layer_get('ymax')  
  color_idx = fn_olay_layer_get('color')  
  color_rgb = WoColorConvert(color_idx,/IndextoColor)
  yrange = ", yrange=[0,"+strtrim(ymax,2)+"]"

  scale = float(!y.crange(1))/float(ymax)
  data = data*scale

  plot_com  = "oplot, data, color=color_rgb, background=Wht, xs=5, ys=5, line=line, thick=thick" + yrange
        
  ..locals 6 0
  results = execute(plot_com)

  _pr_olay_ano_single, color_rgb

end

; dgp rev 5/23/2012 Add the outlier related code to the contour overlay 
pro _olay_layer_contour

  Common colour, Wht, Blk

  data = fn_olay_layer_get('data')  
  lvl_arr = fn_olay_layer_get('levels')
  color_idx = fn_olay_layer_get('color')  
  color_rgb = WoColorConvert(color_idx,/IndextoColor)

  reso = fn_olay_layer_get('reso')
  norm_res = _dsp_get_norm()
  ratio = (float(norm_res)/float(reso))
  plot_com  = "contour, data, color=color_rgb, background=Wht, xs=5, ys=5"
  plot_com = plot_com + ", levels=lvl_arr, /overplot, thick=fn_dmf_get('line thick')"
        
  results = execute(plot_com)
  if fn_olay_layer_exists('outliers') then begin
    dout = fn_olay_layer_get('outliers')
    oplot, dout(0,*),dout(1,*), psym=2,symsize=0.01, color=color_rgb, back=wht, xs=5, ys=5
  endif

  _pr_olay_ano_dual, reso, color_rgb

end

pro _pr_olay_type_col, text

  row_pos = fn_olay_cur_get('cur y')
  color_idx = fn_olay_cur_get('color')
  color_rgb = WoColorConvert(color_idx,/IndextoColor)
  
  col_cnt = fn_olay_cur_get('col cnt')
  col_idx = fn_olay_cur_get('col idx')
  col_arr = fn_olay_cur_get('col arr')
  align   = fn_olay_cur_get('align')
    
  xyouts, col_arr(col_idx), row_pos, text, /device, color=color_rgb, align=align
  align = .5
  col_idx = (col_idx + 1) mod col_cnt
  if (col_idx eq col_cnt-1) then align = 1
  _pr_olay_cur_set,'align',align
  _pr_olay_cur_set,'col idx', col_idx
  if (col_idx eq 0) then _pr_olay_crlf    

end


pro _pr_subt_type_col, txt, color
       
    align = .5
    col_idx = fn_olay_cur_get('col idx') 
    if (col_idx eq 0) then align = 0
    col_cnt = fn_olay_cur_get('col cnt')  
    if (col_idx eq (col_cnt-1)) then align = 1

    row_pos = fn_olay_cur_get('cur y')
    col_arr = fn_olay_cur_get('col arr')  

    xyouts, col_arr(col_idx), row_pos, txt, align=align, color=color, /device
    
    hold = col_idx
    _pr_olay_cur_set, 'col idx', (col_idx + 1) mod fn_olay_cur_get('col cnt') 
  
    if (hold gt col_idx) then _pr_olay_crlf

end

pro _pr_olay_type_row, txt, color
    
    col_arr = fn_olay_cur_get('col arr')  

    xyouts, col_arr(fn_olay_cur_get('col idx') ), fn_olay_cur_get('cur y'), txt, /device, color=color
    
    _pr_olay_crlf

end

pro _pr_dsp_olay_title, one, two

  Common colour, Wht, Blk 

    col_left = fn_olay_cur_get('ano left') 
    col_right = fn_olay_cur_get('ano right') 
    top = fn_olay_cur_get('ano top') 
    ymax = fn_olay_cur_get('row ymax')
    ypos = (top+ymax)/2

    xyouts, col_left,  ypos , string("Proj: ",fn_gen_get('project')), /device, color=blk, align=0
    xyouts, col_right, ypos , string("Sess: ",fn_gen_get('session')), /device, color=blk, align=1

    ; setup cur y position for subtitles
    _pr_olay_cur_set, 'cur y', (fn_olay_cur_get('row orig') - fn_olay_cur_get('row spacing')*2) 
    _pr_gen_set, 'gate names', ""

end

; callback event to activate or deactivate 
; a given overlay.  If activated, then the 
; previous overlay must be turned off.  If 
; deactivated then prep for the next overlay
pro cb_olay_set, wid, value

  ; get the on or off value
  internal = dgpGetValue(wid)
  ; get the name of the overlay
  parent = _wid_get_parent(wid)
  name = WtGet(parent,/name)
  ; determine the win id

  _pr_olay_switch, name
  
  print, "Overlay ",name," set to ",internal

  _pr_olay_cur_set, 'active', internal
  
end

function _olay_get_on

  return, 0

end

pro _pr_olay_placement

    wid = fn_dmf_get('main widget')
    if (fn_wid_exists(wid)) then begin
      _pr_olay_cur_set, 'position', dgpGetValue(wid,/position)
      _pr_olay_cur_set, 'size', dgpGetValue(wid,/size)
    endif else begin
      _pr_olay_cur_set, 'position', fn_dmf_get('position')
      _pr_olay_cur_set, 'size', fn_dmf_get('size')
    endelse
    _pr_olay_cur_set, 'pix perc', fn_dmf_get('pix perc')

end
; dgp rev 9/06/05 scan the overlays for one to hold the 
; new layer.  The overlay must match with parameters and
; be turned on.  If no match, then create.
pro _pr_scan_olays
  ; get all overlays
  keys = fn_olay_active()

  if (keys(0) eq "") then begin
  ; no overlays, so create
    _pr_olay_layer_new
    _pr_olay_glob_set, 'state', 'create'
    _pr_olay_cur_set, 'ident', par_ident
    _pr_olay_placement
    return
  endif
  ; overlay exists, so check current first
  cur_ident = fn_olay_cur_get('ident')
  par_ident = fn_dmf_par_id()

  if (cur_ident eq par_ident) then begin
    ; if they match, then check if overlay is active
    if (fn_olay_cur_get('active')) then begin
      ; append layer to existing overlay
      _pr_olay_layer_append
      _pr_olay_glob_set, 'state', 'append'
      return
    endif
  endif

  ; current overlay is not the one, so loop thru remainder

  for i=0,n_elements(keys)-1 do begin
    _pr_olay_switch, keys(i)
    cur_ident = fn_olay_cur_get('ident')
    if (cur_ident eq par_ident) then begin
      ; if they match, then check if overlay is active
      if (fn_olay_cur_get('active')) then begin
        ; append layer to existing overlay
        print, "Found a valid, active overlay for ",par_ident
        _pr_olay_layer_append
        _pr_olay_glob_set, 'state', 'append'
        return
      endif
    endif
  endfor
  ; no valid overlays, so create a new overlay
  _pr_olay_layer_new
  _pr_olay_glob_set, 'state', 'create'
  _pr_olay_cur_set, 'ident', par_ident
  _pr_olay_placement
end

; dgp rev 8/16/05 the starting point for creating overlays from
; the display windw.  Either add to an existing overlay or create
; a new overlay
pro _pr_olay_add

    ; scan current overlays
    _pr_scan_olays

    ; get some dmf info
    _pr_olay_glob_set, 'single', fn_dmf_single()
    _pr_dmf_calc_ymax, ymax
    _pr_olay_cur_set, 'data ymax', ymax
    
    ; create a new window if necessary
    if (fn_olay_glob_get('state') eq 'create') then begin
      _pr_olay_cur_set, 'position', fn_dmf_get('position') 
      _pr_olay_create
    endif else begin
      olay_win = fn_olay_cur_get('olay win')      
      wset, olay_win
      ; get the name of the current overlay 
      ; get the source of the overlay info
      olay_name = fn_olay_glob_get_current()
      _pr_olay_draw, olay_name      
    endelse

end

pro flow_overlay

  print, "Add overlay routines"

end
