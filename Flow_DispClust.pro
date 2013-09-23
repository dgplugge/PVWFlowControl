declare func, _get_labels
declare func, _car_get_order
declare func, _wms_get_main
declare func, _car_count
declare func, _wid_get_parent
declare func, _win_get_clust
declare func, fn_wms_raise
declare func, fn_gen_get
declare func, dgpGetValue
declare func, fn_olay_cur_get

pro _save_order

  common par_order, tmp_array, par_array, wid_array, par_labels      
  
  par_array = tmp_array
  
end


function _get_main

  common wid_info, top, layout, main
  
  return, main

end

function _get_layout

  common wid_info, top, layout, main
  
  return, layout

end

function _set_main, tmp

  common wid_info, top, layout, main

  main = tmp

  return, 1

end

function _set_layout, tmp

  common wid_info, top, layout, main

  layout = tmp

  return, 1

end

function _find_wid, wid

  common par_order, tmp_array, par_array, wid_array, par_labels    
  
  index = index_conv(wid_array,where(wid_array eq wid))

  if (n_elements(index) eq 1) then index = [[index],[0]]

  return, index

end

function _get_count

  common par_order, tmp_array, par_array, wid_array, par_labels    
  
  return, tmp_array(0)

end


pro _par_redo, arr

  common par_order, tmp_array, par_array, wid_array, par_labels    

  wid_array = arr

end

pro _par_add_wid, arr

  common par_order, tmp_array, par_array, wid_array, par_labels    

  
  wid_array = [[wid_array],[arr]]
  
end

function _get_cols

  common par_order, tmp_array, par_array, wid_array, par_labels    
  
  results = make_array(n_elements(tmp_array),/long)

  for i=0,n_elements(tmp_array) do begin
  
    results(i) = wid_array(tmp_array(i+1))
  
  endfor
  
  return, results
end

pro CB_Apply, wid, value

  ;print, "Calling widget", wid, " value ",value

  while (WtGet(WtGet(wid ,/parent),/shell) ne 1) do wid = WtGet(wid ,/parent)
  
  name = WtGet(wid,/name)
  ;print, "Destroying  ",wid," called ",name
  wid_id = _wms_get_main(name)
  ;print, "Widget id ",wid_id
  status = WtClose(WtGet(wid ,/parent))
  
  _save_order
    
  _car_draw

end


pro _rebuild_but

   frm_name = 'reorder'
   main = _wms_get_main(frm_name)

   top = _wms_get_main('top')
   print, "Getting toplevel ",top
 
   print, "close main ",main
   pos = dgpGetValue(main,/position)
   if (not WtClose(main)) then print, "Did not Close ",main

  main  = WwMainWindow(top,layout,'ccb_wms_destory',pos=pos,/hor,spac=5,bord=3,shell=frm_name)
  print, "open new main ",main
  _wms_set_main, frm_name, main  
  labels = _get_labels()
  rows = n_elements(labels)
  count  = _get_count()
  params = _car_get_order()
  
  lo_arr = make_array(count+1,/long)
  rbox   = make_array(count+1,/long)
  box    = make_array(count+1,/long)
  
  for i=0,count-1 do begin
    lo_arr(i) = WwLayout(layout,/ver,spac=5,bord=3)
  
    t1 = WwText(lo_arr(i),/label,text=labels(params(i+1)-1))
    rbox(i) = WwRadioBox(lo_arr(i), labels, 'RadioCB', $
	/Vertical, Border=2, Spacing=5, toggles=rbox_but)  

    ; if not last item, then place a delete button under it
    
      box(i) = WwButtonBox(lo_arr(i),"X",'CB_DelCol',spacing=5)
      status = WwSetValue(box(i),userdata=i+1)
    ; update widget matrix
    if(i eq 0) then begin
      _par_redo, rbox_but
    endif else begin
      _par_add_wid, rbox_but
    endelse

  endfor
  blank = WwLayout(layout,/ver,spac=5,bord=3)
    
  tb = WwText(blank,/label,text=' ')
  rb = WwRadioBox(blank, labels, 'RadioCB', $
  	          /Vertical, Border=2, Spacing=5, toggles=rbox_but)  
  
      ; if not last item, then place a delete button under it
      ; update widget matrix
  if(count eq 0) then begin
        _par_redo, rbox_but
  endif else begin
        _par_add_wid, rbox_but
  endelse
    
  controls = WwLayout(layout,/ver,spac=5,bord=3)
  c1 = WwButtonBox(controls,"Cancel",'CB_Cancel',spacing=5)
  c2 = WwButtonBox(controls,"Apply",'CB_Apply',spacing=5)
  
  status = WwSetValue(main, /Display)

end

pro CB_DelCol, wid, value

  ud = dgpGetValue(wid,/userdata)
  _del_par, ud
  _rebuild_but

end

pro _set_val, val

  common par_order, tmp_array, par_array, wid_array, par_labels    
  
end

pro _mod_par, col, row

  common par_order, tmp_array, par_array, wid_array, par_labels    
  
  tmp_array(col) = row
  tmp_array(0) = n_elements(tmp_array)-1
   
end

pro _del_par, col

  common par_order, tmp_array, par_array, wid_array, par_labels    
  
  cnt = n_elements(tmp_array) - 1
  if (col lt cnt) then begin
    tmp_array = [tmp_array(0:col-1),tmp_array(col+1:*)]
    tmp_array(0) = n_elements(tmp_array)-1
  endif else begin
    tmp_array = tmp_array(0:col-1)
    tmp_array(0) = n_elements(tmp_array)-1
  endelse
  
end

pro _add_par, row

  common par_order, tmp_array, par_array, wid_array, par_labels    
    
  tmp_array = [tmp_array,row]
  tmp_array(0) = n_elements(tmp_array) - 1
    
end

PRO RadioCB, wid, which

  flag = dgpGetValue(wid)

  if(not flag) then return

    ; get row and col
    val = _find_wid(wid) + 1
    col = val(1)
    row = val(0)
    
    ; get current count  
    count = _get_count()
    
    ; is the count to be incremented?
    if(col lt count) then begin
      ;print, "Just modify"
      _mod_par, col, row
    endif else if (col eq count) then begin
      ;print, "Add a Column"
      _mod_par, col, row
    endif else begin
      _add_par, row
      ;print, "Add a Column and a parameter"
    endelse
    _rebuild_but

END


pro CB_Labels, wid, value

   frm_name = 'reorder'
   main = _wms_get_main(frm_name)
   print, "main wid is ",main

   top = _wms_get_main('top')
   main = WwMainWindow(top, layout, 'ccb_wms_destory', shell_name=frm_name, /ver)   
   _wms_set_main, frm_name, main

   labels = _get_labels()
    
   _rebuild_but

   _car_draw

end

pro ccb_car_win_close, wid, value

  common winClust, winClust_main, winClust_open
  
  winClust_open = 0

end

function _car_win_open

  common winClust, winClust_main, winClust_open
  if (size(winClust_open,/type) eq 0) then winClust_open = 0
  return, winClust_open

end

pro _car_win_main, wid

  common winClust, winClust_main, winClust_open

  winClust_main = wid
  winClust_open = 1

end

pro fchd_car_focus, wid, shell, event 

  print, "fchd_car_focus"
  wset, _win_get_clust()
  
end

PRO dcb_car_draw, wid, index

   _car_draw

  parent = _wid_get_parent(wid)

   status   = WwHandler(parent, 'fchd_car_focus', 'FocusChangeMask')


END

PRO bphd_car_draw, wid, shell, event

  common cm_car_info, counts_cluster, means_cluster, cluster_group, cluster_enable

  siz = n_elements(cluster_enable)

  cursor, x, y, /device
  pos_val = (tvrd(x, y,1,1) - 1)
  cl_num = pos_val(0)
  if (cl_num lt siz and cl_num gt 0) then begin
    print, "Hit cluster ",cl_num, " from widget ",wid, " named ",wtget(wid,/name)
    cluster_enable(cl_num) = 0
    _car_update
    ; redraw cells
    draw_cells   
  endif

END

; dgp rev 8/23/05 relative window size based upon percentage
function fn_calc_clu_win

  pixel_count = float(fn_olay_cur_get('pix perc'))/100.0 * !display_size.y

  win_buffer = float(pixel_count)*.025
  win_text   = float(pixel_count)*.35
  win_margin = float(pixel_count)*.05

  xwin = long(pixel_count+win_margin+win_buffer)
  ywin = long(pixel_count+win_text+win_margin+win_buffer)

  return, [xwin,ywin]

end

; dgp rev 8/23/05 relative window size based upon percentage
function fn_calc_clu_dsp

  pixel_count = float(fn_olay_cur_get('pix perc'))/100.0 * !display_size.y

  win_text   = float(pixel_count)*.35
  win_margin = float(pixel_count)*.05

  xwin = long(pixel_count+win_margin)
  ywin = long(pixel_count+win_text+win_margin)

  return, [xwin,ywin]

end

pro _car_create_win
   
  top  = _wms_get_main('top')

  max_x = !display_size.x
  max_y = !display_size.y

  pos = [max_x/5,max_y/5]

  win = 20

  if (not _car_win_open()) then begin
     
     winClust_main = WwMainWindow(top, layout,'ccb_car_win_close', shell_name='Cluster Window', $
                         position=pos)
     pixel_count = fn_gen_get('pixel count')
     
     win = fn_calc_clu_win()
     dsp = fn_calc_clu_dsp()
         
     draw   = WwDrawing(layout, win, 'dcb_car_draw', win, dsp, $
                    background=Wht, foreground=Blk, /Right, layout='Cluster',/noscroll, area=_car_area)
     if (draw eq 0) then _pr_dac_error

;     print, "Handle widget ",_car_area," with bphd_car_draw"
     
     status   = WwHandler(_car_area, 'bphd_car_draw', 'ButtonPressMask')
                        
     status   = WwSetValue(winClust_main,/display)
     
     _car_win_main, winClust_main
     
 endif
           
end

PRO cb_dcl_form, wid, index

   Common colour, Wht, Blk
   common frmClust, frmClust_main

   name = "Cluster Control"

   if (fn_wms_raise(name)) then return

     top = _wms_get_main('top')
     frmClust_main = WwMainWindow(top, layout, 'ccb_wms_destory', /form, shell_name='Clusters',pos=[50,0])  
     _wms_set_main, name, frmClust_main
     
     widget_layout = WwLayout(layout, /Ver)
     move_lo = WwLayout(widget_layout, /Hor)
     com_lo  = WwLayout(widget_layout, /Hor)

     rad_labs = indgen(_car_count())+1
     col = (_car_count() / 5) + 1
 
     slabel = ['Centroid thichness']
     range  = [1,200]
  
     grid_sliders = WwControlsBox(widget_layout, slabel, range, 'cb_car_slide', $
                                /Vert, /Text, sliders=slide_array)
     rad  = WwRadioBox(widget_layout, rad_labs, 'cb_car_tog', $
                    /Top, /Left, /Vertical, /Nofmany, /AlignLeft, $
                    measure=col, toggle=togs)
     _car_set_togs, togs
                      
     t1   = WwText(widget_layout, 'NoOpCB', /Label, $
               Text='Select cluster')

     pixel_count = fn_gen_get('pixel count')

     _car_create_win
     
     status       = WwSetValue(slide_array(0), 100, /Sensitive)
     thickness = 100

;     prebut   = WwButtonBox(move_lo, '<<Prev',  'CB_Prev')
;     nxtbut   = WwButtonBox(move_lo, 'Next>>',  'CB_Next')

     labelbut = WwButtonBox(com_lo, 'Reorder', 'CB_Labels', /Left, /Bottom)
     exitbut  = WwButtonBox(com_lo, 'Exit', 'CB_Exit', /Left, /Bottom)
     status   = WwSetValue(exitbut, Userdata=frmClust_main)
   
     status   = WwSetvalue(frmClust_main, /Display)
   
END

pro Flow_DispClust

  print, "Load Cluster Display routines"

end