
declare func, fn_wid_exists
declare func, _wms_get_main
declare func, _ct_get_name
declare func, _cdr_get_pars
declare func, _mgs_get_count
declare func, MyROI
declare func, _cfg_get_work
declare func, _gen_unique_name
declare func, _wid_get_parent
declare func, _mgs_get_sort_names
declare func, fn_dmf_calc_res
declare func, _dsp_get_norm
declare func, _data_get_par_cnt
declare func, _par_cur_names
declare func, fn_mgs_get_key
declare func, fn_mgs_get_subset
declare func, _wid_get_onoff
declare func, _ger_gates_on
declare func, fn_dmf_get
declare func, fn_dmf_single
declare func, fn_dmf_current
declare func, fn_data_get_cells
declare func, _mgs_recalc_gate
declare func, fn_wms_form
declare func, fn_wra_get_index
declare func, fn_wra_get
declare func, fn_mgs_get_sorted
declare func, _wms_rebuild
declare func, fn_dmf_get_index
declare func, fn_mgs_get
declare func, fn_stat_get_pptfile
declare func, fn_gen_get
declare func, fn_gen_getstr
declare func, fn_ppt_get
declare func, fn_dsp_get_num
declare func, fn_data_get_par_cnt
declare func, fn_upa_get_labels
declare func, fn_fnt_get
declare func, fn_dmf_get_xaxis
declare func, fn_dmf_get_yaxis
declare func, fn_gen_build_path
declare func, fn_wms_exists
declare func, dgpGetValue
declare func, fn_dmf_xmin_ratio
declare func, fn_dmf_xmax_ratio
declare func, fn_dmf_ymin_ratio
declare func, fn_dmf_ymax_ratio


; Restore is more complex than Save, as it must handle the incorporation 
; of new information.

; Gate stats will involve several type of routines:
; (fto) format text output - formating text on an output display for metafile export to PPT
; (stats) Statistical processing routines and structures
; (ato) ASCII text output - formating for ASCII output to a text file


; stats information is cleared
pro _pr_stats_clear

  common cm_stats_info, cv_stats_hash, cv_stats_key

  cv_stats_hash = complex(1)

end
; stat information is set by gate key
; this common block holds the individual statistics for each gate
pro _pr_stats_set_key, key

  common cm_stats_info, cv_stats_hash, cv_stats_key

;  if (isaskey(cv_stats_key,key)) then cv_stats_key = key
  cv_stats_key = key
 
end

pro _pr_stats_debug

  common cm_stats_info, cv_stats_hash, cv_stats_key

  stop ;debug

end


; dgp rev 10/28/05 new statistics structure
; stat value is stored with a log/lin flag
; stat is retrieved with a significant digit code
; dgp rev 10/28/05 get formatted stat data
function fn_sts_get_sig, label, sig_dig

  common cm_sts_info, cv_sts_hash, cv_sts_ident
  
  if (size(cv_sts_hash,/type) ne 11) then return, string(0,format=sig_dig)

  if (not isaskey(cv_sts_hash(cv_sts_ident),label)) then return, string(0,format=sig_dig)

  return, string(cv_sts_hash(cv_sts_ident,label),format=sig_dig)

end

; dgp rev 10/31/05 get info
function fn_sts_get, label

  common cm_sts_info, cv_sts_hash, cv_sts_ident
  
  if (size(cv_sts_hash,/type) ne 11) then return, 0

  if (not isaskey(cv_sts_hash(cv_sts_ident),label)) then return, 0

  return, cv_sts_hash(cv_sts_ident,label)

end

; dgp rev 10/31/05 get data (force to string)
function fn_sts_get_str, label

  common cm_sts_info, cv_sts_hash, cv_sts_ident
  
  if (size(cv_sts_hash,/type) ne 11) then return, ""

  if (not isaskey(cv_sts_hash(cv_sts_ident),label)) then return, ""

  return, string(cv_sts_hash(cv_sts_ident,label))

end

; dgp rev 10/28/05 get raw stat data
function fn_sts_get_raw, label

  common cm_sts_info, cv_sts_hash, cv_sts_ident
  
  if (size(cv_sts_hash,/type) ne 11) then return, 0

  if (not isaskey(cv_sts_hash(cv_sts_ident),label)) then return, 0

  return, cv_sts_hash(cv_sts_ident,label)

end

; dgp rev 10/28/05 get log/lin converted stat data
function fn_sts_getx, label, sig_dig

  value = fn_sts_get_raw(label)
  if (fn_dmf_get_xaxis()) then begin
    value = 10^(float(value)/float(256.0))
  endif
  return, string(value,format=sig_dig)

end

; dgp rev 10/28/05 get log/lin converted stat data
function fn_sts_gety, label, sig_dig

  value = fn_sts_get_raw(label)
  if (fn_dmf_get_yaxis()) then begin
    value = 10^(float(value)/float(256.0))
  endif
  return, string(value,format=sig_dig)

end

; dgp rev 10/28/05 debug stat data
pro _pr_sts_debug

  common cm_sts_info, cv_sts_hash, cv_sts_ident

  info, cv_sts_hash, /full
  
  stop ;debug

end

; dgp rev 10/28/05 switch namespace
pro _pr_sts_switch, ident

  common cm_sts_info, cv_sts_hash, cv_sts_ident

  cv_sts_ident = ident

end

; dgp rev 10/28/05 switch namespace
pro _pr_sts_clear

  common cm_sts_info, cv_sts_hash, cv_sts_ident

  cv_sts_hash = complex(1)

end

; dgp rev 10/28/05 set stat data to raw value
pro _pr_sts_set, label, value

  common cm_sts_info, cv_sts_hash, cv_sts_ident

  print, "Setting ",label, " to ",value

  if (size(cv_sts_hash,/type) ne 11) then begin
    ; create structure
    if (size(cv_sts_ident,/type) eq 0) then cv_sts_ident = 'default'
    cv_sts_hash = asarr(cv_sts_ident,asarr(label,value))
  endif else begin
    ; append or replace
    if (isaskey(cv_sts_hash,cv_sts_ident)) then begin
      cv_sts_hash(cv_sts_ident) = [cv_sts_hash(cv_sts_ident),asarr(label,value)]
    endif else begin
      cv_sts_hash = [cv_sts_hash,asarr(cv_sts_ident,asarr(label,value))]
    endelse
  endelse

end

pro _pr_stats_set, key, value

  common cm_stats_info, cv_stats_hash, cv_stats_key

  if (size(cv_stats_hash,/type) ne 11) then begin
    ; begin new hash
    cv_stats_hash = asarr(cv_stats_key,asarr(key,value))
  endif else begin
    if (isaskey(cv_stats_hash,cv_stats_key)) then begin
      ;append to existing key
      cv_stats_hash(cv_stats_key) = [cv_stats_hash(cv_stats_key),asarr(key,value)]
    endif else begin
      ; add new key to hash
      cv_stats_hash = [cv_stats_hash,asarr(cv_stats_key,asarr(key,value))]
    endelse
  endelse

end

function fn_stats_get, key

  common cm_stats_info, cv_stats_hash, cv_stats_key
 
  if (size(cv_stats_hash,/type) eq 11) then begin
    if (isaskey(cv_stats_hash,cv_stats_key)) then begin
      if (isaskey(cv_stats_hash(cv_stats_key),key)) then begin
        return, string(cv_stats_hash(cv_stats_key,key))
      endif
    endif
  endif
  return, 0

end

function fn_stats_get_str, key

  common cm_stats_info, cv_stats_hash, cv_stats_key
 
  if (size(cv_stats_hash,/type) eq 11) then begin
    if (isaskey(cv_stats_hash,cv_stats_key)) then begin
      if (isaskey(cv_stats_hash(cv_stats_key),key)) then begin
        return, string(cv_stats_hash(cv_stats_key,key))
      endif
    endif
  endif
  return, ""

end

function fn_stats_get_num, key

  common cm_stats_info, cv_stats_hash, cv_stats_key
 
  if (size(cv_stats_hash,/type) eq 11) then begin
    if (isaskey(cv_stats_hash,cv_stats_key)) then begin
      if (isaskey(cv_stats_hash(cv_stats_key),key)) then begin
        return, long(cv_stats_hash(cv_stats_key,key))
      endif
    endif
  endif
  return, 0

end


function _mgs_get_key_val, key, rec

  common cm_mgs_info, cv_mgs_struct

  if (size(cv_mgs_struct,/type) eq 0) then return, 0

  if (isaskey(cv_mgs_struct,key)) then begin
    tag_list = tag_names(cv_mgs_struct(key))
    idx = where(tag_list eq STRUPCASE(rec),cnt)
    if (cnt ne 1) then return, 0
    return, cv_mgs_struct(key).(idx(0))
  endif

end

pro _pr_mgs_rebuild_frm, wid

  parent = _wid_get_parent(wid)
  pos    = dgpGetValue(parent,/position)
  name   = WtGet(parent,/name)
  
  print, "Position is ",pos, " for ",name
  status = WwSetValue(parent,/close)
  _pr_mgs_reopen, name, pos

end

pro cb_row, wid, pass

  common grid_info, row_wid, col_wid

  name = WtGet(wid,/name)
  val  = dgpGetValue(wid)
  pos = where(row_wid eq wid)
  print, " Row ",pos(0)," is ",val
  
end

pro cb_col, wid, pass

  common grid_info, row_wid, col_wid

  name = WtGet(wid,/name)
  val  = dgpGetValue(wid)
  pos = where(col_wid eq wid)
  print, " Col ",pos(0)," is ",val
  
end


pro create_table, layout

  common grid_info, row_wid, col_wid

  names = _par_cur_names()

  row_lbl = ['Average','Median','Peak']
  col_lbl = names(1:*)

  arr = fn_sts_get('x geo mean','(F7.2)')

  row = n_elements(row_lbl)
  col = n_elements(col_lbl)
  row_wid = 0
  col_wid = 0
  
  rb = 1
  cb = 0
  ab = 1
  
  data = indgen(row,col)
  lo_arr = make_array(row+2,col+2,/long)
  
  hor_lo = WwLayout(layout,bord=rb,/hor)
  for c=0,col+1 do begin
    ver_lo = WwLayout(hor_lo,bord=cb,/ver)
    lo_arr(0,c) = wwlayout(ver_lo,bord=ab)
    for r=0,row+1 do begin
      lo_arr(r,c) = wwlayout(ver_lo,bord=ab)
    endfor
  endfor
  
  ; blanks
  for r=0,1 do begin
    for c=0,1 do begin
      lo_arr(r,c) = WwText(lo_arr(r,c),/label,text=' ')
    endfor
  endfor
  
  rad_row = 1
  lbl_row = 0
  rad_col = 1
  lbl_col = 0
  
  ; col button
  for c=2,col+1 do begin
    s = wwradiobox(lo_arr(rad_row,c),'','cb_col',/nofmany,toggle=tog)
    lo_arr(rad_row,c) = tog(0)
    col_wid = [col_wid,tog(0)]
  endfor
  
  ; col label
  for c=2,col+1 do begin
    lo_arr(lbl_row,c) = WwText(lo_arr(lbl_row,c),/label,text=col_lbl(c-2))
  endfor
  
  ; row button
  for r=2,row+1 do begin
    s = wwradiobox(lo_arr(r,rad_col),'','cb_row',/nofmany,toggle=tog)
    lo_arr(r,rad_col) = tog(0)
    row_wid = [row_wid,tog(0)]
  endfor
  
  ; row lable
  for r=2,row+1 do begin
    lo_arr(r,lbl_col) = WwText(lo_arr(r,lbl_col),/label,text=row_lbl(r-2))
  endfor
  
  ; data
  for r=2,row+1 do begin
    for c=2,col+1 do begin
      lo_arr(r,c) = WwText(lo_arr(r,c),/label,text=' ')
    endfor
  endfor

  s = WwSetValue(top,/display)

end

; dgp rev 8/26/05 structures to handle ATO debugging
pro _pr_ato_debug

  common cm_ato_info, cv_ato_hash

  info
  stop ;debug
  
end
; dgp rev 12/12/05 initialize the ASCII text output namespace
pro _pr_ato_init

  common cm_ato_info, cv_ato_hash

  filename = fn_gen_build_path(_cfg_get_work(),"report.txt")
  cv_ato_hash = asarr('report file',filename)
  cv_ato_hash = [cv_ato_hash,asarr('col left',10)]
  cv_ato_hash = [cv_ato_hash,asarr('col right',122)]
  
end
; dgp rev 8/26/05 structures to handle ATO variables
pro _pr_ato_set, label, val

  common cm_ato_info, cv_ato_hash

  if (size(cv_ato_hash,/type) ne 11) then _pr_ato_init

  cv_ato_hash = [cv_ato_hash,asarr(label,val)]
  
end
; dgp rev 8/26/05 structures to handle ATO variables
function fn_ato_get, label

  common cm_ato_info, cv_ato_hash
  
  if (size(cv_ato_hash,/type) ne 11) then _pr_ato_init
  if (isaskey(cv_ato_hash,label)) then begin
    return, cv_ato_hash(label)
  endif
  return, 0

end
; dgp rev 8/26/05 structures to handle ATO variables
function fn_ato_getstr, label

  common cm_ato_info, cv_ato_hash
  
  if (size(cv_ato_hash,/type) ne 11) then _pr_ato_init
  if (isaskey(cv_ato_hash,label)) then begin
    return, strtrim(cv_ato_hash(label),2)
  endif
  return, ""

end
; dgp rev 8/26/05 structures to handle ATO variables
function fn_ato_get_num, label

  common cm_ato_info, cv_ato_hash
  
  if (size(cv_ato_hash,/type) ne 11) then _pr_ato_init
  if (isaskey(cv_ato_hash,label)) then begin
    return, long(cv_ato_hash(label))
  endif
  return, 0

end

; dgp rev 8/26/05 open the text file for appended output
function fn_ato_open

  ato_file_name = fn_ato_getstr('report file')
  print, "Stat file ",ato_file_name

  found = findfile(ato_file_name,count=cnt)

  if (cnt gt 0) then begin
    comm = "OPENW, unit, ato_file_name, /Get_Lun, /append "
  endif else begin
    comm = "OPENW, unit, ato_file_name, /Get_Lun "
  endelse

  print, comm
  results = execute(comm)

  if (results) then begin

    ; define the unit for printf functions
    _pr_ato_set, 'unit', unit
    return, 1

  endif
  return, 0

end

; dgp rev 8/26/05 clsoe the text file
pro _pr_ato_close

  unit = fn_ato_get('unit')

  comm = "close, unit"
  print, comm
  results = execute(comm)

end
; dgp rev 10/31/05 filter the x median
function fn_sts_get_xmedian

  xmed  = fn_sts_getx('x median','(F7.2)')
  ; reformat if less than 10
  if (xmed lt 10) then xmed  = fn_sts_getx('x median','(F7.3)')

  return, xmed

end
; dgp rev 10/31/05 filter the y median
function fn_sts_get_ymedian

  ymed  = fn_sts_gety('y median','(F7.2)')
  ; reformat if less than 10
  if (ymed lt 10) then ymed  = fn_sts_gety('y median','(F7.3)')

  return, ymed

end
; dgp rev 10/31/05 filter the x mean
function fn_sts_get_xmean

  xmean  = fn_sts_getx('x geo mean','(F7.2)')
  if (fn_dmf_get_xaxis()) then xmean = fn_sts_getx('x mean','(F7.2)')
  ; reformat if less than 10
  if (xmean lt 10) then begin
    xmean  = fn_sts_getx('x geo mean','(F7.3)')
    if (fn_dmf_get_xaxis()) then xmean = fn_sts_getx('x mean','(F7.3)')
  endif

  return, xmean

end
; dgp rev 10/31/05 filter the y mean
function fn_sts_get_ymean

  ymean  = fn_sts_gety('y geo mean','(F8.2)')
  if (fn_dmf_get_yaxis()) then ymean = fn_sts_gety('y mean','(F7.2)')
  ; reformat if less than 10
  if (ymean lt 10) then begin
    ymean  = fn_sts_gety('y geo mean','(F5.3)')
    if (fn_dmf_get_yaxis()) then ymean = fn_sts_gety('y mean','(F7.3)')
  endif

  return, ymean

end
; dgp rev 10/31/05 filter the percent
function fn_sts_get_perc

  raw_perc  = fn_sts_get_raw('percent')
  perc  = fn_sts_get_sig('percent','(F5.2)')
  if (raw_perc lt 1) then perc  = fn_sts_get_sig('percent','(F5.3)')
  if (raw_perc eq 100) then perc  = fn_sts_get_sig('percent','(F5.1)')

  return, perc

end

pro _pr_ato_append, ato_file_name

  common ato_prev, ato_head1, ato_head2

  if (size(ato_head1,/type) eq 0) then begin
    ato_head1 = ""
    ato_head2 = ""
  endif

  found = findfile(ato_file_name,count=cnt)

  if (cnt gt 0) then begin
    comm = "OPENW, unit, ato_file_name, /Get_Lun, /append "
  endif else begin
    comm = "OPENW, unit, ato_file_name, /Get_Lun "
  endelse
  print, comm
  results = execute(comm)

  arr = fn_sts_get_str('applied')

  single_line = string(fn_sts_get_str('file name'),fn_sts_get_str('display name'), $
    Format='("File: ",A, " Display: ",A)')
  
  if (same(ato_head1,single_line) eq 0) then begin
    ato_head1 = single_line
    printf, unit, " "
    printf, unit, single_line
  endif
  
  cnt = n_elements(arr)
  single_line = string("Applied Gates: ",arr, Format='(A,X)')
  if (same(ato_head2,single_line) eq 0) then begin
    ato_head2 = single_line
    printf, unit, single_line
  endif

  name  = fn_sts_get_str('gate name')
  type  = fn_sts_get_str('gate type')

  xlog = fn_dmf_get_xaxis()
  ylog = fn_dmf_get_yaxis()
  print, "Type ",type
  tot  = fn_sts_get_sig('total count','(I7)')
  gat  = fn_sts_get_sig('gate count','(I7)')
  perc = fn_sts_get_sig('percent','(F6.2)')
  
  single = (type eq 'hist')  

  xscale = "Linear"
  if (fn_dmf_get_xaxis()) then xscale = "Log"
  yscale = "Linear"
  if (fn_dmf_get_yaxis()) then yscale = "Log"

  roi = fn_sts_getx('roi','(F7.2)')
  xrange = roi(0:1)
  if (single) then begin
    single_line = string("Name: ",name,"Type: ",type,"X Scale: ",xscale, "X Range: ", $
                         xrange,Format='(7A8,2F7.2)')
  endif else begin
    roi = fn_sts_gety('roi','(F7.2)')
    yrange = roi(2:3)
    single_line = string("Name: ",name,"Type: ",type,"X Scale: ",xscale, "X Range: ", $
                         xrange,"Y Scale: ",yscale,"Y Range: ",yrange,Format='(7A8,2F7.2,3A8,2F7.2)')
  endelse

  xmean = fn_sts_get_xmean()
  ymean = fn_sts_get_ymean()

  xmed = fn_sts_get_xmedian()
  ymed = fn_sts_get_ymedian()

  if (single) then begin
    printf, unit, single_line
    single_line = string("Total","Gated","Percent","Mean","Median", Format='(5(2X,A8))')
    printf, unit, single_line
    single_line = string(tot,gat,perc,xmean,xmed, Format='(I10,2X,I8,3X,F7.4,3X,F7.2,3X,F7.2)')
  endif else begin
    printf, unit, single_line
    single_line = string("Total","Gated","Percent","Mean","Median", Format='(4A10,2X,A18)')
    printf, unit, single_line
    single_line = string(tot,gat,perc,xmean,ymean,xmed,ymed, Format='(I10,2X,I8,4(3X,F8.3))')
  endelse
  printf, unit, single_line
  
  close, unit
  free_lun, unit
   
end

; dgp rev 12/13/05 reflect the status of the report file
pro _pr_ato_reflect

  ; get the report file name
  filename = fn_ato_getstr('report file')

  print, "Report file ",filename

  found = findfile(filename,count=cnt)

  print, "Found ",cnt

  _pr_ato_set, 'report exists', cnt

  ; check to see if form exists  
  frm_name = "Gate Form"
  if (not fn_wms_exists(frm_name)) then return

  ; enable/display buttons
  fl_arr = fn_wra_get('ato flush',frm_name)
  fl = fl_arr(0)
  vi_arr = fn_wra_get('ato view',frm_name)
  vi = vi_arr(0)
  del_arr = fn_wra_get('ato clear',frm_name)
  del = del_arr(0)
  if (cnt eq 0) then begin
    s = WwSetValue(fl,/nonsensitive)
    s = WwSetValue(vi,/nonsensitive)
    s = WwSetValue(del,/nonsensitive)
  endif else begin
    s = WwSetValue(fl,/sensitive)
    s = WwSetValue(vi,/sensitive)
    s = WwSetValue(del,/sensitive)
  endelse

end

; dgp rev 12/13/05 return data based upon current indices
function _indices_to_data, indices, data

  cells = fn_data_get_cells()

  if (size(indices,/ndim) eq 1) then begin
    full_cnt = n_elements(cells(*,0))
    if (indices(n_elements(indices)-1) lt full_cnt) then begin
      data = cells(indices,*)
      return, 1
    endif
  endif
  return, 0
  
end

; create a structure containing the current gate information
; for gate "key"
; place info in applied gate
pro _pr_stats_calc, key

  _pr_sts_switch, key
 
  print, "Calculating statistics for ",key

; get gate info

  if (_ger_gates_on(sorted)) then begin
    unames = make_array(n_elements(sorted),/string)
    for i=0,n_elements(sorted)-1 do begin
      unames(i) = _mgs_get_key_val(sorted(i), 'NAME')
    endfor
    _pr_sts_set, 'applied', unames
  endif else begin
    _pr_sts_set, 'applied', 0
  endelse

  _pr_sts_set, 'file name', fn_dmf_get('ifs key')

  disp_name = fn_dmf_current()
;  print, disp_name
  _pr_sts_set, 'display name', disp_name
  
  _pr_sts_set, 'gate name', strtrim(_mgs_get_key_val(key, 'NAME'),2)
  _pr_sts_set, 'gate type', strtrim(_mgs_get_key_val(key, 'TYPE'),2)

  _pr_sts_set, 'roi', _mgs_get_key_val(key, 'ROI')

; grab total of data set
    tot_cnt = fn_dmf_get('data count')
    _pr_sts_set, 'total count', tot_cnt

; check current data set.  Special exception for zero
  if (fn_dmf_get('data flag')) then begin
    select_values = fn_dmf_get('ssl indices')

; check for data in selected gate -- (index array or -1)
    subset = fn_mgs_get_subset(key)
    if (size(subset,/ndim) ne 0) then begin
       
;       info, subset
       gat_cnt = n_elements(subset)
       _pr_sts_set, 'gate set', gat_cnt
       
       idx = index_and(subset,select_values)
       if (size(idx,/ndim) eq 0) then begin
         combo_cnt = 0
       endif else begin
         combo_cnt = n_elements(idx)
       endelse
       _pr_sts_set, 'gate count', combo_cnt

       perc = (float(combo_cnt)/float(tot_cnt))*100
       _pr_sts_set, 'percent', perc

       if (_indices_to_data(idx,data)) then begin
         pars = _mgs_get_key_val(key, 'params')
         ; if either parameter is zero, then the product will be zero 
         ;   single parameter, otherwise dual parameter
         
         if (product(pars) eq 0) then begin
           par1 = max(pars)
           p1data = data(*,par1-1)
           ; calculate the statistics
           sts_median  = median(p1data)
           sts_average = avg(p1data)
           sts_geomean = exp(avg(alog(p1data)))

           _pr_sts_set, 'x geo mean', sts_geomean
           _pr_sts_set, 'x median', sts_median
           _pr_sts_set, 'x mean', sts_average
         endif else begin
           par1 = pars(0)
           p1data = data(*,par1-1)
           ; calculate the statistics for first parameter
           sts_median  = median(p1data)
           sts_average = avg(p1data)
           sts_geomean = exp(avg(alog(p1data)))

           _pr_sts_set, 'x geo mean', sts_geomean
           _pr_sts_set, 'x median', sts_median
           _pr_sts_set, 'x mean', sts_average

           par2 = pars(1)
           p2data = data(*,par2-1)

           ; calculate the statistics for second parameter
           sts_median  = median(p2data)
           sts_average = avg(p2data)
           sts_geomean = exp(avg(alog(p2data)))

           _pr_sts_set, 'y geo mean', sts_geomean
           _pr_sts_set, 'y median', sts_median
           _pr_sts_set, 'y mean', sts_average
         endelse

      endif       
    endif else begin
    ; else empty gate set
    _pr_sts_set, 'gate count', 0
    endelse
  endif else begin
    ; else empty data set
    _pr_sts_set, 'total count', 0

  endelse  
  
  
  ;ato_gates, keys
 ; ato_text_win, value

end


; dgp rev 12/13/05 routine to create/append ASCII text to
; a given file spec
pro _pr_ato_file, filename

  ; reset the stat buffer  
  _pr_sts_clear
  
  if (fn_mgs_get_sorted(keys)) then begin
    for i=0,n_elements(keys)-1 do begin
      if (fn_mgs_get('stats', keys(i))) then begin
        ; calculate gate statistics
        _pr_stats_calc, keys(i)
        ; append to text file
        print, "Appending to text file ",filename
        _pr_ato_append, filename
      endif
    endfor
  endif
  _pr_ato_reflect

end
; dgp rev 8/26/05 loop thru the active gates and create
; a ascii text output report
pro _pr_ato_report

  ; reset the stat buffer  
  _pr_sts_clear

  if (fn_ato_open()) then begin

    print, "Loop thru enabled stat gates"

    if (fn_mgs_get_sorted(keys)) then begin
      for i=0,n_elements(keys)-1 do begin
        if (fn_mgs_get('stats', keys(i))) then begin
          ; calculate gate statistics
          _pr_stats_calc, keys(i)
        endif
      endfor
    endif

    _pr_ato_header
    
    for i=0,n_elements(keys)-1 do begin
      if (fn_mgs_get('stats', keys(i))) then begin
        ; switch to proper gate
        _pr_sts_switch, keys(i)
        ; output gate stats
        _pr_ato_row
      endif
    endfor

    _pr_ato_close

  endif

  _pr_ato_reflect
      
end

; dgp rev 12/12/05 callback to create ASCII text output
pro cb_ato_file, wid, value

  _pr_ato_report

end

pro _pr_ato_append_all, filename

  ; reset the stat buffer  
  _pr_sts_clear
  print, "Loop thru enabled stat gates"

  if (fn_mgs_get_sorted(keys)) then begin
    for i=0,n_elements(keys)-1 do begin
      if (fn_mgs_get('stats', keys(i))) then begin
        ; calculate gate statistics
        _pr_stats_calc, keys(i)
        ; append to text file
        print, "Appending to file ",filename
        _pr_ato_append, filename
      endif
    endfor
  endif

end
; dgp rev 12/12/05 type a single line to output
pro _pr_fto_type_line, text

    common cm_fto_info, cv_fto_hash 
    Common colour, Wht, Blk 

    xyouts, cv_fto_hash('cur x'), cv_fto_hash('cur y'), text, /device, color=blk
    cv_fto_hash('cur y') = cv_fto_hash('cur y') - (cv_fto_hash('char height')*1.3)

end

; dgp rev 12/12/05 type a single line to text file
pro _pr_ato_type_line, text

  unit = fn_ato_get_num('unit')
  printf, unit, text

end

pro _pr_fto_set, label, value

  common cm_fto_info, cv_fto_hash 

  if (size(cv_fto_hash,/type) ne 11) then begin
    cv_fto_hash = asarr(label,value)
  endif else begin
    cv_fto_hash = [cv_fto_hash,asarr(label,value)]
  endelse

end
; dgp rev 12/14/05 get value from label
; cast into string
function fn_fto_get_str, label

  common cm_fto_info, cv_fto_hash 

  if (size(cv_fto_hash,/type) eq 11) then begin
    if (isaskey(cv_fto_hash,label)) then begin
      return, string(cv_fto_hash(label))
    endif
  endif
  return, ""

end
; dgp rev 12/14/05 get value from label
; cast into number
function fn_fto_get_num, label

  common cm_fto_info, cv_fto_hash 

  if (size(cv_fto_hash,/type) eq 11) then begin
    if (isaskey(cv_fto_hash,label)) then begin
      if (size(cv_fto_hash(label),/type) eq 4) then begin
        return, float(cv_fto_hash(label))
      endif
      return, long(cv_fto_hash(label))
    endif
  endif
  return, 0

end
; dgp rev 12/14/05 newline for text file
pro _pr_ato_crlf

  _pr_ato_set, 'col idx', 0
  _pr_ato_set, 'align', 0

  unit = fn_ato_get_num('unit')

  printf, unit, "", format='()'
  
end
; dgp rev 12/14/05 new display line
pro _pr_fto_crlf

  _pr_fto_set, 'col idx', 0
  _pr_fto_set, 'align', 0

  _pr_fto_set, 'cur y', (fn_fto_get_num('cur y') - fn_fto_get_num('char height')) 
  
end
; dgp rev 12/14/05 type a single column of information to display
pro _pr_fto_type_col, text

  row_pos = fn_fto_get_num('cur y')
  color_idx = fn_fto_get_num('color')
  color_rgb = WoColorConvert(color_idx,/IndextoColor)
  
  col_cnt = fn_fto_get_num('col cnt')
  col_idx = fn_fto_get_num('col idx')
  col_arr = fn_fto_get_num('col arr')
  align   = fn_fto_get_num('align')
    
  xyouts, col_arr(col_idx), row_pos, text, /device, color=color_rgb, align=align
  align = .5
  col_idx = (col_idx + 1) mod col_cnt
  if (col_idx eq col_cnt-1) then align = 1
  _pr_fto_set,'align',align
  _pr_fto_set,'col idx', col_idx
  if (col_idx eq 0) then _pr_fto_crlf    

end
; dgp rev 12/14/05 type a single column of information to text file
pro _pr_ato_type_col, text

  width = fn_ato_get_num('col width')
  col_idx = fn_ato_get_num('col idx')
  col_cnt = fn_ato_get_num('col cnt')
  offset = strlen(text)
  unit = fn_ato_get_num('unit')

  align = .5
  col_idx = (col_idx + 1) mod col_cnt
  if (col_idx eq col_cnt-1) then align = 1
  _pr_ato_set,'align',align
  _pr_ato_set,'col idx', col_idx

  if (col_idx eq 0) then begin
  ; PRINTF format based upon column widths
    format = '(A' + STRTRIM(width, 2) + ')'
  endif else begin
    format = '(A' + STRTRIM(width, 2) + ',$)'
  endelse
  printf, unit, text, format=format


end
; dgp rev 12/14/05 defined the number of output columns
; in the display based format
pro _pr_fto_col_cnt, col_cnt

  _pr_fto_set, 'col idx', 0
  _pr_fto_set, 'col cnt', col_cnt

  col_right = fn_fto_get_num('col right')
  col_left  = fn_fto_get_num('col left')
  if (col_cnt gt 1) then begin
    chunk = (col_right-col_left)/(col_cnt-1)
  endif else begin
    chunk = (col_right-col_left)
  endelse
  col_arr = indgen(col_cnt)*chunk + col_left
  _pr_fto_set, 'col arr', col_arr
  _pr_fto_set, 'col width', chunk
  _pr_fto_set, 'align', 0

end

; dgp rev 12/14/05 defined the number of output columns
; in the text file based format
pro _pr_ato_col_cnt, col_cnt

  _pr_ato_set, 'col idx', 0
  _pr_ato_set, 'col cnt', col_cnt

  col_right = fn_ato_get_num('col right')
  col_left  = fn_ato_get_num('col left')
  chunk = (col_right-col_left)/(col_cnt)
  col_arr = indgen(col_cnt)*chunk + col_left
  _pr_ato_set, 'col arr', col_arr
  _pr_ato_set, 'col width', chunk
  _pr_ato_set, 'align', 0
  
end
; dgp rev 12/14/05 type the applied gates to a text file 
pro _pr_ato_type_applied

  arr = fn_sts_get_str('applied')
  if (size(arr,/ndim) ne 0) then begin
    arr = ["Applied Gates:",arr]
    
    cnt = n_elements(arr)
    cols = cnt
    max_cols = 6
    if (cnt gt max_cols) then cols = max_cols
    _pr_ato_col_cnt, cols
    for i=0,cnt-1 do begin
      _pr_ato_type_col, arr(i)
    endfor
    _pr_ato_crlf
  endif
  str = string("Total Events on display: ",fn_sts_get_sig('total count','(I7)'))
  _pr_ato_type_line, str

end

; dgp rev 12/14/05 type out the applied gates to the display
pro _pr_fto_type_applied

  common cm_fto_info, cv_fto_hash 
  Common colour, Wht, Blk 

  arr = fn_sts_get_str('applied')
  if (size(arr,/ndim) ne 0) then begin
    only_key = fn_dmf_get('1only')
    if (only_key eq "") then begin
      only_name = ""
    endif else begin
      only_name = _mgs_get_key_val(only_key, 'NAME')
    endelse
    arr = ["Applied Gates:",arr]
    color_idx = fn_dmf_get('color')
    
    cnt = n_elements(arr)
    cols = cnt
    max_cols = 6
    if (cnt gt max_cols) then cols = max_cols
    _pr_fto_col_cnt, cols
    _pr_fto_set, 'color', blk
    for i=0,cnt-1 do begin
      if (arr(i) eq only_name) then begin
        _pr_fto_set, 'color', color_idx
        _pr_fto_type_col, arr(i)
        _pr_fto_set, 'color', blk
      endif else begin
        _pr_fto_type_col, arr(i)
      endelse
    endfor
    _pr_fto_crlf
  endif
  str = string("Total Events on display: ",fn_sts_get_sig('total count','(I7)'))
  _pr_fto_type_line, str

end

pro _pr_fto_type_two, one, two

  common cm_fto_info, cv_fto_hash 
  Common colour, Wht, Blk 

    col_left = cv_fto_hash('col left')
    col_right = cv_fto_hash('col right')
    chunk = (col_right-col_left)/2
    center = col_left+chunk

    xyouts, col_left, cv_fto_hash('cur y'), one, /device, color=blk, align=0
    xyouts, center, cv_fto_hash('cur y'), two, /device, color=blk, align=0
    cv_fto_hash('cur y') = cv_fto_hash('cur y') - (cv_fto_hash('char height')*1.3)

end

pro _pr_fto_heading

  common cm_fto_info, cv_fto_hash 
  Common colour, Wht, Blk 

  arr = fn_gen_get('col header')

  col_cnt = n_elements(arr)
  col_left = cv_fto_hash('col left')
  col_right = cv_fto_hash('col right')
  chunk = (col_right-col_left)/(col_cnt)
  offset = col_left+(chunk/2)
  col_arr = indgen(col_cnt)*chunk + offset

  cv_fto_hash = [cv_fto_hash,asarr('col arr',col_arr)]
  cv_fto_hash = [cv_fto_hash,asarr('col width',chunk)]
  cv_fto_hash = [cv_fto_hash,asarr('col cnt',col_cnt)]

  cur_y = cv_fto_hash('cur y')

  for i=0,n_elements(arr)-1 do begin
    xyouts, col_arr(i), cur_y, arr(i), /device, color=blk, align=1
  endfor
  cur_y = cur_y - (cv_fto_hash('char height')*.5)
  plots, [col_left,col_right], [cur_y,cur_y], /device, color=blk
  cur_y = cur_y - (cv_fto_hash('char height'))
  cv_fto_hash = [cv_fto_hash,asarr('cur y',cur_y)]

end
; dgp rev 12/14/05 output the data array to text file
pro _pr_ato_heading

  arr = fn_gen_get('col header')

  col_cnt = n_elements(arr)

  _pr_ato_col_cnt, col_cnt

  for i=0,n_elements(arr)-1 do begin
    _pr_ato_type_col, arr(i)   
  endfor

end

function fn_stat_perc, value

  value = strcompress(float(value),/rem)

  if (value le 1) then begin
    value = string(value,format='(F9.3)')
  endif else begin
    value = string(value,format='(F9.2)')
  endelse

  return, value

end

function fn_stat_mean, value

  value = strcompress(float(value),/rem)
  pos = strpos(value,".")

  if (value le 10) then begin
    value = string(value,format='(F9.3)')
  endif else begin
    value = string(value,format='(F9.2)')
  endelse

  return, value

end

pro _pr_fto_col_arr, arr

  common cm_fto_info, cv_fto_hash 
  Common colour, Wht, Blk 

  cur_y = cv_fto_hash('cur y')
  col_cnt = cv_fto_hash('col cnt')
  col_arr = cv_fto_hash('col arr')
  offset = cv_fto_hash('col width')/2
  
;  arr(3:*) = string(arr(3:*),format='(F9.4)')
  
  for i=0,n_elements(arr)-1 do begin
    xyouts, col_arr(i), cur_y, arr(i), /device, color=blk, align=1
;    xyouts, col_arr(i)+offset, cur_y, arr(i), /device, color=blk, align=1
  endfor
  cur_y = cur_y - (cv_fto_hash('char height'))
  cv_fto_hash = [cv_fto_hash,asarr('cur y',cur_y)]

end
; dgp rev 12/14/05 column 
pro _pr_ato_col_arr, arr

  for i=0,n_elements(arr)-1 do begin
    _pr_ato_type_col, arr(i)
  endfor

end

function fn_dmf_get_header, key

  common cm_ifs, cv_ifs_struct

  if (isaskey(cv_ifs_struct(fn_dmf_get('ifs key'),'header'),key)) then begin
    return, cv_ifs_struct(fn_dmf_get('ifs key'),'header',key)
  endif
  return, " "

end

function fn_stats_fmt_perc, value

  value = strcompress(float(value),/rem)
  pos = strpos(value,".")

  if (value le 1) then begin
    value = string(value,format='(F7.3)')
  endif else begin
    value = string(value,format='(F5.1)')
  endelse

  return, value

end

function fn_stats_fmt_mean, value

  value = strcompress(float(value),/rem)
  pos = strpos(value,".")

  if (value le 10) then begin
    value = string(value,format='(F6.1)')
  endif else begin
    value = string(value,format='(I4)')
  endelse

  return, value

end

function fn_par_name, index

  cv_names = fn_upa_get_labels()

  if (size(cv_names,/ndim) eq 0) then begin

    cv_names = strtrim(indgen(fn_data_get_par_cnt())+1,2)

  endif

  if (index lt 0 or index ge n_elements(cv_names)) then index = 0

  return, cv_names(index)

end

; dgp rev 8/26/05 ascii text output of the header using PRINTF to a file
pro _pr_ato_header

  _pr_ato_col_cnt, 2

  _pr_ato_type_col, string("Project: ",fn_gen_get('project'))
  _pr_ato_type_col, string("Session: ",fn_gen_get('session'))

  _pr_ato_type_col, string("File: ",fn_sts_get_str('file name'))
  _pr_ato_type_col, string("Sample: ",fn_dmf_get_header("$SMNO"))

  _pr_ato_type_applied

  names = _par_cur_names()
  pars  = fn_dmf_get('pars')
  
  if (fn_dmf_single()) then begin
    str = "X Param: "+fn_par_name(pars(0)-1)
    _pr_ato_type_line, str
  endif else begin
    one = "X Param: "+fn_par_name(pars(0)-1)
    two = "Y Param: "+fn_par_name(pars(1)-1)
    _pr_ato_type_two, one, two
  endelse

;  _pr_stat_font_size, +2

  col_head = ["Gate","Events","% of total"]

  if (fn_dmf_get_xaxis()) then begin
    col_head = [col_head,["X-MFI-A","X-Med","X-Lo","X-Hi"]]
  endif else begin
    col_head = [col_head,["X-MFI-G","X-Med","X-Lo","X-Hi"]]
  endelse
  if (fn_dmf_get_yaxis()) then begin
    col_head = [col_head,["Y-MFI-A","Y-Med","Y-Lo","Y-Hi"]]
  endif else begin
    col_head = [col_head,["Y-MFI-G","Y-Med","Y-Lo","Y-Hi"]]
  endelse

  _pr_gen_set, 'col header', col_head

  _pr_ato_heading

end
; dgp rev 8/26/05 formated text output of the header using XYOUTS and EMF
pro _pr_fto_header

  _pr_fto_col_cnt, 2

  _pr_fto_type_col, string("Project: ",fn_gen_get('project'))
  _pr_fto_type_col, string("Session: ",fn_gen_get('session'))

  _pr_fto_type_col, string("File: ",fn_sts_get_str('file name'))
  _pr_fto_type_col, string("Sample: ",fn_dmf_get_header("$SMNO"))

  _pr_fto_type_applied

  names = _par_cur_names()
  pars  = fn_dmf_get('pars')
  
  if (fn_dmf_single()) then begin
    str = "X Param: "+fn_par_name(pars(0)-1)
    _pr_fto_type_line, str
  endif else begin
    one = "X Param: "+fn_par_name(pars(0)-1)
    two = "Y Param: "+fn_par_name(pars(1)-1)
    _pr_fto_type_two, one, two
  endelse

  col_head = ["Gate","Events","% of total"]

  if (fn_dmf_get_xaxis()) then begin
    col_head = [col_head,["X-MFI-A","X-Med","X-Lo","X-Hi"]]
  endif else begin
    col_head = [col_head,["X-MFI-G","X-Med","X-Lo","X-Hi"]]
  endelse
  if (fn_dmf_get_yaxis()) then begin
    col_head = [col_head,["Y-MFI-A","Y-Med","Y-Lo","Y-Hi"]]
  endif else begin
    col_head = [col_head,["Y-MFI-G","Y-Med","Y-Lo","Y-Hi"]]
  endelse

  _pr_gen_set, 'col header', col_head

  _pr_fto_heading

end
; dgp rev 8/26/05 ascii text output of stats using PRINTF to a file
pro _pr_ato_row

  col_head = fn_gen_get('col header')

  row_arr = make_array(n_elements(col_head),/string)

  row_arr(0) = fn_sts_get_str('gate name')
  row_arr(1) = fn_sts_get_sig('gate count','(I7)')

  row_arr(2) = fn_sts_get_perc()
  row_arr(3) = fn_sts_get_xmedian()
  row_arr(4) = fn_sts_get_xmean()
  
  roi = fn_sts_getx('roi','(F7.2)')
  row_arr(5) = string(roi(0),Format='(F7.2)')
  row_arr(6) = string(roi(1),Format='(F7.2)')
  if (fn_sts_get_str('gate type') eq 'hist') then begin
    row_arr(7) = ' '
    row_arr(8) = ' '
    row_arr(9) = ' '
    row_arr(10) = ' '
  endif else begin
    row_arr(7) = fn_sts_get_ymean()
    row_arr(8) = fn_sts_get_ymedian()
    roi = fn_sts_gety('roi','(F7.2)')
    row_arr(9) = string(roi(2),Format='(F7.2)')
    row_arr(10) = string(roi(3),Format='(F7.2)')
  endelse

  _pr_ato_col_arr, row_arr
  
end

; dgp rev 8/26/05 formated text output of a data row.
pro _pr_fto_row

  col_head = fn_gen_get('col header')

  row_arr = make_array(n_elements(col_head),/string)
  
  row_arr(0) = fn_sts_get_str('gate name')
  row_arr(1) = fn_sts_get_sig('gate count','(I7)')
  row_arr(2) = fn_sts_get_perc()
  row_arr(3) = fn_sts_get_xmean()
  row_arr(4) = fn_sts_get_xmedian()
  
  roi = fn_sts_getx('roi','(F7.2)')
  row_arr(5) = string(roi(0),Format='(F7.2)')
  row_arr(6) = string(roi(1),Format='(F7.2)')
  if (fn_sts_get_str('gate type') eq 'hist') then begin
    row_arr(7) = ' '
    row_arr(8) = ' '
    row_arr(9) = ' '
    row_arr(10) = ' '
  endif else begin
    row_arr(7) = fn_sts_get_ymean()
    row_arr(8) = fn_sts_get_ymedian()
    roi = fn_sts_gety('roi','(F7.2)')
    row_arr(9) = string(roi(2),Format='(F7.2)')
    row_arr(10) = string(roi(3),Format='(F7.2)')
  endelse
    
  _pr_fto_col_arr, row_arr
  
end

function fn_stat_init

  common cm_fto_info, cv_fto_hash 
  
  fnt_str = fn_fnt_get('stat font')
  if (size(cv_fto_hash,/type) eq 11) then begin
    if (isaskey(cv_fto_hash,'font')) then begin
      fnt_str = cv_fto_hash('font')
    endif
  endif

  DEVICE, Font=fnt_str

  char_width  = !d.x_ch_size*!p.charsize
  char_height = !d.y_ch_size*!p.charsize

  width = 100*char_width
  height = 24*char_height
  
  col_cnt = 3
  margin = 5*char_width
  col_right = width - margin
  col_left  = margin
  chunk = (col_right-col_left)/(col_cnt-1)
  col_arr = indgen(col_cnt)*chunk + col_left

  cv_fto_hash = asarr('font',fnt_str)
  cv_fto_hash = [cv_fto_hash,asarr('width',width)]
  cv_fto_hash = [cv_fto_hash,asarr('height',height)]
  cv_fto_hash = [cv_fto_hash,asarr('char width',char_width)]
  cv_fto_hash = [cv_fto_hash,asarr('char height',char_height)]
  cv_fto_hash = [cv_fto_hash,asarr('loop cnt',0)]
  cv_fto_hash = [cv_fto_hash,asarr('cur x',col_left)]
  cv_fto_hash = [cv_fto_hash,asarr('cur y',height-(char_height*2))]
  cv_fto_hash = [cv_fto_hash,asarr('col left',col_left)]
  cv_fto_hash = [cv_fto_hash,asarr('col right',col_right)]
  cv_fto_hash = [cv_fto_hash,asarr('col width',chunk)]
  cv_fto_hash = [cv_fto_hash,asarr('col arr',col_arr)]

  return, long([width,height])

end
; dgp rev 8/26/05 export the drawing area fto report to PowerPoint via EMF
pro _pr_fto_export

  win = fn_fto_get_num('win')

  if (win eq -1) then return

    path = _cfg_get_work()
    file_name = strtrim(_gen_unique_name()+".sts",2)
    exportname = fn_gen_build_path(fn_gen_getstr('work path'),file_name)
    print, "Exporting to ",exportname    
    lst = FINDFILE(exportname, Count=cntr)
    if (cntr gt 0) then del_file, exportname
    status = wwrite_meta(win, filename=exportname)
    if (status lt 0) then begin
      msg_str = DC_ERROR_MSG(status)
      print, msg_str
    endif

end

function fn_stat_key_list

  print, "Loop thru enabled stat gates"

  keys = [' ']
  if (fn_mgs_get_sorted(keys)) then begin
    for i=0,n_elements(keys)-1 do begin
      if (fn_mgs_get('stats', keys(i))) then begin
        ; calculate gate statistics
        keys = [keys,keys(i)]
      endif
    endfor
  endif

  return, keys

end

; dgp rev 8/26/05 formated text output report using XYOUTS and EMF
pro dcb_fto_draw, wid, val

  Common colour, Wht, Blk 

  erase, wht

  device, get_fontmap=fnt_arr

  if (fn_mgs_get_sorted(keys)) then begin
    _pr_fto_header
    for i=0,n_elements(keys)-1 do begin
      if (fn_mgs_get('stats', keys(i))) then begin
        ; switch to proper gate
        _pr_sts_switch, keys(i)
        ; output gate stats
        _pr_fto_row
      endif
    endfor
  endif

  device, font=fnt_arr(0)
  if (fn_dmf_get('export')) then _pr_fto_export  

end

; dgp rev 8/26/05 Graphical display of stats report for
; PowerPoint export
pro _pr_fto_form

  frm_name = "Stat Graphics"

  _pr_wms_set, 'state', 'reform'

  if (fn_wms_form(frm_name,layout)) then return
   
  sz_arr = fn_stat_init()
  
  but = WwButtonBox(layout, 'Close', 'CB_Exit')

  draw=WwDrawing(layout, win,'dcb_fto_draw', sz_arr, sz_arr, $
                 Area = darea, /noscroll)
  if (draw eq 0) then _pr_dac_error

  
  _pr_fto_set, 'win', win
  _pr_wms_display, frm_name

end    
; dgp rev 12/12/05 ATO report preview
pro _pr_ato_form

  frm_name = "Report Preview"

  _pr_wms_set, 'state', 'reform'

  if (fn_wms_form(frm_name,layout)) then return
   
  sz_arr = fn_stat_init()
  
  but = WwButtonBox(layout, 'Close', 'CB_Exit')
  
  draw=WwDrawing(layout, win,'dcb_ato_draw', sz_arr, sz_arr, $
                 Area = darea, /noscroll)
  if (draw eq 0) then _pr_dac_error
  
  _pr_fto_set, 'win', win
  _pr_wms_display, frm_name

end    
; dgp rev 12/12/05 calls a text widget for display a text file
pro _pr_ato_text, filename
     
  frm_name = "Current Report"

  if (fn_wms_form(frm_name,layout)) then return
   
  txtwid  = WwText(layout, file=filename, cols=80, row=30, /hscroll, /vscroll) 

  _pr_wms_display, frm_name
        
end
; dgp rev 12/14/05 export stats to PPT
pro _pr_fto_win

  ; reset the stat buffer  
  _pr_sts_clear
  _pr_dmf_set, 'export', 1
  print, "Loop thru enabled stat gates"

  if (fn_mgs_get_sorted(keys)) then begin
    for i=0,n_elements(keys)-1 do begin
      if (fn_mgs_get('stats', keys(i))) then begin
        ; calculate gate statistics
        _pr_stats_calc, keys(i)
      endif
    endfor
  endif

  _pr_fto_form

end
; dgp rev 8/26/05 loop thru the active gates and create
; a formated text output report
pro cb_fto_win, wid, value

  _pr_fto_win
      
end
; dgp rev 12/14/05 preview the export to PPT
pro _pr_fto_preview

  ; reset the stat buffer  
  _pr_sts_clear
  _pr_dmf_set, 'export', 0
  print, "Loop thru enabled stat gates"

  if (fn_mgs_get_sorted(keys)) then begin
    for i=0,n_elements(keys)-1 do begin
      if (fn_mgs_get('stats', keys(i))) then begin
        ; calculate gate statistics
        _pr_stats_calc, keys(i)
      endif
    endfor
  endif

  _pr_fto_form

end
; dgp rev 8/26/05 loop thru the active gates and create
; a formated text output report 
pro cb_fto_pre, wid, value

  _pr_fto_preview
      
end

pro _pr_mess_nogates

  print, "No gates selected"

end
; dgp rev 12/13/05 create a temporary preview of stats to 
; be inserted into ongoing report
pro _pr_ato_preview

  filename = fn_gen_build_path(fn_gen_get('work path'),'report.tmp')
  found = findfile(filename,count=cnt)
  if (cnt ne 0) then del_file, filename

  _pr_ato_file, filename

  found = findfile(filename,count=cnt)
  if (cnt ne 0) then begin

    frm_name = "Preview Report"

    if (fn_wms_form(frm_name,layout)) then return
   
    txtwid  = WwText(layout, file=filename, cols=80, row=30, /hscroll, /vscroll) 

    _pr_wms_display, frm_name

  endif

end

; dgp rev 12/13/05 create a temporary preview of the sub report
; to be appended to the existing report
pro cb_ato_preview, wid, value

  _pr_ato_preview

end
; dgp rev 12/14/05 modify the PPT export font
pro _pr_fto_font

  common cm_fto_info, cv_fto_hash 

  device, get_fontmap=fnt_arr
  fnt_str = fnt_arr(0)
  if (size(cv_fto_hash,/type) eq 11) then begin
    if (isaskey(cv_fto_hash,'font')) then begin
      fnt_str = cv_fto_hash('font')
    endif
  endif

  font_str = WIN32_PICK_FONT(default=fnt_str)

  if (font_str ne "") then cv_fto_hash = asarr('font',font_str)  

end
; dgp rev 12/14/05 callback to modify the PPT export font
pro cb_fto_font, wid, value

  _pr_fto_font
      
end

; dgp rev 12/13/05 callback for report flush
pro _pr_ato_flush

  filename = fn_ato_getstr('report file')
  
  print, "Flush the report ",filename

  found = findfile(filename,count=cnt)
  if (cnt ne 0) then begin
    comm = "ReportPrint "+filename
    print, comm
    spawn, comm, results
    print, results
    del_file, filename
  endif

  _pr_ato_reflect

end
; dgp rev 12/13/05 callback for report flush
pro cb_ato_flush, wid, value

  _pr_ato_flush

end

; dgp rev 9/1/05 update the mgs patch color
PRO dcb_mgs_patch, wid, value

  color_idx = fn_gen_get('mgs color')
  
  sze = dgpGetValue(wid,/size)
  img = rebin(bindgen(1)+color_idx,sze(0),sze(1))
  
  tv, img  

END

function fn_mgs_count

  common cm_mgs_sort, cv_sorted_keys, cv_sorted_count, cv_sorted_names, cv_sorted_prefix

  if (size(cv_sorted_keys,/type) eq 0) then return, 0
  
  return, n_elements(cv_sorted_keys)

end

function fn_win_mgs_color

  device, window_state=winarr
  win = max(where(winarr eq 0))
  _pr_gen_set, 'mgs win', win
  return, win

end

function fn_win_get_tmp

  common win_map, win_tmp, win_cluster

  if (size(win_tmp,/type) eq 0) then win_tmp = [29,30,31]

  device, window_state=winarr

  idx = where(winarr(win_tmp) eq 0,cnt)

  if (cnt eq 0) then return, 0
  
  print, "Grab temp window ",win_tmp(idx(0))
  
  return, win_tmp(idx(0))

end


; dgp rev 8/26/05 draws the color selection grid
PRO dcb_mgs_csg, wid, value

  sze = dgpGetValue(wid,/size)
  dim  = 5
  squ_wid  = sze(0)/dim
  squ_hit  = sze(1)/dim

  full = dim*squ_wid

  strip = rebin(bindgen(dim),full,squ_hit)
  for i=1,dim-1 do begin
    strip = [[strip],[rebin(bindgen(dim)+(i*dim),full,squ_hit)]]
  endfor
  
  tv, strip, order=1

END

; dgp rev 8/25/05 select a color from the color selection grid
PRO bphd_mgs_csg, wid, shell, event

  ; Remove event handler
  
  patch_win = fn_gen_get('mgs patch win')
  patch_wid = fn_gen_get('mgs patch wid')

  patch_sze = dgpGetValue(patch_wid,/size)
  csg_sze   = dgpGetValue(wid,/size)

  ;  save the current window
  old_win = !d.window

  ; Make csg window the active window
  ; necessary for CURSOR to recognize the mouse click in this window
  WSET, fn_gen_get('mgs csg win')

  pnt = WwGetPosition(event)

  x = pnt(0)
  ; invert y for some reason
  y = csg_sze(1) - pnt(1)

  color_idx = tvrd(x, y,1,1)

  _pr_gen_set,'mgs color', color_idx(0)
  wset, patch_win

  img = rebin(bindgen(1)+color_idx,patch_sze(0),patch_sze(1))
  tv, img
    
  wset, old_win

END
; dgp rev 12/12/05 calls a text widget for display a text file
pro _pr_ato_view
     
  frm_name = "Current Report"

  if (fn_wms_form(frm_name,layout)) then return
   
  filename = fn_ato_getstr('report file')

  txtwid  = WwText(layout, file=filename, cols=80, row=30, /hscroll, /vscroll) 

  _pr_wms_display, frm_name
        
end
; dgp rev 12/13/05 callback view the current report file
pro cb_ato_view, wid, value

  _pr_ato_view

end

; dgp rev 12/13/05 toggle on/off the printer dialog option
pro cb_ato_tog, wid, value

  internal = dgpGetValue(wid) 
  print, "Dialog ",internal
  _pr_ato_set, 'dialog', internal

end
; dgp rev 12/14/05 remove the report file, clear the stats
; and reflect the proper widget settings
pro cb_ato_clear, wid, value

  filename = fn_ato_getstr('report file')
  
  print, "Clear the report ",filename

  found = findfile(filename,count=cnt)
  if (cnt ne 0) then begin
    del_file, filename
  endif

  _pr_ato_reflect

end
; dgp rev 11/9/05 Gate Form
pro _pr_frm_gate

   frm_name = "Gate Form"
   
   if (fn_wms_form(frm_name,layout)) then return
   
     lo_info = WwLayout(layout)
     lo_main = WwLayout(layout)

     dsp_gate_wid = WwText(lo_info,/label,text=fn_dmf_current())
     _pr_wra_reg, 'dsp name', dsp_gate_wid

     _pr_show_gates,  lo_main

     control_lo   = WwLayout(lo_main, border=4, /Hor)
     gate_lo      = WwLayout(control_lo, border=0, /Hor)
     
     gate1_lo     = WwLayout(gate_lo, border=4, /ver)
     gate2_lo     = WwLayout(gate_lo, border=4, /ver)
     control1_lo   = WwLayout(control_lo, border=4, /Vert)
     control2_lo   = WwLayout(control_lo, border=4, /Vert)
     control3_lo   = WwLayout(control_lo, border=4, /Vert)

     _ger_reflect_wid

     txt          = WwText(control3_lo,'NoOpCB',/label,text="Gate Color")
     
     if (fn_gen_get('mgs csg win') ne 0) then begin
       free_win2 = fn_gen_get('mgs csg win')
       free_win = fn_gen_get('mgs patch win')
       print, "Gate colors ",free_win, free_win2
     endif
     draw  = WwDrawing(control3_lo, free_win, 'dcb_mgs_patch', [20,20], [20,20], $
                         /NoScroll, area=drawarea)
     if (draw eq 0) then _pr_dac_error


     draw2  = WwDrawing(control3_lo, free_win2, 'dcb_mgs_csg', [60,60], [60,60], $
                    /NoScroll, area=drawarea2)
     if (draw2 eq 0) then _pr_dac_error


     _pr_gen_set, 'mgs csg win', free_win2

     _pr_gen_set, 'mgs patch wid', draw
     _pr_gen_set, 'mgs patch win', free_win

     status = WwHandler(drawarea2, 'bphd_mgs_csg', 'ButtonPressMask')
     status = WwHandler(drawarea, 'bphd_mgs_patch_adv', 'ButtonPressMask')

     txt          = WwText(control1_lo,'NoOpCB',/label,text="PowerPoint")

; display all active statistic gates in graphics window
     vubut        = WwButtonBox(control1_lo, 'Preview', 'cb_fto_pre')
; export the EMF statistics to ppt
     vubut        = WwButtonBox(control1_lo, 'Export', 'cb_fto_win')
; stats font
     vubut        = WwButtonBox(control1_lo, 'Font', 'cb_fto_font')

     txt          = WwText(control2_lo,'NoOpCB',/label,text="Report")

; append all active statistic gates to a running report
; fto - formated text output
; preview what will be written to the report file
     vubut        = WwButtonBox(control2_lo, 'Preview', 'cb_ato_preview')
; append to report file (or create)
     filbut       = WwButtonBox(control2_lo, 'File Append', 'cb_ato_file')
; print and delete the running report, must have the external print executable
     set_lo       = WwLayout(control2_lo, /hor)
     flbut        = WwButtonBox(set_lo, 'File Flush', 'cb_ato_flush')
     flchk        = WwRadioBox(set_lo, 'Dialog', 'cb_ato_tog', /nofmany)
     if (fn_ato_get('report exists') eq 0) then s= WwSetValue(flbut,/nonsensitive) 
     _pr_wra_reg, 'ato flush', flbut
; view the current report file
;     vubut        = WwButtonBox(control2_lo, 'Preview', 'cb_stat_dsp_cont')
     vibut        = WwButtonBox(control2_lo, 'View', 'cb_ato_view')
     if (fn_ato_get('report exists') eq 0) then s= WwSetValue(vibut,/nonsensitive) 
     _pr_wra_reg, 'ato view', vibut

     delbut        = WwButtonBox(control2_lo, 'Clear', 'cb_ato_clear')
     if (fn_ato_get('report exists') eq 0) then s= WwSetValue(delbut,/nonsensitive) 
     _pr_wra_reg, 'ato clear', delbut

     appbut      = WwButtonBox(control3_lo, 'Apply', 'cb_redraw')
    
     exitbut      = WwButtonBox(control3_lo, 'Exit', 'cb_exit')
    
     _pr_wms_display, frm_name

end

; stubs into gating routines 
pro cb_mgs_menu, wid, value
   
  _pr_frm_gate
           
end

pro _pr_mgs_reopen, name, pos

  _pr_wms_set, 'state', 'reform'
        
  _pr_frm_gate
           
end

pro _mgs_rebuild

  _pr_wms_set, 'state', 'reform'

  _pr_frm_gate
           
   
end
; dgp rev 9/13/05 replace the form
pro _frm_replace, name, wid

  common _frm_list, frm_hash
  
  if (size(frm_hash,/type) eq 0) then begin
    frm_hash = asarr(name,wid)
  endif else begin
    if (isaskey(frm_hash,name)) then begin
      old = frm_hash(name)
      if (fn_wid_exists(old)) then begin
        if (old ne wid) then s = WwSetValue(old,/close)
      endif
    endif
    frm_hash = [frm_hash,asarr(name,wid)]
  endelse

end


; dgp rev 9/13/05 display the statistics window for
; a given set of gates
pro _stats_display, key

    filename = fn_ato_getstr('report file')

    _pr_ato_append_all, filename

     ; create layouts
  
    frm_name = "Gate Statistics"

    filename = fn_dmf_get('ifs key')
    
    _pr_wms_set, 'state', 'reform'

       ;print, "Getting toplevel ",top
    if (fn_wms_form(frm_name,layout)) then return

  ;  s = WwText(layout, 'NoOpCB', /label,text='Statistics')
    main_lo   = WwLayout(layout, /hor)
    gate1_lo  = WwLayout(main_lo, /hor, bord=2)
    gate2_lo  = WwLayout(main_lo, /ver, bord=2)
    title2_lo = WwLayout(gate2_lo, /hor)
    par_lo    = WwLayout(gate2_lo, /hor)

    label_lo = WwLayout(gate1_lo, /ver)
    data_lo  = WwLayout(gate1_lo, /ver)
    
    label2_lo = WwLayout(par_lo, /ver)
    par1_lo  = WwLayout(par_lo, /ver)
    par2_lo  = WwLayout(par_lo, /ver)
    
    s = WwText(label_lo, 'NoOpCB', /label,text='Name:')
    s = WwText(data_lo,  'NoOpCB', /label,text=strtrim(_mgs_get_key_val(key, 'UNAME'),2))
    s = WwText(label_lo, 'NoOpCB', /label,text='Display:')
    s = WwText(data_lo,  'NoOpCB', /label,text=strtrim(fn_sts_get_str('display name'),2))
    s = WwText(label_lo, 'NoOpCB', /label,text='Type:')
    s = WwText(data_lo,  'NoOpCB', /label,text=strtrim(_mgs_get_key_val(key, 'TYPE'),2))
    s = WwText(label_lo, 'NoOpCB', /label,text='Total Cells:')
    s = WwText(data_lo,  'NoOpCB', /label,text=strtrim(fn_sts_get_sig('total count','(I7)'),2))
    s = WwText(label_lo, 'NoOpCB', /label,text='Gate Cells: ')
    s = WwText(data_lo,  'NoOpCB', /label,text=strtrim(fn_sts_get_sig('gate count','(I7)'),2))
    s = WwText(label_lo, 'NoOpCB', /label,text='% of Total: ')
    s = WwText(data_lo,  'NoOpCB', /label,text=strtrim(fn_sts_get_sig('percent','(F6.2)'),2))

    s = WwText(title2_lo, 'NoOpCB', /label,text='Parameters Stats')
    s = WwText(label2_lo, 'NoOpCB', /label,text='Index:')
    s = WwText(label2_lo, 'NoOpCB', /label,text='Name:')
    s = WwText(label2_lo, 'NoOpCB', /label,text='Scale:')
    s = WwText(label2_lo, 'NoOpCB', /label,text='Lo Range:')
    s = WwText(label2_lo, 'NoOpCB', /label,text='Hi Range:')
    s = WwText(label2_lo, 'NoOpCB', /label,text='MFI:')
    s = WwText(label2_lo, 'NoOpCB', /label,text='Median:')
    
    roi = fn_sts_getx('roi','(F8.2)')
    x_roi = roi(0:1)
    x_roi_hi = max(x_roi,min=x_roi_lo)
    
    names = _par_cur_names()
    
    xmean = fn_sts_get_xmean()
    xmed  = fn_sts_get_xmedian()
    
    pars = _mgs_get_key_val(key, 'params')
    ; if either parameter is zero, then the product will be zero 
    ;   single parameter, otherwise dual parameter
    if (product(pars) eq 0) then begin
      par1 = max(pars)
    endif else begin
      par1 = pars(0)
      par2 = pars(1)
      roi = fn_sts_gety('roi','(F8.2)')
      y_roi = roi(2:3)
      y_roi_hi = max(y_roi,min=y_roi_lo)
    endelse
    
    x_scale = "Linear"
    if (fn_dmf_get_xaxis()) then x_scale = "Log"
    y_scale = "Linear"
    if (fn_dmf_get_yaxis()) then y_scale = "Log"

    s = WwText(par1_lo,  'NoOpCB', /label,text=strtrim(par1,2))
    s = WwText(par1_lo,  'NoOpCB', /label,text=strtrim(names(par1-1),2))
    s = WwText(par1_lo,  'NoOpCB', /label,text=strtrim(x_scale,2))
    s = WwText(par1_lo,  'NoOpCB', /label,text=x_roi_lo)
    s = WwText(par1_lo,  'NoOpCB', /label,text=x_roi_hi)

    s = WwText(par1_lo,  'NoOpCB', /label,text=strcompress(xmean))
    s = WwText(par1_lo,  'NoOpCB', /label,text=strtrim(xmed,2))

    if (product(pars) ne 0) then begin

      ymean = fn_sts_get_ymean()
      ymed  = fn_sts_get_ymedian()

      s = WwText(par2_lo,  'NoOpCB', /label,text=strtrim(par2,2))
      s = WwText(par2_lo,  'NoOpCB', /label,text=strtrim(names(par2-1),2))
      s = WwText(par2_lo,  'NoOpCB', /label,text=strtrim(y_scale,2))
      s = WwText(par2_lo,  'NoOpCB', /label,text=y_roi_lo)
      s = WwText(par2_lo,  'NoOpCB', /label,text=y_roi_hi)
      s = WwText(par2_lo,  'NoOpCB', /label,text=strcompress(ymean))
      s = WwText(par2_lo,  'NoOpCB', /label,text=strtrim(ymed,2))
    endif

    but_lo = WwLayout(main_lo, /ver)

    s = WwButtonBox(but_lo,'Exit','cb_exit')

    main = _wms_get_main(frm_name)
    _frm_replace, 'stat window', main

    _pr_wms_display, frm_name

end

pro cb_wgi_stats, wid, index

; reset the stat buffer  
  _pr_sts_clear
  state = dgpGetValue(wid)
  
  key = fn_mgs_get_key(index-1)
  print, "Key for stats is ",key
  _pr_mgs_set,'stats' ,key ,state
  
  if (state eq 1) then begin
    if (key ne 0) then begin
       _pr_stats_calc, key
;       _stats_display, key
    endif
  endif

end
; dgprev 12/14/05 create a single gate display
pro _pr_wgi_view, state, index

; reset the stat buffer  
;  _pr_sts_clear
;  _pr_dmf_set_index,'stats',index-1,state
  _pr_mgs_set,'stats',fn_mgs_get_key(index-1),state

  if (state eq 1) then begin
    key = fn_mgs_get_key(index-1)
    print, "Keys is ",key

    if (key ne 0) then begin
;       _pr_stats_calc, key
       _stats_display, key
    endif
  endif
end
; dgp rev 11/2/05 create a single gate view
; of stats.  Reopen on new gate selection
pro cb_wgi_view, wid, index

  state = dgpGetValue(wid)
  _pr_wgi_view, state, index

end
; dgp rev 12/23/05 get information from the 
; current gate being created
function fn_mgc_get, kw

  common cm_mgc_info, cv_mgc_cur

  if (size(cv_mgc_cur,/type) eq 11) then begin
    result = ISASKEY(cv_mgc_cur, kw)
    if (result ne 0) then begin
      return, cv_mgc_cur(kw)
    endif else begin
      return, 0
    endelse
  endif else begin
    return, 0
  endelse

end
; dgp rev 12/23/05 set a value for the current
; creation of a gate
pro _pr_mgc_set, kw, val

  common cm_mgc_info, cv_mgc_cur

  if (size(cv_mgc_cur,/type) eq 0) then  cv_mgc_cur = asarr('flag',0)

  cv_mgc_cur = [cv_mgc_cur,asarr(kw,val)]
  
end

pro _mgs_new_hist

   cells = fn_data_get_cells()

  print, "Calculate histogram subset"
; get region and parameters
    roi    = fn_mgc_get('roi')
    params = fn_mgc_get('parameters')
    reso   = fn_mgc_get('resolution')

    param = MAX(params, maxindex, Min=minval)

    ;print, "Using parameter ",param
    count = n_elements(cells(*,0))
    ;print, "Number of events = ",count

; determine minimums and maximums
    xmin = min([roi(0),roi(1)])
    xmax = max([roi(0),roi(1)])

    if (xmin lt 0) then xmin = 0

; calc subset
    subset = long(where((cells(*,param-1) ge xmin) and $
                        (cells(*,param-1) le xmax)))
    _pr_mgc_set, 'subset', subset

end
; dgp rev 11/21/05 create a new rectangular 
; gate from the current set values.
pro _mgs_new_rect

    cells = fn_data_get_cells()
    
; get region and parameters
    roi    = fn_mgc_get('roi')
    params = fn_mgc_get('parameters')
    reso   = fn_mgc_get('resolution')

    count = n_elements(cells(*,0))
    ;print, "Number of events = ",count

; determine minimums and maximums
    xmin = min([roi(0),roi(1)])
    xmax = max([roi(0),roi(1)])
    ymin = min([roi(2),roi(3)])
    ymax = max([roi(2),roi(3)])
    if (ymin lt 0) then ymin = 0
    if (xmin lt 0) then xmin = 0

; calc subset
    subset = long(where((cells(*,params(0)-1) ge xmin) and $
                       (cells(*,params(0)-1) le xmax) and $
                       (cells(*,params(1)-1) ge ymin) and $
                       (cells(*,params(1)-1) le ymax)))
    _pr_mgc_set, 'subset', subset

end

pro _mgs_new_poly

  cells = fn_data_get_cells()
    
; get region and parameters
    roi        = fn_mgc_get('roi')
    xverts     = fn_mgc_get('xverts')
    yverts     = fn_mgc_get('yverts')
    params     = fn_mgc_get('parameters')
    reso       = fn_mgc_get('resolution')

    count = n_elements(cells(*,0))

; Create an on/off filter for the ROI in a grid

  norm = _dsp_get_norm()

  squ = index_conv(lonarr(norm,norm),POLYFILLV(xverts, yverts, norm, norm))

  roi_count = n_elements(squ(*,0))

  filtr = intarr(norm,norm)

; for each ROI mark the filter with a 1

  for i=0l,roi_count-1 do filtr(squ(i,0),squ(i,1)) = 1
 
;  filtr = resamp(filtr,1024,1024)
 
; smooth or interplate the resolutes

  ;filtr = smooth(filtr,3)

; Now create a mask of indices into the real data

  mask = make_array(1,/long)

; For each event check it with the filter and if it passed add it to the mask

  mask = where(filtr(cells(*,params(0)-1),cells(*,params(1)-1)) eq 1, cnt)

  if (cnt eq 0) then begin
    print, "All events gated out"
    _pr_mgc_set, 'subset', 0
  endif else begin
    _pr_mgc_set, 'subset', mask(1:*)
  endelse
end

pro _mgs_new_subset

  type = fn_mgc_get('type')
  if (type eq 'hist') then begin
    ;print, "Calculate Hist"
    _mgs_new_hist
  endif else if (type eq 'poly') then begin
    ;print, "Calculate Poly"
    _mgs_new_poly
  endif else if (type eq 'rect') then begin
    ;print, "Calculate Rect"
    _mgs_new_rect
  endif
  
end
; Following the addition of a new gate
; Save the current gating information into the multi-gate structure

pro _mgs_store_cur

  common cm_mgs_info, cv_mgs_struct
  common _mgs_roi, xverts, yverts

  key = _gen_unique_name()

; get data
  type   = fn_mgc_get('type')  
  name   = fn_mgc_get('name')  
  uname  = fn_mgc_get('uname')  
  color  = fn_mgc_get('gate color')  
  subset = fn_mgc_get('subset')  
  params = fn_mgc_get('parameters')
  index  = fn_mgc_get('index')  
  roi    = fn_mgc_get('roi')  
  
  reso   = fn_mgc_get('resolution')  
  
; disable by default
  
  count = _mgs_get_count()
  print, "Total gates = ",count

; create structure  
  if (type eq 'poly') then begin
    
    value = {,type:type,name:name,uname:uname,color:color,params:params, $
                index:index,roi:roi,xverts:xverts,yverts:yverts,reso:reso}
  endif else begin
    value = {,type:type,name:name,uname:uname,color:color,params:params, $
                index:index,roi:roi,reso:reso}
  endelse    
  
  if (count eq 0) then begin
    print, "First gate ",uname, " key ", key
    cv_mgs_struct = asarr(key,value)
;    _mgs_set_subset, key, subset
  endif else begin
    print, "Additional gate ",uname, " key ", key
    cv_mgs_struct = [cv_mgs_struct,asarr(key,value)]
    ;print, "Struct type is ",size(cv_mgs_struct,/type)
;    _mgs_set_subset, key, subset
  endelse

end

function fn_dmf_par_id

  return, strsubst(strjoin(string(fn_dmf_get('pars'),format="(I2)")),' ','0',/glob)

end

function _mgs_valid_name, name

  params = fn_mgc_get('parameters')
  mn = params(0)
  mx = params(1)
  if (mn gt mx) then begin
    mn = params(1)
    mx = params(0)
  endif

  nums = strsubst(string(strtrim(mn,2), Format='(A2)'),' ','0',/glob)
  nums = nums + strsubst(string(strtrim(mx,2), Format='(A2)'),' ','0',/glob)

;  nums = strsubst(strcompress(string(mn,mx)),' ','0',/glob)
  uname = nums + name
  print, "Unique name is ",uname
  if (_mgs_get_sort_names(names)) then begin
    index = where(names eq uname,count)
    if (count ne 0) then begin
      return, 0
    endif
  endif

  _pr_mgc_set, 'uname', uname
  return, 1

end
; save the cv_mgs_struct structures to file
pro _mgs_save_struct

  common cm_mgs_info, cv_mgs_struct
  
  path = _cfg_get_work()  

  if (size(cv_mgs_struct,/type) eq 11) then begin 

    ON_IOERROR, null
    savename = fn_gen_build_path(fn_gen_getstr('work path'),'_mgs_struct.xxx') 
    save, cv_mgs_struct, filename=savename

  endif else begin
  
    delname = fn_gen_build_path(fn_gen_getstr('work path'),'_mgs_struct.xxx') 
    dlist = findfile(delname,count=cnt)
    if (cnt ne 0) then del_file, delname
  
  endelse

end

; update the gate layout window with new gating information
pro _mgs_update_layout

  _mgs_rebuild      

  _sense_mod, 'gate_on', "/sensitive"
  _sense_mod, 'gatebut', "/sensitive"

end
; dgp rev 9/13/05 convert the log/lin roi to 
; the normalized setting
; dgp rev 10/19/05 x and y axis stored in DMF directly
function fn_norm_roi, roi

  if (fn_dmf_get_xaxis() eq 1) then begin
    roi(0:1) = 256*alog10(roi(0:1))
  endif
  if (n_elements(roi) eq 4 and (fn_dmf_get_yaxis() eq 1)) then begin
    roi(2:3) = 256*alog10(roi(2:3))
  endif

  return, roi

end

; dgp rev 12/23/05 lock in the settings for the 
; new gate.
function fn_mgs_new

  roi_wid = fn_mgc_get('wid roi')

  ; Grab the currently set values from the form, as they
  ; may have changed.
  ; ROI is held as a LONG, rather than FLOAT
  roi = make_array(n_elements(roi_wid),/float)
  for i=0,n_elements(roi_wid)-1 do begin
    if (fn_wid_exists(roi_wid(i))) then roi(i) = dgpGetValue(roi_wid(i))
  endfor

  ; normalize the values
  roi = fn_norm_roi(roi)
  _pr_mgc_set, 'roi', roi

; get the gate name   
   txt_wid = fn_mgc_get('txt_wid')
   name = dgpGetValue(txt_wid)

   if (_mgs_valid_name(name)) then begin
   
    ; clear the 'gating' flag
     _pr_mgc_set, 'flag', 0

     _pr_mgc_set, 'name', name

; determine new subset for this gate
     _mgs_new_subset
   
; move information into structure
     _mgs_store_cur

; resort the keys
     _mgs_resort_keys
     
; save the structure into a file
     _mgs_save_struct

; update gate layout if present
     _mgs_update_layout
     
     return, 1

  endif
  return, 0

end

; dgp rev 9/13/05 convert the normal roi to 
; the proper log/lin setting
; dgp rev 10/19/05 x and y axis stored in DMF directly
function fn_lgln_roi, roi

  if (fn_dmf_get_xaxis() eq 1) then begin
    roi(0:1) = 10^(float(roi(0:1))/float(256.0))
  endif
  if (n_elements(roi) eq 4 and (fn_dmf_get_yaxis() eq 1)) then begin
    roi(2:3) = 10^(float(roi(2:3))/float(256.0))
  endif

  return, roi

end

pro _pr_win_purge

  device, window_state=arr
  idx = where(arr eq 6,cnt)
  if (cnt ne 0) then begin
    for i=0, n_elements(idx)-1 do begin
      wdelete, idx(i)
    endfor
  endif

end

; dgp rev 9/13/05 lock in the values for the new gate
; no more modifications allowed
PRO cb_mgs_new, wid, index

  main = _wid_get_parent(wid)

  if(fn_mgs_new()) then begin

  ; close the window
    status  = WwSetValue(main, /Close)
    
    _pr_win_purge
    draw_cells

  endif else begin
  
     print, "Invalid name"
     message = ['Name is not unique','for given parameter set']
     button=WwAlert(main, message, "Close")
  endelse


END


PRO _mgs_name, wid, value

  value = dgpGetValue(wid)

  _pr_mgc_set, 'name', value
  
END

PRO CB_Name, wid, value

  common info, color, name

  ;print, "Change name"
  ;print, "wid =",wid," value = ",value
  value = dgpGetValue(wid)
  print, "value ",value
  name = value
  
END

; dgp rev 7/8/09 Mark gating lines on histogram 
function _mgs_mark_hist

    color_idx = fn_gen_get('mgs color')
  
    color_rgb = WoColorConvert(color_idx,/IndextoColor)
  
  ; Initialize info
    ymax = fn_dmf_get('ymax')
    
    xsize = !D.X_Size
    ysize = !D.Y_Size
    win_orig = fn_dmf_get('window')
    print, "Current window is ",win_orig
    
   device, get_graphics_function = old_mask
   
; Create a pixmap copy of the window
    window, /free, xsize=xsize, ysize=ysize, /pixmap
    win_copy = !d.window
    
;    device, set_graphics_function = 6 ; GXxor function
    device,copy=[0,0,xsize,ysize,0,0,win_orig]
    
; Set the displayed window as current
    wset, win_orig
    cur_range = !x.crange(1) - !x.crange(0) + 1

; Get the beginning point (xb,yb)
    print,'Click and drag the mouse to create'
    print,'the rubberband box'
    cursor, x, y, 0, /data, /down
;    cursor, x, y, /data, /change
    xb = x
    yb = y
    xe = x
    ye = y

    moving = 1
; Check cursor
    cursor, x, y, /data, /up

; Loop until user lets up on button
    while moving do begin

; Check cursor
      cursor, x, y, /data, /change

      if (!mouse.button ne 1) then begin
; If position changes, update display
        while xe ne x and ye ne y do begin

; Calculate lower left and upper right points for last line drawn
; (device,copy requires the coordinates to be in this order)
          x1 = min([xb,xe])
          y1 = min([yb,ye])
          x2 = abs(xe-xb)
          y2 = abs(ye-yb)

; If length > 0, restore that part of the image from the pixmap
          if x2 gt 0 and y2 gt 0 then begin
          ;device,copy=[x1,0,x2-x1,fix(ymax)-1,x1,0,win_copy]
          ;device,copy=[0,0,xsize,ysize,0,0,win_orig]
;          device,copy=[0,0,xsize,ysize,0,0,win_copy]
            device,copy=[0,0,xsize,ysize,0,0,win_copy]
          endif

; Draw the new line and update the endpoint

          plots, [xb,xb], [ymax,0],/data, color=color_rgb
	  plots, [x,x]  ,[ymax,0] ,/data, color=color_rgb
	  plots, [xb,x] ,[y,y]     ,/data, color=color_rgb
          xe = x
          ye = y
        endwhile 

      endif else begin
      
	moving = 0

      endelse

    endwhile
    
;    device, set_graphics_function = old_mask
    xb = MIN([xb,xe],max=xe)
    hist = [xb, xe]
    idx = where(hist lt 0,cnt)
    if (cnt ne 0) then hist(idx) = 0
    
    ; normalize the gates to 1024
    
    dsp_norm = _dsp_get_norm()
    hist = (_dsp_get_norm()/cur_range)*hist
    idx = where(hist gt dsp_norm,cnt)
    if (cnt ne 0) then hist(idx) = dsp_norm
    print, "New hist gate is ",hist
        
    return, hist
    
end

pro _pr_dsp_mirror

; grab the current window
    win_orig = fn_dmf_get('window')
    print, "Current window is ",win_orig
    wset, win_orig
; get current window dimensions
    xsize = !D.X_Size
    ysize = !D.Y_Size
; Create a pixmap copy of the current window
    window, /free, xsize=xsize, ysize=ysize, /pixmap
; assign an identifier to this new invisible window (pixmap)
    print, "Copy of window ",win_orig," in window ",!d.window
    _pr_dmf_set, 'win copy', !d.window
; copy original window into hidden window
    device,copy=[0,0,xsize,ysize,0,0,win_orig]    
; Set the displayed window as current
    wset, win_orig

end
; dgp rev 11/21/05 mark a rectangular gate onto display
; filter and return the points
function _mgs_mark_rect

    color_idx = fn_gen_get('mgs color')
  
    color_rgb = WoColorConvert(color_idx,/IndextoColor)

; Initialize info

    xsize = !D.X_Size
    ysize = !D.Y_Size

; grab the current window
    win_orig = fn_dmf_get('window')
    print, "Current window is ",win_orig
; Create a pixmap copy of the current window
    window, /free, xsize=xsize, ysize=ysize, /pixmap
; assign an identifier to this new invisible window (pixmap)
    win_copy = !d.window
; copy original window into hidden window
    device,copy=[0,0,xsize,ysize,0,0,win_orig]
    
; Set the displayed window as current
    wset, win_orig
    cur_range = !x.crange(1) - !x.crange(0) + 1
    
; Get the beginning point (xb,yb)
    empty
    print,'Click and release, then draw gate'
    cursor, x, y, 0, /data, /down
    xb = x
    yb = y
    xe = x
    ye = y

    moving = 1
; Check cursor
    cursor, x, y, /data, /up

; Loop until user lets up on button
    while moving do begin

; Check cursor
      cursor, x, y, /data, /change

      if (!mouse.button ne 1) then begin
; If position changes, update display
        while xe ne x and ye ne y do begin

; Calculate lower left and upper right points for last line drawn
; (device,copy requires the coordinates to be in this order)
          x1 = min([xb,xe])
          y1 = min([yb,ye])
          x2 = abs(xe-xb)
          y2 = abs(ye-yb)

; If length > 0, restore that part of the image from the pixmap
          if x2 gt 0 and y2 gt 0 then begin
            device,copy=[0,0,xsize,ysize,0,0,win_copy]
          endif

; Draw the new line and update the endpoint
          plots, [xb,xb], [yb,y], /data, color=color_rgb
	  plots, [x,x],[yb,y],/data,color=color_rgb
	  plots, [xb,x],[y,y],/data,color=color_rgb
	  plots, [xb,x],[yb,yb],/data,color=color_rgb
          xe = x
          ye = y

        endwhile 

      endif else begin
      
	moving = 0

      endelse

    endwhile
    
    xb = MIN([xb,xe],max=xe)
    yb = MIN([yb,ye],max=ye)
    rect = [xb,xe,yb,ye]
    idx = where(rect lt 0,cnt)
    if (cnt ne 0) then rect(idx) = 0
    ;print, "Gate points:", rect

    ; normalize the gates to 1024
    
    dsp_norm = _dsp_get_norm()
    rect = (_dsp_get_norm()/cur_range)*rect
    idx = where(rect gt dsp_norm,cnt)
    if (cnt ne 0) then rect(idx) = dsp_norm
    print, "New rect gate is ",rect

    return, rect
    
end
; dgp rev 12/23/05 debug to show values
pro _pr_mgc_show

  common cm_mgc_info, cv_mgc_cur

  result = ASKEYS(cv_mgc_cur)
  print, "Keys: "
  for i=0,n_elements(result)-1 do begin
    if ((result(i) ne 'subset') and $
        (result(i) ne 'roi'))   then begin
      ;print, result(i)," = ",cv_mgc_cur(result(i))
    endif
  endfor

end

; dgp rev 11/2/05 before the gates are set firmly
; they can be adjusted by hand or by mouse
pro _pr_mgc_update, num

  name  = "Gate " + string(pad(num,2))

  roi    = fn_mgc_get('roi')    
  
  roi_wid       = fn_mgc_get('wid roi')

  if (fn_wid_exists(roi_wid(0))) then begin
    roi = fn_lgln_roi(roi)
    for i=0,n_elements(roi)-1 do begin
      s = WwSetValue(roi_wid(i),string(roi(i),format='(F7.1)'))
    endfor
  endif

  ; now enable the create button
  
end

pro _mgs_add_hist
  
  hist = _mgs_mark_hist()
  
  count = _mgs_get_count()

  count = count + 1

  _pr_mgc_set, 'type', 'hist'
  _pr_mgc_set, 'roi', hist
  _pr_mgc_set, 'index', count
  _pr_mgc_set, 'parameters', _cdr_get_pars()
  _pr_mgc_set, 'resolution', fn_dmf_calc_res()

; popup color and name widget, however it must be assumed that it is still open
  _pr_mgc_update, count
  
end

; dgp rev 11/21/05 prep the gating information 
; following the marking of the gates.  Call the 
; gate creation form to lock in the values.
pro _mgs_add_rect
  
  rect = _mgs_mark_rect()
  
  count = _mgs_get_count()

  count = count + 1

;  _mgs_ena_handler

  _pr_mgc_set, 'type', 'rect'
  _pr_mgc_set, 'roi', rect
  _pr_mgc_set, 'index', count
  _pr_mgc_set, 'parameters', _cdr_get_pars()
  _pr_mgc_set, 'resolution', fn_dmf_calc_res()

; popup color and name widget, however it must be assumed that it is still open
  _pr_mgc_update, count
  
end

; A gate has been enabled or disabled.
;   Set the enable variable.  
;   Recalculate the gate subset.  
;   Update the gate tally widget.  
;   Draw the cells and/or clusters.

pro _pr_cmd_gate_tog, index, value

  if (fn_mgs_get_sorted(keys)) then begin

    ;print, "Current setting is ",enable(key)
    _pr_mgs_set_enable, keys(index), value
    if (fn_wid_exists(fn_wra_get_index('1only',"Gate Form",index))) then begin
      if (value) then begin
        s = WwSetValue(fn_wra_get_index('1only',"Gate Form",index),/nonsens)
      endif else begin
        s = WwSetValue(fn_wra_get_index('1only',"Gate Form",index),/sens)
      endelse
    endif
    print, "Recombine the active gates"
    _pr_dmf_set, 'mgs flag', 1
    _ssl_recalc

    draw_cells

  endif

end

Pro CB_gate_Tog, wid, pass

  index = pass - 1

  internal = dgpGetValue(wid)
  _pr_cmd_gate_tog, index, internal  
        
End

Pro CB_gate1_Tog, wid, offset

  index = offset-1

  ; determine state
  state = dgpGetValue(wid)

  if (fn_mgs_get_sorted(keys)) then begin
    print, "GER toggle ",index," is ",keys(index)

    if (state eq 0) then begin
      ; just turn it off and done
      ; no redraw, as another call to this routine will be made
      _pr_mgs_set_enable, keys(index), 0
      _pr_dmf_set, '1only', " "
      s = WwSetValue(fn_wra_get_index('applied',"Gate Form",index),/sens)
      return
  
    endif else begin  
      
      if (fn_dmf_get('1only') eq keys(index)) then begin
        ; toggle has already been selected, so turn it off 
        state = 0
        _pr_dmf_set, '1only', " "
        s = WwSetValue(wid,state)
        s = WwSetValue(fn_wra_get_index('applied',"Gate Form",index),/sens)
      endif else begin
        _pr_dmf_set, '1only', keys(index)
        s = WwSetValue(fn_wra_get_index('applied',"Gate Form",index),/nonsens)
        state = 1
      endelse
    endelse

    _pr_mgs_set_enable, keys(index), state
    _pr_dmf_set, 'mgs flag', 1
    _ssl_recalc
    draw_cells
  endif
        
End
; dgp rev 8/28/07 returns the gate name for given index
function fn_mgs_on_key, index

  common cm_dsp_cur, cv_dsp_trans, cv_dsp_keys, cv_dsp_index 

  if (size(cv_dsp_index,/type) eq 0) then return, ""

  if (n_elements(cv_dsp_keys) lt index) then return, ""

  return, cv_dsp_keys(index)

end

; dgp rev 7/21/09 gate display mask
function _mgs_get_mask, key

  val1 = fn_mgs_get('label', key)
  val2 = fn_mgs_get('outline', key)
  val3 = fn_mgs_get('perc', key)
  
  print, (val3)*4+(val1*2)+val2
  
  return, (val3)*4+(val1*2)+val2

end

function fn_mgs_key_field, key, field

  common cm_mgs_info, cv_mgs_struct
  
;  field = STRUPCASE(field)
  name = ""
  if (ISASKEY(cv_mgs_struct,key)) then begin
    comm = "name = cv_mgs_struct(key)."+strtrim(field,2)
;    print, comm
    ..locals 1 0
    results = execute(comm)
  endif
  return, name

end
; dgp dev 9/13/05 get the percentage of display events
; within gate_key
function fn_mgs_get_perc, gate_key

  print, "Calc perc for ",gate_key
  
  tot = fn_dmf_get('data count')
  if (tot eq 0) then return, 0

  cur_subset = fn_mgs_get_subset(gate_key)

  if (size(cur_subset,/ndim) ne 0) then begin
    print, "First index in gate ",cur_subset(0)
    app_subset = fn_dmf_get('ssl indices')
    idx = index_and(app_subset,cur_subset)
    if (size(idx,/ndim) eq 0) then return, 0
    cnt = n_elements(idx)
    return, (float(cnt)/float(tot))*100
  endif
  
  return, 0

end
; dgp rev 8/28/07 returns the number of gates on current display
function fn_mgs_on_count

  common cm_dsp_cur, cv_dsp_trans, cv_dsp_keys, cv_dsp_index 

  if (size(cv_dsp_index,/type) eq 0) then return, 0

  return, n_elements(cv_dsp_keys) - 1

end
; display the region lines, label and percentage for a given gate
; dgp rev 8/11/05 increase font size for gate label
; dgp rev 8/11/05 place labels at the top of rectangle
pro _mgs_disp_rect, i, roi

    device, font=fn_dmf_get('mgs font')

    mask = _mgs_get_mask(fn_mgs_on_key(i))
    name  = fn_mgs_key_field(fn_mgs_on_key(i),"name")
    perc = fn_mgs_get_perc(fn_mgs_on_key(i))

    color_idx = fn_gen_get('mgs color')
  
    color_rgb = WoColorConvert(color_idx,/IndextoColor)
  
    ; dpg rev 8/28/07 Make Axis Order general variable
    xaxis = fn_gen_get("AxisOrder")
    yaxis = (xaxis + 1) mod 2 

    info, xaxis, yaxis
 
;    dev_roi = convert_coord(roi(xaxis*2:xaxis*2+1),roi(yaxis*2:yaxis*2+1),/data,/to_device)
    dev_roi = convert_coord(roi(0:1),roi(2:3),/data,/to_device)
    dev_squ = convert_coord(!x.crange,!y.crange,/data,/to_device)
                
;    plots, [dev_squ(0,0),dev_squ(0,1)], [dev_squ(1,0),dev_squ(1,1)], /device,color=color_rgb

    xe = max(dev_roi(0,0:1),min=xb)
    ye = max(dev_roi(1,0:1),min=yb)

    xmin = dev_squ(0,0) 
    xmax = dev_squ(0,1) 
    ymin = dev_squ(1,0) 
    ymax = dev_squ(1,1) 

; outline gates
  if ((mask and 1) ne 0) then begin

    xminidx = where([xb,xe] ge xmin,xmincnt)
    xmaxidx = where([xb,xe] le xmax,xmaxcnt)
    if xmaxcnt gt 0 and  xmincnt gt 0 then begin
      yminidx = where([yb,ye] ge ymin,ymincnt)
      ymaxidx = where([yb,ye] le ymax,ymaxcnt)
      if ymaxcnt gt 0 and ymincnt gt 0 then begin
        if xmincnt lt 2 then xb = xmin
        if xmaxcnt lt 2 then xe = xmax
        if ymincnt lt 2 then yb = ymin
        if ymaxcnt lt 2 then ye = ymax
        plots, [xb,xb], [yb,ye], /device,color=color_rgb
        plots, [xe,xe], [yb,ye], /device,color=color_rgb
        plots, [xb,xe], [ye,ye], /device,color=color_rgb
        plots, [xb,xe], [yb,yb], /device,color=color_rgb        
      endif
    endif
       
  endif
  label_size = fn_fnt_get('mgs ratio')
; label gates
  if ((mask and 2) ne 0) then begin
    txtx = (xe+xb)/2
    txty = ye
    XYOUTS, txtx, txty, name, /device, color=color_rgb, align=.5, size=label_size
  endif
; annotate the percentage  
  if ((mask and 4) ne 0) then begin
    fperc = string(perc,format="(F6.2)")
    txty = ye - fn_dsp_get_num('spacing')*1.5
    XYOUTS, xe, txty, fperc, /device, color=color_rgb, align=1, size=label_size
  endif
  
  device, font=fn_dmf_get('label font')

end

; dgp rev 7/21/09 Display active poly components -- name, outline, percentage
pro _mgs_disp_poly, i, xverts, yverts

    device, font=fn_dmf_get('mgs font')

    mask = _mgs_get_mask(fn_mgs_on_key(i))
    name  = fn_mgs_key_field(fn_mgs_on_key(i),"name")

    color_idx = fn_gen_get('mgs color')
  
    color_rgb = WoColorConvert(color_idx,/IndextoColor)

    reso = fn_dmf_calc_res()
    xverts = (float(reso)/float(_dsp_get_norm()))*xverts
    yverts = (float(reso)/float(_dsp_get_norm()))*yverts
    
    ; dgp rev 7/21/09  outline the gate
    if ((mask and 1) ne 0) then begin
      plots, transpose(xverts), transpose(yverts), /data, color=color_rgb
    endif
  
    label_size = fn_fnt_get('mgs ratio')
    y = max(yverts)
    xmn = min(xverts, max=xmx)
    x = (xmx+xmn)/2
    ; dgp rev 7/21/09 lable the gate
    if ((mask and 2) ne 0) then XYOUTS, x, y, name, color=color_rgb, align=.5, size=label_size
  
    ; dgp rev 7/21/09   ; annotate the percentage  
    if ((mask and 4) ne 0) then begin
      perc = fn_mgs_get_perc(fn_mgs_on_key(i))
      fperc = string(perc,format="(F6.2)")
      txty = y - fn_dsp_get_num('spacing')*1.5
;      XYOUTS, x, txty, fperc, /device, color=color_rgb, align=1, size=label_size
      XYOUTS, x, txty, fperc, color=color_rgb, align=1, size=label_size
    endif

    device, font=fn_dmf_get('label font')

end

; dgp rev 7/21/09 Display the gate information not being masked out
pro _mgs_disp_hist, i, roi
 
  device, font=fn_dmf_get('mgs font')

  mask = _mgs_get_mask(fn_mgs_on_key(i))

  name  = fn_mgs_key_field(fn_mgs_on_key(i),"name")

  perc = fn_mgs_get_perc(fn_mgs_on_key(i))

  color_idx = fn_gen_get('mgs color')
  
  color_rgb = WoColorConvert(color_idx,/IndextoColor)
  
  ymax = fn_dmf_get('ymax')

  dev_roi = convert_coord(roi,[0,ymax],/data,/to_device)
        
  xb = dev_roi(0,0)
  xe = dev_roi(0,1)
  yb = dev_roi(1,0)
  ye = dev_roi(1,1)

  xmin = !x.crange(1) * float(fn_dmf_xmin_ratio())
  xmax = !x.crange(1) * float(fn_dmf_xmax_ratio())
  ; draw gate
  if ((mask and 1) ne 0) then begin

    if xb le xmax and xb ge xmin then plots, [xb,xb], [yb,ye] ,/device, color=color_rgb
    if xe le xmax and xe ge xmin then plots, [xe,xe], [yb,ye] ,/device, color=color_rgb
  
  endif
  ; annotate label
  label_size = fn_fnt_get('mgs ratio')
  txty = fn_dsp_get_num('top')
  if xe gt xmax then xe = xmax

  if ((mask and 2) ne 0) then begin
    txty = txty - fn_dsp_get_num('spacing')
    XYOUTS, xe, txty, name, color=color_rgb, /device,  align=1, size=label_size
  endif
; annotate the percentage  
  if ((mask and 4) ne 0) then begin
    fperc = string(perc,format="(F6.2)")
    txty = txty - fn_dsp_get_num('spacing')
    XYOUTS, xe, txty, fperc, color=color_rgb, align=1, /device, size=label_size
  endif
  
  device, font=fn_dmf_get('label font')

end

; dgp rev 9/14/05 get the subset size of a given gate
function fn_mgs_get_size, gate_key

  common cm_ifs, cv_ifs_struct

  print, "Get subset ",gate_key
  file_key = fn_dmf_get('ifs key')
  
  if (isaskey(cv_ifs_struct,file_key)) then begin
  if (isaskey(cv_ifs_struct(file_key),'subsets')) then begin
    ; subset has been previously created, therefore check for key
    if (isaskey(cv_ifs_struct(file_key,'subsets'),gate_key)) then begin
      ; key exists, so return it
    endif else begin
      ; key must be calculated
      subset = _mgs_recalc_gate(gate_key)
      cv_ifs_struct(file_key,'subsets') = [cv_ifs_struct(file_key,'subsets'),asarr(gate_key,subset)]
    endelse
  endif else begin
    ; create the first subset
    subset = _mgs_recalc_gate(gate_key)
    cv_ifs_struct(file_key) = [cv_ifs_struct(file_key),asarr('subsets',asarr(gate_key,subset))]
  endelse
  
  return, n_elements(cv_ifs_struct(file_key,'subsets',gate_key))
  endif
  
  print, "******** No ifs key found"
  return, 0

end

function fn_dsp_trans

  common cm_dsp_cur, cv_dsp_trans, cv_dsp_keys, cv_dsp_index 

  if (size(cv_dsp_trans,/type) eq 0) then return, 0

  return, cv_dsp_trans

end

pro _pr_ano_on_add, key, item, value

  common cm_ano_info, cv_ano_struct

  if (size(cv_ano_struct,/type) ne 11) then begin
    ; new structure
;    print, "ANO - new"
    cv_ano_struct = asarr('on',asarr(key,asarr(item,value)))
  endif else begin
    ; check on
    if (isaskey(cv_ano_struct,'on')) then begin
      if (isaskey(cv_ano_struct('on'),key)) then begin
        ; key exists - append key with new item
        if (isaskey(cv_ano_struct('on',key),item)) then begin
          ; item exists - append value
;          print, "ANO - append value"
          cv_ano_struct('on',key,item) = [cv_ano_struct('on',key,item),value]
        endif else begin
          ; no item - add
;          print, "ANO - add item"
          cv_ano_struct('on',key) = [cv_ano_struct('on',key),asarr(item,value)]
        endelse
      endif else begin
        ; no key - add and key
;          print, "ANO - add key and item"
          cv_ano_struct('on') = [cv_ano_struct('on'),asarr(key,asarr(item,value))]
      endelse
    endif else begin
          cv_ano_struct = [cv_ano_struct,asarr('on',asarr(key,asarr(item,value)))]    
    endelse
  endelse

end

pro _pr_ano_app_add, key, item, value

  common cm_ano_info, cv_ano_struct

  if (size(cv_ano_struct,/type) ne 11) then begin
    ; new structure
;    print, "ANO - new"
    cv_ano_struct = asarr('applied',asarr(key,asarr(item,value)))
  endif else begin
    ; check on
    if (isaskey(cv_ano_struct,'applied')) then begin
      if (isaskey(cv_ano_struct('applied'),key)) then begin
        ; key exists - append key with new item
        if (isaskey(cv_ano_struct('applied',key),item)) then begin
          ; item exists - append value
;          print, "ANO - append value"
          cv_ano_struct('applied',key,item) = [cv_ano_struct('applied',key,item),value]
        endif else begin
          ; no item - add
;          print, "ANO - add item"
          cv_ano_struct('applied',key) = [cv_ano_struct('applied',key),asarr(item,value)]
        endelse
      endif else begin
        ; no key - add and key
;          print, "ANO - add key and item"
          cv_ano_struct('applied') = [cv_ano_struct('applied'),asarr(key,asarr(item,value))]
      endelse
    endif else begin
          cv_ano_struct = [cv_ano_struct,asarr('applied',asarr(key,asarr(item,value)))]    
    endelse
  endelse

end

pro _pr_ano_load

  common cm_ano_info, cv_ano_struct
  
  if (size(cv_ano_struct,/type) ne 11) then return
  
  _pr_dmf_set_qp, 'annotate', cv_ano_struct  

end

pro _pr_ano_clear

  common cm_ano_info, cv_ano_struct
  
  cv_ano_struct = complex(1)

end

pro _pr_ano_debug

  common cm_ano_info, cv_ano_struct
  
  stop ;debug

end

; gates to display for given parameter pair
; dgp rev 10/18/05 allow for font ratios based on absolute size
pro _mgs_disp_gates

  _pr_ano_clear

  if (not fn_dmf_get('data flag')) then return

  pars = _cdr_get_pars()
  print, "Display gates for parameters ",pars(0),pars(1)

  applied_count = fn_mgs_on_count()

  print, "Displaying ",applied_count," keys"

  if (applied_count gt 0) then begin

    _pr_dsp_set_cols, applied_count

    disp_reso = fn_dmf_calc_res()
    print, "Displaying at resolution ",disp_reso
    mult = float(disp_reso)/float(_dsp_get_norm())
      
    FOR I = 1, applied_count DO begin
        
      mask = _mgs_get_mask(fn_mgs_on_key(i))
      if (mask gt 0) then begin
        name  = fn_mgs_key_field(fn_mgs_on_key(i),"name")
        uname = fn_mgs_key_field(fn_mgs_on_key(i),"uname")
        roi   = fn_mgs_key_field(fn_mgs_on_key(i),"roi")
	roi = (mult)*roi

        type = fn_mgs_key_field(fn_mgs_on_key(i),"type")
        perc = fn_mgs_get_perc(fn_mgs_on_key(i))
        print, "Gate ",name," is ",perc," percent"
        _pr_ano_on_add, fn_mgs_on_key(i), 'mask', mask
        _pr_ano_on_add, fn_mgs_on_key(i), 'perc', perc
        _pr_ano_on_add, fn_mgs_on_key(i), 'name', name


        _pr_ano_on_add, fn_mgs_on_key(i), 'sample', fn_dmf_get_header("$SMNO")

        if (type eq 'hist') then begin
          _pr_ano_on_add, fn_mgs_on_key(i), 'roi', roi
          _mgs_disp_hist, i, roi
	endif else begin
          gpars   = fn_mgs_key_field(fn_mgs_on_key(i),"params")
          ; dgp rev 8/28/07 flip if gate is transposed to actual display
   	  if (pars(0) ne gpars(0)) then begin
	    print, "Flip axis for ",name
	    roi = shift(roi,2)
	  endif
          if (type eq 'rect') then begin
            _pr_ano_on_add, fn_mgs_on_key(i), 'roi', roi
            _mgs_disp_rect, i, roi
          endif else if (type eq 'poly') then begin
            xverts = fn_mgs_key_field(fn_mgs_on_key(i),"xverts")
            yverts = fn_mgs_key_field(fn_mgs_on_key(i),"yverts")
            _mgs_disp_poly, i, xverts, yverts
          endif
	endelse
      endif

    ENDFOR

  endif
  
  _pr_ano_load
    
end
; dgp rev 11/9/05 finalize the gate settings 
pro _pr_final_gate, roi

    Common colour, Wht, Blk 
    
      color_rgb = blk
      reso       = fn_mgc_get('resolution')
      ratio      = fn_mgc_get('ratio')
  ; Draw the new line and update the endpoint
  
      idx = where(roi lt 0,cnt)
      if (cnt ne 0) then roi(idx) = 0
      idx = where(roi gt reso-1,cnt)
      if (cnt ne 0) then roi(idx) = reso
      
      plots, [roi(0),roi(0)], [roi(2),roi(3)], /data, color=color_rgb
      plots, [roi(1),roi(1)],[roi(2),roi(3)],/data,color=color_rgb
      plots, [roi(0),roi(1)],[roi(3),roi(3)],/data,color=color_rgb
      plots, [roi(0),roi(1)],[roi(2),roi(2)],/data,color=color_rgb

      roi = roi / ratio
      _pr_mgc_set, 'roi', roi
      roi_wid       = fn_mgc_get('wid roi')
      if (fn_wid_exists(roi_wid(0))) then begin
        roi = fn_lgln_roi(roi)
        for i=0,n_elements(roi_wid)-1 do begin
          s = WwSetValue(roi_wid(i),string(roi(i),format='(F7.1)'))
        endfor
      endif

end

pro cb_mod_gate, wid, value

  print, "Change gate"

  _pr_mgs_grab_cur
  _pr_mgs_draw_cur

end
; dgp rev 12/23/05 drag and drop the current poly
; gate being created before it is locked in.
pro _pr_mgc_move_poly

    Common colour, Wht, Blk 

      color_idx = fn_gen_get('mgs color')
      color_rgb = wocolorconvert(color_idx,/indextocolor)
      color_mov = wocolorconvert(color_idx+2,/indextocolor)

      reso       = fn_mgc_get('resolution')
      xverts     = fn_mgc_get('xverts')
      yverts     = fn_mgc_get('yverts')
      
      norm = _dsp_get_norm()

      ratio = float(reso)/float(norm)

  ; Initialize info
      xsize = !D.X_Size
      ysize = !D.Y_Size  
  ; grab the current window
      win_orig = fn_dmf_get('window')
      print, "Current window is ",win_orig
      wset, win_orig
  ; assign an identifier to this new invisible window (pixmap)
      win_copy = fn_dmf_get('win copy')
  ; copy original window into hidden window
      device,copy=[0,0,xsize,ysize,0,0,win_copy]
      
      roi    = fn_mgc_get('roi')
      roi = roi * ratio
      start  = roi
      xverts = xverts * ratio
      yverts = yverts * ratio
  ; Draw the gate
      plots, [roi(0),roi(0)],[roi(2),roi(3)],/data,color=color_mov
      plots, [roi(1),roi(1)],[roi(2),roi(3)],/data,color=color_mov
      plots, [roi(0),roi(1)],[roi(3),roi(3)],/data,color=color_mov
      plots, [roi(0),roi(1)],[roi(2),roi(2)],/data,color=color_mov

  ; Get the beginning point (roi(0),roi(2))
      print,'Click and drag the mouse to move the gate'
      cursor, x, y, 0, /data, /down
  
      old_x = x
      old_y = y

  ; Loop until user lets up on button
      while !err gt 0 do begin
  
  ; If position changes, update display
        while old_x ne x and old_y ne y do begin
  
  ; Calculate lower left and upper right points for last line drawn
  ; (device,copy requires the coordinates to be in this order)
          chg_x = old_x - x
          chg_y = old_y - y
          old_x = x
          old_y = y

          roi(0) = roi(0) - chg_x
          roi(1) = roi(1) - chg_x
          roi(2) = roi(2) - chg_y
          roi(3) = roi(3) - chg_y
          device,copy=[0,0,xsize,ysize,0,0,win_copy]
   
  ; Draw the new line and update the endpoint
          plots, [roi(0),roi(0)],[roi(2),roi(3)],/data,color=color_mov
  	  plots, [roi(1),roi(1)],[roi(2),roi(3)],/data,color=color_mov
  	  plots, [roi(0),roi(1)],[roi(3),roi(3)],/data,color=color_mov
  	  plots, [roi(0),roi(1)],[roi(2),roi(2)],/data,color=color_mov

        endwhile 
  
  ; Check cursor
        cursor, x, y, /data, /chan
      endwhile

  ; Draw the new line and update the endpoint
  
      idx = where(roi lt 0,cnt)
      if (cnt ne 0) then roi(idx) = 0
      idx = where(roi gt reso-1,cnt)
      if (cnt ne 0) then roi(idx) = reso-1
      
      chg_x = start(0) - roi(0)
      chg_y = start(2) - roi(2)
      print, "Shifted: ",chg_x, chg_y
      xverts = xverts - chg_x
      yverts = yverts - chg_y
      device,copy=[0,0,xsize,ysize,0,0,win_copy]
      plots, transpose(xverts), transpose(yverts), /data, color=color_rgb
      roi = roi / ratio
      xverts = xverts / ratio
      yverts = yverts / ratio
      _pr_mgc_set, 'roi', roi
      _pr_mgc_set, 'xverts', xverts
      _pr_mgc_set, 'yverts', yverts
      roi_wid       = fn_mgc_get('wid roi')
      if (fn_wid_exists(roi_wid(0))) then begin
        roi = fn_lgln_roi(roi)
        ; dgp rev 4/27/2010 use roi variable instead of widget array
        for i=0,n_elements(roi)-1 do begin
          s = WwSetValue(roi_wid(i),string(roi(i),format='(F6.1)'))
        endfor
      endif

end
; dgp rev 12/23/05 drag and drop the current histogram
; gate being created before it is locked in.
pro _pr_mgc_move_hist

    Common colour, Wht, Blk 

      color_idx = fn_gen_get('mgs color')
      color_rgb = wocolorconvert(color_idx,/indextocolor)
      color_mov = wocolorconvert(color_idx+2,/indextocolor)

      reso       = fn_mgc_get('resolution')
      ymax = fn_dmf_get('ymax')      
      
      norm = _dsp_get_norm()

      ratio = float(reso)/float(norm)

  ; Initialize info
      xsize = !D.X_Size
      ysize = !D.Y_Size  
  ; grab the current window
      win_orig = fn_dmf_get('window')
      print, "Current window is ",win_orig
      wset, win_orig
  ; assign an identifier to this new invisible window (pixmap)
      win_copy = fn_dmf_get('win copy')
  ; copy original window into hidden window
      device,copy=[0,0,xsize,ysize,0,0,win_copy]
      
      roi    = fn_mgc_get('roi')
      roi = roi * ratio
  ; Draw the gate

    top = ymax
    bot = 0
    mid = ymax/2

    plots, [roi(0),roi(0)], [top,bot],/data, color=color_mov
    plots, [roi(1),roi(1)], [top,bot] ,/data, color=color_mov
    plots, [roi(0),roi(1)], [mid,mid]     ,/data, color=color_mov

  ; Get the beginning point (roi(0),roi(2))
      print,'Click and drag the mouse to move the gate'
      cursor, x, y, 0, /data, /down
  
      old_x = x

  ; Loop until user lets up on button
      while !err gt 0 do begin
  
  ; If position changes, update display
        while old_x ne x do begin
  
  ; Calculate lower left and upper right points for last line drawn
  ; (device,copy requires the coordinates to be in this order)
          chg_x = old_x - x
          old_x = x

          roi(0) = roi(0) - chg_x
          roi(1) = roi(1) - chg_x
          device,copy=[0,0,xsize,ysize,0,0,win_copy]
   
  ; Draw the new line and update the endpoint
        plots, [roi(0),roi(0)], [top,bot],/data, color=color_mov
        plots, [roi(1),roi(1)], [top,bot] ,/data, color=color_mov
        plots, [roi(0),roi(1)], [mid,mid]     ,/data, color=color_mov
  
        endwhile 
  
  ; Check cursor
        cursor, x, y, /data, /chan
      endwhile

  ; Draw the new line and update the endpoint
  
      idx = where(roi lt 0,cnt)
      if (cnt ne 0) then roi(idx) = 0
      idx = where(roi gt reso-1,cnt)
      if (cnt ne 0) then roi(idx) = reso-1
      
      plots, [roi(0),roi(0)], [top,bot],/data, color=color_rgb
      plots, [roi(1),roi(1)], [top,bot] ,/data, color=color_rgb
      plots, [roi(0),roi(1)], [mid,mid]     ,/data, color=color_rgb
      roi = roi / ratio
      _pr_mgc_set, 'roi', roi
      roi_wid       = fn_mgc_get('wid roi')
      if (fn_wid_exists(roi_wid(0))) then begin
        roi = fn_lgln_roi(roi)
        ; dgp rev 4/27/2010 update roi widget using roi element loop
        for i=0,n_elements(roi)-1 do begin
          s = WwSetValue(roi_wid(i),string(roi(i),format='(F6.1)'))
        endfor
      endif
end

; dgp rev 4/27/2010 grab the current widget values
pro _pr_mgs_grab_cur

  roi_wid = fn_mgc_get('wid roi')
  roi     = fn_mgc_get('roi')
  if (fn_wid_exists(roi_wid(0))) then begin
    ; dgp rev 4/27/2010 loop thru the roi element values instead of widget
    for i=0,n_elements(roi)-1 do begin
      roi(i) = dgpGetValue(roi_wid(i))
      print, roi(i)
    endfor
    roi = fn_norm_roi(roi)
  endif
  _pr_mgc_set, 'roi', roi

end

pro _pr_mgs_draw_cur

  Common colour, Wht, Blk 

  color_idx = fn_gen_get('mgs color')
  color_rgb = wocolorconvert(color_idx,/indextocolor)

  roi      = fn_mgc_get('roi')
  win_copy = fn_dmf_get('win copy')
  
  ; Initialize info
  xsize = !D.X_Size
  ysize = !D.Y_Size  
  device,copy=[0,0,xsize,ysize,0,0,win_copy]
  reso       = fn_mgc_get('resolution')
  norm = _dsp_get_norm()

  ratio = float(reso)/float(norm)

  roi = roi * ratio
  type = fn_mgc_get('type')
  
  print, ratio, type
  
  ; Draw the gate  
  if (type eq 'hist') then begin
    ymax = fn_dmf_get('ymax')      
    top = ymax
    bot = 0
    mid = ymax/2
    plots, [roi(0),roi(0)], [top,bot],/data, color=color_rgb
    plots, [roi(1),roi(1)], [top,bot],/data, color=color_rgb
    plots, [roi(0),roi(1)], [mid,mid],/data, color=color_rgb
  endif else begin  
    plots, [roi(0),roi(0)],[roi(2),roi(3)],/data,color=color_rgb
    plots, [roi(1),roi(1)],[roi(2),roi(3)],/data,color=color_rgb
    plots, [roi(0),roi(1)],[roi(3),roi(3)],/data,color=color_rgb
    plots, [roi(0),roi(1)],[roi(2),roi(2)],/data,color=color_rgb
  endelse
  
end
; dgp rev 12/23/05 drag and drop the current rect
; gate being created before it is locked in.
pro _pr_mgc_move_rect

    Common colour, Wht, Blk 

      color_idx = fn_gen_get('mgs color')
      color_rgb = wocolorconvert(color_idx,/indextocolor)
      color_mov = wocolorconvert(color_idx+2,/indextocolor)

      reso       = fn_mgc_get('resolution')
      
      norm = _dsp_get_norm()

      ratio = float(reso)/float(norm)
      _pr_mgc_set, 'ratio', ratio

  ; Initialize info
      xsize = !D.X_Size
      ysize = !D.Y_Size  
  ; grab the current window
      win_orig = fn_dmf_get('window')
      print, "Current window is ",win_orig
      wset, win_orig
  ; assign an identifier to this new invisible window (pixmap)
      win_copy = fn_dmf_get('win copy')
  ; copy original window into hidden window
      device,copy=[0,0,xsize,ysize,0,0,win_copy]
      
      roi    = fn_mgc_get('roi')
      roi = roi * ratio
  ; Draw the gate
      plots, [roi(0),roi(0)],[roi(2),roi(3)],/data,color=color_mov
      plots, [roi(1),roi(1)],[roi(2),roi(3)],/data,color=color_mov
      plots, [roi(0),roi(1)],[roi(3),roi(3)],/data,color=color_mov
      plots, [roi(0),roi(1)],[roi(2),roi(2)],/data,color=color_mov

  ; Get the beginning point (roi(0),roi(2))
      print,'Click and drag the mouse to move the gate'
      cursor, x, y, 0, /data, /down
  
      old_x = x
      old_y = y

  ; Loop until user lets up on button
      while !err gt 0 do begin
  
  ; If position changes, update display
        while old_x ne x and old_y ne y do begin
  
  ; Calculate lower left and upper right points for last line drawn
  ; (device,copy requires the coordinates to be in this order)
          chg_x = old_x - x
          chg_y = old_y - y
          old_x = x
          old_y = y

          roi(0) = roi(0) - chg_x
          roi(1) = roi(1) - chg_x
          roi(2) = roi(2) - chg_y
          roi(3) = roi(3) - chg_y
          device,copy=[0,0,xsize,ysize,0,0,win_copy]
   
  ; Draw the new line and update the endpoint
          plots, [roi(0),roi(0)], [roi(2),roi(3)], /data, color=color_mov
  	  plots, [roi(1),roi(1)],[roi(2),roi(3)],/data,color=color_mov
  	  plots, [roi(0),roi(1)],[roi(3),roi(3)],/data,color=color_mov
  	  plots, [roi(0),roi(1)],[roi(2),roi(2)],/data,color=color_mov
  
        endwhile 
  
  ; Check cursor
        cursor, x, y, /data, /chan
      endwhile

      device,copy=[0,0,xsize,ysize,0,0,win_copy]
      _pr_final_gate, roi

end

pro cb_mgc_move, wid, value

  type       = fn_mgc_get('type')

  _pr_dmf_switch, fn_mgc_get('display')

  print, "Type is ",type

  case type of
  'rect': _pr_mgc_move_rect
  'poly': _pr_mgc_move_poly
  'hist': _pr_mgc_move_hist
  else: print, "Invalid gate"
  endcase

end
; dgp rev 12/26/05 enable/disable 'create' button
; for creating a new gate.
pro _pr_mgc_ready, flag

  crebut = fn_mgc_get('wid create')
  if (fn_wid_exists(crebut)) then begin
    if (flag) then begin
      s = WwSetValue(crebut,/sensitive)
    endif else begin
      s = WwSetValue(crebut,/nonsensitive)
    endelse
  endif

end

pro _pr_reflect_gate

  if (fn_dmf_single()) then begin
    _pr_wra_disable, 'poly'
    _pr_wra_disable, 'rect'
    _pr_wra_enable, 'line'
    _pr_wra_disable, 'y roi'
  endif else begin
    _pr_wra_enable, 'poly'
    _pr_wra_enable, 'rect'
    _pr_wra_disable, 'line'
    _pr_wra_enable, 'y roi'
  endelse

end

pro cb_mgs_cancel, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  s = WwSetValue(_wid_get_parent(wid),/close)

  _pr_gen_set, 'poly', 0

  draw_cells

end

; dgp rev 12/21/05 prepare to finalize a new gate
pro _pr_mgs_create, disp_name

  frm_name = "Gate Type"

  _pr_mgc_clear
  
  _pr_wms_set, 'state', 'reform'

  if (fn_wms_form(frm_name,layout)) then return
    
  lo_main      = WwLayout(layout, /hor)
  lo_left      = WwLayout(lo_main, /ver)
  lo_center    = WwLayout(lo_main, /ver)
  lo_centertop = WwLayout(lo_center, /ver)
  lo_axis      = WwLayout(lo_center, /ver)
  lo_axisx   = WwLayout(lo_axis, /hor)
  lo_axisy   = WwLayout(lo_axis, /hor)
  lo_right     = WwLayout(lo_main, /ver)
    
  s = WwText(lo_left,/label,text=disp_name)

; gate type menu
  polbut   = WwButtonBox(lo_left, 'Poly',  'cb_Set_nrg')
  recbut   = WwButtonBox(lo_left, 'Rect',  'cb_Set_Rect')
  hisbut   = WwButtonBox(lo_left, 'Line',  'cb_Set_Hist')
  _pr_wra_reg, 'poly', polbut
  _pr_wra_reg, 'rect', recbut
  _pr_wra_reg, 'line', hisbut

; gate information

  name = "New Gate"
  
  txt     = WwText(lo_centertop, '_mgs_name', Text=name,label="Name")
  _pr_mgc_set, 'txt_wid', txt
  movbut  = WwButtonBox(lo_centertop, 'Move', 'cb_mgc_move')   

  label = ["x-min","x-max","y-min","y-max"]
  roi_wid = [0L,0L,0L,0L]

; dgp rev 2/26/2010 
  name = strtrim(0)
  for i=0, 1 do begin
     name = strtrim(i)
     roi_wid(i) = WwText(lo_axisx,'cb_mod_gate',text='',label=label(i),cols=8,name=name)
  endfor

  for i=2, 3 do begin
     name = strtrim(i)
     roi_wid(i) = WwText(lo_axisy,'cb_mod_gate',text='',label=label(i),cols=8,name=name)
  endfor
  wid_y = roi_wid(2:3)
  

  _pr_wra_reg, 'y roi', lo_axisy
  _pr_mgc_set, 'wid roi', roi_wid
  _pr_mgc_set, 'display', disp_name

; sensitize the widgets

  crebut   = WwButtonBox(lo_right, 'Create', 'cb_mgs_new')   
  canbut   = WwButtonBox(lo_right, 'Cancel',  'cb_mgs_cancel')
    
  _pr_mgc_set, 'wid create', crebut
  _pr_mgc_ready, 0
  _pr_wms_display, frm_name
  _pr_reflect_gate
     
end

pro bphd_mgs_mark, wid, shell, event

  _pr_dmf_set, 'gating', 1
  if (fn_dmf_single()) then begin
    gate_type = 'Line'
  endif else begin
    gate_type = fn_dmf_get('gate type')
    if (gate_type eq 'Rect') then gate_type = 'Poly'
    if (gate_type eq 'Poly') then gate_type = 'Rect'
  endelse
  _pr_dmf_set, 'gate type', gate_type
  s = WtSet(wid,{,text:gate_type})
  status = WwHandler(wid, 'bphd_mgs_mark', 'ButtonPressMask')

end
;dgp rev 12/23/05 prepare for a new gate
pro _pr_mgc_prep

  ; display is redraw to assure the proper coordinates
  draw_cells
  ; create a mirror of the display
  _pr_dsp_mirror

  ; check for unclosed gate dialog
  if (fn_mgc_get('flag') eq 1) then begin
    print, "Abort previous gate"
;    _mgs_dis_handler
    main = fn_mgc_get('widget')
    if (fn_wid_exists(main)) then status  = WwSetValue(main, /Close)
  endif else begin
    print, "No open gates"
  endelse
  
end

; dgp rev 4/11/08 place an additional layer on the WSET procedure for analysis purposes
pro _pr_win_set, win

  print, "Setting window to ",win
;  on_error, 3, /continue
  wset, win

end

; dgp rev 1/30/06 last vertex is joined to first
; dgp rev 7/9/09 add the new non-rect gating interface
function fn_nrg_done

  common polyinfo, polystruct
  common vectinfo, vect_x, vect_y

  polystruct.startx = vect_x(0)
  polystruct.starty = vect_y(0)
  vect_x = [vect_x,polystruct.startx]
  vect_y = [vect_y,polystruct.starty]

  device,copy=[0,0,polystruct.xsize,polystruct.ysize,0,0,polystruct.win_copy]

  plots,  vect_x, vect_y, /device, color=1

  wset,polystruct.win_copy
; Create a pixmap copy of the window
  device,copy=[0,0,polystruct.xsize,polystruct.ysize,0,0,polystruct.win_orig]

  _pr_win_set, polystruct.win_orig

;  dat_verts = Convert_Coord(vect_x,vect_y,/device,/to_data)

  return, [[vect_x],[vect_y]]
    
end

; dgp rev 12/23/05 mark a new poly gate
; dgp rev 12/28/07 enhance poly gate for display ranges
; dgp rev 7/9/09 add the new non-rect gating interface
pro _pr_nrg_done

  common _mgs_roi, xverts, yverts

; Select a "Region of Interest" from the 512 by 512 device coordinates. 
; The x and y coordinates contained in xsubs and ysubs representing 
; the perimenter of the region.  While "device_roi" contains a 1D array of elements
; within the region out of a possible 512 times 512 number of elements.

  verts = fn_nrg_done()

  xverts = verts(*,0)
  yverts = verts(*,1)

; revert to data coordinates
  dat_verts = Convert_Coord(xverts,yverts,/device,/to_data)
; fetch the resolution for clipping and normalization
  reso = fn_dmf_calc_res()
  norm = _dsp_get_norm()
; clip
  idx = where((dat_verts lt 0),cnt)
  print, "Trim the region of ",cnt," clipped coors"
  if (cnt ne 0) then dat_verts(idx) = 0
  idx = where((dat_verts ge reso),cnt)
  print, "Trim the region of ",cnt," clipped coors"
  if (cnt ne 0) then dat_verts(idx) = reso
; normalize
  dat_verts = (norm/reso)*dat_verts
; reduce to xverts and yverts -- removing zverts
  xverts = transpose(dat_verts(0,*))
  yverts = transpose(dat_verts(1,*))

  _pr_mgc_set, 'xverts', xverts
  _pr_mgc_set, 'yverts', yverts

  pollie = [min(xverts),max(xverts),min(yverts),max(yverts)]
  count = _mgs_get_count()

  count = count + 1

; popup color and name widget, however it must be assumed that it is still open
   wait, .6
   status = WtProcessEvent( /Drain )

  _pr_mgc_set, 'type', 'poly'
  _pr_mgc_set, 'roi', pollie
  _pr_mgc_set, 'index', count
  _pr_mgc_set, 'parameters', _cdr_get_pars()
  _pr_mgc_set, 'resolution', fn_dmf_calc_res()

  _pr_mgc_update, count
  _pr_mgc_ready, 1

end


; dgp rev 1/30/06 button press handler marking of a vertex
; dgp rev 7/9/09 add the new non-rect gating interface
pro bphd_nrg_mark, wid, shell, event

  common vectinfo, vect_x, vect_y
  common polyinfo, polystruct

  if (event.button ne 1) then begin
    status = WwHandler(wid, 'bphd_nrg_mark', 'ButtonPressMask', /remove)
    status = WwHandler(wid, 'pmhd_nrg_move', 'PointerMotionMask', /remove)
    _pr_nrg_done
    _pr_gen_set, 'poly', 0
    _pr_reflect_gate
    return
  endif

  polystruct.startx = event.x
  polystruct.starty = abs(event.y-polystruct.ysize)
  vect_x = [vect_x,polystruct.startx]
  vect_y = [vect_y,polystruct.starty]

  device,copy=[0,0,polystruct.xsize,polystruct.ysize,0,0,polystruct.win_copy]

  plots,  vect_x, vect_y, /device, color=1

  wset,polystruct.win_copy
; Create a pixmap copy of the window
  device,copy=[0,0,polystruct.xsize,polystruct.ysize,0,0,polystruct.win_orig]

  _pr_win_set, polystruct.win_orig

end
; dgp rev 1/30/06 pointer motion handler creates the rubberband effect
; dgp rev 7/9/09 add the new non-rect gating interface
pro pmhd_nrg_move, wid, shell, event
  
  common polyinfo, polystruct

  ye = abs(event.y-polystruct.ysize)
  xe = event.x
  
  x1 = min([polystruct.startx,xe])
  y1 = min([polystruct.starty,ye])
  x2 = abs(xe-polystruct.startx)
  y2 = abs(ye-polystruct.starty)
  
  device,copy=[x1-10,y1-10,x2+20,y2+20,x1-10,y1-10,polystruct.win_copy]

  plots,  [polystruct.startx,xe],[polystruct.starty,ye], /device, color=1

end

; dgp rev 1/30/06 button press handler marking first vertex
; dgp rev 7/9/09 add the new non-rect gating interface
pro bphd_nrg_start, wid, shell, event

  common vectinfo, vect_x, vect_y
  common polyinfo, polystruct

  print, "Initialization handler"
  polystruct.startx = event.x
  polystruct.starty = abs(event.y-polystruct.ysize)
  vect_x = [polystruct.startx]
  vect_y = [polystruct.starty]
  
  status = WwHandler(wid, 'bphd_nrg_start', 'ButtonPressMask',/remove)
  status = WwHandler(wid, 'bphd_nrg_mark', 'ButtonPressMask')
  status = WwHandler(wid, 'pmhd_nrg_move', 'PointerMotionMask')

end

; dgp rev 1/30/06 initialize the drawing area for poly mark
; dgp rev 1/30/06 
; dgp rev 7/9/09 add the new non-rect gating interface
pro _pr_nrg_begin

  common polyinfo, polystruct
  
  draw = fn_dmf_get('draw widget')
  win_orig = fn_dmf_get('window')
  _pr_win_set, win_orig
  xsize = !d.x_size
  ysize = !d.y_size

  status = WwHandler(draw, 'bphd_nrg_start', 'ButtonPressMask')

  device, get_graphics_function = old_mask

  print, "Device mask is ",old_mask

; Create a pixmap copy of the window
  window, /free, xsize=xsize, ysize=ysize, /pixmap
  win_copy = !d.window
  device,copy=[0,0,xsize,ysize,0,0,win_orig]

; Set the displayed window as current
  _pr_win_set, win_orig

  polystruct = {,win_copy:win_copy,win_orig:win_orig,xsize:xsize,ysize:ysize,startx:0,starty:0,wid:draw}

  _pr_gen_set, 'poly', 1

end

; dgp rev 12/23/05 callback to create a poly gate
; on a given display
; dgp rev 7/9/09 add the new non-rect gating interface
pro cb_set_nrg, wid, value

  _pr_mgc_set, 'type', 'poly'

  _pr_mgc_prep

  _pr_nrg_begin

end

; dgp rev 12/26/05 gate form is up and 
; a rectangular gate is ready to be marked
pro cb_set_rect, wid, value

  draw_cells

  _pr_dsp_mirror

  ; check for unclosed gate dialog
  if (fn_mgc_get('flag') eq 1) then begin
    print, "Abort previous gate"
;    _mgs_dis_handler
    main = fn_mgc_get('widget')
    if (fn_wid_exists(main)) then status  = WwSetValue(main, /Close)
  endif else begin
    print, "No open gates"
  endelse
  
  _mgs_add_rect

  _pr_mgc_ready, 1
  
  _pr_reflect_gate
  
end
; dgp rev 1/3/06 display information to create a 
; dgp rev 1/3/06 hist gate and prep to lock it in.
pro cb_set_hist, wid, value

  draw_cells

  _pr_dsp_mirror

  ; check for unclosed gate dialog
  if (fn_mgc_get('flag') eq 1) then begin
    print, "Abort previous gate"
;    _mgs_dis_handler
    main = fn_mgc_get('widget')
    if (fn_wid_exists(main)) then status  = WwSetValue(main, /Close)
  endif else begin
    print, "No open gates"
  endelse
  
  _mgs_add_hist

  _pr_mgc_ready, 1
  
  _pr_reflect_gate

end

; dgp rev 2/24/2010 Non-Rect Gate status
function fn_nrg_status

  common cm_nrg, cv_nrg_active

  if (size(cv_nrg_active,/type) eq 0) then cv_nrg_active = 0

  return, (cv_nrg_active eq 1)

end

; dgp rev 2/24/2010 Non-Rect Gate status toggle
pro _pr_nrg_onoff, status

  common cm_nrg, cv_nrg_active
  
  if status ne 1 then status = 0

  cv_nrg_active = status
  
end

pro flow_gate

  print, "Load all gating routines

end

