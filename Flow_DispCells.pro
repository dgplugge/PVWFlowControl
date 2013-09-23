declare func, _car_active
declare func, _wms_get_main
declare func, _hist
declare func, t_histn
declare func, _mgs_subset_count
declare func, _car_cur_subset
declare func, _cfg_get_work
declare func, _mgs_active
declare func, _gen_unique_name
declare func, fn_wid_exists
declare func, _wid_get_parent
declare func, fn_mgs_get_sorted
declare func, _wid_get_onoff
declare func, savgol
declare func, _car_present
declare func, _olay_get_on
declare func, _mgs_present
declare func, fn_fcr_move_absolute
declare func, fn_fcr_move_relative
declare func, poly_smooth
declare func, _par_cur_names
declare func, _pfl_get_names
declare func, _ger_gates_on
declare func, fn_data_get_cnt
declare func, mymenuitem
declare func, fn_dmf_get
declare func, fn_dmf_get_str
declare func, fn_full_indices
declare func, _ct_get_name
declare func, fn_win_get_tmp
declare func, fn_par_name
declare func, fn_dmf_current
declare func, fn_data_get_cells
declare func, fn_wms_form
declare func, fn_dmf_get_other
declare func, fn_wra_get
declare func, fn_gen_get
declare func, fn_gen_getstr
declare func, fn_dmf_get_header
declare func, _dsp_get_norm
declare func, fn_dmf_key_exists
declare func, fn_ifs_param
declare func, fn_fnt_get
declare func, fn_ifs_get_header
declare func, fn_mgs_get
declare func, fn_wra_get_index
declare func, fn_mgs_get_enable
declare func, fn_dmf_get_xaxis
declare func, fn_dmf_get_yaxis
declare func, fn_data_get_par_cnt
declare func, fn_par_get
declare func, DGPDrawing
declare func, fn_wid_parent_name
declare func, fn_gen_unique
declare func, fn_dsp_active
declare func, fn_ifs_get_keys
declare func, fn_ifs_get
declare func, fn_dmf_restore_prep
declare func, fn_pfl_get_index
declare func, dgpGetValue
declare func, fn_ifs_display
declare func, fn_upa_get_labels
declare func, fn_dmf_find_file
declare func, fn_upa_get
declare func, _FindKV

pro cb_redraw, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  draw_cells
  
end

pro _debug_show_call

_pr_gen_ident ; dgp rev 1/28/10 debugging

  info, calls=calls
  print, calls(1)
  
end

pro _debug_show_calls

_pr_gen_ident ; dgp rev 1/28/10 debugging

  info, calls=calls
  pm, calls

end

; dmf level routines
; dmf add creates a new key based upon the current dmf settings
; the key is the display name (ie "Display 1")
; dgp rev 8/16/05 add a redraw count
pro _pr_dmf_fillin, key

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_dmf_info, cv_dmf_key, cv_dmf_hash

  if (size(cv_dmf_hash,/type) ne 11) then return
  if (size(cv_dmf_key,/type) ne 7) then return
  if (size(key,/type) ne 7) then return

  if (not isaskey(cv_dmf_hash,key)) then begin
    if (isaskey(cv_dmf_hash,cv_dmf_key)) then begin
      tmp = cv_dmf_hash(cv_dmf_key)
    endif else begin
      tmp = cv_dmf_hash('default')
    endelse
    cv_dmf_hash = [cv_dmf_hash,asarr(key,tmp)]
  endif
  
  idx = strsplit(key," ")
  num = long(idx(1))
  cv_dmf_hash(key,'num') = num

  cv_dmf_key = key
 
  _ssl_recalc

end

; dmf level routines
; dmf add creates a new key based upon the current dmf settings
; the key is the display name (ie "Display 1")
pro _pr_dmf_add_branch, key, branch

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_dmf_info, cv_dmf_key, cv_dmf_hash

  if (size(cv_dmf_hash,/type) ne 11) then return

  if (isaskey(cv_dmf_hash,key)) then return
  
  cv_dmf_hash = [cv_dmf_hash,asarr(key,branch)]
 
end

pro _pr_dmf_switch, key

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_dmf_info, cv_dmf_key, cv_dmf_hash

  if (key eq cv_dmf_key) then return
  
  if (isaskey(cv_dmf_hash,key)) then begin
    cv_dmf_key = key
  endif
  
end

pro _dmf_reflect_lvl_min

  common dmf_levels_min, lvl_min_wid, lvl_min_type_wid

  ;_debug_show_call
  
  if (fn_wid_exists(lvl_min_wid)) then begin
    ;print, "Update min widgets"
    val = fn_dmf_get('lvl min type')
    ;print, "Type = ",val
    s = WwSetValue(lvl_min_type_wid(val),1)    

    arr = fn_dmf_get('lvl min val')
    idx = arr(fn_dmf_get('lvl min type'))
    ;print, "Value = ",idx
    s = WwSetValue(lvl_min_wid,strtrim(idx,2))    

  endif

end

pro _dmf_reflect_lvl_mult

  common dmf_levels_mult, lvl_mult_wid, lvl_mult_type_wid

  ;_debug_show_call
  
  if (fn_wid_exists(lvl_mult_wid)) then begin
    ;print, "Update min widgets"
    val = fn_dmf_get('lvl mult type')
    ;print, "Type = ",val
    s = WwSetValue(lvl_mult_type_wid(val),1)    

    arr = fn_dmf_get('lvl mult val')
    idx = arr(fn_dmf_get('lvl mult type'))
    ;print, "Value = ",idx
    s = WwSetValue(lvl_mult_wid,strtrim(idx,2))    

  endif

end

pro _dmf_reg_lvl_min_wid, txt_wid, tog_wids

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common dmf_levels_min, lvl_min_wid, lvl_min_type_wid

  ;_debug_show_call

  lvl_min_type_wid = tog_wids
  lvl_min_wid = txt_wid
  
  _dmf_reflect_lvl_min

end

pro _dmf_reg_lvl_mult_wid, txt_wid, tog_wids

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common dmf_levels_mult, lvl_mult_wid, lvl_mult_type_wid

  ;_debug_show_call

  lvl_mult_type_wid = tog_wids
  lvl_mult_wid = txt_wid
  
  _dmf_reflect_lvl_mult

end

pro cb_dmf_lvl_min, wid, pass

_pr_gen_ident ; dgp rev 1/28/10 debugging

  ;_debug_show_call
  value = float(dgpGetValue(wid))
  if (value le 0.0) then begin
    value = .0001
    s = WwSetValue(wid,strtrim(value,2))
  endif

  arr = fn_dmf_get('lvl min val')
  idx = fn_dmf_get('lvl min type')

  arr(idx) = value

  _pr_dmf_set, 'lvl min val', arr
  draw_cells

end

pro cb_dmf_lvl_mult, wid, pass

_pr_gen_ident ; dgp rev 1/28/10 debugging

  ;_debug_show_call
  value = float(dgpGetValue(wid))
  if (value le 0.0) then begin
    value = .0001
    s = WwSetValue(wid,strtrim(value,2))
  endif

  arr = fn_dmf_get('lvl mult val')
  idx = fn_dmf_get('lvl mult type')

  arr(idx) = value

  _pr_dmf_set, 'lvl mult val', arr
  draw_cells

end

pro cb_dmf_lvl_min_type, wid, pass

_pr_gen_ident ; dgp rev 1/28/10 debugging
  
  if (dgpGetValue(wid) eq 0) then return

  new_set = pass - 1

  _pr_dmf_set, 'lvl min type', new_set
  _dmf_reflect_lvl_min
  draw_cells

end

pro cb_dmf_lvl_mult_type, wid, pass

_pr_gen_ident ; dgp rev 1/28/10 debugging

  ;print, wid, pass, dgpGetValue(wid)
  ;_debug_show_call
  if (dgpGetValue(wid) eq 0) then return

  new_set = pass - 1

  _pr_dmf_set, 'lvl mult type', new_set
  _dmf_reflect_lvl_mult
  draw_cells

end

pro _dmf_set_lvl_arr, arr

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common dmf_levels_arr, lvl_arr

  lvl_arr = arr

end

function _dmf_get_lvl_arr

  common dmf_levels_arr, lvl_arr

  if (size(lvl_arr,/type) eq 0) then lvl_arr = findgen(fn_dmf_get('lvl mult val')) + fn_dmf_get('lvl min val')

  return, lvl_arr

end

; end of dmf level routines

pro  _stat_update_orig, omin, omax, oave, ocnt

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common _stat_dsp, _stat_arr

  if (size(_stat_arr,/type) eq 0) then return
  if (fn_wid_exists(_stat_arr(0))) then begin
    s = WwSetValue(_stat_arr(0),"Min: "+strtrim(omin,2))
    s = WwSetValue(_stat_arr(1),"Max: "+strtrim(omax,2))
    s = WwSetValue(_stat_arr(2),"Ave: "+strtrim(oave,2))
    s = WwSetValue(_stat_arr(7),"Count: "+strtrim(ocnt,2))
  endif

end

pro  _stat_update_man, mmin, mmax, mave

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common _stat_dsp, _stat_arr

  if (size(_stat_arr,/type) eq 0) then return
  if (fn_wid_exists(_stat_arr(3))) then begin
    s = WwSetValue(_stat_arr(3),"Min: "+strtrim(mmin,2))
    s = WwSetValue(_stat_arr(4),"Max: "+strtrim(mmax,2))
    s = WwSetValue(_stat_arr(5),"Ave: "+strtrim(mave,2))
  endif

end

; dgp rev 10/31/05 original and manipulated data correlation
pro _stats_set_orig, data

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common stat_info, raw_dsp_data, manip_dsp_data, _dsp_correl
  
  raw_dsp_data = data
  omin = min(data)
  omax = max(data)
  oave = avg(data)
  arr = where(data,cnt)

  ocnt = sum(data)

  _stat_update_orig, omin, omax, oave , ocnt

end

; dgp rev 10/31/05 original and manipulated data correlation
pro _stats_set_manip, data

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common stat_info, raw_dsp_data, manip_dsp_data, _dsp_correl
  
  manip_dsp_data = data

  mmin = min(data)
  mmax = max(data)
  mave = avg(data)
  arr = where(data,cnt)

  _stat_update_man, mmin, mmax, mave

end

; dgp rev 10/31/05 original and manipulated data correlation
function _stats_get_data, data

  common stat_info, raw_dsp_data, manip_dsp_data, _dsp_correl
  
  if (size(raw_dsp_data,/type) eq 0) then return, 0
  
  data = raw_dsp_data
  return, 1
  
end
; dgp rev 10/31/05 original and manipulated data correlation
pro _stats_dsp_correlation

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common stat_info, raw_dsp_data, manip_dsp_data, _dsp_correl
  common _stat_dsp, _stat_arr

  if (size(manip_dsp_data,/ndim) ne 0 and size(raw_dsp_data,/ndim) ne 0) then begin
    if (same(manip_dsp_data,raw_dsp_data,/noval)) then begin
      results = correlate(manip_dsp_data,raw_dsp_data)
      ;print, "Correlation is ",results
      if (size(_stat_arr,/type) eq 0) then return
      if (fn_wid_exists(_stat_arr(6))) then begin
        s = WwSetValue(_stat_arr(6),'0-1: '+string(results,format="(F5.2)"))
      endif      
    endif
  endif

end

pro set_clcnt_wid, UC_T

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cell_widgets, cl_count, rg_count, total_count
  
  cl_count = UC_T

end

pro set_rgcnt_wid, UC_T

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cell_widgets, cl_count, rg_count, total_count
  
  rg_count = UC_T

end

function _car_indices_count

  _car_indices = fn_ifs_get('car indices')

  if (size(_car_indices,/type) eq 0) then begin
    return, 0
  endif else begin
    return, n_elements(_car_indices)
  endelse
end

pro update_cl_count

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cell_widgets, cl_count, rg_count, total_count

  if (dgpGetValue(cl_count,/exists)) then begin
    if (not dgpGetValue(cl_count,/destroyed)) then begin
       comm = "status = WwSetValue(cl_count,'"+string(_car_indices_count())+"')"
       ;print, comm
       results = execute(comm)
    endif
  endif
  if (dgpGetValue(rg_count,/exists)) then begin
    if (not dgpGetValue(rg_count,/destroyed)) then begin
       comm = "status = WwSetValue(rg_count,'"+string(fn_dmf_get('mgs count'))+"')"
       ;print, comm
       results = execute(comm)
    endif
  endif
  
end

pro _disp_set_reso, val

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common plot_info, peak, reso

  reso = fix(val)
  
  if (reso lt 64) then reso = 64

end


function _get_bincnt

  return, long(64)

end

pro _set_levels, wid								

_pr_gen_ident ; dgp rev 1/28/10 debugging


  common levels, lvl_mn, lvl_t_add, lvl_add, lvl_mult, lvl_t_mult, lvl_cnt, lvl_t_cnt

  lvl_cnt = wid

  ;print, "Set level widget: ",lvl_cnt

end

pro cb_dmf_min_type, wid, pas_val

_pr_gen_ident ; dgp rev 1/28/10 debugging

  state = dgpGetValue(wid)
    
  if (state ne 0) then begin
    _dmf_set_min_type, pas_val
    ;print, "Min type changed to ",pas_val
  
    draw_cells
  endif

end

pro cb_dmf_mult_type, wid, pas_val

_pr_gen_ident ; dgp rev 1/28/10 debugging

  state = dgpGetValue(wid)
    
  if (state ne 0) then begin
    _dmf_set_mult_type, pas_val
    ;print, "Mult type changed to ",pas_val
  
    draw_cells
  endif

end

pro _dmf_set_lvlmin, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common _dmf_levels, lvlmin, lvlmult
  
  lvlmin = float(value)

end

function _dmf_get_lvlmin

  common _dmf_levels, lvlmin, lvlmult
  
  if (size(lvlmin,/type) eq 0) then lvlmin = float(4)
  return, lvlmin

end

pro _dmf_set_lvlmult, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common _dmf_levels, lvlmin, lvlmult
  
  if (value lt 1) then value = 1
  lvlmult = float(value)

end

function _dmf_get_lvlmult

  common _dmf_levels, lvlmin, lvlmult
  
  if (size(lvlmult,/type) eq 0) then lvlmult = float(1.8)
  return, lvlmult

end

function _cdr_get_pars
  
  cdr_pars = fn_dmf_get('pars')  
  return, cdr_pars
 
end

function fn_dmf_calc_res
  
  if (long(product(_cdr_get_pars())) eq 0) then begin
    reso = fn_dmf_get('hist res')
    if (reso eq 0) then reso = 10
  endif else begin
    reso = fn_dmf_get('dual res')
    if (reso eq 0) then reso = 7
  endelse
  val = 2^reso

  return, val 
end
; dgp rev 11/7/05 calculate the ymax from the current 
; data percent
pro _pr_dmf_calc_ymax, ymax

_pr_gen_ident ; dgp rev 1/28/10 debugging

  ymax_perc = float(fn_dmf_get('ymax perc'))
  if (ymax_perc eq 0) then ymax_perc = 1
  all = fn_dmf_get('data count')
  ymax = all * float(float(ymax_perc)/100)
  _pr_dmf_set, 'ymax', ymax

end
; dgp rev 1/5/06 percentage of total cells as y axis limit
pro cb_dmf_ymax_perc, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging
  
  _pr_dmf_set,'ymax perc', dgpGetValue(wid)
  
  draw_cells

end

pro CB_dmf_Smo_wdth, wid, val

_pr_gen_ident ; dgp rev 1/28/10 debugging

  value = dgpGetValue(wid)
  print, "Changing smooth width ",value
  _pr_dmf_set, 'smo width', value

;  draw_cells

end

pro CB_dmf_Smo_algor, wid, val

_pr_gen_ident ; dgp rev 1/28/10 debugging

  internal = dgpGetValue(wid)

  if (internal eq 1) then begin
    _pr_dmf_set, 'smo algor', val

    draw_cells
  endif

end

pro CB_dmf_Smo_rep, wid, val

_pr_gen_ident ; dgp rev 1/28/10 debugging

  value = dgpGetValue(wid)
  print, "Changing smooth repeat ",value
  _pr_dmf_set, 'smo rep', value

;  draw_cells

end

pro CB_dmf_Smo_on, wid, val

_pr_gen_ident ; dgp rev 1/28/10 debugging

  value = dgpGetValue(wid)
  print, "Enable Smooth ",value
  _pr_dmf_set, 'smo on', value

  ;draw_cells

end

pro CB_dmf_res, wid, pas_val

_pr_gen_ident ; dgp rev 1/28/10 debugging

  wid_val = dgpGetValue(wid)
  
  print, "Wid value    ",wid_val
  print, "Passed value ",pas_val
  if (long(product(_cdr_get_pars())) eq 0) then begin
    _pr_dmf_set, 'hist res', pas_val+2
  endif else begin
    _pr_dmf_set, 'dual res', pas_val+2
  endelse
  draw_cells

end

pro CB_dmf_quad_degree, wid, val

_pr_gen_ident ; dgp rev 1/28/10 debugging

  value = dgpGetValue(wid)
  print, "Changing Quad degree to ",value

  _pr_dmf_set, 'quad degree', value

;  draw_cells

end

pro CB_dmf_quad_on, wid, val

_pr_gen_ident ; dgp rev 1/28/10 debugging

  value = dgpGetValue(wid)
  ;print, "Change quad ",value
  _pr_dmf_set, 'quad on', value

  ;draw_cells

end

pro CB_dmf_quad_width, wid, val

_pr_gen_ident ; dgp rev 1/28/10 debugging

  value = dgpGetValue(wid)
  print, "Changing Quad width to ",value
  _pr_dmf_set, 'quad width', value

;  draw_cells

end

pro CB_dmf_quad_rep, wid, val

_pr_gen_ident ; dgp rev 1/28/10 debugging

  value = dgpGetValue(wid)
  print, "Changing Quad rep to ",value
  _pr_dmf_set, 'quad rep', value

;  draw_cells

end

pro _dmf_set_levels, arr

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common _dmf_lvl_info, _dmf_level_arr, _dmf_lvl_force, _dmf_level_wid

  _dmf_level_arr = arr

end

function _dmf_mod_lvl, i, val

  common _dmf_lvl_info, _dmf_level_arr, _dmf_lvl_force, _dmf_level_wid

  if (size(_dmf_level_arr,/type) ne 0) then begin
    siz = n_elements(_dmf_level_arr) - 1
    if (i le siz and i ge 0) then begin
      _dmf_level_arr(i) = val
    endif
  endif
  return, 0

end

function _dmf_get_levels, arr

  common _dmf_lvl_info, _dmf_level_arr, _dmf_lvl_force, _dmf_level_wid

  if (size(_dmf_level_arr,/type) eq 0) then begin
    return, 0
  endif else begin
    arr = _dmf_level_arr
    return, 1
  endelse

end

; use % of total to determine first level 
pro _calc_lvl_root_pot, limit

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common contour_info, orig_dat, mod_dat
  common dmf_levels_arr, lvl_arr

  perc_min   = float(limit)/100.0
  tally_val  = sum(mod_dat)

  val = perc_min * tally_val

  print, "Percent ",perc_min," of total, ",tally_val," is ",val

  lvl_arr = make_array(1,/float,value=val)
  
end

; use absolute value to determine first level 
pro _calc_lvl_root_abs, limit

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common contour_info, orig_dat, mod_dat
  common dmf_levels_arr, lvl_arr

  lvl_arr = make_array(1,/float,value=limit)

end

; use probability to determine first level 
pro _calc_lvl_root_pro, limit

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common contour_info, orig_dat, mod_dat
  common dmf_levels_arr, lvl_arr

  ; get the min and mult
  perc_min = float(limit)/100.0


  ; peak of data set    
  peak   = max(mod_dat)
  peak_idx = long(peak+1)
  hi_idx = peak_idx
  lo_idx = long(min(mod_dat))
  fork_hi_idx   = long((hi_idx*perc_min)+1)
  fork_lo_idx   = long(hi_idx*perc_min)

  ; create array of values
  lvl_cnt = make_array(hi_idx+2,/float,value=0)

  ; tally of events
  tally_val     = sum(mod_dat)
  hi_val = tally_val
  lo_val = 0
  target_val   = perc_min*tally_val
  print, "Target value ",target_val

  ; determine floor level and mid level counts

    repeat begin
       ; calculate values for new fork
       if (lvl_cnt(fork_lo_idx) eq 0) then begin
         lo_list = where(mod_dat le fork_lo_idx)
         fork_lo_val = float(sum(mod_dat(lo_list)))
         lvl_cnt(fork_lo_idx) = fork_lo_val
       endif else begin
         fork_lo_val = lvl_cnt(fork_lo_idx)
       endelse
       if (lvl_cnt(fork_hi_idx) eq 0) then begin
         hi_list       = where(mod_dat le fork_hi_idx)
         fork_hi_val = float(sum(mod_dat(hi_list)))
         lvl_cnt(fork_hi_idx) = fork_hi_val
       endif else begin
         fork_hi_val = lvl_cnt(fork_hi_idx)
       endelse

       if (target_val le fork_hi_val and target_val ge fork_lo_val) then begin
         print, "Caught it!"
         ; final guess
         hi_idx = fork_hi_idx
         lo_idx = fork_lo_idx
         hi_val = fork_hi_val
         lo_val = fork_lo_val
         ratio = float(lo_idx) + float((float(hi_idx-lo_idx)*((target_val - lo_val)/(hi_val - lo_val))))
       endif else if (target_val lt fork_lo_val) then begin
         print, "Too Hi"
         hi_idx = fork_lo_idx
         hi_val = fork_lo_val
         print ,"Guess using:"
         print, lo_idx, hi_idx, lo_val, hi_val, target_val
         ; new guess
         ratio = float(lo_idx) + float((float(hi_idx-lo_idx)*((target_val - lo_val)/(hi_val - lo_val))))
         fork_hi_idx = long(ratio+1)
         fork_lo_idx = long(ratio)
       endif else begin
         print, "Too Low"
         lo_idx = fork_hi_idx
         lo_val = fork_hi_val
         print ,"Guess using:"
         print, lo_idx, hi_idx, lo_val, hi_val, target_val
         ; new guess
         ratio = float(lo_idx) + float((float(hi_idx-lo_idx)*((target_val - lo_val)/(hi_val - lo_val))))
         fork_hi_idx = long(ratio+1)
         fork_lo_idx = long(ratio)
       endelse
       print, "New Guess = ",fork_lo_idx, fork_hi_idx 
     
    endrep until (hi_idx-lo_idx le 1)
  
  lvl_arr = make_array(1,/float,value=ratio)

end

function _calc_lvl_get_max

  return, 50

end

; use % of total to fill the remaining levels 
pro _calc_lvl_arr_pot, mult

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common contour_info, orig_dat, mod_dat
  common dmf_levels_arr, lvl_arr

  perc_mult   = float(mult)/100.0
  tally_val  = sum(mod_dat)

  val = perc_mult * tally_val
  
  cnt = 100 / mult

  clip = _calc_lvl_get_max()

  if (cnt gt clip) then cnt = clip

  limit = lvl_arr(0)

  tmp_arr = (findgen(cnt)+1)*val + limit

  lvl_arr = [lvl_arr,tmp_arr]

end

; use absolute value to fill the remaining levels 
pro _calc_lvl_arr_abs, mult

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common contour_info, orig_dat, mod_dat
  common dmf_levels_arr, lvl_arr  
     
  peak = max(mod_dat)

  incr = lvl_arr(0) * mult  
  
  clip = _calc_lvl_get_max()
  
  while ((incr lt peak) and (n_elements(lvl_arr) lt clip)) do begin  
    lvl_arr = [lvl_arr,incr]
    incr = lvl_arr(n_elements(lvl_arr)-1) * mult  
  endwhile

  lvl_arr = unique(lvl_arr)

end

function fn_lvl_get_volume, index

  common cm_lvl_calc, cv_lvl_cnt, cv_lvl_max
  common contour_info, orig_dat, mod_dat

  if (index gt cv_lvl_max) then return, 0 

  if (cv_lvl_cnt(index) eq -1) then begin
    vol_list = where(mod_dat le index)
    cv_lvl_cnt(index) = float(sum(mod_dat(vol_list)))
  endif
  ;print, "Calculate index ",index," is ",cv_lvl_cnt(index)

  return, cv_lvl_cnt(index)

end

function fn_lvl_init

  common contour_info, orig_dat, mod_dat
  common cm_lvl_calc, cv_lvl_cnt, cv_lvl_max
  
  ; get the min and mult

  ; peak of data set    
  peak   = max(mod_dat)
    ; create array of values
  cv_lvl_cnt = make_array(peak+2,/float,value=-1)
  cv_lvl_max = long(peak + 1)

  return, cv_lvl_max

end

function fn_lvl_get_perc
  
  ; get the min and mult

  Mult_val = fn_dmf_get('lvl mult val')
  
  mult_type = fn_dmf_get('lvl mult type')

  mult = mult_val(mult_type)

  perc_mult = float(mult)/100.0

  return, perc_mult

end

function fn_lvl_tally

  common contour_info, orig_dat, mod_dat
  return, sum(mod_dat)

end

pro _pr_lvl_scale, factor

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common dmf_levels_arr, lvl_arr  
  common contour_info, orig_dat, mod_dat

  lvl_arr = lvl_arr * factor  
  mod_dat = mod_dat * factor

end
; use probability to fill the remaining levels 
pro _calc_lvl_arr_pro

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common dmf_levels_arr, lvl_arr

  print, "Continue from first level of",lvl_arr(0)

  peak_idx = fn_lvl_init()
  hi_idx = peak_idx

  lo_idx = long(lvl_arr(0))

  fork_hi_idx = long(lvl_arr(0)+1)
  fork_lo_idx = long(lvl_arr(0))

  fork_lo_val = fn_lvl_get_volume(fork_lo_idx)
  fork_hi_val = fn_lvl_get_volume(fork_hi_idx)
  
  ; calculate first target value
  target_val = fork_lo_val + (fork_hi_val-fork_lo_val)*((lvl_arr(0)-fork_lo_idx)/(fork_hi_idx-fork_lo_idx))
  ;print, "First target value is ",target_val

  ; tally of events
  tally_val = fn_lvl_tally()
  hi_val = tally_val
  lo_val = 0
 
  perc_mult = fn_lvl_get_perc()

  ;print, "Percent ",perc_mult
  ;print, "Tally   ",tally_val

  target_chunk = perc_mult*tally_val
  if (target_chunk lt 1) then target_chunk = 1
  target_val = target_val + target_chunk  
  ;print, "Target value ",target_val

  ; determine floor level and mid level counts
  tmp_arr = make_array(1,/float)

  max_cnt = 150

  while ((target_val lt tally_val) and n_elements(tmp_arr) lt max_cnt) do begin
         
    got_it = 0

    repeat begin
       ; calculate values for new fork
      fork_lo_val = fn_lvl_get_volume(fork_lo_idx)
      fork_hi_val = fn_lvl_get_volume(fork_hi_idx)

      if (target_val le fork_hi_val and target_val ge fork_lo_val) then begin
         ;print, "Caught it!"
         ; final guess
         got_it = 1
         hi_idx = fork_hi_idx
         lo_idx = fork_lo_idx
         hi_val = fork_hi_val
         lo_val = fork_lo_val
         ratio = float(lo_idx) + float((float(hi_idx-lo_idx)*((target_val - lo_val)/(hi_val - lo_val))))
       endif else if (target_val lt fork_lo_val) then begin
         ;print, "Too Hi"
         hi_idx = fork_lo_idx
         hi_val = fork_lo_val
         ; new guess
         ratio = float(lo_idx) + float((float(hi_idx-lo_idx)*((target_val - lo_val)/(hi_val - lo_val))))
         ;print, lo_idx, hi_idx, lo_val, hi_val, target_val, ratio
         fork_hi_idx = long(ratio+1)
         fork_lo_idx = long(ratio)
       endif else begin
         ;print, "Too Low"
         lo_idx = fork_hi_idx
         lo_val = fork_hi_val
;         print, lo_idx, hi_idx, lo_val, hi_val, target_val
         ; new guess
         ratio = float(lo_idx) + float((float(hi_idx-lo_idx)*((target_val - lo_val)/(hi_val - lo_val))))
         fork_hi_idx = long(ratio+1)
         fork_lo_idx = long(ratio)
       endelse
       ;print, fork_lo_idx, fork_hi_idx
     
    endrep until (hi_idx-lo_idx le 1 and got_it)
  
    ; Add value to level array

    tmp_arr = [tmp_arr,ratio]
    target_val = target_val + target_chunk
    ;print, "Target value ",target_val
    ;print, "Level count is ",n_elements(tmp_arr)

    ; new best guess
    hi_idx = peak_idx
    hi_val = tally_val
    lo_idx = fork_lo_idx
    lo_val = fork_lo_val

    ratio = float(lo_idx) + float((float(hi_idx-lo_idx)*((target_val - lo_val)/(hi_val - lo_val))))
    ;print, lo_idx, hi_idx, lo_val, hi_val, target_val, ratio
    fork_hi_idx = long(ratio+1)
    fork_lo_idx = long(ratio)

  endwhile
  
  lvl_arr = [lvl_arr,tmp_arr(1:*)]
  diff = tmp_arr(2) - tmp_arr(1)
 
  if (diff lt 1) then begin
    factor = float(1.0/diff)
    print, "Factor is ",factor
    _pr_lvl_scale, factor
  endif  

end

; calculate levels based upon a minimum percentage
; for the lowest level and multiplier for each 
; higher level
pro _calc_levels

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common contour_info, orig_dat, mod_dat

  ; get the data range
  hi = max(mod_dat)
  lo = min(mod_dat)
  all = sum(mod_dat)  
 
  ; get the variables
  min_val = fn_dmf_get('lvl min val')
  
  min_type = fn_dmf_get('lvl min type')

  limit = min_val(min_type)

  Mult_val = fn_dmf_get('lvl mult val')
  
  mult_type = fn_dmf_get('lvl mult type')

  mult = mult_val(mult_type)

  print, limit, mult, min_type, mult_type

  if (min_type eq 0) then begin
    _calc_lvl_root_pot, limit
  endif else if (min_type eq 1) then begin
    _calc_lvl_root_abs, limit
  endif else begin
    _calc_lvl_root_pro, mult
  endelse

  if (mult_type eq 0) then begin
    _calc_lvl_arr_pot, mult
  endif else if (mult_type eq 1) then begin
    _calc_lvl_arr_abs, mult
  endif else begin
    _calc_lvl_arr_pro
  endelse

end

pro calc_levels2

_pr_gen_ident ; dgp rev 1/28/10 debugging

  print, "Calc levels with limit of ",limit," and multiple ", mult

  lo_perc = (float(limit)/100)*all
  print, "Minimum count is ",lo_perc
  mn    = lo
  mx    = hi
  
  val   = float(mn)
  sm   = val
  last = val
  
; use percent of total
    idx = 0

    tmp = where(mod_dat ge 1,start)

    cntarr = make_array(1,/float,value=start)
    sumarr = make_array(1,/float,value=start)

; fill reference table arrays with values
    last = start

    for val=2,10 do begin
      tmp = where(mod_dat ge val,sm)
      cntarr = [cntarr,sm]
      run    = last + sm
      sumarr = [sumarr,run]
      last   = run
    endfor

    for val=11,mx do begin
      tmp = where(mod_dat ge val,sm)
      cntarr = [cntarr,sm]
      run    = last + sm
      sumarr = [sumarr,run]
      last   = run
    endfor
    tip = n_elements(sumarr)-1
    all = sum(mod_dat)
    sumtot = sum(cntarr)

    chunk = lo_perc
    
    tally = 0
    goal  = chunk
    index = -1
; loop thru chunks
    while (index lt tip and tally lt goal) do begin
      index = index + 1
      tally = sumarr(index)
    endwhile
    index = index + 1
    lvls = make_array(1,/long,value=index)
    index = index * mult
    while (index lt tip) do begin
      lvls = [lvls,index]
      index = index * mult
    endwhile

  _dmf_set_levels, lvls

end
; dgp rev 1/9/06 is the current display dual or single
function fn_dmf_single

  return, (product(fn_dmf_get('pars')) eq 0)

end

pro _pr_dsp_debug

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_dsp_info, cv_dsp_hash 

  stop ;debug
  
end

pro _pr_dsp_set, label, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_dsp_info, cv_dsp_hash 

  if (size(cv_dsp_hash,/type) ne 11) then begin
    cv_dsp_hash = asarr(label,value)
  endif else begin
    cv_dsp_hash = [cv_dsp_hash,asarr(label,value)]
  endelse

end

function fn_dsp_get_str, label

  common cm_dsp_info, cv_dsp_hash 

  if (size(cv_dsp_hash,/type) eq 11) then begin
    if (isaskey(cv_dsp_hash,label)) then begin
      return, string(cv_dsp_hash(label))
    endif
  endif
  return, ""

end

function fn_dsp_get_num, label

  common cm_dsp_info, cv_dsp_hash 

  if (size(cv_dsp_hash,/type) eq 11) then begin
    if (isaskey(cv_dsp_hash,label)) then begin
      if (size(cv_dsp_hash(label),/type) eq 4) then begin
        return, float(cv_dsp_hash(label))
      endif
      return, long(cv_dsp_hash(label))
    endif
  endif
  return, 0

end

pro _pr_dsp_crlf

_pr_gen_ident ; dgp rev 1/28/10 debugging

  _pr_dsp_set, 'col idx', 0
  _pr_dsp_set, 'align', 0

  _pr_dsp_set, 'cur y', (fn_dsp_get_num('cur y') - fn_dsp_get_num('spacing')) 
  
end

; dgp rev 10/12/05 this is a wrapper around the XYOUTS command
; The exclamation can cause problems within a string unless doubled
; Use this for raw unformatted strings
; dgp rev 10/18/05 add font ratio
pro _pr_xyo_rawdev, x, y, str, align=aval, color=cval

_pr_gen_ident ; dgp rev 1/28/10 debugging

  ..locals 5 0
  ratio = fn_fnt_get('xyo ratio')
  if (ratio eq 0) then ratio = 1
  sstr = ",size=ratio"

  str = strsubst(str,'!','!!',/glob)
  astr = ""
  if (PARAM_PRESENT(aval)) then astr = ",align=aval"
  cstr = ""
  if (PARAM_PRESENT(cval)) then cstr = ",color=cval"
  comm = "xyouts, x, y, str, /device " + astr + cstr + sstr

  res = execute(comm)

end


pro _dsp_subline, text

_pr_gen_ident ; dgp rev 1/28/10 debugging

  Common colour, Wht, Blk
  common cm_dsp_annotate, _dsp_count, _dsp_sect, _dsp_row, _dsp_incr

  placement = _dsp_count mod 3
  if (placement eq 1) then placement = 2
  just      = 0.5 * float(placement)

  y_coor    = _dsp_row - ((_dsp_count / 3) * (_dsp_incr*1.2))

  x_coor    = _dsp_sect(placement)

  _dsp_count = _dsp_count + 1
  if (placement eq 0) then _dsp_count = _dsp_count + 1 
  
  text = strtrim(text,2)
  
  _pr_xyo_rawdev, x_coor, y_coor, text, color=Blk, align=just

end

pro _pr_dsp_set_cols, col_cnt

_pr_gen_ident ; dgp rev 1/28/10 debugging

  _pr_dsp_set, 'col idx', 0
  _pr_dsp_set, 'col cnt', col_cnt

  col_right = fn_dsp_get_num('ano right')
  col_left  = fn_dsp_get_num('ano left')
  if (col_cnt gt 1) then begin
    chunk = (col_right-col_left)/(col_cnt-1)
  endif else begin
    chunk = (col_right-col_left)
  endelse
  col_arr = indgen(col_cnt)*chunk + col_left
  _pr_dsp_set, 'col arr', col_arr
  _pr_dsp_set, 'col width', chunk
  _pr_dsp_set, 'align', 0

end

pro _pr_dsp_type_col, text

_pr_gen_ident ; dgp rev 1/28/10 debugging

  row_pos = fn_dsp_get_num('cur y')
  color_idx = fn_dsp_get_num('color')
  color_rgb = WoColorConvert(color_idx,/IndextoColor)
  
  col_cnt = fn_dsp_get_num('col cnt')
  col_idx = fn_dsp_get_num('col idx')
  col_arr = fn_dsp_get_num('col arr')
  align   = fn_dsp_get_num('align')
    
  _pr_xyo_rawdev, col_arr(col_idx), row_pos, text, color=color_rgb, align=align
  align = .5
  col_idx = (col_idx + 1) mod col_cnt
  if (col_idx eq col_cnt-1) then align = 1
  _pr_dsp_set,'align',align
  _pr_dsp_set,'col idx', col_idx
  if (col_idx eq 0) then _pr_dsp_crlf    

end

; dgp rev 10/26/05 get a keyword value from the current
; instance of the IFS structure
function fn_ifs_status

  common cm_ifs, cv_ifs_struct
  
  label = 'status'
  
  if (size(cv_ifs_struct,/type) eq 11) then begin
    if (isaskey(cv_ifs_struct,fn_dmf_get('ifs key'))) then begin
      if (isaskey(cv_ifs_struct(fn_dmf_get('ifs key')),label)) then begin
        return, cv_ifs_struct(fn_dmf_get('ifs key'),label)
      endif
    endif
  endif
  return, 0

end

; dgp rev 10/18/05 add font ratio to the heading
; dgp rev 10/26/05 must check for invalid FCS file
pro _pr_dsp_heading

_pr_gen_ident ; dgp rev 1/28/10 debugging

  _pr_fnt_set, 'xyo ratio', fn_fnt_get('title ratio') 

  height = fn_dsp_get_num('y size')
  chunk = (height-fn_dsp_get_num('top'))/3

  _pr_dsp_set, 'cur y', (height-chunk)
  _pr_dsp_set_cols, 2

  _pr_dsp_type_col, string("Proj: ",fn_gen_get('project'))
  _pr_dsp_type_col, string("Sess: ",fn_gen_get('session'))

  _pr_dsp_type_col, string("File: ",fn_dmf_get('ifs key'))
  if (fn_ifs_status()) then  _pr_dsp_type_col, string("Sample: ",fn_dmf_get_header("$SMNO"))

end

function fn_gen_setname

  common cm_gen, cv_gen_hash

  if (isaskey(cv_gen_hash,'setnames')) then begin
    if (isaskey(cv_gen_hash('setnames'),fn_dmf_get('ifs key'))) then begin
      return, cv_gen_hash('setnames',fn_dmf_get('ifs key'))
    endif
  endif
  return, ""

end

; dgp rev 10/18/05 add font sizing support
; dgp rev 12/10/2010 subtitles displayed from key EIBxK, EIBxV
pro _pr_dsp_table

  _pr_gen_ident ; dgp rev 1/28/10 debugging
  _pr_fnt_set, 'xyo ratio', fn_fnt_get('table ratio') 
  _pr_dsp_set_cols, 1
  _pr_dsp_set, 'cur y', fn_dsp_get_num('ano bot')
  _pr_dsp_type_col, string(fn_gen_setname())

  subtitles = fn_upa_get('sublabels')
  if size(subtitles,/type) eq 11 then begin
  
    keys = askeys(subtitles)
    count = n_elements(keys)
    if (count ne 0) then begin
      _pr_dsp_set_cols, 2
      for i=0,count-1 do begin
        text = keys(i) + " = " + subtitles(keys(i))
        _pr_dsp_type_col, text
      endfor
    endif
  endif
  
end
; dgp rev 11/9/05 display the applied gates
pro _pr_dsp_applied

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_mgs_info, cv_mgs_struct
  Common colour, Wht, Blk
  common cm_dsp_annotate, _dsp_count, _dsp_sect, _dsp_row, _dsp_incr

  _pr_dmf_set_qp, 'filename', fn_dmf_get('ifs key') 

  if (_ger_gates_on(sorted)) then begin
    _pr_dsp_crlf
    _pr_dsp_set_cols, 3
    _pr_fnt_set, 'xyo ratio', fn_fnt_get('applied ratio') 
    for i=0, n_elements(sorted)-1 do begin
      _pr_dmf_set_qp, 'applied', cv_mgs_struct(sorted(i)).name
      if (fn_dmf_get('1only') eq sorted(i)) then begin
        _pr_dsp_set, 'color', fn_dmf_get('color')
        _pr_dsp_type_col, cv_mgs_struct(sorted(i)).name
        _pr_dsp_set, 'color', 0
        _pr_dmf_set_qp, '1only', cv_mgs_struct(sorted(i)).name
      endif else begin
        _pr_dsp_type_col, cv_mgs_struct(sorted(i)).name
      endelse
    endfor
  endif

end

pro _manip_data, data

_pr_gen_ident ; dgp rev 1/28/10 debugging

  data_dim = size(data,/ndim)

  if (fn_dmf_get('quad on') eq 1) then begin
    ; pad the data
    neighbors = fn_dmf_get('quad width')
    pad_data = data
    for i=1,neighbors do begin
      pad_data = padit(pad_data)
    endfor
    width = neighbors*2+1
    degree = fn_dmf_get('quad degree')
    rep    = fn_dmf_get('quad rep')
    print, "Neighbor:    ",neighbors
    print, "Degree:   ",degree
    if (width gt degree) then begin
        kernel = poly_smooth(neighbors, degree, data_dim)
        for i=1,rep do begin
          pad_data = convol(pad_data,kernel)
        endfor
        data_size = n_elements(data(*,0))
        if (data_dim eq 2) then begin
          data = pad_data(neighbors:neighbors+data_size-1,neighbors:neighbors+data_size-1)
        endif else begin
          data = pad_data(neighbors:neighbors+data_size-1)
        endelse
    endif
  endif

  if (fn_dmf_get('smo on') eq 1) then begin
    neighbors = fn_dmf_get('smo width')
    neighbors = (neighbors*2)+1
    rep    = long(fn_dmf_get('smo rep'))
    if (neighbors ne 1) then begin
      pad_data = data
      for i=1,neighbors do begin
        pad_data = padit(pad_data)
      endfor
      smocom = "pad_data = leefilt(pad_data,neighbors,edge='zero')"

      for i=1,rep do begin
        results = execute(smocom)
      endfor
      data_size = n_elements(data(*,0))
      if (data_dim eq 2) then begin
        data = pad_data(neighbors:neighbors+data_size-1,neighbors:neighbors+data_size-1)
      endif else begin
        data = pad_data(neighbors:neighbors+data_size-1)
      endelse      
    endif
  endif

end

pro show_data, data

_pr_gen_ident ; dgp rev 1/28/10 debugging

   ;info, data
   idx = where(data,zr_cnt)
   ;print, "Number of zeros is ",zr_cnt
   ;print, "Sum of data is ",sum(sum(data,0),0)     
   ;print, "Hi: ",max(data)
   ;print, "Lo: ",min(data)
   ;print, "Av: ",avg(data)

end

pro _ssl_merge_both

_pr_gen_ident ; dgp rev 1/28/10 debugging

   _ssl_indices = fn_dmf_get('ssl indices')
   _mgs_indices = fn_dmf_get('mgs indices')
   _car_indices = fn_dmf_get('car indices')
   
   _ssl_indices = INDEX_AND(_mgs_indices, _car_indices)
   _pr_dmf_set, 'ssl indices', _ssl_indices
   if (size(_ssl_indices,/ndim) eq 0) then begin
     print, "Merge no data"
     _pr_dmf_set, 'data flag', 0
     _pr_dmf_set, 'data count', 0
   endif else begin
     _pr_dmf_set, 'data flag', 1
     _pr_dmf_set, 'data count', n_elements(_ssl_indices)
   endelse   

end

pro _ssl_merge_mgs

_pr_gen_ident ; dgp rev 1/28/10 debugging

   _ssl_indices = fn_dmf_get('ssl indices')
   _mgs_indices = fn_dmf_get('mgs indices')
   
   _ssl_indices = _mgs_indices
   _pr_dmf_set, 'ssl indices', _ssl_indices
   print, "Merge mgs ",n_elements(_ssl_indices)
   if (size(_ssl_indices,/ndim) eq 0) then begin
     print, "Merge no data"
     _pr_dmf_set, 'data flag', 0
     _pr_dmf_set, 'data count', 0
   endif else begin
     _pr_dmf_set, 'data flag', 1
     _pr_dmf_set, 'data count', n_elements(_ssl_indices)
   endelse   

end

pro _ssl_merge_car

_pr_gen_ident ; dgp rev 1/28/10 debugging

  _ssl_indices = fn_dmf_get('ssl indices')
  _car_indices = fn_dmf_get('car indices')
   
   _ssl_indices = _car_indices
   _pr_dmf_set, 'ssl indices', _ssl_indices
;   print, "Merge car ",n_elements(_ssl_indices)
   if (size(_ssl_indices,/ndim) eq 0) then begin
;     print, "Merge no data"
     _pr_dmf_set, 'data flag', 0
     _pr_dmf_set, 'data count', 0
   endif else begin
     _pr_dmf_set, 'data flag', 1
     _pr_dmf_set, 'data count', n_elements(_ssl_indices)
   endelse   

end

pro _ssl_merge_all

_pr_gen_ident ; dgp rev 1/28/10 debugging

  arr = fn_full_indices()
  _pr_dmf_set, 'ssl indices', arr

;   print, "Merge all ",n_elements(arr)
  if (size(arr,/ndim) eq 0) then begin
;     print, "Merge no data"
     _pr_dmf_set, 'data flag', 0
     _pr_dmf_set, 'data count', 0
  endif else begin
     _pr_dmf_set, 'data flag', 1
     _pr_dmf_set, 'data count', n_elements(arr)
  endelse   

end

; dgp rev 10/19/05 recalculate the display indices
; due to changes that affect the indices
pro _ssl_recalc

_pr_gen_ident ; dgp rev 1/28/10 debugging

  _pr_gen_report1, "BeginRecalc"
  if (_mgs_active()) then begin
    if (_car_active()) then begin
      print, "Gates and clusters"
      _mgs_combo
      _car_combo
      _ssl_merge_both
    endif else begin
      print, "Gates only"
      _mgs_combo
      _ssl_merge_mgs
    endelse
  endif else begin
    if (_car_active()) then begin
      print, "Clusters only"
      _car_combo
      _ssl_merge_car
    endif else begin
      print, "All Data"
      _ssl_merge_all
    endelse
  endelse
  _pr_gen_report1, "EndRecalc"

end

; dgp rev 10/26/05 output text for all data gated out
pro _pr_dsp_no_par
    
  Common colour, Wht, Blk

  XYOUTS, .3, .6, "Invalid Parameter", /normal, color=Blk, charsize=1.1

end

; dgp rev 10/26/05 output text for all data gated out
pro _pr_dsp_no_data

_pr_gen_ident ; dgp rev 1/28/10 debugging
    
  Common colour, Wht, Blk

  XYOUTS, .3, .6, "All data gated out", /normal, color=Blk, charsize=1.1

end
; dgp rev 10/26/05 output text for invalid data file 
pro _pr_dsp_invalid

_pr_gen_ident ; dgp rev 1/28/10 debugging
    
  Common colour, Wht, Blk

  text = fn_ifs_get('message')

  XYOUTS, .3, .6, text, /normal, color=Blk, charsize=1.1

end

PRO _pr_dsp_surface

_pr_gen_ident ; dgp rev 1/28/10 debugging

  Common colour, Wht, Blk
  cells = fn_data_get_cells()
   
  _disp_set_reso, 64

  names = _par_cur_names()
  nlist = [' ',names]
   
  par = _cdr_get_pars()

  ;show_data, cells
 
  bin_scale = _get_bincnt()
  bin_range = [1024/bin_scale,1024/bin_scale]
  hist_range = bin_scale
  range = [0,(bin_scale-1)]

  if (fn_dmf_get('data flag')) then begin

      select_values = fn_dmf_get('ssl indices')

      cnt = n_elements(select_values)

      select_values = select_values(0:cnt-(cnt mod 2)-1)
     
      nhisto = float(t_histn([[cells(select_values,par(0)-1)],[cells(select_values,par(1)-1)]], $
                      binsize=bin_range,range=hist_range))   
     ;show_data, data

      _manip_data, nhisto
 
     ;show_data, data
    
      surface, nhisto(1:*,1:*), color=Blk, background=Wht, $
         xs=1, ys=1, $
         xrange=range, yrange=range, $
         xtitle=nlist(par(0)), ytitle=nlist(par(1))         

  endif else begin

      fcsname = fn_dmf_get('ifs key')
      plot, [0,1023],[0,1023],/nodata, color=Blk, background=Wht, $
              xs=1, ys=1, $
              xrange=[0,1023], yrange=[0,1023], $
              xtitle=nlist(par(0)), ytitle=nlist(par(1))
              
      _pr_dsp_no_data
      
  endelse

END

; dgp rev 12/14/05 determine the display characteristics and 
; record them in the display structure
pro _pr_dsp_init

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_dsp_info, cv_dsp_hash
  
  coors   = convert_coord(!x.crange,!y.crange,/data,/to_device)

  _pr_dmf_set, 'x crange', coors(0,0:1)
  _pr_dmf_set, 'y crange', coors(1,0:1)

  spacing = !d.y_ch_size*!p.charsize
 
  row_pos = coors(1,0) - (spacing*1.5)
  col_left = coors(0,0)
  col_right = coors(0,1)
  bottom = coors(1,0)
  top = coors(1,1)

  dev_coors = convert_coord(!x.region,!y.region,/norm,/to_device)
  ano_top = dev_coors(1,1)
  ano_bot = dev_coors(1,0)
  ano_left = dev_coors(0,0)
  ano_right = dev_coors(0,1)
  
  col_idx = 0
  col_cnt = 3
  chunk = (col_right-col_left)/(col_cnt-1)
  col_arr = indgen(col_cnt)*chunk + col_left

  char_width  = !d.x_ch_size*!p.charsize
  char_height = !d.y_ch_size*!p.charsize

  cv_dsp_hash = asarr('crange',coors)
  cv_dsp_hash = [cv_dsp_hash,asarr('spacing',spacing)]
  cv_dsp_hash = [cv_dsp_hash,asarr('cur y',row_pos)]
  cv_dsp_hash = [cv_dsp_hash,asarr('col left',col_left)]
  cv_dsp_hash = [cv_dsp_hash,asarr('col right',col_right)]
  cv_dsp_hash = [cv_dsp_hash,asarr('col idx',col_idx)]
  cv_dsp_hash = [cv_dsp_hash,asarr('col cnt',col_cnt)]
  cv_dsp_hash = [cv_dsp_hash,asarr('col arr',col_arr)]
  cv_dsp_hash = [cv_dsp_hash,asarr('col width',chunk)]
  cv_dsp_hash = [cv_dsp_hash,asarr('top',top)]
  cv_dsp_hash = [cv_dsp_hash,asarr('bottom',bottom)]
  cv_dsp_hash = [cv_dsp_hash,asarr('char width',char_width)]
  cv_dsp_hash = [cv_dsp_hash,asarr('char height',char_height)]
  cv_dsp_hash = [cv_dsp_hash,asarr('x size',!d.x_size)]
  cv_dsp_hash = [cv_dsp_hash,asarr('y size',!d.y_size)]
  cv_dsp_hash = [cv_dsp_hash,asarr('ano top',ano_top)]
  cv_dsp_hash = [cv_dsp_hash,asarr('ano bot',ano_bot)]
  cv_dsp_hash = [cv_dsp_hash,asarr('ano left',ano_left)]
  cv_dsp_hash = [cv_dsp_hash,asarr('ano right',ano_right)]

end

; LGLN structure for x axis labeling
; returns the appropriate axis information
; based upon log/lin setting
function fn_lgln_get, label

  common cm_lgln_info, cv_lgln_hash

  tokens = strtrim(strsplit(label," "),2)

  ; key will be '0' or '1'
  if (tokens(0) eq 'x') then begin
    key = strtrim(fn_dmf_get_xaxis(),2)
  endif else begin
    key = strtrim(fn_dmf_get_yaxis(),2)
  endelse

  if (isaskey(cv_lgln_hash,key)) then begin
    if (isaskey(cv_lgln_hash(key),label)) then begin
      return, cv_lgln_hash(key,label)
    endif
  endif
  
  return, ""
  
end

; dgp rev 8/22/2012 current x par
function fn_dmf_xpar

  if (fn_dmf_single()) then begin
    par = sum(fn_dmf_get('pars'))
  endif else begin
    both = fn_dmf_get('pars')
    par = both(0)
  endelse  
  par = par - 1
  return, fix(par)
  
end

; dgp rev 8/22/2012 current x par
function fn_dmf_ypar

  if (fn_dmf_single()) then begin
    par = sum(fn_dmf_get('pars'))
  endif else begin
    both = fn_dmf_get('pars')
    par = both(1)
  endelse  
  par = par - 1
  return, fix(par)
  
end

; dgp rev 8/21/2012
pro _pr_dmf_xrange_min, val

  common cm_dmf_info, cv_dmf_key, cv_dmf_hash

  if (not isaskey(cv_dmf_hash(cv_dmf_key),'range')) then _pr_dmf_init_range

  par = fn_dmf_xpar()
  cv_dmf_hash(cv_dmf_key,'range',par,2) = val
  
end

; dgp rev 8/21/2012
pro _pr_dmf_xrange_max, val

  common cm_dmf_info, cv_dmf_key, cv_dmf_hash

  if (not isaskey(cv_dmf_hash(cv_dmf_key),'range')) then _pr_dmf_init_range

  par = fn_dmf_xpar()
  
  cv_dmf_hash(cv_dmf_key,'range',par,3) = val
  
end

; dgp rev 8/21/2012
pro _pr_dmf_yrange_min, val

  common cm_dmf_info, cv_dmf_key, cv_dmf_hash

  if (not isaskey(cv_dmf_hash(cv_dmf_key),'range')) then _pr_dmf_init_range

  par = fn_dmf_ypar()
  
  cv_dmf_hash(cv_dmf_key,'range',par,2) = val
  
end

; dgp rev 8/21/2012
pro _pr_dmf_yrange_max, val

  common cm_dmf_info, cv_dmf_key, cv_dmf_hash

  if (not isaskey(cv_dmf_hash(cv_dmf_key),'range')) then _pr_dmf_init_range

  par = fn_dmf_ypar()
  
  cv_dmf_hash(cv_dmf_key,'range',par,3) = val
  
end


; dgp rev 8/21/2012
function fn_dmf_yrange

  common cm_dmf_info, cv_dmf_key, cv_dmf_hash

  if (not isaskey(cv_dmf_hash(cv_dmf_key),'range')) then _pr_dmf_init_range

  par = fn_dmf_ypar()
  
  return, [cv_dmf_hash(cv_dmf_key,'range',par,2),cv_dmf_hash(cv_dmf_key,'range',par,3)]

end

; dgp rev 8/21/2012
function fn_dmf_yrange_lim

  common cm_dmf_info, cv_dmf_key, cv_dmf_hash

  if (not isaskey(cv_dmf_hash(cv_dmf_key),'range')) then _pr_dmf_init_range

  par = fn_dmf_ypar()

  return, [cv_dmf_hash(cv_dmf_key,'range',par,0),cv_dmf_hash(cv_dmf_key,'range',par,1)]

end

; dgp rev 11/1/05 get parameter info
function fn_par_get_other, par, label

  common cm_par_info, cv_par_hash, cv_par_ident
  
  if (size(cv_par_hash,/type) ne 11) then return, 0

  if (size(par,/type) eq 0) then return, 0
  par = strtrim(par,2)

  if (not isaskey(cv_par_hash,par)) then return, 0

  if (not isaskey(cv_par_hash(par),label)) then return, 0

  return, cv_par_hash(par,label)

end


; dgp rev 8/21/2012
function fn_dmf_xmin_ratio

  common cm_dmf_info, cv_dmf_key, cv_dmf_hash

  if (not isaskey(cv_dmf_hash(cv_dmf_key),'range')) then _pr_dmf_init_range

  par = fn_dmf_xpar()
  
  if fn_par_get_other(par,'log') eq 1 then begin
    return, alog10(float(cv_dmf_hash(cv_dmf_key,'range',par,2)))/alog10(float(cv_dmf_hash(cv_dmf_key,'range',par,1)))
  endif else begin
    return, float(cv_dmf_hash(cv_dmf_key,'range',par,2))/float(cv_dmf_hash(cv_dmf_key,'range',par,1))
  endelse

end

; dgp rev 8/21/2012
function fn_dmf_xmax_ratio

  common cm_dmf_info, cv_dmf_key, cv_dmf_hash

  if (not isaskey(cv_dmf_hash(cv_dmf_key),'range')) then _pr_dmf_init_range

  par = fn_dmf_xpar()
  
  if fn_par_get_other(par,'log') eq 1 then begin
    return, alog10(float(cv_dmf_hash(cv_dmf_key,'range',par,3)))/alog10(float(cv_dmf_hash(cv_dmf_key,'range',par,1)))
  endif else begin
    return, float(cv_dmf_hash(cv_dmf_key,'range',par,3))/float(cv_dmf_hash(cv_dmf_key,'range',par,1))
  endelse

end

; dgp rev 8/21/2012
function fn_dmf_ymin_ratio

  common cm_dmf_info, cv_dmf_key, cv_dmf_hash

  if (not isaskey(cv_dmf_hash(cv_dmf_key),'range')) then _pr_dmf_init_range

  par = fn_dmf_ypar()
  
  if fn_par_get_other(par,'log') eq 1 then begin
    return, alog10(float(cv_dmf_hash(cv_dmf_key,'range',par,2)))/alog10(float(cv_dmf_hash(cv_dmf_key,'range',par,1)))
  endif else begin
    return, float(cv_dmf_hash(cv_dmf_key,'range',par,2))/float(cv_dmf_hash(cv_dmf_key,'range',par,1))
  endelse


end

; dgp rev 8/21/2012
function fn_dmf_ymax_ratio

  common cm_dmf_info, cv_dmf_key, cv_dmf_hash

  if (not isaskey(cv_dmf_hash(cv_dmf_key),'range')) then _pr_dmf_init_range

  par = fn_dmf_ypar()
  
  if fn_par_get_other(par,'log') eq 1 then begin
    return, alog10(float(cv_dmf_hash(cv_dmf_key,'range',par,3)))/alog10(float(cv_dmf_hash(cv_dmf_key,'range',par,1)))
  endif else begin
    return, float(cv_dmf_hash(cv_dmf_key,'range',par,3))/float(cv_dmf_hash(cv_dmf_key,'range',par,1))
  endelse

end

; dgp rev 8/21/2012
function fn_dmf_xrange

  common cm_dmf_info, cv_dmf_key, cv_dmf_hash

  if (not isaskey(cv_dmf_hash(cv_dmf_key),'range')) then _pr_dmf_init_range

  par = fn_dmf_xpar()
  
  return, [cv_dmf_hash(cv_dmf_key,'range',par,2),cv_dmf_hash(cv_dmf_key,'range',par,3)]

end

; dgp rev 8/21/2012
function fn_dmf_xrange_lim

  common cm_dmf_info, cv_dmf_key, cv_dmf_hash

  if (not isaskey(cv_dmf_hash(cv_dmf_key),'range')) then _pr_dmf_init_range

  par = fn_dmf_xpar()
  
  return, [cv_dmf_hash(cv_dmf_key,'range',par,0),cv_dmf_hash(cv_dmf_key,'range',par,1)]

end


; dgp rev 10/26/05 single blank for for invalid data
pro _pr_dsp_single_blank

_pr_gen_ident ; dgp rev 1/28/10 debugging

  Common colour, Wht, Blk

  ..locals 6 0

  disp_res = fn_dmf_calc_res()
  real_res = _dsp_get_norm()
  res_ratio = float(disp_res)/float(real_res)

  lbl_x = fn_lgln_get('x labels')
  tk_max_x = fn_lgln_get('x max ticks')*res_ratio
  blnk_x = fn_lgln_get('x blanks')

  ymax = 1

  frame_com = "plot, [0,disp_res-1], [0, ymax], /nodata, xs=1, ys=1, background=wht, color=blk, "
  frame_com = frame_com + "xtickname=lbl_x, xticks = n_elements(tk_max_x)-1, xtickv=tk_max_x, "
  frame_com = frame_com + "xtitle='no data'"

  ..locals 6 0
  results = execute(frame_com)
  
  _pr_dsp_init

end
; dgp rev 10/26/05 single frame for histogram data
pro _pr_dsp_single_frame, ymax

_pr_gen_ident ; dgp rev 1/28/10 debugging

  Common colour, Wht, Blk

  ..locals 6 0

  disp_res = fn_dmf_calc_res()
  real_res = _dsp_get_norm()
  res_ratio = float(disp_res)/float(real_res)

  lbl_x = fn_lgln_get('x labels')
  tk_max_x = fn_lgln_get('x max ticks')*res_ratio
  blnk_x = fn_lgln_get('x blanks')

  par = max(_cdr_get_pars())

  label = fn_par_name(par-1)

  xlow = (disp_res-1) * fn_dmf_xmin_ratio()
  xhi = (disp_res-1) * fn_dmf_xmax_ratio()

  x_rng = [xlow,xhi]

  xidx = where(tk_max_x ge xlow and tk_max_x le xhi,xcnt) 

  frame_com = "plot, x_rng, [0, ymax], /nodata, xs=1, ys=1, background=wht, color=blk, "
  if xcnt ne 0 then frame_com = frame_com + "xtickname=lbl_x(xidx), xticks = xcnt, xtickv=tk_max_x(xidx), "
  frame_com = frame_com + "xtitle=label"

  on_error, 2
  results = execute(frame_com)
  
  _pr_dsp_init

end

; dgp rev 11/8/05 create an empty frame for dots and contours
pro _pr_dsp_dual_frame

_pr_gen_ident ; dgp rev 1/28/10 debugging

  Common colour, Wht, Blk

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
  xlab = fn_par_name(pars(0)-1)
  ylab = fn_par_name(pars(1)-1)

   xlow = (disp_res-1) * fn_dmf_xmin_ratio()
   xhi = (disp_res-1) * fn_dmf_xmax_ratio()

   ylow = (disp_res-1) * fn_dmf_ymin_ratio()
   yhi = (disp_res-1) * fn_dmf_ymax_ratio()

   x_rng = [xlow,xhi]
   y_rng = [ylow,yhi]

  xidx = where(tk_max_x ge xlow and tk_max_x le xhi,xcnt) 
  yidx = where(tk_max_y ge ylow and tk_max_y le yhi,ycnt) 

  frame_com = "plot, x_rng, y_rng, /nodata, xs=1, ys=1, background=wht, color=blk, "
  if xcnt ne 0 then frame_com = frame_com + "xtickname=lbl_x(xidx), xticks = xcnt, xtickv=tk_max_x(xidx), "
  if ycnt ne 0 then frame_com = frame_com + "ytickname=lbl_y(yidx), yticks = ycnt, ytickv=tk_max_y(yidx), "
  frame_com = frame_com + "xtitle=xlab, ytitle=ylab "

  ..locals 10 0
  results = execute(frame_com)

  _pr_dsp_init

end
; dgp rev 1/5/06 histogram routine for FCS v3.0 data
pro _pr_dsp_histo3

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_tables, cv_base2, cv_base10
  Common colour, Wht, Blk

  par = max(_cdr_get_pars())
  
  names = _par_cur_names()

  real_res = _dsp_get_norm()

  disp_res = fn_dmf_calc_res()
  
  hist_bin = real_res/disp_res
  
  hist_binsize = [hist_bin,hist_bin]
  
  plot_range  = [0,disp_res-1]
  hist_range = [[0,real_res-1],[0,real_res-1]]
  
  frc_rng = [0,disp_res-1]
   
  nlist = [' ',names]
  
  fcsname = fn_dmf_get('ifs key')

  !p.position = [.2,.5,.95,.9]

  if (fn_dmf_get('data flag')) then begin

      select_values = fn_dmf_get('ssl indices')

      cnt = n_elements(select_values)

      tmp_all = fn_ifs_param(par-1)
      tmp_sel = tmp_all(select_values)
      
      ; determine the actual data range
      data_max = max(tmp_sel,min=data_min)
      data_rng = data_max - data_min
      data_mid = (data_max+data_min)/2
      ; now look at how that data is spread out 
      ; over 2 standard deviations
      data_std = stdev(tmp_sel)
      data_avg = avg(tmp_sel)
      data_start = data_avg - (data_std*2)
      data_end = data_avg + (data_std*2)
      data_rng2 = data_end - data_start
      ; take the smaller area as the range
      if (data_rng2 lt data_rng) then begin
        data_rng  = data_rng2
        data_min = data_start
        data_max = data_end
        data_mid = data_avg
      endif
      
      idx = where(data_rng lt cv_base2,cnt)

      if (cnt eq 0) then begin
        chunk = cv_base2(n_elements(base2)-1)
      endif else begin
        chunk = cv_base2(idx(0))
        if (chunk lt cv_base2(12)) then chunk = cv_base2(12)
      endelse

      data_extra = (chunk) /2
      hist_min = data_mid - data_extra
      hist_max = hist_min + (chunk - 1.0)

      hist_bin = long(chunk / disp_res)
      if (hist_bin eq 0) then hist_bin = 1

      print, "Bin ",hist_bin
      print, "Min ", hist_min
      print, "Max ", hist_max

      histo = float(histogram(tmp_sel,max=hist_max,min=hist_min,binsize=hist_bin))
      
      data = histo
      
      _manip_data, histo
    
      last = n_elements(histo) - 2

      _pr_dmf_calc_ymax, ymax
            
      res_ratio = float(disp_res)/float(real_res)
      res_ratio = 1
           
      xtitle = fn_par_name(max(_cdr_get_pars())-1)
      
      plot_info = asarr('data',histo)
      plot_info = [plot_info,asarr('type','histo')]
      plot_info = [plot_info,asarr('reso',disp_res)]
      plot_info = [plot_info,asarr('ymax',ymax)]
      plot_info = [plot_info,asarr('range',[hist_min,hist_max])]
      plot_info = [plot_info,asarr('xtitle',xtitle)]
      plot_info = [plot_info,asarr('color',fn_dmf_get('color'))]
      plot_info = [plot_info,asarr('line style',fn_dmf_get('line'))]
      plot_info = [plot_info,asarr('thick',fn_dmf_get('line thick'))]

      _pr_dmf_set, 'quick plot', plot_info
      _pr_dsp_draw3

;      axis, 0, xaxis=0, xticklen=-.01, xtickv=tk_min_x, xticks=n_elements(tk_min_x)-1, $
;       xtickname=blnk_x, color=Blk, xrange=frc_rng, xs=1, ys=1

    endif else begin

      _pr_dsp_single_frame, 1
      
      _pr_dsp_no_data

    endelse

    !p.position = [.2,.33,.95,.9]
      
end

; dpg rev 6/2/2010 DMF Ready to plot
function fn_dmf_ready

  ; verify that the DMF settings don't exceed the IFS parameters
  
  if (max(fn_dmf_get('pars')) gt fn_ifs_get_header('$PAR')) then begin
    _pr_dmf_set, 'error', "Invalid Parameter"
    return, 0
  endif

  if (not fn_dmf_get('data flag')) then begin
    _pr_dmf_set, 'error', "Data Gated Out"
    return, 0
  endif

  if (not fn_ifs_status()) then begin
    _pr_dmf_set, 'error', "File Error"
    return, 0
  endif

  _pr_dmf_set, 'error', " "
  return, 1

end

; dgp rev 10/27/05 work on log/lin labeling
pro _pr_dsp_histo2

_pr_gen_ident ; dgp rev 1/28/10 debugging

  Common colour, Wht, Blk
  cells = fn_data_get_cells()
  
  lgln_mx     = fn_lgln_get('x max ticks')
  lgln_min    = fn_lgln_get('x min ticks')
  lgln_lbls   = fn_lgln_get('x labels')
  lgln_blanks = fn_lgln_get('x blanks')

  par = max(_cdr_get_pars())
  
  names = _par_cur_names()

  real_res = _dsp_get_norm()

  disp_res = fn_dmf_calc_res()
  
  hist_bin = real_res/disp_res

  hist_binsize = [hist_bin,hist_bin]
  
  plot_range  = [0,disp_res-1]
  hist_range = [[0,real_res-1],[0,real_res-1]]
  
  frc_rng = [0,disp_res-1]
   
  nlist = [' ',names]
  
  fcsname = fn_dmf_get('ifs key')

  !p.position = [.2,.5,.95,.9]

  if (fn_dmf_ready()) then begin

      select_values = fn_dmf_get('ssl indices')

      cnt = n_elements(select_values)

      tmp = cells(select_values,par-1)

      histo = float(histogram(tmp,max=real_res-1,min=0,binsize=hist_bin))
;      histo = float(histogram(tmp,max=real_res-1,min=0,binsize=hist_bin))
      data = histo
      
      _manip_data, histo
    
      last = n_elements(histo) - 2

      _pr_dmf_calc_ymax, ymax
            
      res_ratio = float(disp_res)/float(real_res)
     
      lbl_x = fn_lgln_get('x labels')
      tk_max_x = fn_lgln_get('x max ticks')*res_ratio
      tk_min_x = fn_lgln_get('x min ticks')*res_ratio
      blnk_x = fn_lgln_get('x blanks')
            
      _pr_dsp_single_frame, ymax
      
      xtitle = fn_par_name(max(_cdr_get_pars())-1)
      
      plot_info = asarr('data',histo)
      plot_info = [plot_info,asarr('type','histo')]
      plot_info = [plot_info,asarr('reso',disp_res)]
      plot_info = [plot_info,asarr('ymax',ymax)]
      plot_info = [plot_info,asarr('xtitle',xtitle)]
      plot_info = [plot_info,asarr('color',fn_dmf_get('color'))]
      plot_info = [plot_info,asarr('line style',fn_dmf_get('line'))]
      plot_info = [plot_info,asarr('thick',fn_dmf_get('line thick'))]

      _pr_dmf_set, 'quick plot', plot_info
      _pr_dsp_draw

      axis, 0, xaxis=0, xticklen=-.01, xtickv=tk_min_x, xticks=n_elements(tk_min_x)-1, $
       xtickname=blnk_x, color=Blk, xrange=frc_rng, xs=1, ys=1

    endif else begin

      _pr_dsp_single_frame, 1
      
      _pr_dsp_no_data

    endelse

    !p.position = [.2,.33,.95,.9]
      
end

; dgp rev 9/28/05 display the histogram data
; use a log/lin overlay for FCS 2.0 data
; use actual linear data for FCS 3.0
pro _pr_dsp_histo

_pr_gen_ident ; dgp rev 1/28/10 debugging

  ; if invalid data file, then
  ; simply draw an empty fram

  if (fn_dmf_ready()) then begin

    real_res = _dsp_get_norm()

    if (real_res eq 262144) then begin
      _pr_dsp_histo3
    endif else begin
      _pr_dsp_histo2
    endelse

  endif else begin

      _pr_dsp_single_blank
      
      _pr_dsp_invalid

  endelse

end

; dgp rev 9/30/05 display the dot plot data
; use a log/lin overlay for FCS 2.0 data
; use actual linear data for FCS 3.0
pro _pr_dsp_dot

  real_res = _dsp_get_norm()

  if (real_res eq 262144) then begin
    _pr_dsp_dot3
  endif else begin
    _pr_dsp_dot2
  endelse

end
pro cb_dmf_out, wid, which

_pr_gen_ident ; dgp rev 1/28/10 debugging

  value = dgpGetValue(wid)
  
  _pr_dmf_set,'outliers', value

  draw_cells

end
; dgp rev 11/10/05 the maximum number of dots
; to display in a dot plot 
function _dmf_get_dot_max

  dot_max = fn_dmf_get('dot max')
  
  if (dot_max eq 0) then begin
    dot_max = 5000
    _pr_dmf_set, 'dot max', dot_max
  endif

  return, dot_max

end

; dgp rev 8/30/05 given a form name, return the main widget
; if the widget has been closed or doesn't exist,then return 0
function fn_wms_exists, ident

  common cm_wms_info, cv_main_hash

  if (ISASKEY(cv_main_hash, ident)) then begin
    if (isaskey(cv_main_hash(ident),'wid')) then begin
      wid = cv_main_hash(ident,'wid')
      if (fn_wid_exists(wid)) then begin
        if (ident eq WtGet(wid,/name)) then return, 1
      endif
    endif
  endif
  
  return, 0

end


; dgp rev 11/10/05 modify the underlying widget
; this will trigger a callback, so set callback
; to abort
; dgp rev 3/14/08 if file count is less than max dot, then use file count as max dot
pro _pr_reflect_dots

  frm_name = "Control Panel"

  if (not fn_wms_exists(frm_name)) then return

  _pr_surpress_callback
  
  wid_arr = fn_wra_get('dot max', frm_name)

  dot_slider_wid = wid_arr(0)

  minval = 0
  maxval = fn_data_get_cnt()
  dotmax = _dmf_get_dot_max()
  
  if (dotmax gt maxval) then dotmax = maxval

  ; dgp rev 1/31/08 set to zero, then change maximum in case the current settting is higher than max
  s = WwSetValue(dot_slider_wid, 0)
  args = {,minimum: minval, maximum: maxval }
  status = WtSet(dot_slider_wid,args)
  s = WwSetValue(dot_slider_wid, dotmax)

  _pr_activate_callback  

end

; dgp rev 11/10/05 modify the underlying widget
; this will trigger a callback, so set callback
; to abort
pro _pr_reflect_dotsx

  frm_name = "Control Panel"

  if (not fn_wms_exists(frm_name)) then return

  _pr_surpress_callback
  
  wid_arr = fn_wra_get('dot max', frm_name)

  dot_slider_wid = wid_arr(0)

  minval = 0
  maxval = fn_data_get_cnt()
  args = {,minimum: minval, maximum: maxval }
  status = WtSet(dot_slider_wid,args)

  s = WwSetValue(dot_slider_wid, _dmf_get_dot_max())

  _pr_activate_callback  

end

; dgp rev 9/30/05 FCS version 2.0 dot plot
PRO _pr_dsp_dot2

   Common colour, Wht, Blk

   cells = fn_data_get_cells()
   
   par = _cdr_get_pars()

   dot_max = long(_dmf_get_dot_max())

   names = _par_cur_names()

   real_res = _dsp_get_norm()

   disp_res = fn_dmf_calc_res()
   
   res_ratio = float(disp_res)/float(real_res)

   frc_rng = [0,disp_res-1]

   nlist = [' ',names]
       
   hist_binsize = [real_res/disp_res,real_res/disp_res]

   plot_range  = [0,disp_res-1]
   hist_range = [[0,real_res-1],[0,real_res-1]]

   fcsname = fn_dmf_get('ifs key')
   
   lbl_x = fn_lgln_get('x labels')
   tk_max_x = fn_lgln_get('x max ticks')*res_ratio
   tk_min_x = fn_lgln_get('x min ticks')*res_ratio
   lbl_y = fn_lgln_get('y labels')
   tk_max_y = fn_lgln_get('y max ticks')*res_ratio
   tk_min_y = fn_lgln_get('y min ticks')*res_ratio
   blnk_x = fn_lgln_get('x blanks')
   blnk_y = fn_lgln_get('y blanks')

  if (fn_dmf_get('data flag')) then begin
   
     select_values = fn_dmf_get('ssl indices')

     cnt = n_elements(select_values)

     if (cnt gt dot_max) then cnt = dot_max
      
     data_set = cells(select_values(0:cnt-1),*)

     hist_bin  = float(real_res)/float(disp_res)

     hist1 = data_set(*,par(0)-1)*res_ratio     
     ;_manip_data, hist1
     hist2 = data_set(*,par(1)-1)*res_ratio
     ;_manip_data, hist2
          
     _pr_dsp_dual_frame

     xtitle = fn_par_name(par(0)-1)
     ytitle = fn_par_name(par(1)-1)

     plot_info = asarr('hist1',hist1)
     plot_info = [plot_info,asarr('hist2',hist2)]
     plot_info = [plot_info,asarr('type','dot')]
     plot_info = [plot_info,asarr('xtitle',xtitle)]
     plot_info = [plot_info,asarr('ytitle',ytitle)]
     plot_info = [plot_info,asarr('reso',disp_res)]
     plot_info = [plot_info,asarr('color',fn_dmf_get('color'))]

     _pr_dmf_set, 'quick plot', plot_info
     _pr_dsp_draw

     axis, 0, yaxis=0, yticklen=-.01, ytickv=tk_min_y, yticks=n_elements(tk_min_y)-1, $
       ytickname=blnk_y, color=Blk, yrange=frc_rng, ys=1
     axis, 0, xaxis=0, xticklen=-.01, xtickv=tk_min_x, xticks=n_elements(tk_min_x)-1, $
       xtickname=blnk_x, color=Blk, xrange=frc_rng, xs=1

   endif else begin
   
     _pr_dsp_dual_frame

     _pr_dsp_no_data

   endelse
END

; dgp rev 9/30/05 FCS version 3.0 dot plot
PRO _pr_dsp_dot3

   Common colour, Wht, Blk

   par = _cdr_get_pars()

   dot_max = long(_dmf_get_dot_max())

   names = _par_cur_names()

   real_res = _dsp_get_norm()

   disp_res = fn_dmf_calc_res()
   
   res_ratio = float(disp_res)/float(real_res)

   frc_rng = [0,disp_res-1]

   nlist = [' ',names]
       
   hist_binsize = [real_res/disp_res,real_res/disp_res]

   plot_range  = [0,disp_res-1]
   hist_range = [[0,real_res-1],[0,real_res-1]]

   fcsname = fn_dmf_get('ifs key')
   
   lbl_x = fn_lgln_get('x labels')
   tk_max_x = fn_lgln_get('x max ticks')*res_ratio
   tk_min_x = fn_lgln_get('x min ticks')*res_ratio
   lbl_y = fn_lgln_get('y labels')
   tk_max_y = fn_lgln_get('y max ticks')*res_ratio
   tk_min_y = fn_lgln_get('y min ticks')*res_ratio
   blnk_x = fn_lgln_get('x blanks')
   blnk_y = fn_lgln_get('y blanks')

  if (fn_dmf_get('data flag')) then begin
   
     select_values = fn_dmf_get('ssl indices')

     cnt = n_elements(select_values)
     print, "Dots ",cnt
     print, "Dot max ",dot_max

     if (cnt gt dot_max) then cnt = dot_max
      
     tmp_all = fn_ifs_param(par(0)-1)
     hist1 = tmp_all(select_values)
     
     ytitle = fn_par_name(par(1)-1)
     
     tmp_all = 0
     tmp_all = fn_ifs_param(par(1)-1)
     hist2 = tmp_all(select_values)
           
     hist_bin  = float(real_res)/float(disp_res)
          
     xtitle = fn_par_name(par(0)-1)
     ytitle = fn_par_name(par(1)-1)

     plot_info = asarr('hist1',hist1)
     plot_info = [plot_info,asarr('hist2',hist2)]
     plot_info = [plot_info,asarr('type','dot')]
     plot_info = [plot_info,asarr('xtitle',xtitle)]
     plot_info = [plot_info,asarr('ytitle',ytitle)]
     plot_info = [plot_info,asarr('reso',disp_res)]
     plot_info = [plot_info,asarr('color',fn_dmf_get('color'))]

     _pr_dmf_set, 'quick plot', plot_info
     _pr_dsp_draw3

   endif else begin
   
     _pr_dsp_dual_frame

     _pr_dsp_no_data

   endelse
END

function _ssl_get_fullset, data_set

  if (fn_dmf_get('data flag')) then begin
   
    cells = fn_data_get_cells()
    select_values = fn_dmf_get('ssl indices')
    cnt = n_elements(select_values)
   
    if (cnt gt 2) then begin
      select_values = select_values(0:cnt-(cnt mod 2)-1)
      data_set = cells(select_values,*)
      return, 1
    endif
  endif

  return, 0

end

function _ssl_get_set, val

   cells = fn_data_get_cells()
   
   if (fn_dmf_get('data flag')) then begin
     
     select_values = fn_dmf_get('ssl indices')

     cnt = fn_dmf_get('data count')
  
     if (cnt gt 2) then select_values = select_values(0:cnt-(cnt mod 2)-1)

     print, "Number of events is ",cnt
       
     hist = cells(select_values,val-1)

     return, hist

   endif
   return, 0

end

function _ssl_init_color

   if (_olay_get_on()) then begin
     return, 2
   endif else begin
     return, 0
   endelse

end

function fn_dmf_get_open, open_win



end

; create a valid path from a root and append string
function fn_gen_build_path, root, append

  sep = fn_gen_getstr('backslash')

  spec = strjoin([strsplit(root,sep),strsplit(append,sep)],sep)

  return, spec

end

; dgp rev 5/21/2012
function fn_calc_outliers

   CV_RawDataReso = _dsp_get_norm()

   CV_DispReso = fn_dmf_calc_res()

   head = {, type:0b, lohi:0b, levl:0, numb:0l, valu:0.0 }

;   Disp2Outlier = 8
   Disp2Outlier = 16
   Raw2Outlier = float(CV_DispReso*Disp2Outlier)/float(CV_RawDataReso)
   
   CV_Mask = Lonarr(CV_DispReso*Disp2Outlier, CV_DispReso*Disp2Outlier)

   last = 0
   closed = 0B

   file_name = 'outliers.xxx'

   restname = fn_gen_build_path(fn_gen_getstr('work path'),file_name)

   found = findfile(restname,count=cnt)
    
   if (cnt eq 0) then print, restname," not found"

   openr, unit, restname, /get_lun
   first = 1
   vert_min = 5

   MyHead = list(0)
   MyLst = list(0)
   cnt = 0
   
   while not eof(unit) do begin

     readu, unit, head
     npts = head.numb
     closed = head.type
     Holder = fltarr(2,npts)
     readu, unit, Holder

     if npts ge vert_min then begin
     
;       if not closed then begin
;         if fn_closable(Holder) then begin
;           Holder(0,0) = Holder(0,npts-2)
;	   Holder(1,0) = Holder(1,1)
;	   Holder(0,npts-1) = Holder(0,npts-2)
;           Holder(1,npts-1) = Holder(1,1)
;           closed = 1B
;         endif
;       endif
     
       if closed then begin
         print, npts," closed verts"
         point = CONVERT_COORD(Holder(0,*), Holder(1,*), /Normal, /To_Data) 
         contx = transpose(point(0,*)*float(Disp2Outlier))
         conty = transpose(point(1,*)*float(Disp2Outlier))
         print, "Area: ",poly_area(contx,conty)
         on_error_goto, SkipPoly
         if poly_area(contx,conty) gt 0.0 then begin
           pp = Polyfillv(contx, conty, CV_DispReso*Disp2Outlier, CV_DispReso*Disp2Outlier)
           aa = transpose([[contx],[conty]])
           cnt = cnt + 1
           MyLst(0) = cnt
           MyLst = [MyLst,aa]
           MyHead(0) = cnt
           MyHead = [MyHead,head]
         endif
         SkipPoly:
       endif
       print, "Mask size ",n_elements(pp)

     endif

   endwhile

   free_lun, unit

   contcnt = myhead(0)

   SizeArr = [myhead(1).NUMB]
   for i=2, contcnt do begin
     SizeArr = [SizeArr,myhead(i).NUMB]
   endfor
   SizeOrder = sort(SizeArr)
   SizeOrder = reverse(SizeOrder)

   CV_Mask = Lonarr(CV_DispReso*Disp2Outlier, CV_DispReso*Disp2Outlier)
   for c=0, contcnt-1 do begin
     i = SizeOrder(c)+1
     print, "Contour index ",i
     pf1 = polyfillv(mylst(i,0,*),mylst(i,1,*),CV_DispReso*Disp2Outlier, CV_DispReso*Disp2Outlier)   
     Size1 = n_elements(pf1)
     print, "  Size = ",Size1
     CV_Mask(pf1) = myhead(i).LOHI
     Area1 = sum(CV_Mask)
     print, "  Area = ",Area1
   endfor

   par = _cdr_get_pars()
   CV_RawX = _ssl_get_set(par(0))
   CV_RawY = _ssl_get_set(par(1))

   CV_DataIndexX = long(CV_RawX * Raw2Outlier)
   CV_DataIndexY = long(CV_RawY * Raw2Outlier)

   OutlierIndex = where(CV_Mask(CV_DataIndexX,CV_DataIndexY) eq 0, cnt)
   return, transpose([[cv_rawx(outlierindex)], [cv_rawy(outlierindex)]])

end

; dgp rev 5/23/2012 modified contour routine to handle outliers
; dgp rev 1/5/06 include DMF font sizing 
; dgp rev Contour Routine
pro _pr_dsp_prep_outliers

_pr_gen_ident ; dgp rev 1/28/10 debugging

  plot_info = fn_dmf_get('quick plot')
  dataorig = plot_info('data')
  disp_res = plot_info('reso')

  data = REPLICATE(-1.0, disp_res+2, disp_res+2)
  data(1,1) = dataorig

  lvl_arr = plot_info('levels')
  color_idx = plot_info('color')
  color_rgb = WoColorConvert(color_idx, /IndexToColor)
  wht = WoColorConvert(1)
  plot_com  = "contour, data, color=color_rgb, background=Wht, xs=5, ys=5"
  plot_com = plot_com + ", levels=lvl_arr, /overplot, thick=fn_dmf_get('line thick')"
  results = execute(plot_com)
        
  file_name = 'outliers.xxx'

  restname = fn_gen_build_path(fn_gen_getstr('work path'),file_name)

  plot_com  = "contour, data, color=color_rgb, background=Wht, xs=5, ys=5"
  plot_com = plot_com + ", levels=lvl_arr, /overplot, thick=fn_dmf_get('line thick'), path=restname" 
  results = execute(plot_com)

end

; dgp rev 12/15/2010 does the label exists for a given key
function fn_dmf_exists, label

  common cm_dmf_info, cv_dmf_key, cv_dmf_hash

  if (size(cv_dmf_hash,/type) ne 11) then return, 0
  if (size(cv_dmf_key,/type) ne 7) then return, 0
  if (not isaskey(cv_dmf_hash,cv_dmf_key)) then return, 0
  return, isaskey(cv_dmf_hash(cv_dmf_key),label)
  
end

; dgp rev 5/23/2012 modified contour routine to handle outliers
; dgp rev 1/5/06 include DMF font sizing 
; dgp rev Contour Routine
pro _pr_dsp_draw_contour

_pr_gen_ident ; dgp rev 1/28/10 debugging

  plot_info = fn_dmf_get('quick plot')
  dataorig = plot_info('data')
  disp_res = plot_info('reso')

  data = REPLICATE(-1.0, disp_res+2, disp_res+2)
  data(1,1) = dataorig

  lvl_arr = plot_info('levels')
  color_idx = plot_info('color')
  color_rgb = WoColorConvert(color_idx, /IndexToColor)
  wht = WoColorConvert(1)
  plot_com  = "contour, data, color=color_rgb, background=Wht, xs=5, ys=5"
  plot_com = plot_com + ", levels=lvl_arr, /overplot, thick=fn_dmf_get('line thick')"
  results = execute(plot_com)
        
  if isaskey(plot_info,'outliers') then begin
    dout = plot_info('outliers')
    print, "Contour Quick Plot"
    info, dout

       psym = 2
       if (fn_dmf_exists('psym')) then psym = fn_dmf_get('psym')
       dotsize = 0.25
       if fn_dmf_exists('dot size') then dotsize = float(fn_dmf_get('dot size'))
       info, psym
       info, dotsize

       oplot, dout(0,*),dout(1,*), psym=psym,symsize=dotsize, color=color_rgb, back=wht


;    oplot, dout(0,*),dout(1,*), psym=2,symsize=0.01, color=color_rgb, back=wht, xs=5, ys=5
  endif

end

; dgp rev 10/19/05 draw FCS version 3 histograms
; dgp rev 10/19/05 place x axis into DMF directly
pro _pr_dsp_draw_histo3

_pr_gen_ident ; dgp rev 1/28/10 debugging

  lgln_state = fn_dmf_get_xaxis()
  print, "X Axis state ",lgln_state

  plot_info = fn_dmf_get('quick plot')
  data = plot_info('data')
  range = plot_info('range')
  color_idx = plot_info('color')
  color_rgb = WoColorConvert(color_idx, /IndexToColor)
  wht = WoColorConvert(1)
  
  line = plot_info('line style')
  
  res = n_elements(data)
  
  if (lgln_state eq 1) then begin
    hist_rng = interpol([100.,range(1)],res)
    plot_com  = "plot_oi, hist_rng, data, color=color_rgb, background=Wht, xs=1, ys=1, linestyle=line"
    plot_com  = plot_com + ", thick=fn_dmf_get('line thick')"
  endif else begin
    hist_rng = interpol(range,res)
    plot_com  = "plot, hist_rng, data, color=color_rgb, background=Wht, xs=1, ys=1, linestyle=line"
    plot_com  = plot_com + ", thick=fn_dmf_get('line thick')"
  endelse
        
  ..locals 6 0
  results = execute(plot_com)
  
  _pr_dsp_init

end
; dgp rev 1/5/06 draw histogram for FCS v2.0 data
pro _pr_dsp_draw_histo

_pr_gen_ident ; dgp rev 1/28/10 debugging

  plot_info = fn_dmf_get('quick plot')
  data = plot_info('data')
  color_idx = plot_info('color')
  color_rgb = WoColorConvert(color_idx, /IndexToColor)
  wht = WoColorConvert(1)
  
  line = plot_info('line style')
  
  plot_com  = "oplot, data, color=color_rgb, background=Wht, xs=5, ys=5, linestyle=line"
  plot_com  = plot_com + ", thick=fn_dmf_get('line thick')"
        
  ..locals 6 0
  results = execute(plot_com)

end
;dgp rev 1/5/06 dot size can be changed
pro _pr_dsp_draw_dot

  plot_info = fn_dmf_get('quick plot')
  hist1 = plot_info('hist1')
  hist2 = plot_info('hist2')
  color_idx = plot_info('color')
  color_rgb = WoColorConvert(color_idx, /IndexToColor)
  wht = WoColorConvert(1)
  dotsize = float(fn_dmf_get('dot size'))
  
  psym = fn_dmf_get('psym')
  if (psym eq 0) then psym = 2
  
  plot_com  = "oplot, hist1, hist2, psym=psym, symsize=dotsize, color=color_rgb, background=Wht, xs=5, ys=5"
        
  ..locals 6 0
  results = execute(plot_com)

end

pro _pr_scale_defaults

_pr_gen_ident ; dgp rev 1/28/10 debugging

  if (fn_dmf_get('x lo') eq 0) then _pr_dmf_set, 'x lo', 0
  if (fn_dmf_get('x hi') eq 0) then _pr_dmf_set, 'x hi', 7
  if (fn_dmf_get('y lo') eq 0) then _pr_dmf_set, 'y lo', 0
  if (fn_dmf_get('y hi') eq 0) then _pr_dmf_set, 'y hi', 7

end

; dgp rev 9/30/05 FCS 3.0 dot plot
pro _pr_dsp_draw_dot3

  x_state = fn_dmf_get_xaxis()
  y_state = fn_dmf_get_yaxis()

  plot_info = fn_dmf_get('quick plot')
  hist1 = plot_info('hist1')
  hist2 = plot_info('hist2')
  color_idx = plot_info('color')
  color_rgb = WoColorConvert(color_idx, /IndexToColor)
  wht = WoColorConvert(1)
  
  x_lo = fn_dmf_get('x lo')
  x_rng_lo = double(10)^x_lo
  if (x_lo eq 7) then x_rng_lo = min(hist1)  
  if (x_lo eq 0) then x_rng_lo = min(hist1)

  x_hi = fn_dmf_get('x hi')
  x_rng_hi = double(10)^x_hi
  if (x_hi eq 7) then x_rng_hi = max(hist1) 
  if (x_hi eq 0) then x_rng_hi = max(hist1)

  y_lo = fn_dmf_get('y lo')
  y_rng_lo = double(10)^y_lo
  if (y_lo eq 7) then y_rng_lo = min(hist2)
  if (y_lo eq 0) then y_rng_lo = min(hist2)

  y_hi = fn_dmf_get('y hi')  
  y_rng_hi = double(10)^y_hi
  if (y_hi eq 7) then y_rng_hi = max(hist2)
  if (y_hi eq 0) then y_rng_hi = max(hist2)

  if (x_state or y_state) then begin
    if (x_state and y_state) then begin
      plot_com  = "plot_oo, "
    endif else if (y_state) then begin
      plot_com  = "plot_io, "
    endif else begin
      plot_com  = "plot_oi, "
    endelse
  endif else begin
    plot_com  = "plot,"
  endelse
  x_rng = [x_rng_lo,x_rng_hi]  
  y_rng = [y_rng_lo,y_rng_hi]  
  
  dotsize = float(fn_dmf_get('dot size'))
  plot_com = plot_com + "xrange=x_rng, yrange=y_rng, "
  plot_com = plot_com + "hist1, hist2, psym=2, symsize=dotsize, color=color_rgb, background=Wht, xs=1, ys=1"
  print, plot_com
        
  ..locals 6 0
  results = execute(plot_com)

  _pr_dsp_init

end

pro _pr_dsp_draw

_pr_gen_ident ; dgp rev 1/28/10 debugging

  plot_info = fn_dmf_get('quick plot')
  if (size(plot_info,/type) ne 11) then return
  if (isaskey(plot_info,'type')) then begin
      if (plot_info('type') eq 'contour') then _pr_dsp_draw_contour
      if (plot_info('type') eq 'histo') then _pr_dsp_draw_histo
      if (plot_info('type') eq 'dot') then _pr_dsp_draw_dot
  endif
  
end

; dgp rev 9/30/05 FCS version 3.0 draw
pro _pr_dsp_draw3

_pr_gen_ident ; dgp rev 1/28/10 debugging

  plot_info = fn_dmf_get('quick plot')
  if (size(plot_info,/type) ne 11) then return
  if (isaskey(plot_info,'type')) then begin
      if (plot_info('type') eq 'contour') then _pr_dsp_draw_contour
      if (plot_info('type') eq 'histo') then _pr_dsp_draw_histo3
      if (plot_info('type') eq 'dot') then _pr_dsp_draw_dot3
  endif
  
end

; dgp rev 8/22/2012 Dual Axis
pro _pr_dsp_axis_dual, disp_res, real_res, par

_pr_gen_ident ; dgp rev 1/28/10 debugging

   Common colour, Wht, Blk

   names = _par_cur_names()
   nlist = [' ',names]
   
   fcsname = fn_dmf_get('ifs key')
      
   res_ratio = float(disp_res)/float(real_res)

   xlow = (disp_res-1) * fn_dmf_xmin_ratio()
   xhi = (disp_res-1) * fn_dmf_xmax_ratio()

   ylow = (disp_res-1) * fn_dmf_ymin_ratio()
   yhi = (disp_res-1) * fn_dmf_ymax_ratio()

   x_rng = [xlow,xhi]
   y_rng = [ylow,yhi]

   lbl_x = fn_lgln_get('x labels')
   tk_max_x = fn_lgln_get('x max ticks')*res_ratio
   tk_min_x = fn_lgln_get('x min ticks')*res_ratio
   lbl_y = fn_lgln_get('y labels')
   tk_max_y = fn_lgln_get('y max ticks')*res_ratio
   tk_min_y = fn_lgln_get('y min ticks')*res_ratio
   blnk_x = fn_lgln_get('x blanks')
   blnk_y = fn_lgln_get('y blanks')

   xidx = where(tk_max_x ge xlow and tk_max_x le xhi,xcnt) 
   yidx = where(tk_max_y ge ylow and tk_max_y le yhi,ycnt) 

   frame_com = "plot, x_rng, y_rng, /nodata, xs=1, ys=1, background=wht, color=blk, "
   if xcnt ne 0 then frame_com = frame_com + "xtickname=lbl_x(xidx), xticks = xcnt, xtickv=tk_max_x(xidx), "
   if ycnt ne 0 then frame_com = frame_com + "ytickname=lbl_y(yidx), yticks = ycnt, ytickv=tk_max_y(yidx), "
   frame_com = frame_com + "xtitle=nlist(par(0)), ytitle=nlist(par(1)) "         

;   on_ioerror, problem

  ..locals 10 0
   results = execute(frame_com)
   
   pm, results
   
   on_error_goto, null

   return
   
   problem:
   print, !err
   print, !err_string
   
end

; dpg rev 5/23/2012 Contour Plot
; dpg rev 6/2/2010 Contour Plot
PRO _pr_dsp_contour

_pr_gen_ident ; dgp rev 1/28/10 debugging

   Common colour, Wht, Blk
   common contour_info, orig_dat, mod_dat
      
   active_win = !d.window

   real_res = _dsp_get_norm()

   disp_res = fn_dmf_calc_res()

   res_ratio = float(disp_res)/float(real_res)

   trm = fix(32*res_ratio)   

   frc_rng = [0,disp_res-1]
       
   ;disp_res = _get_bincnt()
   hist_binsize = [real_res/disp_res,real_res/disp_res]

   plot_range  = [0,disp_res-1]
   hist_range = [[0,real_res-1],[0,real_res-1]]

   par = _cdr_get_pars()
   
   cindex = _ssl_init_color()

   _pr_dsp_axis_dual, disp_res, real_res, par

   if (fn_dmf_ready()) then begin
   
     hist1 = _ssl_get_set(par(0))
     hist2 = _ssl_get_set(par(1))
     
     orig_dat = float(_hist([[hist1],[hist2]],b=hist_binsize,r=hist_range))

     _stats_set_orig, orig_dat

     mod_dat = orig_dat

     _manip_data, mod_dat
     
     _calc_levels
     
     lvl_arr = _dmf_get_lvl_arr()
     
;     mod_dat = mod_dat(trm:*,trm:*)
;     mod_dat(0:trm,0:trm) = 0
;     mod_dat = resamp(mod_dat,disp_res,disp_res,/inter)
;    manip = mod_dat
  
     _stats_set_manip, mod_dat
     _stats_dsp_correlation

     no_lab = REPLICATE(0, n_elements(lvl_arr))
     
;     print, "Loop ",plot_index

     color = WoColorConvert(cindex, /IndexToColor)   

     xtitle = fn_par_name(par(0)-1)
     ytitle = fn_par_name(par(1)-1)

     plot_info = asarr('data',mod_dat)
     plot_info = [plot_info,asarr('levels',lvl_arr)]
;     plot_info = [plot_info,asarr('outliers',outliers)]
     plot_info = [plot_info,asarr('type','contour')]
     plot_info = [plot_info,asarr('xtitle',xtitle)]     
     plot_info = [plot_info,asarr('ytitle',ytitle)]     
     plot_info = [plot_info,asarr('reso',disp_res)]
     plot_info = [plot_info,asarr('real',real_res)]
     plot_info = [plot_info,asarr('color',fn_dmf_get('color'))]

     _pr_dmf_set, 'quick plot', plot_info

     if(fn_dmf_get('outliers')) then begin
     
       _pr_dsp_prep_outliers
       outliers = fn_calc_outliers()
       dout = (outliers*float(disp_res))/float(real_res)
       print, "Contour Routine"
       info, dout
       plot_info = [plot_info,asarr('outliers',dout)]
       _pr_dmf_set, 'quick plot', plot_info

;       psym = fn_dmf_get('psym')
;       if (psym eq 0) then psym = 2
;       dotsize = float(fn_dmf_get('dot size'))
;       info, psym

;       oplot, dout(0,*),dout(1,*), psym=psym,symsize=dotsize, color=fn_dmf_get('color'), back=wht

     endif

     _pr_dsp_draw

     _pr_dsp_init

   endif else begin
   
     print, "No data to display"
        
     _pr_dsp_no_data

   endelse
         
END

pro _cdr_set_sense

_pr_gen_ident ; dgp rev 1/28/10 debugging

  par  = _cdr_get_pars()
  mode = fn_dmf_get('format')

  if (fn_dmf_single()) then begin
    _sense_mod,'levels','/nonsensitive' 
    _sense_mod, 'DualInfo', '/nonsensitive'
    _sense_mod, 'HistInfo', '/sensitive'
    _sense_mod, 'DualType', '/nonsensitive'
  endif else if (mode eq 'dot') then begin
    _sense_mod,'levels','/nonsensitive' 
    _sense_mod, 'DualInfo', '/sensitive'
    _sense_mod, 'HistInfo', '/nonsensitive'
    _sense_mod, 'DualType', '/sensitive'
  endif else if (mode eq 'contour') then begin
    _sense_mod,'levels','/sensitive' 
    _sense_mod, 'DualInfo', '/sensitive'
    _sense_mod, 'HistInfo', '/nonsensitive'
    _sense_mod, 'DualType', '/sensitive'
  endif else begin
    _sense_mod,'levels','/nonsensitive' 
    _sense_mod, 'DualInfo', '/sensitive'
    _sense_mod, 'HistInfo', '/nonsensitive'
    _sense_mod, 'DualType', '/sensitive'
  endelse

end
; dgp rev 11/9/05 grab the focus away from handler
function fn_focus_grab, wid

  disp_name = fn_wid_parent_name(wid)
  _pr_dmf_switch, disp_name

  info, calls=tree, level=1
  vals = strsplit(tree(0)," ")
  _pr_gen_set, 'callback', vals(0)  

  return, disp_name

end
; dgp rev 11/9/05 release the grab on the focus handler
pro _pr_focus_release

_pr_gen_ident ; dgp rev 1/28/10 debugging

  _pr_gen_set, 'callback', ""

end

; dgp rev 11/9/05 Drawing area callback
; verify the widget display name, then force
; the switch and disable any focus change until
; the end
PRO DCB_Draw, wid, winindex

  _pr_gen_report1, "BeginDCB_Draw" ; dgp rev 1/28/10 debugging

  disp_name = fn_focus_grab(wid)

  print, "Draw CB for ",disp_name
  Draw_cells

  _pr_focus_release

  _pr_gen_report1, "EndDCB_Draw" ; dgp rev 1/28/10 debugging

END

PRO CB_format, wid, idx

_pr_gen_ident ; dgp rev 1/28/10 debugging

  val = dgpGetValue(wid)
  
  if (val ne 0) then begin
  
    if (idx eq 1) then begin
      _pr_dmf_set, 'format', 'dot'
    endif else if (idx eq 2) then begin
      _pr_dmf_set, 'format', 'contour'
    endif else begin
      _pr_dmf_set, 'format', 'surface'
    endelse
    _cdr_set_sense
    Draw_Cells

  endif
  
END

pro _dmf_reflect_res

  dmf_res_wid = fn_wra_get('reso pulldown',"Display Form")

  if (size(dmf_res_wid,/ndim) eq 0) then return

  if (not fn_wid_exists(dmf_res_wid(0))) then return 

  if (long(product(_cdr_get_pars())) eq 0) then begin
    cur_res = fn_dmf_get('hist res') - 3
  endif else begin
    cur_res = fn_dmf_get('dual res') - 3
  endelse
  status = WtSet(dmf_res_wid(0), {, whichItem: cur_res})

end

; dgp rev 10/23/05 callback for  parameter toggles
Pro cb_par_tog, wid, index

_pr_gen_ident ; dgp rev 1/28/10 debugging
  ; on or off flag
  flag = dgpGetValue(wid)
  if(not flag) then  return
  
  ; get current parameter settings
  cdr_pars = fn_dmf_get('pars')

  p1vals = fn_wra_get('Par 1', "Control Panel")

  ; which column was pressed
  sel = 1
  if (wid eq p1vals(index-1)) then sel = 0
  
  ; did the value change
  if(cdr_pars(sel) ne index-1) then begin
    cdr_pars(sel) = index-1
    _pr_dmf_set, 'pars', cdr_pars
    ; xxx only need to reflect the parameter change
    ; xxx not the entire control panel
; dgp rev 4/27/2010 must reflect entire control panel
    _pr_reflect_cdr
    draw_cells
  endif


END

pro reset_CB, wid, index

_pr_gen_ident ; dgp rev 1/28/10 debugging




end

pro Use_Clusters, wid, index

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common active, use_clusters, use_gates

  use_clusters = dgpGetValue(wid)

  if (use_gates) then begin
    ;print, "Use gates"
  endif
  if (use_clusters) then begin
    ;print, "Use clusters"
  endif

end

pro Use_gates, wid, index

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common active, use_clusters, use_gates

  use_gates = dgpGetValue(wid)
  
  if (use_gates) then begin
    ;print, "Use gates"
  endif
  if (use_clusters) then begin
    ;print, "Use clusters"
  endif

end

pro CB_lvl_mn, wid, which

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common levels, lvl_mn, lvl_t_add, lvl_add, lvl_mult, lvl_t_mult, lvl_cnt, lvl_t_cnt

  value = dgpGetValue(wid)
  print, "Value ",value

  draw_cells

end

pro CB_Lvl_add, wid, which

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common levels, lvl_mn, lvl_t_add, lvl_add, lvl_mult, lvl_t_mult, lvl_cnt, lvl_t_cnt

  value = dgpGetValue(wid)

  print, "Value ",value

  draw_cells

end

pro CB_dmf_Lvlmult, wid, which

_pr_gen_ident ; dgp rev 1/28/10 debugging

  value = dgpGetValue(wid)
  
  _dmf_set_lvlmult, value

  ;print, "Change Multiple to ",value

  draw_cells

end

pro CB_dmf_Lvlmin, wid, which

_pr_gen_ident ; dgp rev 1/28/10 debugging

  value = dgpGetValue(wid)
  
  _dmf_set_lvlmin, value

  ;print, "Change Minimum to ",value

  draw_cells

end

pro CB_tog_cnt, wid, which

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common levels, lvl_mn, lvl_t_add, lvl_add, lvl_mult, lvl_t_mult, lvl_cnt, lvl_t_cnt

  value = dgpGetValue(wid)
  
  print, "Value ",value
  
  draw_cells

end

pro CB_Print, wid, index

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common print1, printer_name

  if (size(printer_name,/type) ne 0) then begin

    wprint, 3, printer_name=printer_name
  
  endif else begin
  
    wprint, 3

  endelse

end

pro CB_Printer, wid, index

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common print1, printer_name

  printer_name = WIN32_PICK_PRINTER()

end

pro _pr_dmf_reflect

_pr_gen_ident ; dgp rev 1/28/10 debugging

     print, "Reflect the new DMF settings"

     _pr_wra_set_wid, 'lin smo on', "Smooth Menu", (fn_dmf_get('smo on') eq 1)
     _pr_wra_set_wid, 'lin smo width', "Smooth Menu",fn_dmf_get('smo width')
     _pr_wra_set_wid, 'lin smo rep', "Smooth Menu",fn_dmf_get('smo rep')

     _pr_wra_set_wid, 'log smo on', "Smooth Menu", (fn_dmf_get('quad on') eq 1)
     _pr_wra_set_wid, 'log smo width', "Smooth Menu",fn_dmf_get('quad width')
     _pr_wra_set_wid, 'log smo rep', "Smooth Menu",fn_dmf_get('quad rep')
     _pr_wra_set_wid, 'log smo degree', "Smooth Menu",fn_dmf_get('quad degree')
     
     _pr_dsp_reflect
     
     _ger_reflect_wid

     _dmf_reflect_type
     _dmf_reflect_res
     
     _pr_wra_set_wid, 'ymax', "Control Panel", string(fn_dmf_get('ymax perc'))
     _pr_wra_set_wid, 'cur display', "Control Panel",fn_dmf_current()
     _pr_wra_set_wid, 'cur file', "Control Panel",fn_dmf_get('ifs key')

     _pr_reflect_pars

end

; dgp rev 10/23/05 reflect the current smooth settings
pro _pr_reflect_smo

  frm_name = "Smooth Menu"

  if (not fn_wms_exists(frm_name)) then return

     _pr_wra_set_wid, 'lin smo on', frm_name, (fn_dmf_get('smo on') eq 1)
     _pr_wra_set_wid, 'lin smo width', frm_name,fn_dmf_get('smo width')
     _pr_wra_set_wid, 'lin smo rep', frm_name,fn_dmf_get('smo rep')

     _pr_wra_set_wid, 'log smo on', frm_name, (fn_dmf_get('quad on') eq 1)
     _pr_wra_set_wid, 'log smo width', frm_name,fn_dmf_get('quad width')
     _pr_wra_set_wid, 'log smo rep', frm_name,fn_dmf_get('quad rep')
     _pr_wra_set_wid, 'log smo degree', frm_name,fn_dmf_get('quad degree')

end

; dgp rev 10/23/05 reflect the current resolution pulldown
pro _pr_reflect_reso

    dmf_res_wid = fn_wra_get('reso pulldown',"Display Form")

    if (size(dmf_res_wid,/ndim) eq 0) then return

    if (not fn_wid_exists(dmf_res_wid(0))) then return 

    if (long(product(_cdr_get_pars())) eq 0) then begin
      cur_res = fn_dmf_get('hist res') - 3
    endif else begin
      cur_res = fn_dmf_get('dual res') - 3
    endelse
    status = WtSet(dmf_res_wid(0), {, whichItem: cur_res})

end

; dgp rev 10/23/05 reflect the current file menu
pro _pr_reflect_file

  common fcr_info, menu_wid, fcr_range_slider

    if (size(menu_wid,/type) eq 0) then return
    if (fn_wid_exists(menu_wid)) then begin
      index = fn_dmf_get('file index') + 1
      status = WtSet(menu_wid, {, whichItem: index})
    endif
    
end

; dgp rev 10/28/05 reflect the gate settings
pro _pr_reflect_mgs

  frm_name = "Gate Form"
  
  ; no form, then exist
  if (not fn_wms_exists(frm_name)) then return
  
  print, "Gate Form reflect for display ",fn_dmf_current()
  
  ; grab main form widget
  main = _wms_get_main(frm_name)
  if (not fn_ifs_status()) then begin
    ; if no data, then disable form
    print, "No data for ",fn_dmf_get('ifs key')
    if (fn_wid_exists(main)) then s = WwSetValue(main, /nonsensitive)
    return
  endif
  ; verify form is enabled
  if (fn_wid_exists(main)) then s = WwSetValue(main, /sensitive)

  _pr_wra_set_wid, 'dsp name', "Gate Form", fn_dmf_current()
    
  if (fn_mgs_get_sorted(keys)) then begin
      for i=0,n_elements(keys)-1 do begin
        _pr_wra_set_index, 'mgs outline', "Gate Form", i, fn_mgs_get('outline',keys(i))
        _pr_wra_set_index, 'mgs label', "Gate Form", i, fn_mgs_get('label',keys(i))
        _pr_wra_set_index, 'mgs perc', "Gate Form", i, fn_mgs_get('perc',keys(i))
        _pr_wra_set_index, 'mgs stats', "Gate Form", i, fn_mgs_get('stats',keys(i))
      endfor
  endif

  if (fn_mgs_get_sorted(keys)) then begin
        for i=0,n_elements(keys)-1 do begin
          if (fn_dmf_get('1only') eq keys(i)) then begin
            ; this is the "only one" key
            wid = fn_wra_get_index('1only',"Gate Form",i)
            if (fn_wid_exists(wid)) then s = WwSetValue(wid,/sens)
            _pr_wra_set_index, '1only', "Gate Form", i, 1
            _pr_wra_set_index, 'applied', "Gate Form", i, 0
          endif else begin
            ; check state
            if (fn_mgs_get_enable(keys(i))) then begin
              wid = fn_wra_get_index('1only',"Gate Form",i)
              if (fn_wid_exists(wid)) then s = WwSetValue(wid,/nonsens)
              wid = fn_wra_get_index('applied',"Gate Form",i)
              if (fn_wid_exists(wid)) then s = WwSetValue(wid,/sens)
              _pr_wra_set_index, 'applied', "Gate Form", i, 1
              _pr_wra_set_index, '1only', "Gate Form", i, 0
            endif else begin
              wid = fn_wra_get_index('1only',"Gate Form",i)
              if (fn_wid_exists(wid)) then s = WwSetValue(wid,/sens)
              wid = fn_wra_get_index('applied',"Gate Form",i)
              if (fn_wid_exists(wid)) then s = WwSetValue(wid,/sens)
              _pr_wra_set_index, 'applied', "Gate Form", i, 0
              _pr_wra_set_index, '1only', "Gate Form", i, 0
            endelse
          endelse
        endfor
  endif

end
; dgp rev 11/2/05 reflect the parlog settings
; if 'force', then assume existance of widget
pro _pr_reflect_parlog, force

  frm_name = "Control Panel"
  
  if (not force) then begin
  ; no form, then exist
    if (not fn_wms_exists(frm_name)) then return
  endif
  
  pars = fn_data_get_par_cnt()
  for i=0,pars-1 do begin
    _pr_par_switch, strtrim(i,2)
    _pr_wra_set_index, 'Par Log', frm_name, i, fn_par_get('log')
  endfor

end

; dgp rev 10/19/05 use WRA to register the widgets
; reflect the control panel settings
; dgp rev 10/23/05 only "Control Panel" reflect and submenus
pro _pr_reflect_cdr

_pr_gen_ident ; dgp rev 1/28/10 debugging

  frm_name = "Control Panel"
  
  ; no form, then exist
  if (not fn_wms_exists(frm_name)) then return
  
  print, "Reflect the current DMF ",fn_dmf_current()
  
  ; grab main form widget
  main = _wms_get_main(frm_name)
  if (not fn_ifs_status()) then begin
    ; if no data, then disable form
    print, "No data for ",fn_dmf_get('ifs key')
    if (fn_wid_exists(main)) then s = WwSetValue(main, /nonsensitive)
    return
  endif
  ; verify form is enabled
  if (fn_wid_exists(main)) then s = WwSetValue(main, /sensitive)

  _pr_reflect_smo
  
  _pr_reflect_parlog, 1
       
  arr = ['dot','contour','surface']
  
  dmf_dual_togs = fn_wra_get('Dual Format', "Control Panel")

  out = fn_wra_get('Outliers', "Control Panel")
  status  = WwSetValue(out(0),(fn_dmf_get('outliers') eq 1))

  if (size(dmf_dual_togs,/type) ne 0) then begin
      if (fn_wid_exists(dmf_dual_togs(0))) then begin
        mode = fn_dmf_get('format')
        idx = where(arr eq mode,cnt)
        ; dgp rev 3/13/08 dual toggles may not be set if format is histogram
        if (cnt ne 0) then s = WwSetValue(dmf_dual_togs(idx(0)),1)
      endif
  endif

  _pr_reflect_dots

  _pr_reflect_reso
  
  _pr_wra_set_wid, 'ymax', "Control Panel", string(fn_dmf_get('ymax perc'))
  _pr_wra_set_wid, 'cur display', "Control Panel",fn_dmf_current()

     ; dgp rev 1/27/10 fix 
  _pr_wra_set_wid, 'cur file', "Control Panel", fn_dmf_get('ifs key')

  _pr_reflect_pars

  par  = _cdr_get_pars()
  mode = fn_dmf_get('format')
    
  if (fn_dmf_single()) then begin
	_sense_mod,'levels','/nonsensitive' 
	_sense_mod, 'DualInfo', '/nonsensitive'
	_sense_mod, 'HistInfo', '/sensitive'
	_sense_mod, 'DualType', '/nonsensitive'
	endif else if (mode eq 'dot') then begin
	_sense_mod,'levels','/nonsensitive' 
	_sense_mod, 'DualInfo', '/sensitive'
	_sense_mod, 'HistInfo', '/nonsensitive'
	_sense_mod, 'DualType', '/sensitive'
	endif else if (mode eq 'contour') then begin
	_sense_mod,'levels','/sensitive' 
	_sense_mod, 'DualInfo', '/sensitive'
	_sense_mod, 'HistInfo', '/nonsensitive'
	_sense_mod, 'DualType', '/sensitive'
	endif else begin
	_sense_mod,'levels','/nonsensitive' 
	_sense_mod, 'DualInfo', '/sensitive'
	_sense_mod, 'HistInfo', '/nonsensitive'
	_sense_mod, 'DualType', '/sensitive'
  endelse
     
end

; dgp rev 8/12/05 select a display from menu
pro mcb_dmf_sel, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  new_dsp = dgpGetValue(wid)

  if (strpos(new_dsp,"Select") ne -1) then return  

  cur_dsp = fn_dmf_current()
    
  if (new_dsp eq cur_dsp) then return

  if (fn_dmf_key_exists(new_dsp)) then begin
    print, "  DMF switch to ",new_dsp
    _pr_dmf_switch, new_dsp
    _pr_dmf_set, 'position', dgpGetValue(fn_dmf_get('main widget'),/position)
    _pr_dmf_set, 'size', dgpGetValue(fn_dmf_get('main widget'),/size)
    _pr_reflect_cdr
  endif
  
end

; dgp rev 11/17/05 create a option menu of active displays
pro _pr_dsp_menu, layout

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_dsp_wid, dsp_menu, dsp_callback
  
     lo_disp1 = WwLayout(layout,/ver)
  
     dsp_list = fn_gen_get('dsp lst')

     wid_name = ['pulldsp','option']
     
     dsp_callback = 'mcb_dmf_sel'
     _pr_gen_set, 'disp callback', 'mcb_dmf_sel'
     stub = {,callback:dsp_callback, $
       button:dsp_list(0)}
     
     dsp_menu = WwOptionMenu(layout, 'Display:', stub, name=wid_name )
     
     _pr_wra_reg, 'disp list', dsp_menu
     
     for i=0,n_elements(dsp_list)-1 do begin
       status = MyMenuItem(dsp_menu(0), i+1, dsp_list(i), dsp_callback, /Add, wid=wid)
     endfor
     
end

; dgp rev 1/4/06 reflect the locked exports on display form
pro _pr_reflect_export

  disp_list = fn_gen_get('dsp lst')
  fmf_flag = 0

  if (fn_wms_exists("Display Form")) then begin  
    for i=1, n_elements(disp_list)-1 do begin 
      value   = fn_dmf_get_other(disp_list(i),'export')
      _pr_wra_set_index, 'export', "Display Form", i-1, value
    endfor
  endif
  
end

; dgp rev 9/2/05 reflect the locked displays on display form
pro _pr_reflect_locks

  disp_list = fn_gen_get('dsp lst')
  fmf_flag = 0

  if (fn_wms_exists("File Form")) then begin
    for i=1, n_elements(disp_list)-1 do begin 
      wid_arr = fn_dmf_get_other(disp_list(i),'fmf wids')
      value   = fn_dmf_get_other(disp_list(i),'locked')
      mov_val   = long(fn_dmf_get_other(disp_list(i),'movement'))
      mov_val   = mov_val mod 2

      info, disp_list(i)
      info, wid_arr
      info, mov_val
      if (size(wid_arr,/ndim) ne 0) then begin

        s = WwSetValue(wid_arr(mov_val),1)
        if (value) then begin
          s = WwSetValue(wid_arr(0),/sensitive)
          s = WwSetValue(wid_arr(1),/sensitive)
        endif else begin
          s = WwSetValue(wid_arr(0),/nonsensitive)
          s = WwSetValue(wid_arr(1),/nonsensitive)
        endelse
      endif

    endfor
  endif

  if (fn_wms_exists("Display Form")) then begin  
    for i=1, n_elements(disp_list)-1 do begin 
      value   = fn_dmf_get_other(disp_list(i),'locked')
      _pr_wra_set_index, 'lock', "Display Form", i-1, value
    endfor
  endif
  
end

; dgp rev 1/27/10 flush the DMF of closed display information
pro _pr_dmf_flush

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_dmf_info, cv_dmf_key, cv_dmf_hash
    
  if (size(cv_dmf_hash,/type) ne 11) then return

  keys = askeys(cv_dmf_hash)
  keys = keys(sort(keys))

  if (n_elements(keys) le 1) then return

  if (isaskey(cv_dmf_hash,'default')) then begin
  
    tmp = asarr('default',cv_dmf_hash('default'))
    for i=0,n_elements(keys)-1 do begin
      if (keys(i) ne 'default') then begin
        if (fn_dmf_key_exists(keys(i))) then begin
          print, "Keep ",keys(i)
          tmp = [tmp,asarr(keys(i),cv_dmf_hash(keys(i)))]
        endif else begin
          print, "Remove ",keys(i)
        endelse
      endif
    endfor

    info, tmp
    info, cv_dmf_hash
    cv_dmf_hash = tmp
  
  endif
  
end

; dgp rev 9/06/05 get all the DMF keys
function fn_dmf_get_keys

  common cm_dmf_info, cv_dmf_key, cv_dmf_hash
    
  if (size(cv_dmf_hash,/type) ne 11) then return, [""] 

  keys = askeys(cv_dmf_hash)
  keys = keys(sort(keys))
  return, keys
  
end

; dgp rev 12/7/05 count the number of active displays
function fn_dmf_ref_cnt

  keys = fn_dmf_get_keys()
  cnt = 0
  if (n_elements(keys) le 1) then return, cnt
  
  for i=0,n_elements(keys)-1 do begin
    if (keys(i) ne 'default') then begin
      if (fn_dmf_key_exists(keys(i))) then cnt = cnt + 1
    endif
  endfor
  return, cnt
  
end

; dgp rev 11/7/05 move refcnt into WRA
pro _pr_reflect_refcnt

  _pr_wra_set, 'ref cnt', fn_dmf_ref_cnt()

end

; dgp rev 11/5/05 callback for dmf save/restore form
pro cb_dsp_form, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  _pr_dmf_form

end

; dgp rev 11/5/05 callback for dmf save/restore form
pro cb_dsp_flush, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  _pr_dmf_flush

end

; dgp rev 9/1/05 creates a form for control display settings
pro _pr_frm_display

_pr_gen_ident ; dgp rev 1/28/10 debugging

   frm_name = "Display Form"
      
   if (fn_wms_form(frm_name,layout)) then return
     
     lo_main  = WwLayout(layout,/hor)
     lo_sub  = WwLayout(lo_main,/ver)


     lo_new      = WwLayout(lo_sub, border=4, /Vert)
     lo_control  = WwLayout(lo_sub, border=4, /Vert)
     lo_state    = WwLayout(lo_sub, border=4, /Vert)
     lo_menus    = WwLayout(lo_main,/ver)

     lo_info  = WwLayout(lo_menus,/ver)
     lo_disp  = WwLayout(lo_menus,/ver)

     _pr_dsp_scan

     _pr_dsp_menu, lo_disp

     _pr_dsp_res_menu, lo_info
          
     _pr_dsp_show,  lo_state

     nudspbut   = WwButtonBox(lo_new, 'New Display',  'cb_dac_new_win')
     txtref   = WwText(lo_new,'NoOpCB',/label,text="Display Count")
     refcnt = WwText(lo_new,'NoOpCB',/label,text=string(fn_dmf_ref_cnt()))
     descr = WwText(lo_new,'NoOpCB',/label,text=string(fn_dmf_get_other('default','description')))
      
     _pr_wra_reg, 'ref cnt', refcnt

     savebut       = WwButtonBox(lo_control, 'Save/Restore', 'cb_dsp_form')
     flushbut      = WwButtonBox(lo_control, 'Flush Displays', 'cb_dsp_flush')
     exitbut       = WwButtonBox(lo_control, 'Close', 'cb_exit')
    
     _pr_wms_display, frm_name

end

; dgp rev 8/15/05 count open windows in DMF
function fn_dmf_open

  common cm_dmf_info, cv_dmf_key, cv_dmf_hash

  if (size(cv_dmf_hash,/type) ne 11) then return, ['default']

  keys = askeys(cv_dmf_hash)

  nkeys = n_elements(keys)

  if (nkeys eq 0) then return, ['default']

  count = 0
  
  arr = ['default']
  closing_key = fn_gen_getstr('closing')
  
  print, "Current key ",cv_dmf_key
  print, "Closing key ",closing_key
  
  for i=0,nkeys-1 do begin
    if (keys(i) ne 'default' and keys(i) ne closing_key) then begin
      if (fn_dmf_key_exists(keys(i))) then begin
        arr = [arr,keys(i)]
      endif
    endif
  endfor
  
  return, arr

end

;dgp rev 12/29/05 initialize the DMF structure
pro _pr_dmf_reinit

  ; restore the DMF structure or create new
  
  print, "Restore DMF settings"
  _pr_dmf_restore
    
  ; fill in identification information for CDS 
  _pr_dmf_id
  
  ; IFS structure should have 1 file loaded
  ; get the PFL file index and the IFS key
  keys = fn_ifs_get_keys()
  ; reset all keys to the same valid file  
  _pr_dmf_set_all, 'ifs key', keys(0)
  _pr_dmf_set_all, 'file index', fn_pfl_get_index(keys(0))
   
  ; continue with init
         
  _ssl_recalc

end

; dgp rev 12/8/05 unwind the closing of all displays
; dgp rev 1/4/06  'file form' reform
pro _pr_dac_unwind

_pr_gen_ident ; dgp rev 1/28/10 debugging

    cnt = fn_gen_get('close count')

    cnt = cnt - 1    

    _pr_gen_set, 'close count', cnt

    if (cnt eq 0) then begin

      print, "*** All displays closed, now reload"

      _pr_dmf_reinit

      _pr_dmf_open_active
      
      _pr_gen_set, 'state', ''
      
      ; rebuild (not reflect) the display form because
      ; display count may be different
      _pr_wms_set, 'state', 'reform'
      _pr_frm_display
      ; reflect new display info on control panel
      _pr_reflect_cdr

; dgp rev 5/26/2010 don't think this update is needed
;      _pr_dmf_update
      ; reform the file movement form
      ; the number of displays might have changed
      _pr_wms_set, 'state', 'reform'
      _pr_frm_files

    endif else begin
      ; break out until last display is closed
      print, "Break out"
    endelse

end

; dgp rev 1/28/10 
function fn_dmf_any_open

  keys = fn_dmf_get_keys()

  cnt = n_elements(keys)
  
  if (cnt eq 0) then return, ""

  for i=0, cnt-1 do begin

    line = keys(i)
    wid = fn_dmf_get_other(keys(i),'main widget')
    line = line + " wid " + strtrim(wid,2)
    print, line
    if (wid ne 0) then begin
      if (fn_wid_exists(wid)) then begin
        if (keys(i) eq WtGet(wid,/name)) then return, keys(i)
      endif
    endif

  endfor

  return, ""

end



; dgp rev 12/8/05 close a single display
pro _pr_dac_close_one, parent

_pr_gen_ident ; dgp rev 1/28/10 debugging

; zero out the list reference

_pr_gen_ident ; dgp rev 1/28/10 debugging
print, "*** did NOT abort _pr_dac_close"
  if (parent ne 0) then begin
    par_name = WtGet(parent,/name)
    print, "Closing ",par_name
    _pr_gen_set, 'closing', par_name
    print, "Get position before closing"
    pos = dgpGetValue(parent,/position)
    _pr_dmf_set, 'position', pos
    _pr_dmf_set_other, par_name, 'main widget', 0
  endif
  
; if other window are open, set current window to first available
  any = fn_dmf_any_open()
  print, "Default to any open display -- ",any
  _pr_dmf_switch, any
  
; adjust reference counter (-1)
  _pr_reflect_refcnt
 
  _pr_reflect_cdr

  ; rebuild the display list if present
  _pr_wms_set, 'state', 'reform'
  _pr_frm_display
  _pr_reflect_locks  

end
; dgp rev 10/18/05 close a display
; 3 basic states
; - normal close of a single display
; - special close of all displays
; - close specifically for reopen
pro _pr_dac_close, parent

_pr_gen_ident ; dgp rev 1/28/10 debugging

_pr_gen_ident ; dgp rev 1/28/10 debugging
  if (fn_gen_getstr('state') eq 'reload') then begin
    _pr_dac_unwind
    return
  endif

  if (fn_dmf_get('state') eq 'reopen') then begin
    ; reopen the display, no need to reflect the settings
    cur_dsp = fn_dmf_current()
    _pr_dac_create, cur_dsp
    return
  endif

  _pr_dac_close_one, parent

end

; dgp rev 8/15/05 reorganize close
pro ccb_dac_close, shell, lo_wid

_pr_gen_ident ; dgp rev 1/28/10 debugging

; determine the parent
; if program exit, then forget the bookkeeping
  if (fn_gen_getstr('state') eq "exit") then return
    
; determine the parent
  parent = _wid_get_parent(lo_wid)

  _pr_dac_close, parent

end

pro _pr_ppt_set, label, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_ppt_info, cv_ppt_hash
  
  if (size(cv_ppt_hash,/type) ne 11) then begin
    cv_ppt_hash = asarr('rows','4')
    cv_ppt_hash = [cv_ppt_hash,asarr('cols','4')]
  endif

  cv_ppt_hash = [cv_ppt_hash,asarr(label,value)]

end

function fn_ppt_get, label

  common cm_ppt_info, cv_ppt_hash
    
  if (size(cv_ppt_hash,/type) ne 11) then begin
    cv_ppt_hash = asarr('rows','4')
    cv_ppt_hash = [cv_ppt_hash,asarr('cols','4')]
  endif

  if (isaskey(cv_ppt_hash,label)) then return, cv_ppt_hash(label)

  return, 0

end

function fn_dsp_win, wid

  common cm_disp_info, cv_name_hash
  
  name = WtGet(_wid_get_parent(wid),/name)
  
  if (size(cv_name_hash,/type) ne 11) then return, -1
  keys = askeys(cv_name_hash)
  idx = where(keys eq name,cnt)
  if (cnt eq 0) then return, -1
  return, cv_name_hash(keys(idx(0)))

end

; status = image_write( outfilename, image_create(tvrd()) )

; dgp rev 12/9/05 export the specified window to PowerPoint
pro _pr_ppt_export, win

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cv_ppt_info, cv_ppt_hash

  if (win eq -1) then return

    path = _cfg_get_work()
    file = fn_gen_get('unique name')
    if (fn_gen_get('single')) then begin
      exportname = fn_gen_build_path(_cfg_get_work(),fn_gen_get('unique name')+".emf1")
    endif else begin
      exportname = fn_gen_build_path(_cfg_get_work(),fn_gen_get('unique name')+".emf2")
    endelse

    lst = FINDFILE(exportname, Count=cntr)
    if (cntr gt 0) then del_file, exportname
    print, exportname
    status = wwrite_meta(win, filename=exportname)
    if (status lt 0) then begin
      msg_str = DC_ERROR_MSG(status)
      print, msg_str
    endif
  
end
; dgp rev 12/9/05 export to PowerPoint
; dgp rev 12/9/05 revised export to handle multiple displays
; dgp rev 1/9/06 determine if single parameter before export
pro _pr_dmf_export, key

_pr_gen_ident ; dgp rev 1/28/10 debugging

  keys = fn_dmf_get_keys()
  index = 0
  _pr_gen_set, 'unique name', fn_gen_unique()+strtrim(index,2)

  if (fn_dmf_get_other(key,'export')) then begin
    for i=0,n_elements(keys)-1 do begin
    ; check to see if export is set for each display
      if (fn_dmf_get_other(keys(i),'export')) then begin
        win = fn_dmf_get_other(keys(i),'window')
        _pr_gen_set, 'single', (product(fn_dmf_get_other(keys(i),'pars')) eq 0)
        _pr_ppt_export, win
        index = index + 1
        _pr_gen_set, 'unique name', fn_gen_unique()+strtrim(index,2)
      endif
    endfor
  endif else begin
    ; display is not linked to multiple export
    ; simply export this one display
        win = fn_dmf_get_other(key,'window')
        _pr_gen_set, 'single', (product(fn_dmf_get_other(key,'pars')) eq 0)
        _pr_ppt_export, win
  endelse
end

pro cb_dex_ppt, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  internal = dgpGetValue(wid)
  name     = WtGet(wid,/name)
  print, name," Value: ",value," Internal: ",internal

end

pro cb_save_ppt, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  u_dat = dgpGetValue(wid, /userdata)
  _pr_ppt_set, 'rows', dgpGetValue(u_dat(0))
  _pr_ppt_set, 'cols', dgpGetValue(u_dat(1))
  parent = _wid_get_parent(wid)
  s = WwSetValue(parent,/close)

end

pro _pr_dex_setup

_pr_gen_ident ; dgp rev 1/28/10 debugging

    rows = fn_ppt_get('rows')   
    cols = fn_ppt_get('cols')   
  
    frm_name = "PPT Setup"
  
     if (fn_wms_form(frm_name,layout)) then return
    
     widget_layout = WwLayout(layout, /Vertical)
    
     row = WwText(widget_layout,'cb_dex_ppt',text=string(fn_ppt_get('rows'), format="(I4)"), $
                  label='Rows', cols=5,name='rows')
     col = WwText(widget_layout,'cb_dex_ppt',text=string(fn_ppt_get('cols'), format="(I4)"), $
                  label='Columns', cols=5,name='cols')
     x_but = WwButtonBox(widget_layout,'Apply','cb_save_ppt')
     u_dat = [row,col]
     s = WwSetValue(x_but,userdata=u_dat)
    
     _pr_wms_display, frm_name
     
end

pro cb_dex_setup, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  _pr_dex_setup

end

; dgp rev 10/12/05 set font for specific display
; dgp rev 10/18/05 move font from general to font namespace
pro cb_dmf_font, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  def_font = fn_dmf_get_str('font')
  if (def_font eq "") then def_font = fn_fnt_get('dmf font')
  print, "Default font ",def_font
  font_string = win32_pick_font(/scal,default=def_font)
  _pr_dmf_set, 'font', font_string
  device, font=font_string

end
; dgp rev 11/9/05 apply the new DMF settings
; dgp rev 1/5/06 make styles display specific
pro cb_save_style, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  frm_name = "Style Form"
  ; grab and set the char size
  widarr = fn_wra_get('char size',frm_name)
  charsize = dgpGetValue(widarr(0))
  _pr_dmf_set, 'char size', charsize
;  if (val ne '') then !P.charsize = val

  widarr = fn_wra_get('line thick',frm_name)
  thick = dgpGetValue(widarr(0))
  _pr_dmf_set, 'line thick', thick
;  if (val ne '') then !P.thickness = val

  widarr = fn_wra_get('dot size',frm_name)
  dotsize = dgpGetValue(widarr(0))
  _pr_dmf_set, 'dot size', dotsize
;  if (val ne '') then !P.thickness = val

  ; close the form
  parent_wid = _wid_get_parent(wid)
  if (fn_wid_exists(parent_wid)) then begin 
    status = WtClose(parent_wid)
  endif

  ; redraw with new settings
  draw_cells

end

; dgp rev 8/26/05 draws the color selection grid
PRO dcb_dmf_csg, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

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

; dgp rev 8/26/05 draws the dmf color patch
PRO dcb_dmf_patch, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  color_idx = fn_dmf_get('color')
  
  sze = dgpGetValue(wid,/size)
  img = rebin(bindgen(1)+color_idx,sze(0),sze(1))
  
  tv, img

END

; dgp rev 8/25/05 advance the current displays color
pro bphd_dmf_patch_adv, wid, shell, event

_pr_gen_ident ; dgp rev 1/28/10 debugging
  
  print, "Single advance of dmf patch"
  
  color_idx = fn_dmf_get('color')

  color_idx = color_idx + 1

  _pr_dmf_set, 'color', color_idx
        
  old_win = !d.window
  
  patch_wid = fn_gen_get('dmf patch wid')
  patch_win = fn_gen_get('dmf patch win')
 
  wset, patch_win
 
  sze = dgpGetValue(patch_wid,/size)

  img = rebin(bindgen(1)+color_idx,sze(0),sze(1))
  tv, img

  wset, old_win

end

; dgp rev 8/25/05 advance the gate color
pro bphd_mgs_patch_adv, wid, shell, event

_pr_gen_ident ; dgp rev 1/28/10 debugging
  
  print, "Single advance of dmf patch"
  
  color_idx = fn_gen_get('mgs color')

  color_idx = color_idx + 1

  _pr_gen_set, 'mgs color', color_idx
        
  old_win = !d.window
  
  patch_wid = fn_gen_get('mgs patch wid')
  patch_win = fn_gen_get('mgs patch win')
 
  wset, patch_win
 
  sze = dgpGetValue(patch_wid,/size)

  img = rebin(bindgen(1)+color_idx,sze(0),sze(1))
  tv, img

  wset, old_win

end

; dgp rev 11/9/05 change the current DMF line type
pro cb_change_line, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_gen_style, cv_gen_line

  if (size(cv_gen_line,/type) eq 0) then $
    cv_gen_line = ['solid','dot','dash','dash dot','dash dots','long dash']

  new_line = fn_dmf_get('line') + 1
  
  if (new_line ge n_elements(cv_gen_line)) then new_line = 0
  
  _pr_dmf_set,'line',new_line
  
  s = WtSet(wid,{, text: cv_gen_line(new_line)})

end
; dgp rev 1/5/06 set the line thickness for the
; dgp rev 1/5/06 current display
pro cb_dsp_thick, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  value = dgpGetValue(wid)
  print, "Changing line thickness to ",value
  _pr_dmf_set, 'line thick', value

end
; dgp rev 1/5/06 change the size of the dot symbol
pro cb_dsp_dot, wid, value

  value = dgpGetValue(wid)
  print, "Changing dot size to ",value
  _pr_dmf_set, 'dot size', value

end

pro cb_dsp_charsize, wid, value

  value = dgpGetValue(wid)
  print, "Changing char size to ",value
  _pr_dmf_set, 'char thick', value

end

function fn_win_dsp_color

  device, window_state=winarr
  win = max(where(winarr eq 0))
  _pr_gen_set, 'dsp win', win
  return, win

end

; dgp rev 8/25/05 color selection grid handler
PRO bphd_dmf_csg, wid, shell, event

_pr_gen_ident ; dgp rev 1/28/10 debugging

  ; Remove event handler
  
  patch_win = fn_gen_get('dmf patch win')
  patch_wid = fn_gen_get('dmf patch wid')

  patch_sze = dgpGetValue(patch_wid,/size)
  csg_sze   = dgpGetValue(wid,/size)

  ;  save the current window
  old_win = !d.window

  ; Make csg window the active window
  ; necessary for CURSOR to recognize the mouse click in this window
  WSET, fn_gen_get('dmf csg win')

  pnt = WwGetPosition(event)

  x = pnt(0)
  ; invert y for some reason
  y = csg_sze(1) - pnt(1)

  color_idx = tvrd(x, y,1,1)

  _pr_dmf_set,'color', color_idx(0)
  wset, patch_win

  img = rebin(bindgen(1)+color_idx,patch_sze(0),patch_sze(1))
  tv, img
    
  wset, old_win

END

; dgp rev 3/16/2010
pro CB_cust_dot, wid, value

  val = 2
  if (WwGetValue(wid) eq 1) then val = 8
  _pr_dmf_set, 'psym', val

end

; dgp rev 8/25/05 form for selecting display style options
pro _pr_dsp_style, key

_pr_gen_ident ; dgp rev 1/28/10 debugging

  frm_name = "Style Form"

  _pr_wms_set, 'state', 'reform'
  if (fn_wms_form(frm_name,layout)) then return

    print, "Called from ",key

    sp = 2
    bd = 1

    lo_main  = WwLayout(layout,  /hor,spac=sp,bord=bd)
    lo_form1 = WwLayout(lo_main, /ver,spac=sp,bord=bd)
    lo_form2 = WwLayout(lo_main, /ver,spac=sp,bord=bd)
    lo_form3 = WwLayout(lo_main, /ver,spac=sp,bord=bd)

    ;if (!p.charsize eq 0) then !p.charsize = 1
    lbl      = WwText(lo_form1,'NoOpCB',/label,text='Char Size')
    cszwid   = WwText(lo_form1,'cb_dsp_charsize',text=string(fn_dmf_get('char size')))
    _pr_wra_reg, 'char size', cszwid

    ;if (!p.thick eq 0) then !p.thick = 1
    lbl      = WwText(lo_form1,'NoOpCB',/label,text='Line Thickness')
    ltkwid   = WwText(lo_form1,'cb_dsp_thick',text=string(fn_dmf_get('line thick')))
    _pr_wra_reg, 'line thick', ltkwid

    lbl      = WwText(lo_form1,'NoOpCB',/label,text='Dot Size')
    ltkwid   = WwText(lo_form1,'cb_dsp_dot',text=string(fn_dmf_get('dot size')))
    _pr_wra_reg, 'dot size', ltkwid

    fntbut   = WwButtonBox(lo_form1, 'Font', 'cb_dmf_font')

    name_wid = WwText(lo_form3,'NoOpCB', /label, text=fn_dmf_current())
    exitbut  = WwButtonBox(lo_form3, 'Apply', 'CB_save_style')
    custbut  = WwRadioBox(lo_form3, 'Custom', 'CB_cust_dot', toggle=tog, /nofmany)
    if (fn_dmf_get('psym') eq 8) then begin
      s= WwSetValue(tog(0),1)
    endif else begin
      s= WwSetValue(tog(0),0)
    endelse    
    
    color_idx = fn_dmf_get('color')
    
;    color_name = _ct_get_name(color_idx)
    
    draw1  = WwDrawing(lo_form2, free_win1, 'dcb_dmf_patch', [20,20], [20,20], $
                    /NoScroll, area=drawarea1)

    if (draw1 eq 0) then _pr_dac_error

    linbut   = WwButtonBox(lo_form2, 'Solid Line', 'cb_change_line')

    draw2  = WwDrawing(lo_form2, free_win2, 'dcb_dmf_csg', [60,60], [60,60], $
                    /NoScroll, area=drawarea2)

    if (draw2 eq 0) then _pr_dac_error

    _pr_gen_set, 'dmf patch wid', draw1
    _pr_gen_set, 'dmf patch win', free_win1
    _pr_gen_set, 'dmf csg win', free_win2

    status = WwHandler(drawarea1, 'bphd_dmf_patch_adv', 'ButtonPressMask')
    status = WwHandler(drawarea2, 'bphd_dmf_csg', 'ButtonPressMask')

    _pr_wms_display, frm_name

end

PRO kphd_draw_key, wid, shell, event

_pr_gen_ident ; dgp rev 1/28/10 debugging

  key = WwGetKey(event, State = state, Keysym = keysym)
  
  print, "Keysym is ",keysym  
  if (keysym eq 39) then begin
    CB_Next
  endif
  if (keysym eq 37) then begin
    CB_Prev
  endif

end


; dgp rev 2/25/2010
pro _dsp_reflect_menu, index

  common cm_dsp_wid, dsp_menu, dsp_callback
  
  if (size(dsp_menu,/type) eq 0) then return
  if (fn_wid_exists(dsp_menu)) then begin
     status = WtSet(dsp_menu, {, whichItem: index})
  endif
  _pr_dsp_reflect

end

; dgp rev 8/16/05 routine to reopen a display
pro _pr_dac_reopen

_pr_gen_ident ; dgp rev 1/28/10 debugging

  cur_dsp = fn_dmf_current()

  if (fn_dmf_key_exists(cur_dsp)) then begin

    parent = fn_dmf_get('main widget')
    _pr_dmf_set, 'position', dgpGetValue(parent,/position)
    _pr_dmf_set, 'size', dgpGetValue(parent,/size)
    s      = WwSetValue(parent,/close)

  endif

end

pro _pr_focus_ena

_pr_gen_ident ; dgp rev 1/28/10 debugging

  info, calls=tree, level=1
  vals = strsplit(tree(0)," ")
  _pr_gen_set, 'callback', vals(0)  

end

; dgp rev 8/22/05 resize is a dmf independent percentage
; of total display size
pro cb_resize, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging
  
  !quiet = 0

  disp_name = fn_focus_grab(wid)

  print, "Display to resize is ",disp_name

  _pr_dmf_switch, disp_name
  
  percentage = dgpGetValue(wid)
  _pr_dmf_set, 'pix perc', percentage

  _pr_dmf_set, 'state', 'reopen'
  _pr_dac_reopen

  _pr_focus_release

end

pro cb_cancel, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  s = WwSetValue(_wid_get_parent(wid),/close)

end

; dgp rev 8/11/05 display button actions
; overlay - create or add current display to an overlay display
; style   - change the display style
; export  - export the display to PowerPoint
; gate    - open gating form to create a gate
pro cb_dsp_but, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  key = fn_focus_grab(wid)

  internal = dgpGetValue(wid)

  print, "Display ",key," button ",internal

  ; from the callback id, determine the window number

  if (value eq 4) then begin

    _pr_mgs_create, key

  endif else begin

    case internal of
      'Overlay': _pr_olay_add
      'Style':   _pr_dsp_style, key
      'Export':  _pr_dmf_export, key
    endcase
  endelse

  _pr_focus_release

end

function _dsp_get_pos, old_wid

print, "Determine new window position"
  max_x = !display_size.x
  max_y = !display_size.y
  new_pos = make_array(2,/long)
  new_pos(0) = 0
  new_pos(1) = max_y/3
  if (old_wid ne 0) then begin 
    if (fn_wid_exists(old_wid)) then begin
      new_pos = dgpGetValue(old_wid,/position)
      sze = dgpGetValue(old_wid,/size)
      new_pos(0) = new_pos(0) + sze(0)
      if (new_pos(0) ge max_x) then new_pos(0) = 0
    endif
  endif

  return, new_pos

end


function fn_dmf_get_focus, focus_wid

  common cm_dmf_info, cv_dmf_key, cv_dmf_hash

  if (size(cv_dmf_hash,/type) ne 11) then return, "default"
  
  keys = askeys(cv_dmf_hash)

  nkeys = n_elements(keys)

  if (nkeys eq 1) then return, "default"
  
  for i=0,nkeys-1 do begin

    if (keys(i) ne 'default') then begin
      if (isaskey(cv_dmf_hash(keys(i)),'main widget')) then begin
        wid = cv_dmf_hash(keys(i),'main widget')
        if (wid eq focus_wid) then return, keys(i)
      endif
    endif
  endfor

  return, "default"

end

; dgp rev 10/12/05 set the font for the current display
; dgp rev 10/18/05 move font from general to font namespace
pro _pr_dmf_setfont

return

  font_str = fn_dmf_get_str('font')
  if (font_str eq "") then begin
    font_str = fn_fnt_get('dmf font')
    _pr_dmf_set, 'font', font_str
  endif
  print, "Setting current device font to ",font_str
  device, font=font_str
  device, get_fontmap=fnt_arr
  print, fnt_arr(0)
  
end
; dgp rev 8/12/05 dmf focus handler
; dgp rev 10/18/05 change dmf font on focus change
pro fchd_dmf_focus, wid, shell, event

_pr_gen_ident ; dgp rev 1/28/10 debugging

  print, "Event ",event.type  
  ; focus in event (9)
  if (event.type ne 9) then return

  parent = _wid_get_parent(wid)

  if (fn_gen_getstr('callback') ne "") then begin
    print, "***** Abort focus while running ",fn_gen_getstr('callback')
    return
  endif
  ; current DMF setting
  cur_dsp = fn_dmf_current()
  print, cur_dsp," old focus"
    
  ; in-focus display name
  new_dsp = fn_dmf_get_focus(_wid_get_parent(wid))

  ; no active displays
  if (new_dsp eq 'default') then return

  ; active display reselected
  if (new_dsp eq cur_dsp) then return

  ; validate DMF display info
  if (fn_dmf_key_exists(new_dsp)) then begin

    _pr_dmf_switch, new_dsp
    _pr_dmf_setfont
    wid = fn_dmf_get('main widget')
    _pr_dmf_set, 'position', dgpGetValue(wid,/position)
    _pr_dmf_set, 'size', dgpGetValue(wid,/size)
    _pr_reflect_cdr
    _pr_reflect_mgs
    update_dsp_range

  endif

end

; dgp rev 8/15/05 use dmf struct to find next display number
; if no structure, then display is one
; loop thru display and look for a closed widget
; if no closed widget, then display is keys plus one
function fn_dmf_next

  keys = fn_dmf_get_keys()

  keys = keys(sort(keys))

  nkeys = n_elements(keys)
  
  ; create new overlay
  rkey = 'Display ' + strtrim(string(nkeys),2)
  for i=0,nkeys-1 do begin
    if (keys(i) ne 'default') then begin
      key = 'Display ' + strtrim(string(i+1),2)
      if (fn_dmf_key_exists(key) ne 1) then begin
        print, "No key for ",key
        rkey = key
      endif
    endif
  endfor
  
  return, rkey

end

; dgp rev 8/15/05 display position, shift display if new
function fn_calc_dsp_pos, disp_name

     cur_pos = fn_dmf_get('position')

     if (fn_dmf_get('state') eq 'add') then begin
       ; increment old_pos
       max_x = !display_size.x
       sze = fn_dmf_get('size')
       cur_pos(0) = cur_pos(0) + sze(0)
       if (cur_pos(0) ge max_x-sze(0)) then begin
         cur_pos(0) = 0
         cur_pos(1) = cur_pos(1) + (float(sze(1))/2.0)
       endif
     endif
     
     return, cur_pos

end

; dgp rev 8/15/05 relative display size based upon percentage
function fn_calc_dsp_size, perc

  pixel_count = float(perc)/100.0 * !display_size.y

  win_text   = float(pixel_count)*.35
  win_margin = float(pixel_count)*.05

  xdsp = long(pixel_count+win_margin)
  ydsp = long(pixel_count+win_text+win_margin)

  return, [xdsp,ydsp]

end
; dgp rev 8/15/05 relative window size based upon percentage
function fn_calc_win_size, perc

  pixel_count = float(perc)/100.0 * !display_size.y

  win_buffer = float(pixel_count)*.025
  win_text   = float(pixel_count)*.35
  win_margin = float(pixel_count)*.05

  xwin = long(pixel_count+win_margin+win_buffer)
  ywin = long(pixel_count+win_text+win_margin+win_buffer)

  return, [xwin,ywin]

end

; dgp rev 9/2/05 display locking on/off
pro cb_mdu_lock, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  disp_name = fn_focus_grab(wid)

  ; lock on/off 
  internal = dgpGetValue(wid)

  _pr_dmf_set, 'locked', internal

  _pr_reflect_locks

  _pr_focus_release

end

; dgp rev 9/28/05 get an individual parameter for the data location
function fn_ifs_total, display

  common cm_ifs, cv_ifs_struct
  
  if not fn_dmf_key_exists(display) then return, 0
  key = fn_dmf_get_other(display,'ifs key')

  if (not isaskey(cv_ifs_struct,key)) then return, 0
  if (not isaskey(cv_ifs_struct(key),'data')) then return, 0

  return, n_elements(cv_ifs_struct(key,'data',*,0))

end

; dgp rev 2/22/2010 display the statistics window for a given display
pro _pr_dmf_stats, display

     ; create layouts
  
    frm_name = "Display Statistics"

;    display = fn_dmf_current()

    filename = fn_dmf_get_other(display,'ifs key')
    
    _pr_wms_set, 'state', 'reform'

       ;print, "Getting toplevel ",top
    if (fn_wms_form(frm_name,layout)) then return

  ;  s = WwText(layout, 'NoOpCB', /label,text='Statistics')
    main_lo  = WwLayout(layout, /ver)
    top_lo   = WwLayout(main_lo, /hor, bord=2)
    key_lo   = WwLayout(top_lo, /ver)
    val_lo   = WwLayout(top_lo, /ver)

    bot_lo   = WwLayout(main_lo, /hor, bord=2)
    par_lo   = WwLayout(bot_lo, /ver)
    par1_lo  = WwLayout(bot_lo, /ver)
    par2_lo  = WwLayout(bot_lo, /ver)
    
    dsptot = n_elements(fn_dmf_get_other(display,'ssl indices'))
    filtot = fn_ifs_total(display)

    perc = string(float(dsptot)*100/float(filtot),format='(F6.2)')

    s = WwText(key_lo, 'NoOpCB', /label,text='Display:')
    s = WwText(val_lo, 'NoOpCB', /label,text=strtrim(display,2))
    s = WwText(key_lo, 'NoOpCB', /label,text='File:')
    s = WwText(val_lo, 'NoOpCB', /label,text=strtrim(filename,2))
    s = WwText(key_lo, 'NoOpCB', /label,text='Display Total:')
    s = WwText(val_lo, 'NoOpCB', /label,text=strtrim(dsptot,2))
    s = WwText(key_lo, 'NoOpCB', /label,text='File Total:')
    s = WwText(val_lo, 'NoOpCB', /label,text=strtrim(filtot,2))
    s = WwText(key_lo, 'NoOpCB', /label,text='Percent:')
    s = WwText(val_lo, 'NoOpCB', /label,text=strtrim(perc,2))

    s = WwText(par_lo, 'NoOpCB', /label,text='Name:')
    s = WwText(par_lo, 'NoOpCB', /label,text='Index:')
    s = WwText(par_lo, 'NoOpCB', /label,text='Scale:')
    s = WwText(par_lo, 'NoOpCB', /label,text='Decades:')
    s = WwText(par_lo, 'NoOpCB', /label,text='MFI:')
    s = WwText(par_lo, 'NoOpCB', /label,text='Median:')
    
    par = fn_dmf_get_other(display,'pars')
    par_names = fn_upa_get_labels()
    
    p1 = par(0) + par(1)
    if (not fn_dmf_single()) then begin
      p1 = par(0)
      par_names = fn_upa_get_labels()
      scale = 'log'
      decades = fn_dmf_get_other(display,'y decade')      
      if (fn_dmf_get_other(display,'y log') eq 0) then begin
        scale = 'linear'
        decades = '-'
      endif
      s = WwText(par2_lo,  'NoOpCB', /label,text=strtrim(par_names(par(1)-1),2))
      s = WwText(par2_lo,  'NoOpCB', /label,text=strtrim(par(1),2))
      s = WwText(par2_lo,  'NoOpCB', /label,text=strtrim(scale,2))
      s = WwText(par2_lo,  'NoOpCB', /label,text=strtrim(decades,2))
      s = WwText(par2_lo,  'NoOpCB', /label,text=strtrim(fn_dmf_get_other(display,'y geo mean'),2))
      s = WwText(par2_lo,  'NoOpCB', /label,text=strtrim(fn_dmf_get_other(display,'y median'),2))
    endif

    par_names = fn_upa_get_labels()
    scale = 'log'
    decades = fn_dmf_get_other(display,'x decade')      
    if (fn_dmf_get_other(display,'x log') eq 0) then begin
      scale = 'linear'
      decades = '-'
    endif
    s = WwText(par1_lo,  'NoOpCB', /label,text=strtrim(par_names(p1-1),2))
    s = WwText(par1_lo,  'NoOpCB', /label,text=strtrim(p1,2))
    s = WwText(par1_lo,  'NoOpCB', /label,text=strtrim(scale,2))
    s = WwText(par1_lo,  'NoOpCB', /label,text=strtrim(decades,2))
    s = WwText(par1_lo,  'NoOpCB', /label,text=strtrim(fn_dmf_get_other(display,'x geo mean'),2))
    s = WwText(par1_lo,  'NoOpCB', /label,text=strtrim(fn_dmf_get_other(display,'x median'),2))

    but_lo = WwLayout(main_lo, /ver)

    s = WwButtonBox(but_lo,'Exit','cb_exit')

    _pr_wms_display, frm_name

end


; dgp rev 2/22/10 Stats callback
pro bphd_dmf_stats, wid, shell, event

  display = fn_wid_parent_name(wid)

  if (event.button eq 3 and fn_gen_get('poly') eq 0) then _pr_dmf_stats, display

end
; dgp rev 8/15/05 new method to handle display name
; dgp rev 8/15/05 new method to handle position and size
; dgp rev 10/20/05 validate that DMF and IFS are compatible
pro _pr_dac_create, disp_name

_pr_gen_ident ; dgp rev 1/28/10 debugging

     ; add the new display -- dmf information may be from 
     ; current display or pre-existing dmf

     _pr_dmf_fillin, disp_name
     
     print, "Display ",disp_name
    
     cur_pos = fn_calc_dsp_pos(disp_name)
     
     top  = _wms_get_main('top')

     main = WwMainWindow(top, lo_main, 'ccb_dac_close', shell_name=disp_name, /ver, $
                         position=cur_pos, layout_name=disp_name, title=disp_name)
                         
     lo_buttons = WwLayout(lo_main,/ver,/top,/left,/right)
     lo_button1 = WwLayout(lo_buttons,/hor)
     lo_button2 = WwLayout(lo_buttons,/hor)
     lo_draw    = WwLayout(lo_main,/form,top=lo_buttons,/Left, /Right,/Bottom)
     
     draw_name = 'DSP Area'
     layout_name = 'DSP Window'
     
     resize_txt = wwtext(lo_button1,'cb_resize',cols=3,text=strtrim(fn_dmf_get('pix perc'),2))
     
     labels = ['Overlay','Style','Export','Gate']
     olay_but = WwButtonBox(lo_button2, labels  ,'cb_dsp_but')
     arr = WwGetValue(olay_but,/child)

     mdu_tog  = WwRadioBox(lo_button1, 'File Lock'  ,'cb_mdu_lock' ,name='MDU Toggle', /nofmany, tog=togarr)
     _pr_wra_reg, 'lock', togarr(0)
     _pr_wra_set_index,  'lock', disp_name, 0, (fn_dmf_get('locked') eq 1)
         
     perc = fn_dmf_get('pix perc')

     win = fn_calc_win_size(perc)
     dsp = fn_calc_dsp_size(perc)
     
     draw   = DGPDrawing(lo_draw, new_win, 'DCB_Draw', $
                        win, dsp, layout=layout_name, $
                        background=Wht, foreground=Blk, name=draw_name, /noscroll, area=_draw_area, $
                        /top, /left,/right,/bottom)
                                  
     s = WwHandler(main,'fchd_dmf_focus','FocusChangeMask')

     ; dgp rev 2/23/2010 add the statistics handler
     s = WwHandler(draw, 'bphd_dmf_stats', 'ButtonPressMask')
                        
     _pr_dmf_set, 'window', new_win
     _pr_dmf_set, 'main widget', main
     _pr_dmf_set, 'draw widget', draw
     _pr_dmf_set, 'state', 'created'

     status = WwSetValue(main,/display)
                  
     _pr_dmf_set, 'position', dgpGetValue(main,/position)
     _pr_dmf_set, 'size', dgpGetValue(main,/size)

end
; dgp rev 9/9/05 get the list of locked displays
; or a single unlock display
; dgp rev 12/20/05 locked displays can be mapped 
; to either the left or right control set
function fn_dmf_get_dlist

  cur_key = fn_dmf_current()

  if (fn_dmf_get('locked')) then begin

    dlist = ['default']

    keys = fn_dmf_get_keys()
    ; look to locked displays or simply return current
    for i=0,n_elements(keys)-1 do begin
      if (fn_dmf_key_exists(keys(i))) then begin
        if (fn_dmf_get_other(keys(i),'locked')) then begin
          if (fn_gen_get('movement') eq 2) then begin
            dlist = [dlist,keys(i)]
          endif else begin
            if (fn_dmf_get_other(keys(i),'movement') eq fn_gen_get('movement')) then dlist = [dlist,keys(i)]
          endelse
        endif
      endif
    endfor
    dlist = dlist(sort(dlist))
  endif else begin
    dlist = [cur_key]
  endelse

  return, dlist

end

; dgp rev 9/2/05 reflect the locked displays
pro _pr_reflect_dsp

  common cm_dmf_info, cv_dmf_key, cv_dmf_hash

  keys = askeys(cv_dmf_hash)
  keys = keys(sort(keys))

  nkeys = n_elements(keys)
  
  for i=0,nkeys-1 do begin

    if (keys(i) ne 'default') then begin
      ; check for wid
      if (fn_dmf_key_exists(keys(i))) then begin
         _pr_wra_set_index,  'lock', keys(i), 0, fn_dmf_get_other(keys(i),'locked')
      endif
    endif

  endfor

end

; dgp rev 8/16/05 display locking from menu
pro _pr_dsp_lock, name, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  ; flag the lock
  _pr_dmf_set_other, name, 'locked', value 
  ; change the toggles
  
  _pr_reflect_dsp

end

; dgp rev 12/9/05 display export
pro _pr_dsp_export, name, value

_pr_gen_ident ; dgp rev 1/28/10 debugging
  
  _pr_dmf_set_other, name, 'export', value 
  ; change the toggles
  
end

function fn_olay_win

  return, 0

end

pro hd_clk_dsp_mod, wid, shell, event

_pr_gen_ident ; dgp rev 1/28/10 debugging

  info, event, /full

end


pro hd_style, wid, shell, event

_pr_gen_ident ; dgp rev 1/28/10 debugging

  print, "Style event ",event.type

end


; dgp rev 9/06/05 get a single DMF display
function fn_dmf_get_branch, key

  common cm_dmf_info, cv_dmf_key, cv_dmf_hash

  if (isaskey(cv_dmf_hash,key)) then begin

    print, "Get the branch for ",key
    return, cv_dmf_hash(key)

  endif

  print, "Problem with key ",key
  return, -1

end



; dgp rev 12/6/05 filter out only the control items in DMF
; position must be restored in addition to control settings
; dgp rev 8/29/07 change the default dot size
function fn_dmf_save_prep, hash_item

  control_keys = ['dual res','hist res','format','ymax perc','quad on','quad rep','quad width','quad degree']
  control_keys = [control_keys,'smo on','smo rep','smo width','pars','pix perc','color','position']
  control_keys = [control_keys,'lvl min type','lvl min val','lvl mult type','lvl mult val','locked']
  control_keys = [control_keys,'dot max','applied','par select','1only','description']
  control_keys = [control_keys,'line thick','char thick','char size','dot size','tube on']
  control_keys = [control_keys,'outliers','psym']

  ; fill-in any missing vlaues
  if (not isaskey(hash_item,'line thick')) then hash_item = [hash_item,asarr('line thick',!p.thick)]
  if (not isaskey(hash_item,'char thick')) then hash_item = [hash_item,asarr('char thick',!p.charthick)]
  if (not isaskey(hash_item,'char size')) then hash_item = [hash_item,asarr('char size',!p.charsize)]
  if (not isaskey(hash_item,'dot size')) then hash_item = [hash_item,asarr('dot size',.25)]

  keys = askeys(hash_item)
  keys = keys(sort(keys))
  key_count = n_elements(keys)
  valid_idx = wherein(keys,control_keys,cnt)
  tmp = hash_item(keys(valid_idx))  

  if (isaskey(hash_item,'main widget')) then begin
    main = hash_item('main widget')
    if (fn_wid_exists(main)) then begin
      print, "Adjust position and size for ",WtGet(main,/name)
      tmp = [tmp,asarr('position',dgpGetValue(main,/position))]
      tmp = [tmp,asarr('size',dgpGetValue(main,/size))]
    endif
  endif
  
  return, tmp

end
; dgp rev 12/6/05 retrieve description for the
; "Display Settings" form
function fn_dmf_descript

  wid = fn_wra_get('description', "Display Settings")
  wid = wid(0)
  print, "Widget: ",wid," Value: ",dgpGetValue(wid)
  desc_text = ""

  if (fn_wid_exists(wid)) then begin
    desc_text = dgpGetValue(wid)
  endif
  return, desc_text

end

; dgp rev 9/06/05 save the current DMF along
; with all previously saved values
pro _pr_dmf_save_def, cv_dmf_hash

  savename = fn_gen_build_path(fn_gen_getstr('setup path'),'dmf_struct.sav')
  
  print, "Save Default ",savename
  save, cv_dmf_hash, filename=savename

end


; dgp rev 9/06/05 save the current DMF along
; with all previously saved values
pro _pr_dmf_save_cds, cv_dmf_hash

  uname = fn_gen_unique()


  cds_file = fn_gen_build_path(fn_gen_getstr('setup path'),strtrim(uname+".cds",2))

  ; mark the current DMF with a unique CDS key
  ; and description

  ; grab the description from the text field
  desc_text = fn_dmf_descript()
  cv_dmf_hash('default') = [cv_dmf_hash('default'),asarr('description',fn_dmf_descript())]
  
  print, "Saving DMF to ",cds_file
  save, cv_dmf_hash, filename=cds_file

end

; dgp rev 9/06/05 save the active DMF settings
pro _pr_dmf_save_work, cv_dmf_hash

  savename = fn_gen_build_path(fn_gen_getstr('work path'),'dmf_struct.sav') 
  
  print, "Save ",savename
  save, cv_dmf_hash, filename=savename

end

; dgp rev 9/06/05 save the active DMF settings
pro _pr_dmf_save_layout

  ; prep the DMF for saving as a temporary variable
  keys = fn_dmf_get_keys()

  count = 0
  ; save at least the default settings
  org_dmf_hash = fn_dmf_get_branch('default')
  tmp_dmf_hash = asarr('default',fn_dmf_save_prep(org_dmf_hash))
  
  ; now check for any existing DMF displays
  for i=0,n_elements(keys)-1 do begin
  
    if (keys(i) ne 'default') then begin
      wid = fn_dmf_get_other(keys(i), 'main widget')
      if (fn_wid_exists(wid)) then begin
        if (keys(i) eq WtGet(wid,/name)) then begin
          print, "Save display ",keys(i)
          _pr_dmf_set_other, keys(i), 'position', dgpGetValue(wid,/position)
          tmp_dmf_hash = [tmp_dmf_hash,asarr(keys(i),fn_dmf_save_prep(fn_dmf_get_branch(keys(i))))]
          count = count + 1
        endif
      endif
    endif
  endfor
  
  ; confirm
  main = _wms_get_main("Display Settings")
  str1 = 'Saving ' + strtrim(count,2)
  message = [str1,'Displays']
  button=WwAlert(main, message, ["OK","Cancel"])

  print, "Button pressed ",button

; dgp rev 6/2/2010 save new "dmf_struct.sav" and duplicate *.cds file
; dgp rev 6/2/2010 if selected, save master "dmf_struct.sav"
  if (button eq 1) then begin
    ON_IOERROR, null
    _pr_dmf_save_cds, tmp_dmf_hash
    _pr_dmf_save_work, tmp_dmf_hash
    if (fn_gen_get('cds default') ne 0) then _pr_dmf_save_def, tmp_dmf_hash
    _pr_cds_scan
    _pr_wms_set, 'state', 'reform'
    _pr_dmf_form
  endif
end
; dgp rev 12/5/05 callback for DMF save current
pro cb_dmf_save, wid, value

  _pr_dmf_save_layout

end

; dgp rev 12/2705 debug for CDS
pro _pr_cds_debug

  common cm_cds_info, cv_cds_hash
  common cm_cds_struct, cv_cds_files, cv_cds_names

  info, cv_cds_hash, /full
  stop ;debug

end

; dgp rev 12/27/05 get the description text for
; the current display set
function fn_dmf_get_desc

    nut = "No Description"

    if (size(cv_dmf_hash,/type) ne 11) then return, nut
    
    if (not isaskey(cv_dmf_hash,'default')) then return, nut

    if (not isaskey(cv_dmf_hash('default'),'description')) then return, nut
    
    return, cv_dmf_hash('default','description')
    
end

; dgp rev12/27/05 get the current DMF structure if it exists
function fn_dmf_get_all, tmp_dmf_hash

  common cm_dmf_info, cv_dmf_key, cv_dmf_hash

  if (size(cv_dmf_hash,/type) ne 11) then return, 0
  
  tmp_dmf_hash = cv_dmf_hash
  
  return, 1

end

; dgp rev 5/26/2010
function fn_cds_exists

  common cm_cds_struct, cv_cds_files, cv_cds_names

  return, (size(cv_cds_names,/type) eq 11)

end

; dgp rev 5/26/2010
function fn_cds_list

  common cm_cds_struct, cv_cds_files, cv_cds_names

  if (size(cv_cds_names,/type) eq 11) then return, askeys(cv_cds_names)

  return, [""]

end

; dgp rev 5/26/2010
function fn_cds_name_exists, name

  common cm_cds_struct, cv_cds_files, cv_cds_names

  if (size(cv_cds_names,/type) ne 11) then return, 0

  return, isaskey(cv_cds_names,name)

end



; dgp rev 5/26/2010
function fn_cds_get_file, name

  common cm_cds_struct, cv_cds_files, cv_cds_names

  if (size(cv_cds_names,/type) ne 11) then return, [""]

  if (not isaskey(cv_cds_names,name)) then return , [""]

  return, cv_cds_names(name)

end

; dgp rev 5/26/2010 wip when to call
; dgp rev 5/25/2010 pare the cds list
pro _pr_cds_scan

  common cm_cds_struct, cv_cds_files, cv_cds_names

  ; stub out the restore variable
  cv_dmf_hash = 0
  file = "*.cds"
  path = fn_gen_get('setup path')
  spec = fn_gen_build_path(path,file)

  found = findfile(spec,count=cnt)
  
  if (cnt ne 0) then begin

    cds_spec = fn_gen_build_path(path,found(0))
    restore, filename=cds_spec

    cv_cds_names = asarr(cv_dmf_hash('default','description'),found(0))
    cur = cv_dmf_hash('default','description')
    print, "Add ",found(0)," with ",cur

    key = 'description'
    for i=1, n_elements(found) - 1 do begin

      cds_spec = fn_gen_build_path(path,found(i))
      restore, filename=cds_spec

      if (size(cv_dmf_hash,/type) eq 11) then begin
        if (isaskey(cv_dmf_hash,'default')) then begin
          if (isaskey(cv_dmf_hash('default'),'description')) then begin
            cur = cv_dmf_hash('default','description')
            if (isaskey(cv_cds_names,cur)) then begin
              print, "Delete ",cv_cds_names(cur)," with ",cur
              cds_spec = fn_gen_build_path(path,cv_cds_names(cur))
              del_file, cds_spec
              print, "Add ",found(i)
              cv_cds_names(cur) = found(i)
            endif else begin
              print, "Add ",found(i)," with ",cur
              cv_cds_names = [cv_cds_names,asarr(cv_dmf_hash('default','description'),found(i))]
            endelse
          endif
        endif
      endif

    endfor
  
    keys = askeys(cv_cds_names)
    cv_cds_files = asarr(cv_cds_names(keys(0)),keys(0))
    for i=1,n_elements(keys)-1 do begin
      cv_cds_files = [cv_cds_files,asarr(cv_cds_names(keys(i)),keys(i))]
    endfor
    
  endif

end

; dgp rev 12/6/05 close all open displays
pro _pr_dac_close_all

_pr_gen_ident ; dgp rev 1/28/10 debugging

  keys = fn_dmf_get_keys()
  _pr_gen_set, 'close count', fn_dmf_ref_cnt()
  _pr_gen_set, 'state', 'reload'
  
  for i=0,n_elements(keys)-1 do begin
    if (keys(i) ne 'default') then begin
      wid = fn_dmf_get_other(keys(i), 'main widget')
      if (fn_wid_exists(wid)) then begin
        if (keys(i) eq WtGet(wid,/name)) then begin
          print, "Close display ",keys(i)
	  s = WwSetValue(wid,/close)
        endif
      endif
    endif
    
  endfor

end

; dgp rev 5/26/2010 Switch specific CDS for current DMF
pro _pr_cds_switch, name

  print, "Switch CDS ",name

  if (fn_cds_name_exists(name)) then begin
  
    file = fn_cds_get_file(name)
    path = fn_gen_get('setup path')
    cds_spec = fn_gen_build_path(path,file)
    found = findfile(cds_spec,count=cnt)
    
    if (cnt ne 0) then begin
      ; current DMF structure location
      if (fn_dmf_find_file(target)) then begin
        com = 'copy ' + cds_spec + ' ' + target
print, com
        SPAWN, com, results    
        test = strpos(results,"copied")
        if (test(0) ne -1) then begin
          _pr_dac_close_all
        endif
      endif
    endif
  endif

end

; dgp rev 12/5/05 load a new DMF structure
pro cb_cds_sel, wid, value

  name = WtGet(wid,/name)

  _pr_cds_switch, name

end

; dgp rev 6/1/2010 
pro cb_cds_default, wid, value

  internal = WwGetValue(wid)
  _pr_gen_set, 'cds default', internal

end

; dgp rev 12/5/05 DMF settings form
pro _pr_dmf_form

_pr_gen_ident ; dgp rev 1/28/10 debugging

  frm_name = "Display Settings"
  
  setup_list = fn_cds_list()

  if (fn_wms_form(frm_name,layout)) then return
    
  widget_layout = WwLayout(layout, /Vertical)
    
  savbut = WwButtonBox(widget_layout,'Save Current','cb_dmf_save')
  defflg = WwRadioBox(widget_layout, ['Default'], 'cb_cds_default', toggle=tog, /nofmany)
  s = WwSetValue(tog(0),fn_gen_get('cds default'))

  desc   = WwText(widget_layout, 'NoOpCB', /Label, Text='Description')
  destxt = WwText(widget_layout, 'CB_dmf_desc', Text=fn_dmf_get_other('default','description'), col=15)
    
  _pr_wra_reg, 'description', destxt
  
  lo_move       = WwLayout(widget_layout, /Hor, /Bot)
  lo_pull       = WwLayout(widget_layout, /Hor, /Bot)

  setup_wids = ['pullsetup','setupopt']
     
  setup_sel = {,callback:'CB_NoOp', button:'Select Set'}
     
  setup_menu_wid = WwOptionMenu(lo_pull, '', setup_sel, name=setup_wids)
          
  setup_name = WtGet(setup_menu_wid,/name)
  setup_kids = dgpGetValue(setup_menu_wid,/children)
     
  idx = 1
  for i=0,n_elements(setup_list)-1  do begin
    status = WwMenuItem(setup_menu_wid, idx, setup_list(i), 'cb_cds_sel', name=setup_list(i), /Add)
  endfor
   
  _pr_wms_display, frm_name

end

; dgp rev 10/18/05 callback to save current display settings
pro cb_dsp_save, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  _pr_dmf_form

end
; dgp rev 9/1/05 create a new window and
; reform the display form if it exists
pro _pr_dac_new_win

_pr_gen_ident ; dgp rev 1/28/10 debugging

  disp_name = fn_dmf_next()

  _pr_dmf_set, 'state', 'add'

  _pr_dac_create, disp_name

  _pr_wms_set, 'state', 'reform'

  _pr_frm_display  

  _pr_wms_set, 'state', 'reform'

  _pr_frm_files

  _pr_reflect_locks

  _pr_reflect_cdr

end
; dgp rev 9/1/05 callback for a new display
pro cb_dac_new_win, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  _pr_dac_new_win

end

pro CB_icp_OnOff, wid, switch

_pr_gen_ident ; dgp rev 1/28/10 debugging

  print, "Wid   ",wid
  print, "switch ",switch
  onoff = dgpGetValue(wid)
  print, "Setting ",onoff

  ; update settings
  if (switch eq 1) then begin
    _car_set_on, onoff
  endif else begin
    _mgs_set_on, onoff
  endelse
  
  draw_cells
    
end

pro _wid_state_hist

_pr_gen_ident ; dgp rev 1/28/10 debugging

  _sense_mod, 'DualInfo', '/nonsensitive'
  _sense_mod, 'HistInfo', '/sensitive'
  _sense_mod, 'DualType', '/nonsensitive'

end

pro _wid_state_dual, mode

_pr_gen_ident ; dgp rev 1/28/10 debugging

  _sense_mod, 'DualInfo', '/sensitive'
  _sense_mod, 'HistInfo', '/nonsensitive'
  _sense_mod, 'DualType', '/sensitive'

end

pro _dsp_car_info

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_car_info, counts_cluster, means_cluster, cluster_group, cluster_enable

  if (_car_active()) then begin
    idx = where(cluster_enable(1:*) eq 1,cnt)
    if (cnt ne 0) then begin
      clu_tot = sum(counts_cluster)
      for i=0,cnt-1 do begin
        clu_cnt = counts_cluster(idx(i))
        perc = fix(10000*float(clu_cnt)/float(clu_tot))
        perc = string(float(perc)/100.0,Format="(F5.2)")
        text = "Cluster "+strtrim(idx(i)+1,2)+": "+strtrim(clu_cnt,2)
        text = text + " cells "+ perc + " percent"
        _dsp_subline, text
      endfor
    endif
  endif

end

; determine gate keys to display for give parameter pair
pro _mgs_dsp_keys

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_mgs_info, cv_mgs_struct
  common cm_dsp_cur, cv_dsp_trans, cv_dsp_keys, cv_dsp_index 

  ; dgp rev 8/28/07 loop thru all gates and select gates matching display
  ; create a list of keys, key indices and transpose flag

  cv_dsp_keys = make_array(1,/string,value="")
  cv_dsp_index = [0]

  if (fn_mgs_get_sorted(keys)) then begin

    pars = _cdr_get_pars()

    loops = N_ELEMENTS(keys) - 1
      
    FOR I = 0, loops DO begin
      gpars = cv_mgs_struct(keys(i)).params
      results = wherein(gpars,pars,cnt)
      if (cnt eq 2) then begin
; oops, transpose only set once, must be array
        cv_dsp_trans = 1
        if (same(gpars,pars,/notype)) then begin
          cv_dsp_trans = 0
        endif
        cv_dsp_keys = [cv_dsp_keys,keys(i)]
        cv_dsp_index = [cv_dsp_index,i]
      endif
    ENDFOR

  endif
    
end

; dgp rev 9/9/05 get the list of locked displays
; or a single unlock display
; dgp rev 12/20/05 locked displays can be mapped 
; to either the left or right control set
function fn_dmf_get_all

    dlist = ['default']

    keys = fn_dmf_get_keys()
    ; look to locked displays or simply return current
    for i=0,n_elements(keys)-1 do begin
      if (fn_dmf_key_exists(keys(i))) then begin
         dlist = [dlist,keys(i)]
      endif
    endfor
    dlist = dlist(sort(dlist))

  return, dlist

end


; dgp rev 11/16/05 redraw all the active displays
; 
Pro _pr_dsp_draw_all

  info, !quiet
  !quiet = 0

  ; grab the main widget and display
  ; a wait cursor - hourglass
  top = _wms_get_main('top')
  status = WtCursor('WAIT',top)

; get all displays to update
  dlist = fn_dmf_get_all()

  _pr_gen_set, 'display', 'cycle'
; loop thru each display
  ; create a WAIT cursor
  for i=0,n_elements(dlist)-1 do begin
    if (dlist(i) ne 'default') then begin
      _pr_dmf_switch, dlist(i)
      draw_cells
    endif
  endfor
  _pr_gen_set, 'display', 'single'
  status = WtCursor('DEFAULT',top)

END
; dgp rev 11/16/05 redraw all the active displays
; 
Pro _pr_dsp_draw_locked

_pr_gen_ident ; dgp rev 1/28/10 debugging

  info, !quiet
  !quiet = 0

  ; grab the main widget and display
  ; a wait cursor - hourglass
  top = _wms_get_main('top')
  status = WtCursor('WAIT',top)

; get all displays to update
  dlist = fn_dmf_get_dlist()

  _pr_gen_set, 'display', 'cycle'
; loop thru each display
  ; create a WAIT cursor
  for i=0,n_elements(dlist)-1 do begin
    if (dlist(i) ne 'default') then begin
      _pr_dmf_switch, dlist(i)
      draw_cells
    endif
  endfor
  _pr_gen_set, 'display', 'single'
  status = WtCursor('DEFAULT',top)

END
; dgp rev 1/9/06 check font status after draw
function fn_fnt_check

  return, 1

end


; dgp rev 11/1/05 get current x parameter data
function fn_conv_data, value, index
    
  if index ne 1 then index = 2
  kw = ['x log','y log']
  if (fn_dmf_get(kw(index-1)) eq 0) then return, value
  
  return, 10^(float(value)/float(256.0))

end

; dgp rev 2/23/2010 Precalc the statistic
pro _pr_dmf_calc_stats, key

  if (fn_dmf_single()) then begin
           ; dgp rev 2/23/2010 calculate the statistics
           sts_median  = median(fn_conv_data(fn_ifs_display(key,1),1))
           sts_average = avg(fn_conv_data(fn_ifs_display(key,1),1))
           sts_geomean = exp(avg(alog(fn_conv_data(fn_ifs_display(key,1),1))))

           _pr_dmf_set, 'x geo mean', sts_geomean
           _pr_dmf_set, 'x median', sts_median
           _pr_dmf_set, 'x mean', sts_average    
  endif else begin
           ; dgp rev 2/23/2010 calculate the statistics
           sts_median  = median(fn_conv_data(fn_ifs_display(key,1),1))
           sts_average = avg(fn_conv_data(fn_ifs_display(key,1),1))
           sts_geomean = exp(avg(alog(fn_conv_data(fn_ifs_display(key,1),1))))

           _pr_dmf_set, 'x geo mean', sts_geomean
           _pr_dmf_set, 'x median', sts_median
           _pr_dmf_set, 'x mean', sts_average

           ; dgp rev 2/23/2010 calculate the statistics
           sts_median  = median(fn_conv_data(fn_ifs_display(key,2),2))
           sts_average = avg(fn_conv_data(fn_ifs_display(key,2),2))
           sts_geomean = exp(avg(alog(fn_conv_data(fn_ifs_display(key,2),2))))

           _pr_dmf_set, 'y geo mean', sts_geomean
           _pr_dmf_set, 'y median', sts_median
           _pr_dmf_set, 'y mean', sts_average
  endelse

end

; dgp rev 2/23/2010 parameter info for given display
pro _pr_dmf_par_info

  pars = fn_dmf_get('pars')
  if (fn_dmf_single()) then begin
    p1 = fix(sum(pars))
  endif else begin
    p1 = pars(0)
    kw = '$P' + strtrim(pars(1),2) + 'E'
    vals = strsplit(fn_ifs_get_header(kw),",")
    logon = fix(vals(0))
    decade = logon
    ; default to 4 decades
    if (decade eq 0) then decade = 4
    _pr_dmf_set, 'y decade', decade
    _pr_dmf_set, 'y log', fix(logon gt 0)
  endelse
  kw = '$P' + strtrim(p1,2) + 'E'
  vals = strsplit(fn_ifs_get_header(kw),",")
  logon = fix(vals(0))
  decade = logon
  ; default to 4 decades
  if (decade eq 0) then decade = 4
  _pr_dmf_set, 'x decade', decade
  _pr_dmf_set, 'x log', fix(logon gt 0)
  
end

; dgp rev 8/16/05 draw cells
; dgp rev 11//8/05 remove 'redraws'
; dgp rev 1/4/06 reset font on each draw
pro draw_cells

  _pr_gen_ident ; dgp rev 1/28/10 debugging

  _pr_gen_report1, "BeginDraw "
  num = fn_dmf_get('window')
  key = fn_dmf_current()
  if (fn_dmf_get('close')) then begin
    print, "Display is closed -- ",num
    return
  endif
  
    ; specify device and reset font on each draw
; dgp rev 2/25/2010 wset error on rebuild
    print, "Setting window to number ",num
    wset, num
    _pr_dmf_setfont    
    
    _mgs_dsp_keys
        
    _pr_dmf_par_info
    _pr_dmf_calc_stats, key
    if (fn_dmf_single()) then begin
      _pr_dsp_histo 
    endif else begin
      mode = fn_dmf_get('format')
      if (mode eq 'contour') then begin
        _pr_dsp_contour
      endif else if (mode eq 'surface') then begin
        _pr_dsp_surface
      endif else begin
        _pr_dsp_dot
      endelse
    endelse
    if (_mgs_active()) then begin
      _mgs_disp_gates
    endif
    _pr_dsp_heading
    _pr_dsp_table
    _pr_dsp_applied
    _dsp_car_info
    ; debugging routine to check for sporadic font error
    if (not fn_fnt_check()) then print, "Font error"
    _pr_gen_report1, "EndDraw "
    
    if (fn_gen_get('display loop')) then begin
      _pr_gen_set, 'display index', fn_gen_get('display index') - 1
      if (fn_gen_get('display index') eq 0) then begin
        _pr_gen_set, 'display loop', 0
;        _pr_tst_all
      endif
    endif
    
end

function _dsm_indices_count, count

  common dsm1, dsm_all, dsm_indices
  
  if (size(dsm_indices,/type) ne 0) then begin
    count = n_elements(dsm_indices)
    return, 1
  endif
  count = -1
  return, 0

end

function _dsm_data_pars, count

  common dsm1, dsm_all, dsm_indices
  
  if (size(dsm_all,/type) ne 0) then begin
    count = n_elements(dsm_all(0,*))
    return, 1
  endif
  count = -1
  return, 0

end


function _dsm_data_count, count

  common dsm1, dsm_all, dsm_indices
  
  if (size(dsm_all,/type) ne 0) then begin
    count = n_elements(dsm_all)
    return, 1
  endif
  count = -1
  return, 0

end

function _dsm_get_data, data

  common dsm1, dsm_all, dsm_indices
  
  pars = _cdr_get_pars()

  print, "dsm all type is ",size(dsm_all,/type)
  if (size(dsm_all,/type) ne 0) then begin
    data = dsm_all
    return, 1
  endif
  data = -1
  return, 0

end

function _dsm_get_subset, data

  common dsm1, dsm_all, dsm_indices
  
  pars = _cdr_get_pars()

  print, "dsm all type is ",size(dsm_all,/type)
  if (size(dsm_all,/type) ne 0) then begin
    data = [[dsm_all(dsm_indices,pars(0)-1)],[dsm_all(dsm_indices,pars(1)-1)]]
    return, 1
  endif
  data = -1
  return, 0

end

pro _dsm_init, cells

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common dsm1, dsm_all, dsm_indices

  ; init data values
  dsm_all     = cells
  cell_count = n_elements(cells(*,0))
  
  ; init indices
  dsm_indices = make_array(cell_count,1,/long)
  tmp = lindgen(cell_count)
  dsm_indices = tmp

end

function _pfl_get_list, items

  common cm_pfl, cv_pfl_list, cv_file_count, cv_pfl_name_list

  if (size(cv_pfl_list,/type) ne 0) then begin
    items = cv_pfl_list
    return, 1
  endif
  return, 0

end
     
pro cb_pfl_select, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  print, "Call from widget ",wid
  print, "With parameter ",value
  print, "Value ",dgpGetValue(wid)

end

pro icp_upd

_pr_gen_ident ; dgp rev 1/28/10 debugging

     _mgs_car_state = 0
     if (_car_active()) then begin
       _mgs_car_state = 1
       _sense_add,'cluster_on'     ,togs(0) ,'/sensitive'
       status = WwSetValue(togs(0),1)
     endif else begin
       _sense_add,'cluster_on'     ,togs(0) ,'/nonsensitive'
       status = WwSetValue(togs(0),0)
     endelse
     
     if (_mgs_active()) then begin
       _mgs_car_state = _mgs_car_state + 1
       _sense_add,'gatebut'     ,gatebut,'/sensitive'
       _sense_add,'gate_on'     ,togs(1) ,'/sensitive'
       status = WwSetValue(togs(1),1)
     endif else begin
       _sense_add,'gatebut'     ,gatebut,'/nonsensitive'
       _sense_add,'gate_on'     ,togs(1) ,'/nonsensitive'
       status = WwSetValue(togs(1),0)
     endelse

     if (_mgs_car_state eq 2) then begin
       _sense_add,'both_on'     ,togs(2) ,'/sensitive'
       status = WwSetValue(togs(2),1)
     endif else begin
       _sense_add,'both_on'     ,togs(2) ,'/nonsensitive'
       status = WwSetValue(togs(2),0)
     endelse

     results = WwSetValue(combo,userdata=togs)                    

end

pro CB_dmf_lvl_mod, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  print, "Widget : ",wid
  print, "Name   : ",WtGet(wid,/name)
  print, "Passed : ",value

  Intern = dgpGetValue(wid)
  print, "Internal : ",intern
  print, "Name   : ",WtGet(value,/name)

end

pro _pr_cdr_levels, lo_dual

_pr_gen_ident ; dgp rev 1/28/10 debugging

     sp = 2
     bd = 1

     lo_dual3  = WwLayout(lo_dual,/hor,spac=sp,bord=bd)
     lo_dual4  = WwLayout(lo_dual,/hor,spac=sp,bord=bd)

     out     = WwRadioBox(lo_dual3, ['% of total','Absolute','Probability'], $
                          'cb_dmf_lvl_min_type', toggle=tog, /oneofmany, /ver)
                          
     lvl_mn   = WwText    (lo_dual3,'CB_dmf_lvl_min', $
                           text=string(fn_dmf_get('lvl min val'),format="(F5.2)"),cols=5)
     _dmf_reg_lvl_min_wid, lvl_mn, tog
     
     lvltmp2  = WwText    (lo_dual3,'NoOpCB',/label,text='Minimum')

     out     = WwRadioBox(lo_dual4, ['% of total','Absolute','Probability'], $
                          'cb_dmf_lvl_mult_type', toggle=tog, /oneofmany, /ver)
     lvl_mult = WwText    (lo_dual4,'cb_dmf_Lvl_mult', $
                           text=string(fn_dmf_get('lvl mult val'),format="(f5.2)"),cols=5)
     _dmf_reg_lvl_mult_wid, lvl_mult, tog

     lvltmp4  = WwText    (lo_dual4,'NoOpCB',/label,text='Multiple')

end


pro cb_dmf_set_lvls, wid, index

_pr_gen_ident ; dgp rev 1/28/10 debugging

  print, "Levels ",wid,index

  common _dmf_lvl_info, _dmf_level_arr, _dmf_lvl_force, _dmf_level_wid
  common dmf_levels_arr, lvl_arr
    
  frm_name = "Set Levels Form"

  if (fn_wms_form(frm_name,layout)) then return
    
     _pr_cdr_levels, layout

     s = WwButtonBox(layout,'Exit','cb_exit')
  
     _pr_wms_display, frm_name

end

pro cb_dmf_show_levels, wid, index

_pr_gen_ident ; dgp rev 1/28/10 debugging

  print, "Levels ",wid,index

  common _dmf_lvl_info, _dmf_level_arr, _dmf_lvl_force, _dmf_level_wid
  common dmf_levels_arr, lvl_arr
  
  if (size(lvl_arr,/type) eq 0) then return
  
  frm_name = "Levels Form"

   if (fn_wms_form(frm_name,layout)) then return
  
     widget_layout = WwLayout(layout, /Vertical)
  
     _dmf_level_wid = make_array(n_elements(lvl_arr),/long)
     for i=0, n_elements(lvl_arr) - 1 do begin
       name = strtrim(i)
       _dmf_level_wid(i) = $
         WwText(widget_layout,/label,text=string(lvl_arr(i),format="(F9.2)"),cols=9,name=name)
     endfor
     s = WwButtonBox(widget_layout,'Exit','cb_exit')
  
     _pr_wms_display, frm_name

end

pro _tab_init, wid_arr

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common _tab_order, tab_array, tab_index

  print, "Init tab order with "
  tab_array = wid_arr
  tab_index = 0

end

; routine to tab thru the text boxes on the 
; controls form
pro _tab_advance

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common _tab_order, tab_array, tab_index

  if (size(tab_array,/type) ne 0) then begin
    if (tab_index ge n_elements(tab_array)) then tab_index = 0
    wid = tab_array(tab_index)
    tab_index = tab_index + 1
    print, tab_index, " is ",wid
    if (fn_wid_exists(wid)) then begin
      in = dgpGetValue(wid)
      theEnd = STRLEN(in)
      status = WtSet(wid, {, selectionStart:0, selectionEnd:theEnd})
    endif
  endif

end

; if the form exists, then raise it
pro _pr_wms_raise, wid

_pr_gen_ident ; dgp rev 1/28/10 debugging

  if (dgpGetValue(wid,/exists)) then begin
    if (not dgpGetValue(wid,/destroyed)) then begin
      status = WtSet(wid, /unrealize)
      status = WtSet(wid, /realize)
    endif
  endif
  
end

pro _pr_next_form, wid

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_wms_info, cv_main_hash

  name = WtGet(wid,/name)
  if (name eq "FCS Controls") then begin
    name = 'top'
  endif
  keys = askeys(cv_main_hash)
  keys = keys(sort(keys))
  idx = where(name eq keys,cnt)
  if (cnt ne 0) then begin
    pos = (idx(0)+1) mod n_elements(keys)
    _pr_wms_raise, cv_main_hash(keys(pos))
  endif

end

PRO kphd_keystroke, wid, shell, event

_pr_gen_ident ; dgp rev 1/28/10 debugging

  key = WwGetKey(event, State = state, Keysym = keysym)
  
  case keysym of
  9: _pr_next_form, wid
  39: cb_next
  37: cb_prev
  else:
  endcase  

end

pro _stat_init, arr

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common _stat_dsp, _stat_arr
  
  _stat_arr = arr

end
; dgp rev 11/16/05 pulldown list file selection
pro cb_pullfile_right, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  _pr_gen_set, 'movement', 1
  if (fn_fcr_move_absolute(value-2)) then begin
    _pr_dsp_draw_locked
  endif
  _pr_reflect_cdr

  
end

; dgp rev 11/16/05 pulldown list file selection
pro cb_pullfile_left, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  _pr_gen_set, 'movement', 0
  if (fn_fcr_move_absolute(value-2)) then begin
    _pr_dsp_draw_locked
  endif
  _pr_reflect_cdr

  
end
; dgp rev 11/16/05 pulldown list file selection
pro cb_pullfile_all, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  _pr_gen_set, 'movement', 2
  if (fn_fcr_move_absolute(value-2)) then begin
    _pr_dsp_draw_locked
  endif
  _pr_reflect_cdr
  
end

; dgp rev 11/10/05 callback is enabled
function fn_gen_callback_on

  common cm_gen_callback, cv_callback_on

  if (size(cv_callback_on,/type) eq 0) then cv_callback_on = 1

  return, (cv_callback_on eq 1)

end
; dgp rev 11/10/05 callback for dot field
; only change slider if active
pro cb_dot_max, wid, index

  if (not fn_gen_callback_on()) then return
  
  dot_max = dgpGetValue(wid)

  _pr_dmf_set, 'dot max', dot_max

  draw_cells

end
; dgp rev 11/10/05 callback for dot slider
; only change slider if active
pro CB_dot_slider, wid, index

  if (not fn_gen_callback_on()) then return

  dot_max = dgpGetValue(wid)
  
  _pr_dmf_set, 'dot max', dot_max

  draw_cells

end
; dgp rev 11/1/05 change the column order for x and y axis
; this routine will switch the axis order
pro CB_AxOrder, wid, index

  cdr_pars = fn_dmf_get('pars')

  order = ['X<->Y','Y<->X']
  
  ; dpg rev 8/28/07 Make Axis Order general variable
  cdr_axis_order = fn_gen_get("AxisOrder")
  cdr_axis_order = (cdr_axis_order + 1) mod 2 
  _pr_gen_set, "AxisOrder", cdr_axis_order

  info, cdr_axis_order
  
  ; swap the button label to reflect order
  s = WtSet(wid, {, text: order(cdr_axis_order)})

  ; swap the widget list
  p1vals = fn_wra_get('Par 1', "Control Panel")
  p2vals = fn_wra_get('Par 2', "Control Panel")

  tmp = p1vals
  _pr_wra_reg_mod, 'Control Panel', 'Par 1', p2vals
  _pr_wra_reg_mod, 'Control Panel', 'Par 2', tmp

  ;reverse the parameter array
  tmp2 = cdr_pars(0)
  cdr_pars(0) = cdr_pars(1)
  cdr_pars(1) = tmp2
  _pr_dmf_set, 'pars', cdr_pars
  
  draw_cells

end

; dgp rev 1/28/10 revise
pro _pr_surpress_callback

  common cm_gen_callback, cv_callback_on

  cv_callback_on = 0

end

; dgp rev 1/28/10 revise
pro _pr_activate_callback

  common cm_gen_callback, cv_callback_on

  cv_callback_on = 1

end

pro _dmf_reflect_type

  dmf_dual_togs = fn_wra_get('Dual Format', "Control Panel")

  arr = ['dot','contour','surface']

  if (size(dmf_dual_togs,/type) ne 0) then begin
    if (fn_wid_exists(dmf_dual_togs(0))) then begin
      mode = fn_dmf_get('format')
      idx = where(arr eq mode)
      s = WwSetValue(dmf_dual_togs(idx(0)),1)
    endif
  endif

end

; Display Control Panel - dcp

pro _dcp_set_cur, kw, val

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common _dcp_info, _dcp_current

  if (size(_dcp_current,/type) eq 0) then  _dcp_current = asarr('flag',0)

  _dcp_current = [_dcp_current,asarr(kw,val)]
  
end

function _dcp_get_cur, kw

  common _dcp_info, _dcp_current

  if (size(_dcp_current,/type) ne 0) then begin
    result = ISASKEY(_dcp_current, kw)
    if (result ne 0) then begin
      return, _dcp_current(kw)
    endif else begin
      return, 0
    endelse
  endif else begin
    return, 0
  endelse

end

pro _dcp_gather_info

_pr_gen_ident ; dgp rev 1/28/10 debugging

  fcsname = fn_dmf_get('ifs key')
   
  uname = _gen_unique_name()

  _dcp_set_cur, 'filename', fcsname
  _dcp_set_cur, 'uname', uname  

; retrieve gating info 
  if (_mgs_get_sort_names(keys)) then begin
    _dcp_set_cur, 'applied count', n_elements(keys)  
    _dcp_set_cur, 'applied', keys  
  endif else begin
    _dcp_set_cur, 'applied count', 0
  endelse
  
; retreive various manipulation settings
; smoothing, levels, y-axis, z-axis
  _dcp_set_cur, 'quad on', fn_dmf_get('quad on') 
  if (fn_dmf_get('quad on')) then begin
    _dcp_set_cur, 'quad rep', fn_dmf_get('quad rep')
    _dcp_set_cur, 'quad degree', fn_dmf_get('quad degree')
    _dcp_set_cur, 'quad width', fn_dmf_get('quad width')
  endif

  _dcp_set_cur, 'lin on', fn_dmf_get('smo on')  
  if (fn_dmf_get('smo on')) then begin
    _dcp_set_cur, 'lin rep', fn_dmf_get('smo rep')
    _dcp_set_cur, 'lin width', fn_dmf_get('smo width')
  endif
       
end

pro _pr_dsp_res_menu, layout

_pr_gen_ident ; dgp rev 1/28/10 debugging

     wid_name = ['pullres','option']
     
     dmf_res_cb = 'cb_dmf_res'
     
     reso = {,callback:dmf_res_cb, $
       button:'Select Resolution'}
     
     dmf_res_wid = WwOptionMenu(layout, 'Resolution:', reso, name=wid_name )
          
     name = WtGet(dmf_res_wid,/name)
     kids = dgpGetValue(dmf_res_wid,/children)
     
     res_opt = strtrim(2^(indgen(7)+4))
     idx = 1
     for i=0,n_elements(res_opt)-1  do begin
       status = WwMenuItem(dmf_res_wid, idx, res_opt(i), 'cb_dmf_res', /Add)
     endfor
     
     if (long(product(_cdr_get_pars())) eq 0) then begin
       cur_res = fn_dmf_get('hist res') - 3
     endif else begin
       cur_res = fn_dmf_get('dual res') - 3
     endelse
     status = WtSet(dmf_res_wid, {, whichItem: cur_res})
     
     _pr_wra_reg, 'reso pulldown', dmf_res_wid
     
end

pro _pr_lo_lin, lo_form

_pr_gen_ident ; dgp rev 1/28/10 debugging

     tmp    = WwText    (lo_form,'NoOpCB',/label,text='Linear Smoothing')
     tmp    = WwRadioBox(lo_form,['Enable'],'cb_dmf_smo_on', toggle=tog, /nofmany)
     _pr_wra_reg, 'lin smo on', tog(0)
     _pr_wra_set_wid, 'lin smo on', "Smooth Menu", (fn_dmf_get('smo on') eq 1)
     
     smo    = WwText(lo_form,'CB_dmf_Smo_wdth',label='Width :', $
                     text=string(fn_dmf_get('smo width'),format="(I4)"),cols=4)
     _pr_wra_reg, 'lin smo width', smo
     _pr_wra_set_wid, 'lin smo width', "Smooth Menu",fn_dmf_get('smo width')

     smo    = WwText(lo_form,'CB_dmf_Smo_rep',label='Repeat:', $
                     text=string(fn_dmf_get('smo rep'),format="(I4)"),cols=4)
     _pr_wra_reg, 'lin smo rep', smo
     _pr_wra_set_wid, 'lin smo rep', "Smooth Menu",fn_dmf_get('smo rep')

end

pro _pr_lo_quad, lo_form

_pr_gen_ident ; dgp rev 1/28/10 debugging

     tmp    = WwText    (lo_form,'NoOpCB',/label,text='Non-linear Smoothing')
     tmp    = WwRadioBox(lo_form,['Enable'],'cb_dmf_quad_on', toggle=tog, /nofmany)
     
     _pr_wra_reg, 'log smo on', tog(0)
     _pr_wra_set_wid, 'log smo on', "Smooth Menu", (fn_dmf_get('quad on') eq 1)

     spl    = WwText(lo_form,'CB_dmf_quad_degree',label='Degree:',text=string(fn_dmf_get('quad degree'),format="(I4)"),cols=4)
;     _wid_register, 'log smo degree', spl, fn_dmf_get('quad degree')
     _pr_wra_reg, 'log smo degree', spl
     _pr_wra_set_wid, 'log smo degree', "Smooth Menu",fn_dmf_get('quad degree')


     spl    = WwText(lo_form,'CB_dmf_quad_width',label='Width:',text=string(fn_dmf_get('quad width'),format="(I4)"),cols=4)
;     _wid_register, 'log smo width', spl, fn_dmf_get('quad width')
     _pr_wra_reg, 'log smo width', spl
     _pr_wra_set_wid, 'log smo width', "Smooth Menu",fn_dmf_get('quad width')

     spl    = WwText(lo_form,'CB_dmf_quad_rep',label='Repeat:',text=string(fn_dmf_get('quad rep'),format="(I4)"),cols=4)
;     _wid_register, 'log smo rep', spl, fn_dmf_get('quad rep')
     _pr_wra_reg, 'log smo rep', spl
     _pr_wra_set_wid, 'log smo rep', "Smooth Menu",fn_dmf_get('quad rep')
     
end

pro _pr_apply_smo

_pr_gen_ident ; dgp rev 1/28/10 debugging

  wid = fn_wra_get('lin smo width',"Smooth Menu")
  if (wid(0) ne 0) then _pr_dmf_set, 'smo width', dgpGetValue(wid(0))
  wid = fn_wra_get('lin smo rep',"Smooth Menu")
  if (wid(0) ne 0) then _pr_dmf_set, 'smo rep', dgpGetValue(wid(0))
  wid = fn_wra_get('log smo rep',"Smooth Menu")
  if (wid(0) ne 0) then _pr_dmf_set, 'quad rep', dgpGetValue(wid(0))
  wid = fn_wra_get('log smo width',"Smooth Menu")
  if (wid(0) ne 0) then _pr_dmf_set, 'quad width', dgpGetValue(wid(0))
  wid = fn_wra_get('log smo degree',"Smooth Menu")
  if (wid(0) ne 0) then _pr_dmf_set, 'quad degree', dgpGetValue(wid(0))
  
  draw_cells
  
end

pro cb_apply_smo, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  _pr_apply_smo

end

pro cb_smo_menu, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

   frm_name = "Smooth Menu"

  _pr_wms_set, 'cb', 'ccb_wms_close'
   
  if (fn_wms_form(frm_name,layout)) then return
 
     lo_main = WwLayout(layout)
     lo_lin  = WwLayout(lo_main,/ver)
     lo_quad = WwLayout(lo_main,/ver)
     
     _pr_lo_lin, lo_lin

     _pr_lo_quad, lo_quad

     control_lo    = WwLayout(lo_main, border=4, /Vert)

     appbut       = WwButtonBox(control_lo, 'Apply', 'cb_apply_smo')
    
     exitbut       = WwButtonBox(control_lo, 'Exit', 'cb_exit')
    
     _pr_wms_display, frm_name
           
end

pro cb_apply_scale, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  internal = dgpGetValue(wid)
  print, "Value    ",value
  print, "Internal ",internal

end
; dgp rev 9/30/05 x display scale
pro cb_xscale_lo_tog, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  internal = dgpGetValue(wid)
  print, "Value    ",value
  print, "Internal ",internal

  if (internal) then _pr_dmf_set, 'x lo', value-1

end
; dgp rev 9/30/05 x display scale
pro cb_yscale_lo_tog, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  internal = dgpGetValue(wid)
  print, "Value    ",value
  print, "Internal ",internal

  if (internal) then _pr_dmf_set, 'y lo', value-1

end

; dgp rev 9/30/05 x display scale
pro cb_xscale_hi_tog, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  internal = dgpGetValue(wid)
  print, "Value    ",value
  print, "Internal ",internal

  if (internal) then _pr_dmf_set, 'x hi', value-1

end
; dgp rev 9/30/05 x display scale
pro cb_yscale_hi_tog, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  internal = dgpGetValue(wid)
  print, "Value    ",value
  print, "Internal ",internal

  if (internal) then _pr_dmf_set, 'y hi', value-1

end

pro _pr_reflect_scale

  tog = fn_gen_get('scale wid')
  if (size(tog,/ndim) ne 2) then return
  if (fn_wid_exists(tog(0,0))) then begin
    x_lo = fn_dmf_get('x lo')
    x_hi = fn_dmf_get('x hi')
    y_lo = fn_dmf_get('y lo')
    y_hi = fn_dmf_get('y hi')
    s = WwSetValue(tog(x_lo,0),1)
    s = WwSetValue(tog(x_hi,1),1)
    s = WwSetValue(tog(y_lo,2),1)
    s = WwSetValue(tog(y_hi,3),1)
  endif

end

; dgp rev 9/30/05 x and y display scales
pro cb_dsp_scale, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  _pr_scale_defaults

  frm_name = "Scale"

  _pr_wms_set, 'cb', 'ccb_wms_close'
   
  if (fn_wms_form(frm_name,layout)) then return
 
     lo_main     = WwLayout(layout,/hor)
     scale1_lo   = WwLayout(lo_main,/ver)
     scale2_lo   = WwLayout(lo_main,/ver)
     scale3_lo   = WwLayout(lo_main,/ver)
     scale4_lo   = WwLayout(lo_main,/ver)
     
     labels = ['open','1','2','3','4','5','6','open']     

     s = WwText(scale1_lo, 'noopcb', text='X Decades',/label)
     s = WwRadioBox(scale1_lo, labels, 'cb_xscale_lo_tog', /oneofmany, /ver,toggles=tog1)
     s = WwText(scale2_lo, 'noopcb', text=' ',/label)
     s = WwRadioBox(scale2_lo, labels, 'cb_xscale_hi_tog', /oneofmany, /ver,toggles=tog2)
     s = WwText(scale3_lo, 'noopcb', text='Y Decades',/label)
     s = WwRadioBox(scale3_lo, labels, 'cb_yscale_lo_tog', /oneofmany, /ver,toggles=tog3)
     s = WwText(scale4_lo, 'noopcb', text=' ',/label)
     s = WwRadioBox(scale4_lo, labels, 'cb_yscale_hi_tog', /oneofmany, /ver,toggles=tog4)

     tog = [[tog1],[tog2],[tog3],[tog4]]

     _pr_gen_set, 'scale wid', tog

     _pr_reflect_scale

     appbut       = WwButtonBox(lo_main, 'Apply', 'cb_apply_scale')
    
     exitbut       = WwButtonBox(lo_main, 'Exit', 'cb_exit')
    
     _pr_wms_display, frm_name
           
end
; dgp rev 12/8/05 callback to create Display Form
pro cb_dsp_menu, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging
   
  _pr_wms_set, 'state', 'create'
  _pr_frm_display
  _pr_reflect_locks          

end

; dpg rev 10/23/05
function fn_dmf_valid, disp_name

  ; verify that the DMF settings don't exceed the IFS parameters
  par_max = fn_ifs_get_header('$PAR')
  pars = fn_dmf_get_other(disp_name,'pars')
     
  print, "Validate display ",disp_name

  if (max(pars) gt par_max) then begin
    print, "Display parameters invalid for display ",disp_name
      return, 0
  endif
  return, 1

end

; dgp rev 12/15/2010 does the label exists for a given key
function fn_dmf_exists_other, key, label

  common cm_dmf_info, cv_dmf_key, cv_dmf_hash

  if (size(cv_dmf_hash,/type) ne 11) then return, 0
  if (size(key,/type) ne 7) then return, 0
  if (not isaskey(cv_dmf_hash,key)) then return, 0
  return, isaskey(cv_dmf_hash(key),label)
  
end

; dgp rev 12/15/2010 force key to default value 
pro _pr_dmf_force

  _pr_gen_report1, "BeginOpenActive" ; dgp rev 1/28/10 debugging

  keys = fn_dmf_get_keys()

  if (size(keys,/ndim) eq 1) then begin
  
      for i=0,n_elements(keys)-1 do begin
        info, fn_dmf_exists_other(keys(i),'tube on')
        if not fn_dmf_exists_other(keys(i),'tube on') then _pr_dmf_set_other, keys(i), 'tube on', 1
        info, strtrim(fn_dmf_get_other(keys(i),'dot size'),2)
        if strtrim(fn_dmf_get_other(keys(i),'dot size'),2) eq '0' then _pr_dmf_set_other, keys(i), 'dot size', .05
      endfor

  endif
  
  _pr_gen_report1, "EndOpenActive" ; dgp rev 1/28/10 debugging

end

; dgp rev 9/06/05 open all active displays or 
; simple create one new display
; dgp rev 10/20/05 one exception is a display that 
; contains an inactive parameter (too many)
pro _pr_dmf_open_active

  _pr_gen_report1, "BeginOpenActive" ; dgp rev 1/28/10 debugging

  keys = fn_dmf_get_keys()

  info, keys

  if (size(keys,/ndim) eq 1) then begin
  
    idx = where(keys ne 'default',cnt)
  
    if (cnt ne 0) then begin

      _pr_gen_set, 'display index', cnt
      _pr_gen_set, 'display loop', 1
  
      for i=0,n_elements(idx)-1 do begin
        _pr_dac_create, keys(idx(i))
      endfor
  
      ; reflect the final display created
      _pr_reflect_cdr

    endif 
    
  endif
  
  _pr_gen_report1, "EndOpenActive" ; dgp rev 1/28/10 debugging

end

; dgp rev 11/1/05 link xy axis controls to 
; each parameter
pro cb_par_xytog, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  internal = dgpGetValue(wid)

  print, "Value ",value
  print, "Inter ",internal

  _pr_par_switch, strtrim(value-1,2)
  _pr_par_set, 'log', internal
  
  draw_cells

end

; dgp rev 11/1/05 get raw stat data
function fn_par_get_raw, label

  common cm_par_info, cv_par_hash, cv_par_ident
  
  if (size(cv_par_hash,/type) ne 11) then return, 0

  if (not isaskey(cv_par_hash(cv_par_ident),label)) then return, 0

  return, cv_par_hash(cv_par_ident,label)

end

; dgp rev 11/1/05 get current x parameter data
function fn_par_getx, label, sig_dig

  value = fn_par_get_raw(label)
  if (fn_dmf_get_xaxis()) then begin
    value = 10^(float(value)/float(256.0))
  endif
  return, string(value,format=sig_dig)

end

; dgp rev 11/1/05 get current y parameter data
function fn_par_gety, label, sig_dig

  value = fn_par_get_raw(label)
  if (fn_dmf_get_yaxis()) then begin
    value = 10^(float(value)/float(256.0))
  endif
  return, string(value,format=sig_dig)

end

; dgp rev 8/21/2012 init range
; dgp rev 8/21/2012
pro _pr_dmf_init_range

  Cnt = long(_FindKV("$PAR"))

  range = make_array(cnt,4,/int,value=1)

  for i=0, cnt-1 do begin
    _pr_par_switch, strtrim(i,2)
    if (fn_par_get('log') eq 1) then begin
      decade = fn_par_get('decade')  
      range(i,0) = 1
      range(i,1) = 10^float(decade)
      range(i,2) = 1
      range(i,3) = 10^float(decade)
    endif else begin
      range(i,0) = 0
      range(i,1) = _dsp_get_norm()
      range(i,2) = 0
      range(i,3) = _dsp_get_norm()
    endelse
  endfor

  print, "Init range"  
  info, range
  _pr_dmf_set, 'range', range

end

; dgp rev 8/31/07 Apply the new range values to current display
pro _pr_reflect_range

  frm_name = "Axis Range"

  if (not fn_wms_exists(frm_name)) then return

  _pr_surpress_callback
  
  ; dgp rev 1/10/08 show the current display
  _pr_wra_set_wid, 'cur display', "Axis Range",fn_dmf_current()

  ; get the current range
  range = [fn_dmf_xrange(),fn_dmf_yrange()]
  limits = [fn_dmf_xrange_lim(),fn_dmf_yrange_lim()]

  if (range(1) gt limits(1)) then range(1) = limits(1)
  if (range(3) gt limits(3)) then range(3) = limits(3)
  ; dgp rev 4/15/08 must be integer, not float
  range = long(range)

  ; dgp rev 11/5/07 use wra structure, not gen
  slide_wid = fn_wra_get('range wid',"Axis Range")

  ; dgp rev 1/10/08 Disable Y if single
  ; dgp rev 1/10/08 label current display
  s = WwSetValue(slide_wid(0),range(0))
  s = WwSetValue(slide_wid(1),range(1))
  if (fn_dmf_single()) then begin
    s = WwSetValue(slide_wid(2),/nonsensitive)
    s = WwSetValue(slide_wid(3),/nonsensitive)
  endif else begin
    s = WwSetValue(slide_wid(2),/sensitive)
    s = WwSetValue(slide_wid(3),/sensitive)
    s = WwSetValue(slide_wid(2),range(2))
    s = WwSetValue(slide_wid(3),range(3))
  endelse

  neg_wid = fn_wra_get('neg wid',"Axis Range")

  s = WwSetValue(neg_wid(0),(fn_dmf_get('neg flag') eq 1))
      
  _pr_activate_callback
  
end

; dgp rev 8/21/2012 range slider
pro CB_Slide_Range, wid, value

  case value of 
        
    1 :  _pr_dmf_xrange_min, dgpGetValue(wid)
    2 :  _pr_dmf_xrange_max, dgpGetValue(wid)
    3 :  _pr_dmf_yrange_min, dgpGetValue(wid)
    4 :  _pr_dmf_yrange_max, dgpGetValue(wid)
          
    else:
  endcase

end

; dgp rev 8/22/2012 
pro cb_apply_range, wid, value

  draw_cells

end

; dgp rev 9/1/05
; Check for widget by name.  If a valid
; widget exists, then raise it.
function fn_wms_exits, name

  common cm_wms_info, cv_main_hash

  if (isaskey(cv_main_hash,name)) then begin
    if (isaskey(cv_main_hash(name),'wid')) then begin      
      wid = cv_main_hash(name,'wid')
      return, fn_wid_exists(wid)
    endif
  endif

  return, 0
  
end

; dgp rev 8/22/2012 Update ranger form only if it exists already
pro update_dsp_range

  frm_name = "Axis Range"
  if fn_wms_exits(frm_name) then frm_dsp_range     

end

pro frm_dsp_range

  frm_name = "Axis Range"

  _pr_wms_set, 'cb', 'ccb_wms_close'
  _pr_wms_set, 'state', 'reform'
   
  if (fn_wms_form(frm_name,layout)) then return
 
     lo_main     = WwLayout(layout,/ver)
     lo_file     = WwLayout(lo_main,/hor)

     scale1_lo   = WwLayout(lo_main,/ver)
     scale2_lo   = WwLayout(lo_main,/hor)

     scale3_lo   = WwLayout(lo_main,/hor)
     scale4_lo   = WwLayout(lo_main,/hor)
     
     s = WwText(lo_file,'NoOpCB',/label,text='Current Display')
     comp = WwText(lo_file,'NoOpCB',/label,text="")
     _pr_wra_reg, 'cur display',comp

     slabel = ['X Range','X Range','Y Range','Y Range']

     ; slide range contains min and max values
     slide_range = [fn_dmf_xrange_lim(),fn_dmf_xrange_lim(),fn_dmf_yrange_lim(),fn_dmf_yrange_lim()]
          
; dgp rev 11/5/07 flag to allow negative numbers
     s = WwRadioBox(lo_main,'Negative','cb_range_neg', tog=neg_wid, /nofmany)
     _pr_wra_reg, 'neg wid', neg_wid
 
     RngX1 = WwControlsBox(scale1_lo, slabel, slide_range, 'CB_Slide_Range', $
                                /ver, /hslider, /Text, sliders=slide_array)                             
     _pr_wra_reg, 'range wid',slide_array

     ;xyzzy possible x toolkit error is scale is out of range
     ; dgp rev 11/5/07 use a reflect range routine instead of directly setting

     _pr_reflect_range

     appbut       = WwButtonBox(lo_main, 'Apply', 'cb_apply_range')
    
     exitbut       = WwButtonBox(lo_main, 'Close', 'cb_exit')
    
     _pr_wms_display, frm_name

end

; dgp rev 9/30/05 x and y display scales
; dgp rev 9/7/07 keep the data range actual, use default values
; dgp rev 1/10/08 Range is perceived value for log scale (based upon decades)
pro cb_dsp_range, wid, value

  frm_dsp_range
           
end

; dgp rev 9/06/05 create the control form
pro _pr_cdr_form

_pr_gen_ident ; dgp rev 1/28/10 debugging

   Common colour, Wht, Blk
   common levels, lvl_mn, lvl_t_add, lvl_add, lvl_mult, lvl_t_mult, lvl_cnt, lvl_t_cnt
                             
   frm_name = "Control Panel"

   names = _par_cur_names()

   cdr_pars = fn_dmf_get('pars')
   
   ; create layouts
   _pr_wms_set, 'cb', 'ccb_wms_close'
   _pr_wms_set, 'qual', '/ver'
   
   if (fn_wms_form(frm_name,layout)) then return
 
     sp = 2
     bd = 1

     lo_params   = WwLayout(layout,      /ver,spac=sp,bord=bd)
     lo_radio    = WwLayout(lo_params,   /hor,spac=sp,bord=bd)
     
     lo_parvar1  = WwLayout(lo_params,   /hor,spac=sp,bord=bd)
     lo_parvar2  = WwLayout(lo_params,   /hor,spac=sp,bord=bd)
     lo_ident    = WwLayout(lo_parvar1,  /ver,spac=sp,bord=bd)
     lo_order    = WwLayout(lo_parvar1,  /ver,spac=sp,bord=bd)
     lo_cntrl1   = WwLayout(layout,      /ver,spac=sp,bord=bd)
     lo_cntrl2   = WwLayout(layout,      /ver,spac=sp,bord=bd)

     lo_contours = WwLayout(lo_cntrl1, /ver,spac=sp,bord=bd)
     lo_form     = WwLayout(lo_cntrl2, /hor,spac=sp,bord=bd)
     lo_form2    = WwLayout(lo_cntrl2, /hor,spac=sp,bord=bd)
     lo_form4    = WwLayout(lo_cntrl2, /hor,spac=sp,bord=bd)
     lo_form6    = WwLayout(lo_cntrl1, /hor,spac=sp,bord=bd)

     lo_type     = WwLayout(lo_form,     /ver,spac=sp,bord=bd)

     lo_comb     = WwLayout(lo_form2,     /ver,spac=sp,bord=bd)
     lo_smooth     = WwLayout(lo_form4,     /hor,spac=sp,bord=bd)
     lo_stat     = WwLayout(lo_form6,     /ver,spac=sp,bord=bd)
     lo_file     = WwLayout(lo_cntrl2,    /ver,spac=sp,bord=bd)
     lo_dots     = WwLayout(lo_cntrl2,    /ver,spac=sp,bord=bd)
     lo_buttons  = WwLayout(lo_cntrl1, /ver,spac=sp,bord=bd)

     lo_move     = WwLayout(lo_buttons,  /hor,spac=sp,bord=bd)
     lo_action1  = WwLayout(lo_buttons,  /hor,spac=sp,bord=bd)
     lo_action2  = WwLayout(lo_buttons,  /hor,spac=sp,bord=bd)
     lo_action3  = WwLayout(lo_buttons,  /hor,spac=sp,bord=bd)
   
   ; create widgets

     names = _par_cur_names()

     nlist = [' ',names]
     
     xylist = replicate(' ',n_elements(nlist))

     lo_radio1 = WwLayout(lo_radio, /ver)
     lo_radio2 = WwLayout(lo_radio, /ver)
     lo_xytog  = WwLayout(lo_radio, /ver)

     par1 = WwRadioBox(lo_radio1, nlist, 'cb_par_tog', $
                    /Ver, /Oneofmany, /AlignLeft, $
                    toggles=p1vals, spac=0)

     par2 = WwRadioBox(lo_radio2, nlist, 'cb_par_tog', $
                    /Ver, /Oneofmany, /AlignLeft, $
                    toggles=p2vals, spac=0)

     txt  = WwText(lo_xytog, 'NoOpCB', /label, text='log on')
     par3 = WwRadioBox(lo_xytog, names, 'cb_par_xytog', $
                    /Ver, /nofmany, /AlignLeft, $
                    toggles=xytog, spac=0)

     _pr_wra_reg, 'Par Log', xytog     

     s    = WwText(lo_ident,'NoOpCB',/label,text='Parameter Names')
     s    = WwRadioBox(lo_ident, ['Tube Specific','Parameter Name'], 'cb_par_ident', $
                    /Ver, /Nofmany, /AlignLeft, toggles=tog, spac=0)

     _pr_wra_reg, 'Par 1', p1vals
     _pr_wra_reg, 'Par 2', p2vals
     _pr_wra_reg, 'Par Labels', tog     
     
     s    = WwText(lo_order,'NoOpCB',/label,text='Axis Order')
     s    = WwButtonBox(lo_order,'X<->Y','CB_AxOrder')


     type = WwRadioBox(lo_type, ['Dot','Contour','Surface'], 'CB_format', $
                    /Ver, /Oneofmany, /AlignLeft, $
                    toggles=tog_type)
     _pr_wra_reg, 'Dual Format', tog_type
     
     _sense_add, 'DualType', lo_type, '/sensitive'
     
     status = WwSetValue(tog_type(0), 1)                    
                    
     status = WwSetValue(p1vals(cdr_pars(0)), 1)
     status = WwSetValue(p1vals(cdr_pars(1)), /nonsensitive)
     status = WwSetValue(p2vals(cdr_pars(1)), 1)
     status = WwSetValue(p2vals(cdr_pars(0)), /nonsensitive)
                          
     combo    = WwRadioBox(lo_comb, ['Clusters','Gates'], 'CB_icp_OnOff', $
                    /Ver, /AlignLeft, /nofmany,  $
                    toggles=togs)

     _pr_wra_reg, 'Cluster On', togs(0)
     _pr_wra_reg, 'Gate On', togs(1)

                    
     _sense_add, 'cluster_on', togs(0), "/nonsensitive"
     _sense_add, 'gate_on',    togs(1), "/nonsensitive"
     _car_reflect_exists
     _mgs_reflect_exists
     
                 
     s = WwText(lo_stat,'NoOpCB',/label,text='Correlation Factor')
     comp = WwText(lo_stat,'NoOpCB',/label,text='0-1')
     stat_arr = [0,0,0,0,0,0,comp,0] 
     _stat_init, stat_arr

     s = WwText(lo_file,'NoOpCB',/label,text='Current File')
     comp = WwText(lo_file,'NoOpCB',/label,text="")
     _pr_wra_reg, 'cur file',comp
     _pr_wra_set_wid, 'cur file', frm_name,fn_dmf_get('ifs key')
     
     s = WwText(lo_file,'NoOpCB',/label,text='Current Display')
     comp = WwText(lo_file,'NoOpCB',/label,text="")
     _pr_wra_reg, 'cur display',comp
     ; dgp rev 1/27/10 fix 
     _pr_wra_set_wid, 'cur display', "Control Panel",fn_dmf_current()

     s = WwText(lo_dots,'NoOpCB',/label,text='Maximum Dots')
     range = [0,fn_dmf_get('data count')]
     dot_slide = WwControlsBox(lo_dots, "", range, 'CB_dot_slider',/text,sliders=comp)
     _pr_wra_reg, 'dot max', comp(0)
;      _dmf_reg_dots, comp(0), _dmf_get_dot_max()
     
     smo_but = WwButtonBox(lo_action1, 'Smooth', 'cb_smo_menu')
     dspbut  = WwButtonBox(lo_action1, 'Displays', 'cb_dsp_menu' ,name='Displays')

     scabut  = WwButtonBox(lo_action2, 'Scale', 'cb_dsp_scale' ,name='Displays')
     gatebut  = WwButtonBox(lo_action2, 'Gates'  ,  'cb_mgs_menu' ,name='Gates')

     expbut   = WwButtonBox(lo_action3, 'Export' ,  'cb_dex_setup'   ,name='Export Meta')
     exitbut  = WwButtonBox(lo_action3, 'Range',  'cb_dsp_range')
         
; Contour layout

     lo_smo  = WwLayout(lo_contours,/hor,spac=sp,bord=bd)
     lo_dual  = WwLayout(lo_contours,/ver,spac=sp,bord=bd)
     lo_both  = WwLayout(lo_contours,/hor,spac=sp,bord=bd)
     lo_hist1  = WwLayout(lo_contours,/hor,spac=sp,bord=bd)
     lo_hist2  = WwLayout(lo_contours,/hor,spac=sp,bord=bd)
     lo_lvl1 = WwLayout(lo_contours,/hor,spac=0,bord=0)
     lo_lvl2 = WwLayout(lo_contours,/hor,spac=0,bord=0)
     lo_lvl3 = WwLayout(lo_contours,/hor,spac=0,bord=0)
     lo_lvl4 = WwLayout(lo_contours,/hor,spac=0,bord=0)
     lo_lvl5 = WwLayout(lo_contours,/hor,spac=0,bord=0)
     lo_lvl6 = WwLayout(lo_contours,/hor,spac=0,bord=0)
     lo_lvl7 = WwLayout(lo_contours,/hor,spac=0,bord=0)
     
     lo_dual1  = WwLayout(lo_dual,/hor,spac=sp,bord=bd)
     lo_dual2  = WwLayout(lo_dual,/hor,spac=sp,bord=bd)

     _sense_add, 'DualInfo', lo_dual, '/sensitive'
     _sense_add, 'HistInfo', lo_hist1, '/sensitive'
     
; Layout for calculating levels: Minimum, Add and Multiple

     lvltmp1 = WwText    (lo_dual1,'NoOpCB',/label,text='Contour Settings:')

     lo_lvl  = WwLayout(lo_dual,/hor,spac=sp,bord=bd)

;     out     = WwRadioBox(lo_lvl, 'Manual', 'cb_dmf_lvl_man', toggle=tog, /nofmany)
     levels  = WwButtonBox (lo_dual2,'Show Levels','cb_dmf_show_levels')
     levels  = WwButtonBox (lo_dual2,'Set Levels','cb_dmf_set_lvls')
;     _sense_add, 'levels', lo_lvl, '/sensitive'

     out     = WwRadioBox(lo_dual1, ['Outliers'], 'cb_dmf_out', toggle=tog, /nofmany)
     _pr_wra_reg, 'Outliers', tog(0)

;     _pr_cdr_levels, lo_dual

     lvltmph  = WwText    (lo_hist1,'NoOpCB',/label,text='Y-Max:')
     
     lvl_mnh  = WwText    (lo_hist1,'CB_dmf_ymax_perc',text=string(fn_dmf_get('ymax perc'),format="(f6.2)"),cols=6)
     lvltmp2  = WwText    (lo_hist1,'NoOpCB',/label,text='(% of total)')

     _pr_wra_reg, 'ymax', lvl_mnh

     tab_arr = [lvl_mnh]
          
     tmp      = WwText    (lo_hist2,'NoOpCB',/label,text='Z-Max:')
     tmp      = WwText    (lo_hist2,'CB_dmf_zmax',text=string(fn_dmf_get('zmax perc'),format="(f6.2)"),cols=6)
     tmp      = WwText    (lo_hist2,'NoOpCB',/label,text='(% of total)')

; pass the new window to the exitbut
     
     _tab_init, tab_arr
   
     _pr_wms_display, frm_name
     
     _pr_reflect_cdr     
     
END

PRO cb_cdr_form, wid, index

_pr_gen_ident ; dgp rev 1/28/10 debugging

  _pr_cdr_form
  
end

pro flow_DispCells

_pr_gen_ident ; dgp rev 1/28/10 debugging

  print, "Loading cell display routines"

end
