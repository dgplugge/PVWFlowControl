declare func, _get_labels
declare func, fn_wid_exists
declare func, _cfg_get_work
declare func, _ct_get_name
declare func, _car_win_open
declare func, _contour
declare func, _wms_get_main
declare func, kmeans
declare func, fn_fcr_move_relative
declare func, fn_fcr_move_absolute
declare func, _FindKV
declare func, fn_data_get_cnt
declare func, fn_dmf_get
declare func, fn_full_indices
declare func, fn_wms_raise
declare func, fn_data_get_cells
declare func, dgpGetValue
declare func, fn_ifs_get

function _win_get_clust

  common win_map, win_tmp, win_cluster
  
  if (size(win_cluster,/type) eq 0) then win_cluster = 28
  
  return, win_cluster

end

function _car_get_auto

    common autocluster, ac_wid, ac_state
    if (size(ac_state,/type) eq 0) then begin
      ac_state = 1
    endif
    return, ac_state

end

pro _car_init_subset

  _car_indices = fn_ifs_get('car indices')

  car_subset_count = fn_data_get_cnt()
  _car_indices = lindgen(car_subset_count)
  _pr_dmf_set, 'car indices', _car_indices

  print, "Update car indices",n_elements(_car_indices)

end

pro _car_reflect_togs

  common cm_car_info, counts_cluster, means_cluster, cluster_group, cluster_enable
  common cm_car_wid,  cluster_tog

  if (size(cluster_tog,/type) ne 0) then begin
    if (fn_wid_exists(cluster_tog(0))) then begin
      for i=0,n_elements(cluster_tog)-1 do begin
        bkgr=WoColorConvert(i+2, /IndexToColor)
        s = wwSetValue(cluster_tog(i), cluster_enable(i+1))
        s = wwSetValue(cluster_tog(i), background=_ct_get_name(i+2))
      endfor
    endif
  endif

end

pro _car_update_wids

  _car_reflect_togs
  _sense_mod, 'dclubut', "/sensitive"
  _sense_mod, 'cluster_on', "/sensitive"

end

pro _mgs_reflect_wids

  ;_mgs_update_togs
  _sense_mod, 'gate_on', "/sensitive"
  _sense_mod, 'gatebut', "/sensitive"

end

function _car_active

  common icp_car_stat, _icp_car_on, _icp_car_change, _icp_car_exist, _car_change

  if (size(_icp_car_exist,/type) eq 0) then _icp_car_exist = 0
  if (size(_icp_car_on,/type) eq 0) then _icp_car_on    = 1
  
  return, ((_icp_car_exist + _icp_car_on) eq 2)

end


pro _car_update

   _car_wave_flag

   ; update forms
   _car_reflect_wids

   if (_car_active()) then begin
     ; now merge in the gates
     _ssl_recalc

   endif

   ; redraw clusters
   _car_draw
   
end


pro _car_set_togs, togs

  common cm_car_wid,  cluster_tog

  if (size(togs,/type) ne 0) then begin
    if (fn_wid_exists(togs(0))) then begin
      cluster_tog = togs
      _car_reflect_togs
    endif
  endif

end

function _car_count

  common cm_car_info, counts_cluster, means_cluster, cluster_group, cluster_enable
     
  if (size(counts_cluster,/type) eq 0) then begin
    return, 0
  endif else begin
    return, n_elements(counts_cluster)
  endelse

end

function _car_restore_ena, index

  common cm_car_info, counts_cluster, means_cluster, cluster_group, cluster_enable

  path = _cfg_get_work()

  restname = strcompress(string(path,'_clust_ena.',index),/rem) 
  list=FINDFILE(restname, Count=cntr)
  if(cntr eq 1 ) then begin
    print, "Restoring file ",restname
    Restore, restname
    if (size(enable,/type) ne 0) then begin
      cluster_enable = enable
      return, 1
    endif
  endif else begin
    cluster_enable = make_array(_car_count()+1,/int,value=1)
  endelse

  _car_wave_flag

  return, 0

end

function _car_present

  common icp_car_stat, _icp_car_on, _icp_car_change, _icp_car_exist, _car_change

  return, _icp_car_exist

end

function _car_restore_sub, index

  common cm_car_info, counts_cluster, means_cluster, cluster_group, cluster_enable
  COMMON values, thickness, file, seed_count

  path = _cfg_get_work()

  ok = 0
  restname = strcompress(string(path,'_mcs_file.',index),/rem) 
  list=FINDFILE(restname, Count=cntr)
  if(cntr eq 1 ) then begin
    print, "Restoring file ",restname
    Restore, restname
    counts_cluster = count
    means_cluster  = mean
    cluster_group  = group
    seed_count     = n_elements(counts_cluster)
    ok = 1
  endif

  return, ok

end

PRO cb_car_slide, wid, index

   COMMON values, thickness, file, seed_count
   thickness = dgpGetValue(wid)

END

function _car_get_order

  common par_order, tmp_array, par_array, wid_array, par_labels      
  return, tmp_array

end

pro _car_draw

  Common colour, Wht, Blk

  common cm_car_info, counts_cluster, means_cluster, cluster_group, cluster_enable
  COMMON values, thickness, file, seed_count

  if (_car_win_open()) then begin  
    total_clusters = sum(counts_cluster,0)

  ; set the individual cluster ratios based upon a total thickness

    siz = counts_cluster/total_clusters
    siz = siz*thickness

  ; determine the number of points on the x axis
  
  ; setup parameters
  
    labels = _get_labels()
    par_array = _car_get_order()
    order = labels(par_array(1:*)-1)
    cnt = par_array(0)
    ;print, "X Axis ticks = ",cnt
    ;print, order

  ; colors
  
    fcsname = fn_dmf_get('ifs key')

  ; create an empty window with device coordinates of 512 x 512
  ; then display an empty axis for the data corrdinates of 0-1023 x 0-1023
 
 
    wset, _win_get_clust()
    PLOT, [0, cnt-1], [0, 1023], /Nodata, xs=1, ys=1, background=wht, $
        xminor=-1, yminor=-1, color=blk, xticks=cnt-1, xtickname=order, $
        xrange=[0,cnt-1], yrange=[0,1023], title=fcsname

    for i=0L,seed_count-1 do begin
  
    ;print, "Cluster ",i+1
      if (cluster_enable(i+1)) then begin

        Color=WoColorConvert(i+2, /IndexToColor)
        oplot, means_cluster(i,par_array(1:*)-1), color=Color, thick=siz(i)
      endif
    endfor
  endif
  
end

; A cluster has been enabled or disabled.
;   Set the enable variable.  
;   Recalculate the cluster subset and combined subset (w/ gates)
;   Update related widgets 
;   Redraw the cells and/or clusters.
PRO cb_car_tog, wid, index
   
  common cm_car_info, counts_cluster, means_cluster, cluster_group, cluster_enable

   onoff = dgpGetValue(wid)
   cluster_enable(index) = onoff

   print, "Select cluster ",onoff, " from widget ",wid, " named ",wtget(wid,/name)
   
   _car_update
     
   ; redraw cells
   draw_cells   

      
END

function _car_cur_indices

   _car_indices = fn_ifs_get('car indices')

   if (size(_car_indices,/type) eq 0) then return, 0
   return, long(_car_indices)

end

function _car_get_enabled, arr

  common cm_car_info, counts_cluster, means_cluster, cluster_group, cluster_enable

  arr = make_array(1,/int)
  if (size(cluster_enable,/type) ne 0) then begin
       for i=1,n_elements(cluster_enable)-1 do begin
         if (cluster_enable(i)) then begin
           arr = [arr,i]             
         endif
       endfor
       if (n_elements(arr) gt 1) then begin
         arr = arr(1:*)
         return, 1
       endif
  endif
  return, 0

end

function _car_clean

  common icp_car_stat, _icp_car_on, _icp_car_change, _icp_car_exist, _car_change

  if (size(_car_change,/type) eq 0) then _car_change = 1
  return, (_car_change = 0)

end


; return the index to active clusters elements
; cluster_subset contains an index into the data array
pro _car_combo

 common cm_car_info, counts_cluster, means_cluster, cluster_group, cluster_enable

 if (not _car_clean()) then begin

   if (_car_get_enabled(arr)) then begin

     cluster_subset = make_array(1,/long,value=-1) 
     for i=0,n_elements(arr)-1 do begin
       next = where(cluster_group eq arr(i))
       cluster_subset = [cluster_subset,next]             
     endfor
     cluster_subset = sort(cluster_subset(1:*))
   endif else begin
     cluster_subset = fn_full_indices()
   endelse

   _pr_dmf_set, 'car indices', cluster_subset
   
 endif
 _car_clear_flag  
   
end

pro _car_show, layout
 
   cluster_lo = WwLayout(layout,border=4, /Horizontal, /Form)

   clnum = _car_count()
   if (clnum gt 0) then begin

     rad_labs = indgen(clnum)+1
   
     col = ( clnum / 5) + 1

     t1   = WwText(cluster_lo, 'NoOpCB', /Label, /Top, $
               Text='Select clusters')

     rad  = WwRadioBox(cluster_lo, rad_labs, 'cb_car_tog', $
                    /Vert, top=t1, /Nofmany, /AlignLeft, $
                    measure=col, toggle=cluster_tog)

     _car_set_togs, cluster_tog

     _car_reflect_togs
     
   endif else begin
      
     t1   = WwText(cluster_lo, 'NoOpCB', /Label, $
                 Text='No clusters set')   
   endelse
                    
end

pro _car_engine

  
  COMMON values, thickness, file, seed_count
  common cm_car_info, counts_cluster, means_cluster, cluster_group, cluster_enable
  common _car_params, _car_par_on

  active_tally = sum(_car_par_on)
  var_columns = make_array(active_tally,/int)
  param_tally = n_elements(_car_par_on)

  ii = 0
  for i=0,param_tally-1 do begin
    if (_car_par_on(i) eq 1) then begin
      var_columns(ii) = i
      ii = ii + 1
    endif
  endfor

  seed_mult = 2
  seed_index = 0
  cells = fn_data_get_cells()

  while(seed_index lt seed_count) do begin
    seeds = slice(cells(1:seed_count*seed_mult,*),1,var_columns)
    tmp_index = n_elements(seeds(*,0))
    seeds = UNIQN( seeds ) 
    seed_index = n_elements(seeds(*,0))
    diff = tmp_index - seed_index
    print, "Dups = ",diff
    seed_mult = seed_mult*2
  endwhile

  seeds = seeds(1:seed_count,*)
  
  cluster_enable = make_array(seed_count+1,value=1)
  cluster_group = KMEANS(cells, seeds, $
     var_columns    = var_columns, $
     Itmax          = 1, $
     Means_Cluster  = partial_means, $
     Ssq_Cluster    = ssq_cluster, $
     Counts_Cluster = counts_cluster)

  ap = 0
  means_cluster = make_array(seed_count,param_tally,/float)
  for p=0,param_tally-1 do begin

    for c=0,seed_count-1 do begin
      if (_car_par_on(p) eq 1) then begin    
        means_cluster(c,p) = partial_means(c,ap)
      endif else begin
        range = where(cluster_group eq c+1)
        arr   = cells(range,p)
        std = STDEV(arr, m)
        means_cluster(c,p) = m
      endelse
    endfor
    if (_car_par_on(p) eq 1) then ap = ap + 1
  endfor
    
end

pro _car_save, i

  common cm_car_info, counts_cluster, means_cluster, cluster_group, cluster_enable
  
  if (size(counts_cluster,/type) ne 0) then begin
    ; get working path
    path = _cfg_get_work()
    ; get cluster values
    count  = counts_cluster
    mean   = means_cluster
    group  = cluster_group
    enable = cluster_enable
    ; save enabled settings
    savename = strcompress(string(path,'_clust_ena.',i),/rem) 
    print, "Saving file ",savename
    save, enable, filename=savename
    ; save the subsets
    savename = strcompress(string(path,'_mcs_file.',i),/rem) 
    print, "Saving file ",savename
    save, count, mean, group, filename=savename
  endif

end


pro _car_auto

  if (fn_fcr_move_absolute(0)) then begin
  
    index = 0
    repeat begin
      _car_engine
      _car_save, index
      index = index + 1
    endrep until (not fn_fcr_move_relative(+1))
       
  endif

end

Pro cb_car_crunch, wid, index

  ;print, "Cluster callback"
  ; create a WAIT cursor for clustering period
  top = _wms_get_main('top')
  status = WtCursor('WAIT',top)
  ;id=WtTimer("ADD", 100, '_car_engine')

  ; run the cluster engine

  if(_car_get_auto()) then begin  
    _car_auto
  endif else begin
    _car_engine
    _car_save, fn_dmf_get('file index') + 1
  endelse
  _car_exists, 1  
  ; recombine clusters
  _car_wave_flag
  ; merge subsets
  _ssl_recalc

  ; close the widget after clustering
  status = WtCursor('DEFAULT', top)
  ;status=WtTimer("REMOVE", id)
  _wms_close_branch, wid, index
  
END


Pro cb_car_seeds, wid, index

   COMMON values, thickness, file, seed_count
   seed_count = dgpGetValue(wid)
   ;print, "Seed count is ",seed_count

END

pro cb_car_auto_tog, wid, index

  common autocluster, ac_wid, ac_state
  
  ac_wid = wid
  ac_state = dgpGetValue(wid)
  
end

pro cb_car_par_tog, wid, switch

  common _car_params, _car_par_on

  value = dgpGetValue(wid)
  _car_par_on(switch-1) = value

end

pro _car_reinit, params

  common _car_params, _car_par_on
  
  _car_par_on = make_array(params,value=1)

end

PRO cb_car_form, wid, index

   COMMON values, thickness, file, seed_count
   COMMON radcall, value
   common frmCL, frmCL_main, cluster_params
   
   name = "Cluster Form"

   if (fn_wms_raise(name)) then return

     top = _wms_get_main('top')
     ;print, "Getting toplevel ",top
     frmCL_main = WwMainWindow(top, layout, 'ccb_wms_destory', /Form, shell_name=name)   
     _wms_set_main, name, frmCL_main

     widget_layout = WwLayout(layout, /Vertical, /Top, /Left)


     slabel = ['Seed Count']
     seed_count = 10
     range  = [10,100]
  
     seed_slide    = WwControlsBox(widget_layout, slabel, range, 'cb_car_seeds', $
                              /Vert, /Text)

     exitbut       = WwButtonBox(widget_layout, 'Exit', 'CB_Exit', /Left, /Bottom)

     clusterbut    = WwButtonBox(widget_layout, 'Cluster', 'cb_car_crunch', $
                            /Left, /Bottom)
   
     autobut       = WwRadioBox(widget_layout, 'Auto Cluster All Files', 'cb_car_auto_tog', /Nofmany ,toggle=tog)
     results       = WwSetValue(tog(0),_car_get_auto())

     params = fix(_FindKV("$PAR"))
     ;print, "Parameters ",params
     labels        = strcompress(sindgen(params)+1,/rem)
     
     _car_reinit, params
    
     autobut       = WwRadioBox(widget_layout, labels, 'cb_car_par_tog', $
                                /Nofmany, /alignleft, toggle=cluster_params, /hor, measure=2 )
     for i=1,params do begin
       status = WwSetValue(cluster_params(i-1),1)
     endfor
    
     status        = WwSetValue(exitbut, Userdata=frmCL_main)
   
     status        = WwSetValue(clusterbut, Userdata=frmCL_main)
    
     status        = WwSetvalue(frmCL_main, /Display)
   
END

pro _car_restore, index

  if (_car_restore_sub(index)) then begin
      status = _car_restore_ena(index)
      _car_exists, 1

  endif else begin
      _car_exists, 0
  endelse
  ; wave for change
  _car_wave_flag

end


pro Flow_cluster

  print, "loading cluster routines

end
