declare func, fn_data_get_cells
declare func, fn_wms_form
declare func, fn_dmf_get
declare func, dgpGetValue

pro CB_list, wid, index

   common cm_raw_info, cv_raw_hash

   table_layout =  cv_raw_hash('layout')
   table_wid    =  cv_raw_hash('table')
   buffer       =  cv_raw_hash('buffer')
   pars_cnt     =  cv_raw_hash('pars cnt')
   viewport     =  cv_raw_hash('viewport')
   active       =  cv_raw_hash('active')
   active_tot   =  cv_raw_hash('active tot')
 
   range = dgpGetValue(wid)
      
   limit = range+buffer-1
   if (limit ge active_tot) then limit = active_tot-1
      
   view_cells = TRANSPOSE(active(range:limit,*))

   status = WtSet(table_wid, /Destroy)
   
   colwidth = make_array(pars_cnt,/int,value=2)

   table_wid    = WwTable(table_layout,'CB_table_edit',view_cells, $
                      visible=[viewport,pars_cnt],cwidth=colwidth)
   cv_raw_hash('table') = table_wid

end

pro CB_raw_hist, wid, value

  

end

pro cb_table_edit, wid, value

  print, "No editing of table"

end

pro _pr_disp_data, layout

   common cm_raw_info, cv_raw_hash

   active_tot = fn_dmf_get('data count')
   ; Is any data available
   if (active_tot eq 0) then return

   ; determine active indices and create full cell list
   idx  = fn_dmf_get('ssl indices')
   cells = fn_data_get_cells()
   active = cells(idx,*)
   ; create layouts      
   widget_layout = WwLayout(layout, /Hor)
   table_layout  = WwLayout(widget_layout, /Hor)
   ; determine cell count and parameter count
   cdim = size(active,/dim)
   cell_cnt = cdim(0)
   pars_cnt = cdim(1)
   ; setup for sliders
   slabel = ['File Range']
   count = 1
   buffer = 100
   viewport = 5
   if (buffer gt cell_cnt) then buffer = cell_cnt
   hi_range = cell_cnt-buffer
   if (hi_range le 0) then hi_range = 1
   range  = [0,hi_range]
   colwidth = make_array(pars_cnt,/int,value=2)
  
   view_cells = TRANSPOSE(active(0:buffer-1,*))
   
   seed_slide = WwControlsBox(widget_layout, slabel, range, 'CB_list', $
                              /Vert, /Text)
                                 
   table    = WwTable(table_layout,'CB_table_edit',view_cells, $
                      visible=[viewport,pars_cnt], cwidth=colwidth)

   cv_raw_hash = asarr('layout',table_layout)
   cv_raw_hash = [cv_raw_hash,asarr('table',table)]
   cv_raw_hash = [cv_raw_hash,asarr('buffer',buffer)]
   cv_raw_hash = [cv_raw_hash,asarr('pars cnt',pars_cnt)]
   cv_raw_hash = [cv_raw_hash,asarr('viewport',viewport)]
   cv_raw_hash = [cv_raw_hash,asarr('active',active)]
   cv_raw_hash = [cv_raw_hash,asarr('active tot',active_tot)]
   
   histbut  = WwButtonBox(widget_layout, 'Histgram', 'CB_raw_hist', /Left, /Bottom)   
   exitbut  = WwButtonBox(widget_layout, 'Exit', 'CB_Exit', /Left, /Bottom)   

end

pro _pr_frm_raw_data

    frm_name = "Raw Data"
  
     if (fn_wms_form(frm_name,layout)) then return
    
     widget_layout = WwLayout(layout, /Vertical)
    
; form widgets
     _pr_disp_data, widget_layout

     _pr_wms_display, frm_name
     
end

PRO cb_raw_data, wid, index

  _pr_frm_raw_data

end


pro Flow_ListCells

  print, "Raw Data Routines"

end
