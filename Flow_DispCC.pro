
declare func, _wms_get_main
declare func, _wms_close_branch
declare func, _win_get_clust
declare func, fn_gen_get
declare func, fn_wms_raise

PRO Flow_DispCC, wid, index

   Common colour, Wht, Blk
   COMMON values, thickness, file, seed_count
   common cluster, counts_cluster, means_cluster, $
                   cluster_group, cells
   COMMON radcall, value
   common settings, cluster_enable, cluster_tog
   
   name = "Cluster Cells"

   if (fn_wms_raise(name)) then return

   top = _wms_get_main('top')
   print,"Top is ",top
   
   top_clust = WwMainWindow(top, layout, 'ccb_wms_destory', /Form, shell_name="Cluster/Cells")   
   _dcl_activate
   widget_layout = WwLayout(layout, /Ver)

   dims = size(cells,/dim)
   rows = dims(1) - 1
   rad_labs = indgen(n_elements(counts_cluster))+1
   
   par_labs = indgen(rows+1)
   col = (seed_count / 10) + 1
   print, "columns = ",col

   slabel = ['Centroid thichness']
   range  = [1,200]
   
   grid_sliders = WwControlsBox(widget_layout, slabel, range, 'CB_Slider', $
                                /Vert, /Text, sliders=slide_array)
   rad  = WwRadioBox(widget_layout, rad_labs, 'CB_ClustSelect', $
                    /Top, /Left, /Vertical, /Nofmany, /AlignLeft, $
                    measure=col, toggle=cluster_tog)

   for i=0,seed_count-1 do begin
     s = wwSetValue(cluster_tog(i), cluster_enable(i+1))
   
   endfor
   t1   = WwText(widget_layout, 'NoOpCB', /Label, $
               Text='Select cluster')

   Wht=WoColorConvert(1, /IndexToColor)
   Blk=WoColorConvert(0, /IndexToColor)
   
   pixel_count = fn_gen_get('pixel count')
   
   win = _win_get_clust()

   draw     = WwDrawing(layout, win, 'CB_DrawCl', $
                    [pixel_count,pixel_count], [pixel_count,pixel_count], $
                    /NoScroll, background=Wht, foreground=Blk, $
                    Left=widget_layout, /Top, /Right, /Bottom)
   status   = WwSetValue(draw, Userdata=seed_count)
             
   siz   = n_elements(cluster_tog)
   combo  = [siz,cluster_tog]

   status = WwSetValue(draw,userdata=combo)
   
   status       = WwSetValue(slide_array(0), 100, /Sensitive)
   thickness = 100

   exitbut  = WwButtonBox(widget_layout, 'Exit', '_wms_close_branch', /Left, /Bottom)
   
   status   = WwSetvalue(top_clust, /Display)

END
