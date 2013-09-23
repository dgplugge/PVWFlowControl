declare func, _car_active
declare func, _stats_get_data
declare func, fn_dmf_get_dlist
declare func, fn_dmf_get_keys
declare func, fn_wms_exists
declare func, fn_gen_build_path
declare func, dgpGetValue

; This is the main program file for the Flow Control package
; Supplemental routines are named Flow_*.pro
; 
; Original Raw Program June 5, 2003 
; Beta version 0.9 July 20, 2005
; 

; Guidelines and design convensions
; 3 letter acronyms are used for grouping purposes
; functions are in the form:
;   fn_{xyz}_{keyword} (arg1...)
; procedures are in the form:
;   _pr_{xyz}_{keyword}, arg1...
;
;   where {xyz} describes the grouping and
;         {keyword} descrips the function or procedure
;

; Acronyms used to distinquish namespaces and data structures
; ssl - Single Subset List
; icp - Indices Combinations and Permutations
; mgs - Multiple Gating System
; car - Cluster Analysis Routines
; fnt - Font
; gen - General 
; wid - Generic Widget Routines
; cfg - Configuration file
; cdr - Cell Display Routines
; dcl - Display Clusters
; fcr - File Change Routine
; dcr - Dataset Change Routine
; cb  - Callbacks
; dcb - Drawing Area Callback
; hd  - Handlers
; dac - Display Active Cells
; ifs - Internal File Structure
; dmf - Data Manipulation Fields
; cds - Custom Display Selection

; PREFIX list
; cm  - common block
; cv  - common block variable
; fn  - function
; _pr - procedure
; ar  - argument
; cb  - callback
; hd  - handler

; dgp rev 12/5/05 general routine to create a unique name
; dgp rev 8/23/07 enhance the DMF structure for axis ordering 

; dgp rev 11/23/2010 Report on Analytics
pro _pr_gen_report2, key, value

  info, trace=tree
  line = strcompress("dgp2 key:"+key +" value:"+ string(value)+" module:"+ tree(1)+" time:"+systime(0))
  print, line

  return

end

; dgp rev 11/23/2010 Report on Analytics
pro _pr_gen_report1, value

  info, trace=tree
  line = strcompress("dgp1 value:"+ string(value)+" module:"+ tree(1)+" time:"+systime(0))
  print, line

  return

end

; dgp rev 11/23/2010 Report on Analytics
pro _pr_gen_report

  info, trace=tree
  line = strcompress("dgp module:"+ tree(1)+" time:"+systime(0))
  print, line

  return

end


function fn_gen_unique

  DT_TO_STR, today(), d, t, Date_Fmt =5, time_fmt=-1
  file = strsubst(d+t,'/','',/glob)
  file = strsubst(file,':','',/glob)
  file = STRMID(file, 0, 14)
;  nn = strtrim(long(RANDOMU(seed)*99),2)
;  return, file + nn
  return, file

end

; dgp rev 8/23/05 basic widget routine to identify the parent
function _wid_get_parent, wid

; dgp rev 2/25/2010 more robust with typing
  if (size(wid,/type) ne 3) then return, 0
  if (size(wid,/ndim) eq 1) then wid = wid(0)

  if (dgpGetValue(wid,/exists)) then begin
    if (not dgpGetValue(wid,/destroyed)) then begin
      parent = wid
      while (WtGet(parent ,/shell) ne 1) do parent = WtGet(parent ,/parent)
      return, parent
    endif
  endif
  return, 0

end

; dgp rev 8/23/05 basic widget to check for existance
function fn_wid_exists, wid

; dgp rev 2/25/2010 more robust with typing
  if (size(wid,/type) ne 3) then return, 0
  if (size(wid,/ndim) eq 1) then wid = wid(0)

  if (dgpGetValue(wid,/exists)) then begin
    if (not dgpGetValue(wid,/destroyed)) then begin
      return, 1
    endif
  endif
  return, 0

end
; dgp rev 1/5/06 get the of the parent widget
function fn_wid_parent_name, wid

  if (size(wid,/type) ne 0) then begin

    if (fn_wid_exists(wid)) then begin

      return, WtGet(_wid_get_parent(wid),/name)
      
    endif

  endif

  return, ""

end

; dgp rev 8/12/05 get widget position or return default position
function _wid_get_pos, wid

  max_x = !display_size.x
  max_y = !display_size.y
  pos = [max_x/3,max_y/3]
  if (size(wid,/type) ne 0) then begin 
    if (fn_wid_exists(wid)) then begin
      pos = dgpGetValue(_wid_get_parent(wid),/position)
    endif
  endif

  return, pos

end

; dgp rev 11/1/05 new parameter structure
function fn_par_get_sig, label, sig_dig

  common cm_par_info, cv_par_hash, cv_par_ident
  
  if (size(cv_par_hash,/type) ne 11) then return, string(0,format=sig_dig)

  if (not isaskey(cv_par_hash(cv_par_ident),label)) then return, string(0,format=sig_dig)

  return, string(cv_par_hash(cv_par_ident,label),format=sig_dig)

end

; dgp rev 11/1/05 get parameter info
function fn_par_get_index, index, label

  common cm_par_info, cv_par_hash, cv_par_ident
  
  if (size(cv_par_hash,/type) ne 11) then return, 0
  index = strtrim(index,2)
  
  if (not isaskey(cv_par_hash,index)) then return, 0

  if (not isaskey(cv_par_hash(index),label)) then return, 0

  return, cv_par_hash(index,label)

end

; dgp rev 11/1/05 get parameter info
function fn_par_get, label

  common cm_par_info, cv_par_hash, cv_par_ident
  
  if (size(cv_par_hash,/type) ne 11) then return, 0

  if (not isaskey(cv_par_hash,cv_par_ident)) then return, 0

  if (not isaskey(cv_par_hash(cv_par_ident),label)) then return, 0

  return, cv_par_hash(cv_par_ident,label)

end

; dgp rev 11/1/05 get data (force to string)
function fn_par_get_str, label

  common cm_par_info, cv_par_hash, cv_par_ident
  
  if (size(cv_par_hash,/type) ne 11) then return, ""

  if (not isaskey(cv_par_hash(cv_par_ident),label)) then return, ""

  return, string(cv_par_hash(cv_par_ident,label))

end

; dgp rev 10/12/05 returns the display settings for a given
; label or zero integer if undefined
function fn_dmf_get, label

  common cm_dmf_info, cv_dmf_key, cv_dmf_hash

  if (size(cv_dmf_hash,/type) ne 11) then return, ""
  if (size(cv_dmf_key,/type) ne 7) then return, ""
  if (not isaskey(cv_dmf_hash,cv_dmf_key)) then return, ""
  if (not isaskey(cv_dmf_hash(cv_dmf_key),label)) then return, ""
  return, cv_dmf_hash(cv_dmf_key,label)

end

; dgp rev 11/1/05 based upon the selected parameters
; return the x axis as log/lin (T/F)
function fn_dmf_get_xaxis

  par = fn_dmf_get('pars')

  xpar = par(0) - 1
  if (par(0) eq 0) then xpar = par(1) - 1

  _pr_par_switch, strtrim(xpar,2)

  val = fn_par_get('log')
  
  if (val ne 0) then val = 1

  return, val

end

; dgp rev 11/1/05 based upon the selected parameters
; return the y axis as log/lin (T/F)
function fn_dmf_get_yaxis

  par = fn_dmf_get('pars')

  ypar = par(1) - 1
  if (par(1) eq 0) then ypar = par(0) - 1

  _pr_par_switch, strtrim(ypar,2)

  val = fn_par_get('log')
  
  if (val ne 0) then val = 1

  return, val

end

; dgp rev 11/1/05 debug parameter structure
pro _pr_par_debug

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_par_info, cv_par_hash, cv_par_ident

  info, cv_par_hash, /full
  
  stop ;debug

end

; dgp rev 11/1/05 switch namespace
pro _pr_par_switch, ident

  common cm_par_info, cv_par_hash, cv_par_ident

  cv_par_ident = ident

end

; dgp rev 11/1/05 switch namespace
pro _pr_par_clear

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_par_info, cv_par_hash, cv_par_ident

  cv_par_hash = complex(1)

end

; dgp rev 11/1/05 set stat data to raw value
pro _pr_par_set, label, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_par_info, cv_par_hash, cv_par_ident

  if (size(cv_par_hash,/type) ne 11) then begin
    ; create structure
    if (size(cv_par_ident,/type) eq 0) then cv_par_ident = 'default'
    cv_par_hash = asarr(cv_par_ident,asarr(label,value))
  endif else begin
    ; append or replace
    if (isaskey(cv_par_hash,cv_par_ident)) then begin
      cv_par_hash(cv_par_ident) = [cv_par_hash(cv_par_ident),asarr(label,value)]
    endif else begin
      cv_par_hash = [cv_par_hash,asarr(cv_par_ident,asarr(label,value))]
    endelse
  endelse

end


; dgp rev 10/12/05 returns the display settings for a given
; label or blank string if undefined
function fn_dmf_get_str, label

  common cm_dmf_info, cv_dmf_key, cv_dmf_hash

  if (isaskey(cv_dmf_hash(cv_dmf_key),label)) then return, cv_dmf_hash(cv_dmf_key,label)

  return, ""

end



function fn_dmf_get_index, label, index

  common cm_dmf_info, cv_dmf_key, cv_dmf_hash

  if (isaskey(cv_dmf_hash(cv_dmf_key),label)) then begin
    if (n_elements(cv_dmf_hash(cv_dmf_key,label)) gt index) then begin
      return, cv_dmf_hash(cv_dmf_key,label,index)
    endif
  endif
  
  return, 0

end
; dgp rev 8/06/05 get a DMF value from a key other than
; the current key
function fn_dmf_get_other, key, label

  common cm_dmf_info, cv_dmf_key, cv_dmf_hash

  if (not isaskey(cv_dmf_hash,key)) then return, 0

  if (isaskey(cv_dmf_hash(key),label)) then return, cv_dmf_hash(key,label)

  return, 0
  
end

pro _gen_load_colors

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common gen_color, color_names, color_index
  
  rgb_file = "rgb.txt"
  
  record = ""
  tmp = make_array(1,/string)
  found = findfile(rgb_file,count=cnt)
  if (cnt ne 0) then begin
    print, "Load file ",found(0)
    OPENR, unit, found(0), /Get_Lun
    while (not EOF(unit)) do begin
      readf, unit, record, Format="(A)"
      result = STRSUBST(record, '[\,\"]', "", /global)
      tmp = [tmp,result]
    endwhile
    free_lun, unit
    color_names = tmp(1:*)
  endif
end

function fn_mgs_exists

  common cm_ifs, cv_ifs_struct

  return, (size(cv_ifs_struct,/type) eq 11)

end

pro _mgs_reflect_exists

  if (fn_mgs_exists()) then begin
    _sense_mod, 'gate_lo','/sensitive' 
    _sense_mod, 'gate_on', "/sensitive"
    _sense_mod, 'gatebut', "/sensitive"
  endif else begin
    _sense_mod, 'gate_lo','/nonsensitive' 
    _sense_mod, 'gate_on', "/nonsensitive"
    _sense_mod, 'gatebut', "/nonsensitive"
  endelse

end

pro _car_reflect_exists

  common icp_car_stat, _icp_car_on, _icp_car_change, _icp_car_exist, _car_change

  if (_icp_car_exist eq 1) then begin
    _sense_mod,'cluster_lo','/sensitive' 
    _sense_mod,'cluster_on','/sensitive'
    _sense_mod,'dclubut','/sensitive'
  endif else begin
    _sense_mod,'cluster_lo','/nonsensitive' 
    _sense_mod,'cluster_on','/nonsensitive'
    _sense_mod,'dclubut','/nonsensitive'
  endelse

end

function _mgs_active

  common cm_icp_mgs, cv_icp_mgs_on

  if (not fn_mgs_exists()) then return, 0

  if (size(cv_icp_mgs_on,/type) eq 0) then cv_icp_mgs_on    = 1
    
  return, cv_icp_mgs_on

end
; dgp rev 10/25/05 IDS routines link IFS and DMF
; return a count of unique keys
function fn_ids_count

  keys = fn_dmf_get_keys()
  if (keys(0) eq "") then return, 0
  ifs_list = [fn_dmf_get_other(keys(0),'ifs key')]
  for i=1, n_elements(keys)-1 do begin
    ifs_list = [ifs_list,fn_dmf_get_other(keys(i),'ifs key')]    
  endfor
  ifs_list = unique(ifs_list(sort(ifs_list)))
  return, n_elements(ifs_list)

end
; dgp rev 10/25/05 IDS routines link IFS and DMF
; return a unique list of active IFS keys
function fn_ids_unique

  keys = fn_dmf_get_keys()  
  ifs_list = [fn_dmf_get_other(keys(0),'ifs key')]
  for i=1, n_elements(keys)-1 do begin
    ifs_list = [ifs_list,fn_dmf_get_other(keys(i),'ifs key')]    
  endfor
  ifs_list = unique(ifs_list(sort(ifs_list)))
  return, ifs_list

end

; dgp rev 9/14/05 is the data already loaded for 
; a given key/file
function fn_ifs_exists, key

  common cm_ifs, cv_ifs_struct

  if (size(cv_ifs_struct,/type) eq 11) then begin
    if (isaskey(cv_ifs_struct,key)) then return, 1
  endif
  print, "Key ",key," does not exist in IFS"
  return, 0

end

; dgp rev 10/25/05 retrieve full file name from PFL structure
function fn_pfl_get_fullname, index

   common cm_pfl, cv_pfl_list, cv_file_count, cv_pfl_name_list
  
  if (index lt 0) then return, ""

  if (size(cv_pfl_list,/type) ne 0) then begin
    if (n_elements(cv_pfl_list) ge index) then begin
      name = cv_pfl_list(index)
      flist = findfile('"'+name+'"',count=cnt)
      if (cnt ne 0) then return, name
    endif
  endif
  return, ""

end

; dgp rev 10/25/05 retrieve full file name from PFL structure
function fn_pfl_get_index, key

  common cm_pfl, cv_pfl_list, cv_file_count, cv_pfl_name_list
  
  print, "Get PFL index from key ",key

  if (size(cv_pfl_name_list,/type) ne 0) then begin
    idx = where(cv_pfl_name_list eq key,cnt)
    if (cnt ne 0) then return, cv_pfl_name_list(idx(0))
  endif
  return, 0

end

; dgp rev 12/10/2010
function fn_keyword_exists, KW

  common cm_header, cv_table_hash, cv_head_hash
  
  print, "Lookup ",KW," in hash"
  if size(cv_head_hash,/type) ne 11 then return, 0
  print, "Hash found"
  if size(kw,/type) ne 7 then return, 0
  print, "Valid text"

  return, (ISASKEY(cv_head_hash, KW))
  
end

; dgp rev 12/10/2010
function _FindKV, KW

  common cm_header, cv_table_hash, cv_head_hash
  
  ;print, "Lookup ",KW," in hash"
  if (ISASKEY(cv_head_hash, KW)) then begin
    val = cv_head_hash(string(KW))
  endif else begin
    val = ' '
  endelse
  return, val
  
end

function _fcr_new_data, new_data

  common cm_dataset, cv_cells

  if (size(new_data,/type) ne 0) then begin
    cv_cells = 0
    cv_cells = Transpose(new_data)   ; If We Need It, We Got It.  
    return, 1
  endif else begin
    print, "No valid data"
    return, 0
  endelse
  
end

; dgp rev 12/10/2010 
pro _get_table_info, text_header

  _pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_header, cv_table_hash, cv_head_hash
  common cm_header2, cv_key_order
  
  cv_table_hash = asarr("count",0)
  count = 0

  key = "$SMNO"
  cv_key_order = make_array(1,/string,value=key)
  pos = strpos(text_header,key)
  pos_arr = make_array(1,/long,value=pos)

  cv_table_hash = [cv_table_hash,asarr(key,_FindKV(key))]
  keys = ASKEYS(cv_head_hash)
  for i=0,n_elements(keys)-1 do begin
    if (strmatch(keys(i),"^#")) then begin
      count = count + 1
      tmp = asarr(keys(i),string(cv_head_hash(keys(i))))
      cv_table_hash = [cv_table_hash,tmp]
      cv_key_order = [cv_key_order,keys(i)]
      pos = strpos(text_header,keys(i))
      pos_arr = [pos_arr,pos]
    endif
  endfor
  
  idx = sort(pos_arr)
  cv_key_order = cv_key_order(idx)
  
  cv_table_hash("count") = count

end


; dgp rev 12/10/2010 
pro _get_table_info, text_header

  _pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_header, cv_table_hash, cv_head_hash
  common cm_header2, cv_key_order
  
  cv_table_hash = asarr("count",0)
  count = 0

  key = "$SMNO"
  cv_key_order = make_array(1,/string,value=key)
  pos = strpos(text_header,key)
  pos_arr = make_array(1,/long,value=pos)

  cv_table_hash = [cv_table_hash,asarr(key,_FindKV(key))]
  keys = ASKEYS(cv_head_hash)
  for i=0,n_elements(keys)-1 do begin
    if (strmatch(keys(i),"^#")) then begin
      count = count + 1
      tmp = asarr(keys(i),string(cv_head_hash(keys(i))))
      cv_table_hash = [cv_table_hash,tmp]
      cv_key_order = [cv_key_order,keys(i)]
      pos = strpos(text_header,keys(i))
      pos_arr = [pos_arr,pos]
    endif
  endfor
  
  idx = sort(pos_arr)
  cv_key_order = cv_key_order(idx)
  
  cv_table_hash("count") = count

end
; dgp rev 12/16/05 determines the similar characters
function fn_similar, target, source, results

  tpos = 0
  spos = 0
  tmax = strlen(target)
  smax = strlen(source)

  done = 0
  reset = 1
  sreset = 1
  matches = ['x']

  while (done eq 0) do begin  
    if (strmid(target,tpos,1) eq strmid(source,spos,1)) then begin
      matches = [matches,strmid(target,tpos,1)]
      tpos = tpos + 1
      spos = spos + 1
      sreset = spos
    endif else begin
      spos = spos + 1
    endelse

    if (tpos gt tmax) then begin
      done = 1
    endif else begin
      if (spos gt smax) then begin
        spos = sreset
        tpos = tpos + 1 
      endif
    endelse
  endwhile  
  
  results = ""
  if (size(matches,/nelements) ne 1) then results = strjoin(matches(1:*),"")
  
  return, strlen(results)
    
end

; dgp rev 11/23/05 match the names with table entries
; if duplicate matches for keys and label, then take the 
; first match.
pro _match_namesII

  common cm_header, cv_table_hash, cv_head_hash
  common cm_ident, cv_names, ps_names, pn_names, cu_names, par_state

  ; $PxS is custom
  cu_names = pn_names

  ; grab all table keys in lowercase with special characters or spaces
  lkeys = STRLOWCASE(askeys(cv_table_hash))
  for i=0,n_elements(lkeys)-1 do begin
    lkeys(i) = strsubst(lkeys(i),'[^a-z,0-9]','',/glob)
  endfor
  keys = askeys(cv_table_hash)

  ; grab all $PxS keys in lowercase with special characters or spaces
  lnames = STRLOWCASE(pn_names)
  for i=0,n_elements(lnames)-1 do begin
    lnames(i) = strsubst(lnames(i),'[^a-z,0-9]','',/glob)
  endfor

  ; loop thru $PxS and match with table keys
  for i=0,n_elements(lnames)-1 do begin
    ; look for matching keywords
    if (last ne 0) then begin
      idx = where(strmatch(match,'488|647|594|670|fitc|pb|pe|apc|tra|cy5'),cnt)  
      if (cnt gt 0) then begin
        cu_names(key_idx) = cv_table_hash(keys(name_idx))
      endif
    endif
  endfor

end

; dgp rev 8/4/2010 setup the protocol matching
pro _pr_prt_init

  common cm_protocol, cv_protocol, cv_prolow
  
  cv_protocol = ['Alexa 488']  
  cv_protocol = [cv_protocol,'Alexa 647']  
  cv_protocol = [cv_protocol,'Alexa 594']  
  cv_protocol = [cv_protocol,'Alexa 670']  
  cv_protocol = [cv_protocol,'Fitc'] 
  cv_protocol = [cv_protocol,'PE'] 
  cv_protocol = [cv_protocol,'Pacific Blue'] 
  cv_protocol = [cv_protocol,'Texas Red'] 
  cv_protocol = [cv_protocol,'APC'] 
  cv_protocol = [cv_protocol,'Cy-5'] 
  cv_protocol = [cv_protocol,'Cy-7'] 
  
  cv_prolow = strlowcase(cv_protocol)
  
end

; dgp rev 8/4/2010 find match 
function fn_prt_match, text

  common cm_protocol, cv_protocol, cv_prolow

  text = strlowcase(text)
  if (strmatch(text,'(488|647|594|670)',reg)) then begin
    w = STRMATCH(cv_prolow,reg(0)) 
    idx = WHERE(w,cnt)
    if (cnt eq 0) then begin
      return, "no match"
    endif else begin
      val = cv_protocol(idx)
      return, val(0)
    endelse
  endif else if strmatch(text,'pe') then begin
    w = STRMATCH(cv_prolow,'pe') 
    val = cv_protocol(WHERE(w))
    return, val(0)
  endif else if strmatch(text,'fitc') then begin
    w = STRMATCH(cv_prolow,'fitc') 
    val = cv_protocol(WHERE(w))
    return, val(0)
  endif else if strmatch(text,'apc') then begin
    w = STRMATCH(cv_prolow,'apc') 
    val = cv_protocol(WHERE(w))
    return, val(0)
  endif else if strmatch(text,'(p.*b.*)') then begin
    w = STRMATCH(cv_prolow,'(p.*b.*)') 
    val = cv_protocol(WHERE(w))
    return, val(0)
  endif else if strmatch(text,'(cy.*)([0-9])',reg) then begin
    w = STRMATCH(cv_prolow,'cy.*'+reg(2)) 
    idx = WHERE(w,cnt)
    if (cnt eq 0) then begin
      return, "no match"
    endif else begin
      val = cv_protocol(idx)
      return, val(0)
    endelse
  endif else if strmatch(text,'(t.*r.*)') then begin
    w = STRMATCH(cv_prolow,'(t.*r.*)') 
    val = cv_protocol(WHERE(w))
    return, val(0)
  endif else begin
  return, "no match"
  endelse

end

; dgp rev 12/7/2010 get raw value of label or '0'
function fn_upa_get, label

  common cm_upa_info, cv_upa_static, cv_upa_hash, cv_upa_ident
  
  if (size(cv_upa_hash,/type) ne 11) then return, 0

  if (not isaskey(cv_upa_hash,cv_upa_ident)) then return, 0

  if (not isaskey(cv_upa_hash(cv_upa_ident),label)) then return, 0

  print, "UPA retrieving ",label," from ",cv_upa_ident

  return, cv_upa_hash(cv_upa_ident,label)

end

; dgp rev 11/23/05 match the names with table entries
; if duplicate matches for keys and label, then take the 
; first match.
pro _match_names

  common cm_header, cv_table_hash, cv_head_hash

  print, "EIB "+_findkv(strcompress("EIB1N",/remove_all))
  print, "PN "+_findkv(strcompress("$P1N",/remove_all))
  print, "PS "+_findkv(strcompress("$P1S",/remove_all))
  return

  ; $PxS is custom
  cu_names = fn_upa_get('2')

  if (_findkv(strcompress("EIB1N",/remove_all)) ne ' ') then return

  ; grab all table keys in lowercase with special characters or spaces
  keys = askeys(cv_table_hash)
  
  for i=0,n_elements(keys)-1 do begin

    match = fn_prt_match(keys(i))
    if (match ne "no match") then begin
      if (size(tables,/type) ne 11) then begin
        tables = asarr(match,keys(i))
      endif else begin
        tables = [tables,asarr(match,keys(i))]
      endelse
    endif

  endfor

  lnames = fn_upa_get('2')

  if (size(tables,/type) eq 11) then begin
    for i=0,n_elements(lnames)-1 do begin
      match = fn_prt_match(lnames(i))
      if (match ne "no match") then begin
        if (size(names,/type) ne 11) then begin
          names = asarr(match,lnames(i))
        endif else begin
          names = [tables,asarr(match,lnames(i))]
        endelse
      endif
    endfor

  cu_names = fn_upa_get('2')

  endif
  
end

; dgp rev 10/26/05 placeholder for an invalid data file
; log the read error
pro _pr_ifs_error, key, text

_pr_gen_ident ; dgp rev 1/28/10 debugging

    common cm_ifs, cv_ifs_struct

    tmp = asarr('status',0)
    tmp = [tmp,asarr('message',text)]

; create, append or modify the entire structure
    if (size(cv_ifs_struct,/type) eq 11) then begin
      cv_ifs_struct = [cv_ifs_struct,asarr(key,tmp)]
    endif else begin
      cv_ifs_struct = asarr(key,tmp)
    endelse

; don't need to trim the IFS as error take up
; no data memory

end

; dgp rev 10/25/05 get file name (ifs key) for a 
; given index or return blank.  This routine check
; for the existance of the file
function fn_pfl_get_name, index

  common cm_pfl, cv_pfl_list, cv_file_count, cv_pfl_name_list
  
  if (size(cv_pfl_list,/type) ne 0) then begin
    if (n_elements(cv_pfl_list) ge index) then begin
      fullname = cv_pfl_list(index)
      flist = findfile('"'+fullname+'"',count=cnt)
      parsefilename, fullname, filename=name
      if (cnt ne 0) then return, name
    endif
  endif
  return, ""

end

; dgp rev 12/3/2010 User Protocol Annotation Exists
function fn_upa_exist

  common cm_upa_info, cv_upa_static, cv_upa_hash, cv_upa_ident
  
  if (size(cv_upa_hash,/type) ne 11) then return, 0

  return, isaskey(cv_upa_hash,cv_upa_ident)

end

; dgp rev 9/13/05 read the data for the next file
; to display, on error create a placeholder in IFS
function fn_ifs_read, index

  fullpath = fn_pfl_get_fullname(index)
  key = fn_pfl_get_name(index)
  _pr_upa_switch, key

  _pr_gen_report1, "BeginRead"
  _pr_gen_report2, "File ",fullpath

  print, fullpath
  
  on_ioerror, open_err
; Open the data file for reading
  openr, unit, fullpath, /Get_lun
; grab fixed header info
  hdr = bytarr(61)

  on_ioerror, read_fx_err
  readu, unit, hdr
    
  locs = string(hdr)
; parse the fixed header
  FCS_VER    = strmid(locs,0,10)
  text_start = long(strmid(locs,10,8))
  text_end   = long(strmid(locs,18,8))
  data_start = long(strmid(locs,26,8))
  data_end   = long(strmid(locs,34,8))
  separator  = strmid(locs,59,3)
; prep to read text portion    
  text_bytes = bytarr(text_end - text_start)
  
  point_lun, unit,  data_end
  point_lun, -unit, test_end
  _pr_gen_report2, "Bytes ",test_end
  if (data_end gt test_end) then begin
    print, "File is trucated - ",data_end, test_end
  endif
  
; read text header
  point_lun, unit, text_start
  on_ioerror, read_hd_err
  readu, unit, text_bytes

; replace any non-ASCII characters (over 126) with
; a space (ASCII 32)

  idx = where(text_bytes gt 126,cnt)
  if (cnt gt 0) then text_bytes(idx) = 32

  text = string(text_bytes)
; parse text header
  _Parse_Text, text

  if fn_upa_exist() then begin
    print, "Protocol Already exists for ",key
  endif else begin
    _pr_upa_extract
  endelse

; gather the table information
  ;_get_table_info, text
    
; attempt to match parameter names with table info
  ;_match_names

  ; get the event count from the $tot value
  
  file_cnt = _FindKV("$TOT")
  _pr_gen_report2, "Total ",file_cnt

  file_cnt = long(file_cnt)  

  if (file_cnt eq 0) then begin
    err_text = "No data in file"
    goto, error_return
  endif

  if (file_cnt mod 2 eq 1) then file_cnt = file_cnt - 1
  
  ; get the parameter count from the $PAR value
  
  NumParms = _FindKV("$PAR")
  ;print, NumParms, " Parameters"
  _pr_gen_report2, "Parameters ",NumParms

  order = indgen(numparms)

  Databytes = Data_End - Data_Start + 1
  Event_Bytes = Databytes / file_cnt
 
  ; get the number of bits from the $PxB values
  
  Bytes = fix(_FindKV("$P1B"))
  ;print, bytes, " Bytes"
  res = _FindKV("$P1R")
  _dsp_set_norm, long(res)
  ;print, res, " Resolution"
  datatype = strtrim(_FindKV("$DATATYPE"),2)
  print, "Data type is ",datatype
  print, "Resolution is ",res
  print, "Bytes ",bytes

  if (bytes eq 8) then begin
    Parmarray = bytarr(Numparms, file_cnt)
  endif else if (bytes EQ 16) then begin
    Parmarray = intarr(Numparms, file_cnt)
  endif else if (bytes EQ 32) then begin
    if (datatype eq 'I') then begin
      Parmarray = lonarr(Numparms, file_cnt)
    endif else begin
      Parmarray = fltarr(Numparms, file_cnt)
    endelse
  endif

  info, parmarray

  ;print, "Container is ",numparms*file_cnt
  ;print, "Bytes to read are ",databytes
  ;print, "Starting at ",Data_start

  Point_Lun, Unit, Data_Start
  bigendian2 = '3,4,1,2'
  bigendian4 = '4,3,2,1'

  on_ioerror, read_da_err
  Readu, Unit, Parmarray
  
  FREE_LUN, Unit

  byte_order = _FindKV("$BYTEORD")
  print, "Byte order ",byte_order

  if(byte_order eq bigendian4) then begin
    if (bytes EQ 16) then begin
      print, "Short swap"
      byteorder, Parmarray, /sswap
    endif else if (bytes EQ 32) then begin
      print, "Long swap"
      byteorder, Parmarray, /lswap
    endif
  endif

  if(_FindKV("$P1R") eq '262144') then begin
    offset  = 1000.0
    parmarray = parmarray + offset
  endif

  if (_fcr_new_data(parmarray)) then begin
    ; once the file information is properly loaded into the 
    ; temporary common blocks, it can be moved to IFS
    _pr_ifs_add, fullpath

    _pr_gen_report1, "EndRead"
    return, 1
  endif else begin
    err_text = "Failed Read"
    print, err_text
    goto, error_return
  endelse
  
  open_err:
    err_text = "Open Error"
    print, err_text
    goto, error_return
  read_fx_err:
    err_text = "Fixed Read Error"
    _pr_err_mess, unit, err_text
    FREE_LUN, Unit
    goto, error_return
  read_hd_err:
    err_text = "Header Read Error"
    _pr_err_mess, unit, err_text
    FREE_LUN, Unit
    print, "Header ends at ",text_end
    goto, error_return
  read_da_err:
    err_text = "Data Read Error"
    _pr_err_mess, unit, err_text
    FREE_LUN, Unit
    print, "Data ends at ",data_end
    goto, error_return

  error_return:

  parsefilename, fullpath, filename=key
  _pr_ifs_error, key, err_text
  return, 0

end

; dgp rev 10/26/05 get all IFS as an array
; if no structure, then blank array
function fn_ifs_get_keys

  common cm_ifs, cv_ifs_struct
  
  if (size(cv_ifs_struct,/type) eq 11) then begin
    keys = askeys(cv_ifs_struct)
    return, keys
  endif
  return, [""]

end
; dgp rev 10/26/05 get a keyword value from the current
; instance of the IFS structure
; dgp rev 6/2/2010 more robust validation
function fn_ifs_get, label

  common cm_ifs, cv_ifs_struct
  
  if (size(cv_ifs_struct,/type) eq 11) then begin
    if (isaskey(cv_ifs_struct(fn_dmf_get('ifs key')),label)) then begin
      if (isaskey(cv_ifs_struct,fn_dmf_get('ifs key'))) then begin
        return, cv_ifs_struct(fn_dmf_get('ifs key'),label)
      endif
    endif
  endif
  return, 0

end

; dgp rev 9/06/05 check for a valid key
function fn_dmf_key_exists, key

  common cm_dmf_info, cv_dmf_key, cv_dmf_hash
  
  if (isaskey(cv_dmf_hash,key)) then begin
    ; key exists
    if (isaskey(cv_dmf_hash(key),'main widget')) then begin
      ; wid id exists
      wid = cv_dmf_hash(key,'main widget')
      if (wid ne 0) then begin
        if (fn_wid_exists(wid)) then begin
          ; does widget name match key -- widgets can be reused
          if (key eq WtGet(wid,/name)) then return, 1 
        endif
      endif
    endif
  endif 
  return, 0

end


; dgp rev 9/28/05 get an individual parameter for the data location
function fn_ifs_display, display, index

  common cm_ifs, cv_ifs_struct
  
  par_cnt = fn_ifs_get('par cnt')
  if index ne 2 then index = 1
  if not fn_dmf_key_exists(display) then return, [0]
  pars = fn_dmf_get_other(display,'pars')
  if (product(pars) eq 0) then begin
    ; single
    pidx = sum(pars)
  endif else begin
    ; dual
    pidx = pars(index-1)
  endelse
  if (pidx gt par_cnt) then return, [0]
  key = fn_dmf_get_other(display,'ifs key')

  if (not isaskey(cv_ifs_struct,key)) then return, [0]
  if (not isaskey(cv_ifs_struct(key),'data')) then return, [0]
  
  ; dgp rev 5/24/2010 handle the case of gating out
  if (size(fn_dmf_get_other(display,'ssl indices'),/ndim) eq 0) then return, [0]

  return, cv_ifs_struct(key,'data',fn_dmf_get_other(display,'ssl indices'),pidx-1)

end



; dgp rev 9/28/05 get an individual parameter for the data location
function fn_ifs_param, index

  common cm_ifs, cv_ifs_struct
  
  par_cnt = fn_ifs_get('par cnt')
  if (index ge par_cnt) then index = par_cnt - 1

  if (isaskey(cv_ifs_struct(fn_dmf_get('ifs key')),'data')) then begin
    return, cv_ifs_struct(fn_dmf_get('ifs key'),'data',*,index)
  endif
  return, 0

end

function fn_gen_get, label

  common cm_gen, cv_gen_hash
  
  if (size(cv_gen_hash,/type) ne 11) then return, 0
  if (isaskey(cv_gen_hash,label)) then begin
    return, cv_gen_hash(label)
  endif
  return, 0

end

function fn_gen_getstr, label

  common cm_gen, cv_gen_hash
  
  if (size(cv_gen_hash,/type) ne 11) then return, ""
  if (isaskey(cv_gen_hash,label)) then begin
    return, strtrim(cv_gen_hash(label),2)
  endif
  return, ""

end

; dp rev 12/3/2010 Retrieve the labels
function fn_ifs_labels_exist

  common cm_ifs, cv_ifs_struct
  
  if (isaskey(cv_ifs_struct,fn_dmf_get('ifs key'))) then begin
    if (isaskey(cv_ifs_struct(fn_dmf_get('ifs key')),'labels')) then return, 1
  endif
  return, 0

end

; dgp rev 12/8/2010 get the appropriate labels
function fn_upa_get_labels

  common cm_upa_info, cv_upa_static, cv_upa_hash, cv_upa_ident
  
  index = fn_dmf_get('parm on') + 2*fn_dmf_get('tube on')
  label = strtrim(index,2)

  cv_upa_ident = fn_dmf_get('ifs key')
  
  if (size(cv_upa_hash,/type) ne 11) then return, 0

  if (not isaskey(cv_upa_hash,cv_upa_ident)) then return, 0

  if (not isaskey(cv_upa_hash(cv_upa_ident),label)) then return, 0

  print, "UPA retrieving ",label," from ",cv_upa_ident

  return, cv_upa_hash(cv_upa_ident,label)  

end


; dgp rev 10/20/05 get a header keyword value from the
; current IFS value related to the current DMF
function fn_ifs_get_header, key, ifs_key 

  common cm_ifs, cv_ifs_struct

  if (size(ifs_key,/type) eq 0) then ifs_key = fn_dmf_get('ifs key')

; dgp rev 4/27/2010 more robust
  if (size(cv_ifs_struct,/type) eq 11) then begin
    if (isaskey(cv_ifs_struct,ifs_key)) then begin
      if (isaskey(cv_ifs_struct(ifs_key),'header')) then begin
        if (isaskey(cv_ifs_struct(ifs_key,'header'),key)) then begin
          return, cv_ifs_struct(ifs_key,'header',key)
        endif
      endif
    endif
  endif
  return, 0

end

function fn_data_get_par_cnt
 
  return, fn_ifs_get('par cnt')

end

function fn_data_get_cnt

  return, fn_ifs_get('total')

end

function fn_data_get_cells

  return, fn_ifs_get('data')

end

function fn_full_indices
   
   cnt = fn_data_get_cnt()
   if (cnt eq 0) then begin
     arr = 0
   endif else begin   
     arr = lindgen(cnt)
   endelse
   return, arr

end

pro _mgs_set_on, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_icp_mgs, cv_icp_mgs_on

  value = value mod 2
  cv_icp_mgs_on = value
  _ssl_recalc

  ; update forms
  ; _mgs_update_wids
  
end

pro _car_wave_flag

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common icp_car_stat, _icp_car_on, _icp_car_change, _icp_car_exist, _car_change

  _icp_car_change = 1
  
end

pro _car_clear_flag

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common icp_car_stat, _icp_car_on, _icp_car_change, _icp_car_exist, _car_change

  _icp_car_change = 0
  
end

pro _car_set_on, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common icp_car_stat, _icp_car_on, _icp_car_change, _icp_car_exist, _car_change

  value = value mod 2

  _icp_car_on = value
  
  _car_wave_flag
  
  _ssl_recalc
  
  ;draw_cells
  
end

pro _car_exists, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common icp_car_stat, _icp_car_on, _icp_car_change, _icp_car_exist, _car_change

  value = value mod 2
  
  _icp_car_exist = value
  _car_reflect_exists
  
end

pro _pr_fnt_debug

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_fnt, cv_fnt_hash

  info, /full
  
  stop ;debug

end
; dgp rev 10/18/05 INIT font namespace 
pro _pr_fnt_init

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_fnt, cv_fnt_hash
  
  dmf_font = 'Times New Roman, 12, Bold'
  stat_font = 'Arial, 12, Regular'
  cv_fnt_hash = asarr('dmf font',dmf_font)
  cv_fnt_hash = [cv_fnt_hash,asarr('stat font',stat_font)]
  cv_fnt_hash = [cv_fnt_hash,asarr('mgs ratio',0.8)]
  cv_fnt_hash = [cv_fnt_hash,asarr('title ratio',0.7)]
  cv_fnt_hash = [cv_fnt_hash,asarr('sub ratio',0.6)]
  cv_fnt_hash = [cv_fnt_hash,asarr('table ratio',0.6)]
  cv_fnt_hash = [cv_fnt_hash,asarr('applied ratio',0.8)]
  device, font=dmf_font 

end
; dgp rev 10/18/05 SET font namespace 
pro _pr_fnt_set, label, val

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_fnt, cv_fnt_hash
  
  cv_fnt_hash = [cv_fnt_hash,asarr(label,val)]

end
; dgp rev 10/18/05 GET font namespace 
function fn_fnt_get, label

  common cm_fnt, cv_fnt_hash

  if (size(cv_fnt_hash,/type) ne 11) then return, ""

  if (isaskey(cv_fnt_hash,label)) then return, cv_fnt_hash(label)

  return, 0

end

; stubs go here
; the answer for not loading all code

; pfl - Process File List
; ger - Gate Enabling Routines - handle any changes in the enabling/disabling of gates
; ssl - Subset List - keeps track to subset values based upon gating and clustering
; dcr - Data Change Routines - handles the process of changing entire data sets.
; fcr - File Change Routines - handles the process of move through the existing data set

; given gate name, set the parameter values

; dgp rev 10/27/05 one location to for setting parameter columns
pro _pr_wra_debug

  common cm_wra_info, cv_wra_hash, cv_wra_form, cv_wra_ident

  stop ;debug

end

; dpg rev 10/27/05 register a widget with respective form
pro _pr_wra_reg_mod, form_name, id, wid

  common cm_wra_info, cv_wra_hash, cv_wra_form, cv_wra_ident

  ; make widget an array even if a single value
  if (size(wid,/ndim) eq 0) then wid = [wid]

;  print, "Reregister ",id," to form ",form_name

  if (size(cv_wra_hash,/type) ne 11) then begin
    ; no hash yet, create new
    cv_wra_hash  = asarr(id,asarr(form_name,wid))
    cv_wra_form  = asarr(form_name,[id])
    cv_wra_ident = asarr(id,[form_name])
    return
  endif

  ; append to existing hash
  if (isaskey(cv_wra_hash,id)) then begin
    if (isaskey(cv_wra_hash(id),form_name)) then begin
      ; replace with new value
      cv_wra_hash(id,form_name) = wid
    endif else begin
      ; id exists, but no form
      cv_wra_hash(id)        = [cv_wra_hash(id),asarr(form_name,wid)]
    endelse
  endif else begin
    ; no id, form irrelative
      cv_wra_hash = [cv_wra_hash,asarr(id,asarr(form_name,wid))]
  endelse

  if (isaskey(cv_wra_form,form_name)) then begin
     ; add to form widgets
    cv_wra_form(form_name) = [cv_wra_form(form_name),[id]]
  endif else begin
    ; no form
    cv_wra_form = [cv_wra_form,asarr(form_name,[id])]
  endelse

  if (isaskey(cv_wra_ident,id)) then begin
    ; ident exists
    cv_wra_ident(id)          = [cv_wra_ident(id),[form_name]]
  endif else begin
    ; new ident
    cv_wra_ident = [cv_wra_ident,asarr(id,[form_name])]
  endelse

end

; dpg rev 10/27/05 register a widget with respective form
pro _pr_wra_reg, id, wid

  common cm_wra_info, cv_wra_hash, cv_wra_form, cv_wra_ident

  ; make widget an array even if a single value
  if (size(wid,/ndim) eq 0) then wid = [wid]
  ; determine the form name
  parent = _wid_get_parent(wid(0))
  form_name = WtGet(parent,/name)

;  print, "Register ",id," to form ",form_name

  if (size(cv_wra_hash,/type) ne 11) then begin
    ; no hash yet, create new
    cv_wra_hash  = asarr(id,asarr(form_name,wid))
    cv_wra_form  = asarr(form_name,[id])
    cv_wra_ident = asarr(id,[form_name])
    return
  endif

  ; append to existing hash
  if (isaskey(cv_wra_hash,id)) then begin
    if (isaskey(cv_wra_hash(id),form_name)) then begin
      ; replace with new value
      cv_wra_hash(id,form_name) = wid
    endif else begin
      ; id exists, but no form
      cv_wra_hash(id)        = [cv_wra_hash(id),asarr(form_name,wid)]
    endelse
  endif else begin
    ; no id, form irrelative
      cv_wra_hash = [cv_wra_hash,asarr(id,asarr(form_name,wid))]
  endelse

  if (isaskey(cv_wra_form,form_name)) then begin
     ; add to form widgets
    cv_wra_form(form_name) = [cv_wra_form(form_name),[id]]
  endif else begin
    ; no form
    cv_wra_form = [cv_wra_form,asarr(form_name,[id])]
  endelse

  if (isaskey(cv_wra_ident,id)) then begin
    ; ident exists
    cv_wra_ident(id)          = [cv_wra_ident(id),[form_name]]
  endif else begin
    ; new ident
    cv_wra_ident = [cv_wra_ident,asarr(id,[form_name])]
  endelse

end

pro _pr_wra_set_index, id, form, index, value

  common cm_wra_info, cv_wra_hash, cv_wra_form, cv_wra_ident

  if (not isaskey(cv_wra_ident,id)) then return

  if (not isaskey(cv_wra_form,form)) then return

  if (not isaskey(cv_wra_hash(id),form)) then return

  if (n_elements(cv_wra_hash(id,form)) gt index) then begin
    if (fn_wid_exists(cv_wra_hash(id,form,index))) then begin
      s = WwSetValue(cv_wra_hash(id,form,index),value)
    endif
  endif

end
; dgp rev 9/9/05 set the widget on the given form to the value
; dgp rev 10/19/05 use widget class to validate the value type
pro _pr_wra_set_wid, id, form, value



  common cm_wra_info, cv_wra_hash, cv_wra_form, cv_wra_ident

  if (not isaskey(cv_wra_ident,id)) then return

  if (not isaskey(cv_wra_form,form)) then return

  if (not isaskey(cv_wra_hash(id),form)) then return

  wid = cv_wra_hash(id,form,0)

  if (fn_wid_exists(wid)) then begin

    wid_class = WtGet(wid,/class)
    if (wid_class eq 7) then begin
      ; on/off toggle
      s = WwSetValue(wid,byte(value))
    endif else begin
      s = WwSetValue(wid,strtrim(value,2))
    endelse
  endif

end

function fn_wra_get_index, id, form, index

  common cm_wra_info, cv_wra_hash, cv_wra_form, cv_wra_ident
  
  if (not isaskey(cv_wra_ident,id)) then return, 0

  if (not isaskey(cv_wra_form,form)) then return, 0

  if (not isaskey(cv_wra_hash(id),form)) then return, 0

  if (n_elements(cv_wra_hash(id,form)) gt index) then begin 

    if (fn_wid_exists(cv_wra_hash(id,form,index))) then return, cv_wra_hash(id,form,index)

  endif
  
  return, 0
  
end
; dgp rev 10/23/05 WRA function to get form widget setting
function fn_wra_get, id, form

  common cm_wra_info, cv_wra_hash, cv_wra_form, cv_wra_ident
  
  if (size(cv_wra_hash,/type) ne 11) then return, 0
  
  if (not isaskey(cv_wra_ident,id)) then return, 0

  if (not isaskey(cv_wra_form,form)) then return, 0

  if (not isaskey(cv_wra_hash(id),form)) then return, 0

  return, cv_wra_hash(id,form)
  
end
; dgp rev 10/27/05 enable a widget (sensitive)
pro _pr_wra_enable, ident

  common cm_wra_info, cv_wra_hash, cv_wra_form, cv_wra_ident

  if (isaskey(cv_wra_ident,ident)) then begin
    forms = cv_wra_ident(ident)
    if (size(forms,/ndim) eq 0) then begin
      if (fn_wms_exists(forms)) then begin
        wid = cv_wra_hash(ident,forms)
        if (fn_wid_exists(wid(0))) then s = WwSetValue(wid(0),/sensitive)
      endif
    endif else begin
      for i=0,n_elements(forms)-1 do begin
        if (fn_wms_exists(forms(i))) then begin
          wid = cv_wra_hash(ident,forms(i))
          if (fn_wid_exists(wid)) then s = WwSetValue(wid,/sensitive)
        endif
      endfor
    endelse
  endif

end

; dgp rev 10/27/05 disable a widget (nonsensitive)
pro _pr_wra_disable, ident



  common cm_wra_info, cv_wra_hash, cv_wra_form, cv_wra_ident

  if (isaskey(cv_wra_ident,ident)) then begin
    forms = cv_wra_ident(ident)
    if (size(forms,/ndim) eq 0) then begin
      if (fn_wms_exists(forms)) then begin
        wid = cv_wra_hash(ident,forms)
        if (fn_wid_exists(wid(0))) then s = WwSetValue(wid(0),/nonsensitive)
      endif
    endif else begin
      for i=0,n_elements(forms)-1 do begin
        if (fn_wms_exists(forms(i))) then begin
          wid = cv_wra_hash(ident,forms(i))
          if (fn_wid_exists(wid)) then s = WwSetValue(wid,/nonsensitive)
        endif
      endfor
    endelse
  endif

end

; dgp rev 10/23/05 set the id for the WRA widget to value
; dgp rev 12/7/05 modified to handle all forms in which the 
; widget is located
pro _pr_wra_set, id, value



  common cm_wra_info, cv_wra_hash, cv_wra_form, cv_wra_ident

  if (size(cv_wra_hash,/type) ne 11) then return
  if (size(id,/type) ne 7) then return

  if (isaskey(cv_wra_hash,id)) then begin
    if (isaskey(cv_wra_ident,id)) then begin
      if (size(cv_wra_ident(id),/ndim) eq 0) then begin
        form = cv_wra_ident(id)
        wid = cv_wra_hash(id,form)
        if (size(wid,/ndim) eq 0) then wid = [wid]
        if (fn_wid_exists(wid(0))) then begin
          s = WwSetValue(wid(0),string(value))
        endif
      endif else begin
        form_arr = cv_wra_ident(id)
        for i=0,n_elements(keys)-1 do begin
          wid = cv_wra_hash(id,form_arr(i))
          if (size(wid,/ndim) eq 0) then wid = [wid]
          if (fn_wid_exists(wid(0))) then begin
            s = WwSetValue(wid(0),string(value))
          endif
        endfor
      endelse

    endif
  endif

end

pro _pr_dmf_debug

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_dmf_info, cv_dmf_key, cv_dmf_hash
  stop ;debug

end

; the DMF key is the basis for all DMF and IFS operations
; dgp rev 10/27/05 set all DMF entries for a given label
pro _pr_dmf_set_all, label, val

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_dmf_info, cv_dmf_key, cv_dmf_hash
  
  keys = askeys(cv_dmf_hash)

  for i=0,n_elements(keys)-1 do begin

    cv_dmf_hash(keys(i)) = [cv_dmf_hash(keys(i)),asarr(label,val)]

  endfor
  
end

; dgp rev 10/28/05 set defaults for all DMF
; only if valid value is not defined
pro _pr_dmf_def_all, label, val

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_dmf_info, cv_dmf_key, cv_dmf_hash
  
  keys = askeys(cv_dmf_hash)

  for i=0,n_elements(keys)-1 do begin

    if (not isaskey(cv_dmf_hash(keys(i)),label)) then begin
      cv_dmf_hash(keys(i)) = [cv_dmf_hash(keys(i)),asarr(label,val)]
    endif

  endfor
  
end

; dgp rev 10/28/05 reflect the parameter settings and labels
pro _pr_reflect_pars

  frm_name = 'Control Panel'

  if (not fn_wms_exists(frm_name)) then return 

  ; set the parameter combination
  cdr_pars = fn_dmf_get('pars')

  p1vals = fn_wra_get('Par 1', "Control Panel")
  p2vals = fn_wra_get('Par 2', "Control Panel")
  parlog = fn_wra_get('Par Log', "Control Panel")
  ; for each parameter, enable and deselect
  for i=0,n_elements(p1vals)-1 do begin
    s = WwSetValue(p1vals(i),0,/sensitive)
    s = WwSetValue(p2vals(i),0,/sensitive)
  endfor
  ; for active parameters, select and disable opposite column
  s = WwSetValue(p1vals(cdr_pars(0)),1)
  s = WwSetValue(p2vals(cdr_pars(1)),1)
  s = WwSetValue(p2vals(cdr_pars(0)),/nonsensitive)
  s = WwSetValue(p1vals(cdr_pars(1)),/nonsensitive)
  ; get parameter names    

  par_names = fn_upa_get_labels()
  ; for each parameter, set name
  for i=1,n_elements(p1vals)-1 do begin
       status = WtSet(p1vals(i), {, text: par_names(i-1)})
       status = WtSet(p2vals(i), {, text: par_names(i-1)})
       status = WtSet(parlog(i-1), {, text: par_names(i-1)})
  endfor

  pl1 = fn_wra_get_index('Par Labels','Control Panel',0) 
  if fn_wid_exists(pl1) then s = WwSetValue(pl1,(fn_dmf_get('tube on') eq 1),/sensitive)

  pl1 = fn_wra_get_index('Par Labels','Control Panel',1) 
  if fn_wid_exists(pl1) then s = WwSetValue(pl1,(fn_dmf_get('parm on') eq 1),/sensitive)
  
end

pro _pr_upa_debug

  common cm_upa_info, cv_upa_static, cv_upa_hash, cv_upa_ident

  if size(cv_upa_hash,/type) eq 11 then keys = askeys(cv_upa_hash)

  stop ; debug

end

; dgp rev 10/20/05 Display a trace tree
pro _pr_dbg_trace

_pr_gen_ident ; dgp rev 1/28/10 debugging

    info, trace=tree
    print, "Trace Tree"
    pm, tree

end
; the DMF key is the basis for all DMF and IFS operations
pro _pr_dmf_set, label, val

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_dmf_info, cv_dmf_key, cv_dmf_hash
  
;  if (label eq 'data count') then _pr_dbg_trace

  cv_dmf_hash(cv_dmf_key) = [cv_dmf_hash(cv_dmf_key),asarr(label,val)]
    
end

pro _pr_dmf_set_index, label, index, val

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_dmf_info, cv_dmf_key, cv_dmf_hash

  if (isaskey(cv_dmf_hash(cv_dmf_key),label)) then begin
    if (n_elements(cv_dmf_hash(cv_dmf_key,label)) gt index) then begin
      cv_dmf_hash(cv_dmf_key,label,index) = val
    endif
  endif

end

pro _pr_dmf_set_qp, label, val

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_dmf_info, cv_dmf_key, cv_dmf_hash

  if (isaskey(cv_dmf_hash(cv_dmf_key),'quick plot')) then begin
    if (isaskey(cv_dmf_hash(cv_dmf_key,'quick plot'),label)) then begin
      cv_dmf_hash(cv_dmf_key,'quick plot',label) = [cv_dmf_hash(cv_dmf_key,'quick plot',label),val]
    endif else begin
      cv_dmf_hash(cv_dmf_key,'quick plot') = [cv_dmf_hash(cv_dmf_key,'quick plot'),asarr(label,val)]
    endelse
  endif

end

; the DMF key is the basis for all DMF and IFS operations
pro _pr_dmf_set_other, key, label, val

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_dmf_info, cv_dmf_key, cv_dmf_hash

  if (isaskey(cv_dmf_hash,key)) then begin
    cv_dmf_hash(key) = [cv_dmf_hash(key),asarr(label,val)]
  endif
  
end

pro _pr_mgs_debug

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_mgs_info, cv_mgs_struct
  
  stop ;debug
  
end
; dgp rev 8/23/07 set pars including the axis order
pro _gen_set_pars, name

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_mgs_info, cv_mgs_struct
  
  print, "Get parameters for key ",name
  if (ISASKEY(cv_mgs_struct,name)) then begin
    par = cv_mgs_struct(name).params
    print, "Go to parameters are ",par
    _pr_dmf_set, 'pars', par
    _pr_reflect_pars
  endif

end

function _gen_get_name, key

  common cm_mgs_info, cv_mgs_struct
  
  print, "Get name for key ",key
  if (ISASKEY(cv_mgs_struct,key)) then begin
    name = cv_mgs_struct(key).name
  endif else begin
    name = ""
  endelse
  return, name

end

function _mgs_get_name, key

  common cm_mgs_info, cv_mgs_struct

  if (size(cv_mgs_struct,/type) eq 11) then begin
    if (isaskey(cv_mgs_struct,key)) then begin
      name = cv_mgs_struct(key).uname
      return, name
    endif
  endif
  return, 0

end

pro _wid_info, wid

_pr_gen_ident ; dgp rev 1/28/10 debugging

  name  = WtGet(wid,/name)
  class = dgpGetValue(wid,/class)
  print, name," class ",class

end

pro _wid_tree, wid

_pr_gen_ident ; dgp rev 1/28/10 debugging

  print, "Wid ",WtGet(wid,/name)
  ;parent = _wid_get_parent(wid)
  parent = wid
  if (fn_wid_exists(parent)) then begin
    children = dgpGetValue(parent,/child)
    i = 0
    while (children(i) ne 0) do begin
        _wid_info, children(i)
        new_kids = dgpGetValue(children(i),/child)
        if (new_kids(0) ne 0) then begin
          children = [children,new_kids]
        endif
        i = i + 1
        if (i ge n_elements(children)) then return
    endwhile
  endif

end

function _wid_get_child, wid

  print, "Find child for ",wid
  if (dgpGetValue(wid,/exists)) then begin
    if (not dgpGetValue(wid,/destroyed)) then begin
      child = wid
      while (child ne 0) do begin
        arr = WtGet(child ,/child)
        last = child
        child = arr(0)
      endwhile
      print, "Child is ",last
      return, last
    endif
  endif else begin
    print, "Widget no longer exists"
  endelse
  return, 0

end

function _get_labels

  common par_order, tmp_array, par_array, wid_array, par_labels      
  
  return, par_labels

end

pro _par_init

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common par_order, tmp_array, par_array, wid_array, par_labels    

  labels = _get_labels()
  cnt = n_elements(labels)
  gen_arr = indgen(cnt) + 1
  tmp_array = [cnt,gen_arr]

end

function _set_labels, arr

  common par_order, tmp_array, par_array, wid_array, par_labels      
  
  par_labels = arr
  
  return, 1

end

; Series of routines to set and change the parameter labeling

; update to the label widgets to reflect the current names
; dgp rev 10/23/05 remove direct reference to pxvals common block
; instead use WRA structure

; dgp rev 10/23/05
function _par_cur_names

  par_names = fn_upa_get_labels()

  if (size(par_names,/ndim) eq 0) then begin

    par_names = strtrim(indgen(fn_data_get_par_cnt())+1,2)

  endif

  return, par_names

end

; dgp rev 12/8/2010 Changing the parameter naming
pro cb_par_ident, wid, value

  _pr_gen_ident ; dgp rev 1/28/10 debugging

  if value eq 1 then begin
    print, "Toggle parm"
    _pr_dmf_set,'tube on', dgpGetValue(wid)
  endif else begin
    print, "Toggle tube"
    _pr_dmf_set,'parm on', dgpGetValue(wid)
  endelse

  _pr_reflect_pars
  draw_cells

end

; dgp rev 12/8/2010 Changing the parameter naming
pro cb_par_identx, wid, value

  _pr_gen_ident ; dgp rev 1/28/10 debugging

  if value eq 1 then begin
    _pr_dmf_set,'parm on', dgpGetValue(pl1)
  endif else begin
    _pr_dmf_set,'tube on', dgpGetValue(pl1)
  endelse


  pl1 = fn_wra_get_index('Par Labels','Control Panel',0) 
  pl2 = fn_wra_get_index('Par Labels','Control Panel',1) 

  index = dgpGetValue(pl1) + (dgpGetValue(pl2) * 2)
  index = index mod 4

  print, "Par Ident "
  print, dgpGetValue(wid)
  print, index
  print, dgpGetValue(pl1)
  print, dgpGetValue(pl2)

  _pr_reflect_pars
  draw_cells

end

; dgp rev 12/7/2010 template for raw set of label with variable
pro _pr_upa_set, label, value

  common cm_upa_info, cv_upa_static, cv_upa_hash, cv_upa_ident

  print, "UPA set ",label," for file ",cv_upa_ident
  info, value
  if (size(cv_upa_hash,/type) ne 11) then begin
    ; create structure
    if (size(cv_upa_ident,/type) ne 7) then cv_upa_ident = "default"
    cv_upa_hash = asarr(cv_upa_ident,asarr(label,value))
  endif else begin
    ; append or replace
    if (isaskey(cv_upa_hash,cv_upa_ident)) then begin
      cv_upa_hash(cv_upa_ident) = [cv_upa_hash(cv_upa_ident),asarr(label,value)]
    endif else begin
      cv_upa_hash = [cv_upa_hash,asarr(cv_upa_ident,asarr(label,value))]
    endelse
  endelse

end

; dgp rev 12/7/2010 template for raw set of label with variable
pro _pr_upa_set_static, label, value

  common cm_upa_info, cv_upa_static, cv_upa_hash, cv_upa_ident

  print, "UPA setting ",label
  if (size(cv_upa_static,/type) ne 11) then begin
    ; create structure
    cv_upa_static = asarr(label,value)
  endif else begin
    ; append or replace
    cv_upa_static = [cv_upa_static,asarr(label,value)]
  endelse

end

; dgp rev 12/7/2010 template for raw set of label with variable
pro _pr_upa_switch, key

  common cm_upa_info, cv_upa_static, cv_upa_hash, cv_upa_ident

  print, "File UPA ",key
  ; simply make sure key is a string
  if (size(key,/type) eq 7) then cv_upa_ident = key

end

; dgp rev 12/7/2010 get raw value of label or '0'
function fn_upa_get_static, label

  common cm_upa_info, cv_upa_static, cv_upa_hash, cv_upa_ident
  
  if (size(cv_upa_static,/type) ne 11) then return, 0
  
  ; make sure label is a string
  if (size(label,/type) ne 7) then return, 0

  if (not isaskey(cv_upa_static,label)) then return, 0

  return, cv_upa_static(label)

end

; dgp rev 12/7/2010 
pro _pr_upa_extract

  params = fix(_FindKV("$PAR"))
  ;results = _set_common('params',params)
  names  = make_array(params,/string)
  for i=1, params do begin
    kw = strcompress("$P"+string(i)+"N",/remove_all)
    kv = _findkv(kw)
    names(i-1) = kv
  endfor
  _pr_upa_set, '0', names
  _pr_upa_set, '1', names
  _pr_upa_set, '2', names
  _pr_upa_set, '3', names

  kw = strcompress("EIB",/remove_all)
  if not fn_keyword_exists(kw) then return

  for i=1, params do begin
    kw = strcompress("EIB"+string(i)+"H",/remove_all)
    kv = _findkv(kw)
    names(i-1) = kv
  endfor
  
  _pr_upa_set, '1', names

  combo = names

  for i=1, params do begin
    kw = strcompress("EIB"+string(i)+"C",/remove_all)
    kv = _findkv(kw)
    names(i-1) = kv
    combo(i-1) = combo(i-1) + " - " + kv
  endfor
  _pr_upa_set, '2', names
  _pr_upa_set, '3', combo
    
  kw = strcompress("EIB"+string(1)+"K",/remove_all)
  if fn_keyword_exists(kw) then begin
    key = _findkv(kw)
    val = _findkv(strcompress("EIB"+string(1)+"V",/remove_all))
    sublabels = asarr(key,val)
    idx = 2
    while fn_keyword_exists(strcompress("EIB"+string(idx)+"K",/remove_all)) do begin
      key = _findkv(strcompress("EIB"+string(idx)+"K",/remove_all))
      val = _findkv(strcompress("EIB"+string(idx)+"V",/remove_all))
      sublabels = [sublabels,asarr(key,val)]
      idx = idx + 1
    endwhile
    _pr_upa_set, 'sublabels', sublabels
  endif

end

; dgp rev 11/28/05 parse the text portion of header
Pro _Parse_Text, text

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_header, cv_table_hash, cv_head_hash
  
  sep = STRMID(text, 0, 1)
  len = fix(STRLEN(text))

  test_ascii = byte(sep)
  
  if (test_ascii(0) eq 12) then sep = '\014'
 
  items = strsplit(text,sep)

  ; dgp rev 8/23/07 handle header errors by making header array even
  hd_cnt = n_elements(items)
    
  if (hd_cnt gt 2) then begin
    cv_head_hash = asarr(items(1),items(2))
    ; after first pair - loop
    for i=3,hd_cnt-3,2 do begin
      cv_head_hash = [cv_head_hash,asarr(items(i),items(i+1))]
    endfor
    if (hd_cnt) then begin
      cv_head_hash = [cv_head_hash,asarr(items(hd_cnt-2),items(hd_cnt-1))]
    endif else begin
      ; header is odd, so add a blank
      cv_head_hash = [cv_head_hash,asarr(items(hd_cnt-1),"")]
    endelse
  endif

end

function fn_get_gates_on 

  common cm_dmf_info, cv_dmf_key, cv_dmf_hash

  arr = [""]
  ; check that applied gates exist
  if (isaskey(cv_dmf_hash(cv_dmf_key),'applied')) then begin
    ; check hash
    keys = askeys(cv_dmf_hash(cv_dmf_key,'applied'))
    for i=0,n_elements(keys)-1 do begin
      if (cv_dmf_hash(cv_dmf_key,'applied',keys(i))) then arr = [arr,keys(i)]
    endfor
    if (n_elements(arr) gt 1) then arr = arr(1:*)
  endif
  return, arr
  
end

function fn_mgs_get_enable, key

  common cm_dmf_info, cv_dmf_key, cv_dmf_hash

  ;print, "Get enable ",key

  if (isaskey(cv_dmf_hash(cv_dmf_key),'applied')) then begin
    ; subset has been previously created, therefore check for key
    if (isaskey(cv_dmf_hash(cv_dmf_key,'applied'),key)) then begin
      ; key exists, so return it
    endif else begin
      ; key defaults to 'off'
      cv_dmf_hash(cv_dmf_key,'applied') = [cv_dmf_hash(cv_dmf_key,'applied'),asarr(key,0)]
    endelse
  endif else begin
    ; create the first subset
    cv_dmf_hash(cv_dmf_key) = [cv_dmf_hash(cv_dmf_key),asarr('applied',asarr(key,0))]
  endelse

  return, cv_dmf_hash(cv_dmf_key,'applied',key)

end

pro _pr_mgs_set_enable, key, val

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_dmf_info, cv_dmf_key, cv_dmf_hash

  print, "Set enable ",key," to ",val

  if (isaskey(cv_dmf_hash(cv_dmf_key),'applied')) then begin
    ; enable hash has been previously created, therefore just change it
      cv_dmf_hash(cv_dmf_key,'applied') = [cv_dmf_hash(cv_dmf_key,'applied'),asarr(key,val)]
  endif else begin
    ; create the first enable hash
    cv_dmf_hash(cv_dmf_key) = [cv_dmf_hash(cv_dmf_key),asarr('applied',asarr(key,val))]
  endelse

end

function _dsp_get_norm

  common dsp_set2, _dsp_norm
  
  if (size(_dsp_norm,/type) eq 0) then _dsp_norm = 1024
  return, _dsp_norm

end

pro _dsp_set_norm, res

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common dsp_set2, _dsp_norm
  
  _dsp_norm = res

end
; dgp rev 11/21/05 recalculate the gate rectangle
; for the current dataset
function _mgs_recalc_rect, key

  common cm_mgs_info, cv_mgs_struct
  
  params = cv_mgs_struct(key).params
  ; roi contains x range, then y range
  roi = cv_mgs_struct(key).roi

  cells = fn_data_get_cells()

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
                       
  return, subset

end

function _mgs_recalc_poly, key

  common cm_mgs_info, cv_mgs_struct
  
  cells = fn_data_get_cells()
    
  params = cv_mgs_struct(key).params
  reso   = cv_mgs_struct(key).reso
  xverts   = long(cv_mgs_struct(key).xverts)
  yverts   = long(cv_mgs_struct(key).yverts)

  norm = _dsp_get_norm()

; get region and parameters

  count = n_elements(cells(*,0))

; Create an on/off filter for the ROI in a grid

  squ = index_conv(lonarr(norm,norm),POLYFILLV(xverts, yverts, norm, norm))

  roi_count = n_elements(squ(*,0))

  filtr = intarr(norm,norm)

; for each ROI mark the filter with a 1

  for i=0l,roi_count-1 do filtr(squ(i,0),squ(i,1)) = 1
 
; For each event check it with the filter and if it passed add it to the mask

  mask = where(filtr(cells(*,params(0)-1),cells(*,params(1)-1)) eq 1, cnt)

  return, mask

end

function _mgs_recalc_hist, key

  common cm_mgs_info, cv_mgs_struct

  params = cv_mgs_struct(key).params
  roi = cv_mgs_struct(key).roi

; get parameters
    param = long(sum(params))
    print, "Parameter ",param

    ;print, "Using parameter ",param
    cells = fn_data_get_cells()
    ;print, "Number of events = ",count

; determine minimums and maximums
    xmin = min([roi(0),roi(1)])
    xmax = max([roi(0),roi(1)])

    if (xmin lt 0) then xmin = 0

; calc subset
    subset = long(where((cells(*,param-1) ge xmin) and $
                        (cells(*,param-1) le xmax)))

    return, subset

end

function _mgs_recalc_gate, key

  common cm_mgs_info, cv_mgs_struct
  
  print, "Recalculating key ",key
    
  algor = cv_mgs_struct(key).type
  if (algor EQ 'rect') then begin
    subset = _mgs_recalc_rect(key)
  endif else if (algor EQ 'poly') then begin
    subset = _mgs_recalc_poly(key)
  endif else if (algor EQ 'hist') then begin
    subset = _mgs_recalc_hist(key)
  endif else begin
    print, "None"
  endelse
  
  return, subset

end

function fn_dmf_current

  common cm_dmf_info, cv_dmf_key, cv_dmf_hash

  if (size(cv_dmf_key,/type) ne 0) then return, cv_dmf_key
  return, 0

end

function fn_mgs_get_subset, gate_key

  common cm_ifs, cv_ifs_struct

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
  
    return, cv_ifs_struct(file_key,'subsets',gate_key)

  endif else begin
    print, "**** Coding error, ifs key not found"
    return, 0
  endelse

end
; dgp rev 12/12/05 get the label information for a given gate key
; using the current display settings
function fn_mgs_get, label, gate_key

  common cm_dmf_info, cv_dmf_key, cv_dmf_hash

  default = 1
  if (label eq 'stats') then default = 0

  if (isaskey(cv_dmf_hash(cv_dmf_key),label)) then begin
    if (isaskey(cv_dmf_hash(cv_dmf_key,label),gate_key)) then begin
    ; label and gate exist
    endif else begin
    ; label exists, but gate not yet defined
        cv_dmf_hash(cv_dmf_key,label) = [cv_dmf_hash(cv_dmf_key,label),asarr(gate_key,default)]
    endelse
  endif else begin
    ; no label, must default
        cv_dmf_hash(cv_dmf_key) = [cv_dmf_hash(cv_dmf_key),asarr(label,asarr(gate_key,default))]
  endelse

  return, cv_dmf_hash(cv_dmf_key,label,gate_key)

end

pro _pr_mgs_set, label, gate_key, val

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_dmf_info, cv_dmf_key, cv_dmf_hash

  if (isaskey(cv_dmf_hash(cv_dmf_key),label)) then begin
    if (isaskey(cv_dmf_hash(cv_dmf_key,label),gate_key)) then begin
    ; label and gate exist
      cv_dmf_hash(cv_dmf_key,label,gate_key) = val
    endif else begin
    ; label exists, but gate not yet defined
        cv_dmf_hash(cv_dmf_key,label) = [cv_dmf_hash(cv_dmf_key,label),asarr(gate_key,val)]
    endelse
  endif else begin
    ; no label, must default
        cv_dmf_hash(cv_dmf_key) = [cv_dmf_hash(cv_dmf_key),asarr(label,asarr(gate_key,val))]
  endelse

end

function _mgs_get_count

  common cm_mgs_info, cv_mgs_struct
     
  if (size(cv_mgs_struct,/type) eq 11) then begin
     
     return, n_elements(cv_mgs_struct)
     
  endif

  return, 0

end

function _mgs_present

  common cm_mgs_info, cv_mgs_struct
     
  if (size(cv_mgs_struct,/type) eq 11) then begin
     
     return, 1
     
  endif

  return, 0

end

function _mgs_get_keys, keys

  common cm_mgs_info, cv_mgs_struct
     
  if (size(cv_mgs_struct,/type) eq 11) then begin

     keys =  ASKEYS(cv_mgs_struct)
     return, 1
     
  endif

  return, 0

end

; dgp rev 10/25/05 get file name (ifs key) for a 
; given index or return blank
function fn_pfl_get_key, index

  common cm_pfl, cv_pfl_list, cv_file_count, cv_pfl_name_list
  
  if (size(cv_pfl_name_list,/type) ne 0) then begin
    if (n_elements(cv_pfl_name_list) ge index and index ge 0) then begin
      return, cv_pfl_name_list(index)
    endif
  endif
  return, ""

end

function _cfg_get_work

  work_path = fn_gen_get('work path')
  if (size(work_path,/type) eq 7) then begin
    return, work_path
  endif
  return, ""
  
end

pro _pr_err_mess, unit, txt

_pr_gen_ident ; dgp rev 1/28/10 debugging

  print, txt
  POINT_LUN, -unit, pos
  print, "I/O Error - ",!Err_String
  print, "File Position is ",pos

end

pro _pr_gen_debug

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_gen, cv_gen_hash

  info
  stop ;debug
  
end
; dgp rev 12/5/05 set variable in general namespace
pro _pr_gen_set, label, val

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_gen, cv_gen_hash

  if (size(label,/type) eq 0) then return
  if (size(val,/type) eq 0) then return

  if (size(cv_gen_hash,/type) ne 11) then begin
    cv_gen_hash = asarr(label,val)
  endif else begin
    cv_gen_hash = [cv_gen_hash,asarr(label,val)]
  endelse  
  
end

function fn_pfl_get_count

  common cm_pfl, cv_pfl_list, cv_file_count, cv_pfl_name_list
    
    return, cv_file_count

end

pro _pr_ifs_debug

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_ifs, cv_ifs_struct

  stop ;debug

end

; dgp rev 8/11/05 ifs routine to remove item list
pro _pr_ifs_remove, arr

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_ifs, cv_ifs_struct
  
; get keys and loops thru
  keys = askeys(cv_ifs_struct)
  full_cnt = n_elements(keys)
  res = wherein(keys,arr,cnt)
  
  keep_list = make_array(full_cnt-cnt,/string)
  index = 0
   
  if (cnt ne 0) then begin
    for i=0, n_elements(keys)-1 do begin
      res = wherein(arr,keys(i),cnt)
      if (cnt eq 0) then begin
        keep_list(index) = keys(i)
        index = index + 1
      endif
    endfor
  endif

  tmp = asarr(keep_list(0),cv_ifs_struct(keep_list(0)))
  for i=1, n_elements(keep_list)-1 do begin
    tmp = [tmp,asarr(keep_list(i),cv_ifs_struct(keep_list(i)))]
  endfor
  
  cv_ifs_struct = 0
  cv_ifs_struct = tmp

end

; dgp rev 8/11/05 fifo routines to reduce the number of
; data files in memory at any time.
; q_max is the limit
; q_chunk is removed once hitting the limit
pro _pr_gen_fifo, item

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_filo, cv_queue, q_max, q_chunk

  ; if q_max equals display count, then all display
  ; will remain in memory

  q_max = fn_ids_count()
  if (size(cv_queue,/type) eq 0) then begin
    q_chunk = 1
    cv_queue = [string(item)]
  endif else begin
    ; is the file already loaded
    results = where(cv_queue eq item,cnt)
    if (cnt eq 0) then begin
      ; add item to queue
      cv_queue = [cv_queue,string(item)]
      ; does the queue need to be cleared
      if (n_elements(cv_queue) ge q_max) then begin
        _pr_ifs_remove, cv_queue(0:q_chunk-1)
        cv_queue = cv_queue(q_chunk:*)
      endif
    endif
  endelse

  info, cv_queue
  pm, cv_queue

end

; dgp rev 10/3/08 incorporate more error checking into keyword request
function fn_Find_KV, KW

  common cm_header, cv_table_hash, cv_head_hash
  
  if (size(cv_head_hash,/type) ne 11) then return, ""
  if (size(kw,/type) ne 7) then return, ""
  
  ;print, "Lookup ",KW," in hash"
  if (ISASKEY(cv_head_hash, KW)) then begin
    val = cv_head_hash(string(KW))
  endif else begin
    val = ' '
  endelse
  return, val
  
end

; dgp rev 1/3/08 set the decades
pro _pr_tnl_decades, value

  common cm_scale, cv_decades, cv_reso

  cv_decades = value

end

; dgp rev 10/24/07 grab a wild character set of parameter values
function fn_par_wild, pre, suf

  par_cnt = fn_Find_KV("$PAR")
  arr = sindgen(par_cnt)
  
  for i=0, par_cnt-1 do begin
    key = pre + strtrim(i+1,2) + suf
    arr(i) = strtrim(fn_Find_KV(key),2)
  endfor
  
  return, arr

end


; dgp rev 11/1/05 load the parameter information for given file
; $PxE contains exponent setting -- log or lin
pro _pr_par_load

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_header, cv_table_hash, cv_head_hash

  _pr_par_clear
  ps_names = fn_upa_get('1')
  cu_names = fn_upa_get('3')
  
  for i=0,n_elements(ps_names)-1 do begin
    _pr_par_switch, strtrim(i,2)
    kw = '$P' + strtrim(i+1,2) + 'E'
    if (isaskey(cv_head_hash,kw)) then begin
      vals = strsplit(cv_head_hash(kw),",")
      value = fix(vals(0))
      decade = value
      ; default to 4 decades
      if (decade eq 0) then decade = 4
      _pr_par_set, 'decade', decade
      _pr_par_set, 'log', fix(value gt 0)
    endif else begin
      _pr_par_set, 'decade', 4
      if (cu_names(i) eq 'FCS-H') then begin
        _pr_par_set, 'log', fix(0)
      endif else begin
        _pr_par_set, 'log', fix(1)
      endelse
    endelse
  endfor

end

; dgp rev 10/18/05 initialize the first file
pro _pr_ifs_init

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_ifs, cv_ifs_struct
  common cm_dataset, cv_cells
  common cm_header, cv_table_hash, cv_head_hash
  common cm_header2, cv_key_order
        
  ; get the name of the first file in the list
  fullpath = fn_pfl_get_fullname(0)  
  ; determine the number of files in list
  max_cnt =  fn_pfl_get_count()

  ; start at begin and read until successful or end of list
  index = 0
  while ((not fn_ifs_read(index)) and (index lt max_cnt-1)) do begin
    ; read until a valid file is found or 
    ; the end of the list
    index = index + 1

  endwhile

  if (index ge max_cnt) then begin
    print, "Failed to load any of the files"
    return
  endif

  ; successfully loaded file
  key = fn_pfl_get_name(index)

; retrieve the information to add to the structure
  arr = size(cv_cells,/dim)

  ps_names = fn_upa_get('1')
  pn_names = fn_upa_get('2')
  cu_names = fn_upa_get('3')


; create the current structure
  tmp = asarr('data',cv_cells)
  tmp = [tmp,asarr('header',cv_head_hash)]
;  tmp = [tmp,asarr('table',cv_table_hash)]
;  tmp = [tmp,asarr('key order',cv_key_order)]
  tmp = [tmp,asarr('labels',[[ps_names],[pn_names],[cu_names]])]
  tmp = [tmp,asarr('total',arr(0))]
  tmp = [tmp,asarr('par cnt',arr(1))]
  tmp = [tmp,asarr('filename',key)]
  tmp = [tmp,asarr('status',1)]
  tmp = [tmp,asarr('message','Successful Init')]

  _pr_par_load

; create IFS structure with only one key
  cv_ifs_struct = asarr(key,tmp)
  
end

; dgp rev 1/28/10 
pro _pr_dmf_report

  keys = fn_dmf_get_keys()

  cnt = n_elements(keys)
  
  if (cnt eq 0) then return

  for i=0, cnt-1 do begin

    line = keys(i)
    wid = fn_dmf_get_other(keys(i),'main widget')
    line = line + " wid " + strtrim(wid,2)
    print, line
    if (wid ne 0) then begin
      if (fn_wid_exists(wid)) then begin
        print, " " + WtGet(wid,/name)
      endif
    endif

  endfor

end

; dgp rev 8/26/05 return a sorted list of active displays, including "default"
function fn_dsp_active

  arr = ["default"]
  
  keys = fn_dmf_get_keys()

  nkeys = n_elements(keys)
  
  for i=0,nkeys-1 do begin
    ; skip default
    if (keys(i) ne 'default') then begin
      ; check for valid key
      if (fn_dmf_key_exists(keys(i))) then begin
        arr = [arr,keys(i)]
      endif
    endif
  endfor

  return, arr
  
end
; dgp rev 10/26/05 loop thru displays and retrieve
; list of active IFS data files.  Remove all but the active 
; files and the default DMF
pro _pr_ifs_cull

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_ifs, cv_ifs_struct
  
  ; get a list of all IFS keys
  ifs_keys = askeys(cv_ifs_struct)

  ; get a list of all active displays
  dmf_keys = fn_dsp_active()

  ; no valid DMF structure (ie before dmf_init)
  if (dmf_keys(0) eq "") then return

  ; get the IFS key for the DMF default
  ifs_key = fn_dmf_get_other('default','ifs key')

  ; create a copy of the IFS record for DMFdefault
  ; this initializes the new IFS structure

  if (not isaskey(cv_ifs_struct,ifs_key)) then begin
    ; ifs the default DMF setting has invalid IFS key
    ifs_key = ifs_keys(0)
    _pr_dmf_set_other, 'default', 'ifs key', ifs_key
    _pr_dmf_set_other, 'default', 'file index', fn_pfl_get_index(ifs_key)
  endif

  ; temporary new structure
  tmp = asarr(ifs_key,cv_ifs_struct(ifs_key))
  ; list of keys in structure
  ifs_used = [ifs_key]

  for i=0, n_elements(dmf_keys)-1 do begin
    ifs_key = fn_dmf_get_other(dmf_keys(i),'ifs key')
    idx = where(ifs_used eq ifs_key,cnt)
    if (cnt eq 0) then begin
      ; skip if already loaded
      tmp = [tmp,asarr(ifs_key,cv_ifs_struct(ifs_key))]
      ifs_used = [ifs_used,ifs_key]
      print, "Keep ifs key ",ifs_key
    endif
  endfor
; clear old structure  
  cv_ifs_struct = 0
; refresh with new structure
  cv_ifs_struct = tmp

end
; dgp rev 10/25/05 add a data file to the IFS structure
pro _pr_ifs_add, fullpath

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_ifs, cv_ifs_struct
  common cm_dataset, cv_cells
  common cm_header, cv_table_hash, cv_head_hash
  common cm_header2, cv_key_order

  parsefilename, fullpath, filename=key
      
; retrieve the information to add to the structure
  data = cv_cells
  arr  = size(cv_cells,/dim)
  header = cv_head_hash
;  table = cv_table_hash
  
; create the current structure
  tmp = asarr('data',data)
  tmp = [tmp,asarr('header',header)]
;  tmp = [tmp,asarr('key order',cv_key_order)]
;  tmp = [tmp,asarr('table',table)]
  tmp = [tmp,asarr('labels',[[fn_upa_get('1')],[fn_upa_get('2')],[fn_upa_get('3')]])]
  tmp = [tmp,asarr('total',arr(0))]
  tmp = [tmp,asarr('par cnt',arr(1))]
  tmp = [tmp,asarr('filename',key)]
  tmp = [tmp,asarr('status',1)]
  tmp = [tmp,asarr('message','Valid Data')]

; create, append or modify the entire structure
  if (size(cv_ifs_struct,/type) eq 11) then begin
    cv_ifs_struct = [cv_ifs_struct,asarr(key,tmp)]
  endif else begin
    cv_ifs_struct = asarr(key,tmp)
  endelse

end

function fn_opt_get_fixed, key

  common cm_optimize, oio_fixed, oio_header, oio_data

  if (size(oio_fixed,/type) eq 11) then begin
    if (isaskey(oio_fixed,key)) then return, oio_fixed(key)
  endif
  return, 0

end

function fn_opt_get_header, key

  common cm_optimize, oio_fixed, oio_header, oio_data

  if (size(oio_header,/type) eq 11) then begin
    if (isaskey(oio_header,key)) then return, oio_header(key)
  endif
  return, 0

end

function fn_opt_get_data, key

  common cm_optimize, oio_fixed, oio_header, oio_data

  if (size(oio_data,/type) eq 11) then begin
    if (isaskey(oio_data,key)) then return, oio_data(key)
  endif
  return, 0

end

pro _pr_opt_set_fixed, key, val

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_optimize, oio_fixed, oio_header, oio_data

  if (size(oio_fixed,/type) eq 11) then begin
    oio_fixed = [oio_fixed,asarr(key,val)]
  endif else begin
    oio_fixed = asarr(key,val)
  endelse

end

pro _pr_opt_set_header, key, val

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_optimize, oio_fixed, oio_header, oio_data

  if (size(oio_header,/type) eq 11) then begin
    oio_header = [oio_header,asarr(key,val)]
  endif else begin
    oio_header = asarr(key,val)
  endelse

end

pro _pr_opt_set_data, key, val

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_optimize, oio_fixed, oio_header, oio_data

  if (size(oio_data,/type) eq 11) then begin
    oio_data = [oio_data,asarr(key,val)]
  endif else begin
    oio_data = asarr(key,val)
  endelse

end

; reflect the current state
pro _wid_ref_onoff, name

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common onoff_wid, onoff_wid_hash, onoff_nlu_hash
  onoff_val_hash = fn_dmf_get('onoff')

  ; if the widget has been registered...
  if (isaskey(onoff_wid_hash,name)) then begin
    ; and the registered widget is available...
    wid = long(onoff_wid_hash(name))  
    if (fn_wid_exists(wid)) then begin
      ; then get the current value and reflect it
      state = onoff_val_hash(name)
      s = WwSetValue(wid,byte(state))
    endif
  endif
  
end

; register the widget and current state
pro _wid_reg_onoff, name, wid, state

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common onoff_wid, onoff_wid_hash, onoff_nlu_hash
  onoff_val_hash = fn_dmf_get('onoff')
    
  ; register the widget and value under the name
  ; ...and the name under the widget itself for cross referencing from the callback
  onoff_wid_hash = [onoff_wid_hash,asarr(name,long(wid))]
  onoff_nlu_hash = [onoff_nlu_hash,asarr(string(wid),name)]
  onoff_val_hash = [onoff_val_hash,asarr(name,byte(state))]
  _pr_dmf_set, 'onoff', onoff_val_hash

  ; reflect the info
  _wid_ref_onoff, name

end

; modify the value and reflect that change
pro _wid_mod_onoff, name, state

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common onoff_wid, onoff_wid_hash, onoff_nlu_hash
  onoff_val_hash = fn_dmf_get('onoff')

  if (size(onoff_val_hash,/type) ne 11) then return

  ; if the widget has been registered...
  if (isaskey(onoff_wid_hash,name)) then begin

    ; then modify the value hash and reflect
    onoff_val_hash = [onoff_val_hash,asarr(name,byte(state))]
    _pr_dmf_set, 'onoff', onoff_val_hash
    _wid_ref_onoff, name

  endif
  
end

; modify the value and reflect that change
function _wid_get_onoff, name

  common onoff_wid, onoff_wid_hash, onoff_nlu_hash
  onoff_val_hash = fn_dmf_get('onoff')

  ; if the value has been set
  if (isaskey(onoff_val_hash,name)) then begin

    ; then return the value
    val = onoff_val_hash(name)
    return, val

  endif
  return, 0
  
end

pro _cb_wid_onoff, name, index

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common onoff_wid, onoff_wid_hash, onoff_nlu_hash
  onoff_val_hash = fn_dmf_get('onoff')

  ; if the callback is a toggle off from an /nofmany, then ignore
  if (index eq 0) then return

  print, "Generic callback for onoff"

  ; get callback data
  state = dgpGetValue(name)
  name = string(name)

  ; determine if the widget has been registered
  if (isaskey(onoff_nlu_hash,name)) then begin
 
    ; get the name of the widget
    name = onoff_nlu_hash(name)    
    ;print, "Specific change to ",name," state = ",state
    ; modify the value
    onoff_val_hash = [onoff_val_hash,asarr(name,byte(state))]
    _pr_dmf_set, 'onoff', onoff_val_hash

    draw_cells

  endif
  
end

function fn_mgs_get_sorted, keys

  common cm_mgs_sort, cv_sorted_keys, cv_sorted_count, cv_sorted_names, cv_sorted_prefix

  if (_mgs_present()) then begin
    keys = cv_sorted_keys
    return, 1
  endif
  return, 0
  
end

pro _pr_dsp_reflect

_pr_gen_ident ; dgp rev 1/28/10 debugging

  _pr_wra_set_wid, 'dsp name', "Gate Form", fn_dmf_current()
  
  if (fn_mgs_get_sorted(keys)) then begin
    for i=0,n_elements(keys)-1 do begin
      _pr_wra_set_index, 'mgs outline', "Gate Form", i, fn_mgs_get('outline',keys(i))
      _pr_wra_set_index, 'mgs label', "Gate Form", i, fn_mgs_get('label',keys(i))
      _pr_wra_set_index, 'mgs perc', "Gate Form", i, fn_mgs_get('perc',keys(i))
      _pr_wra_set_index, 'mgs stats', "Gate Form", i, fn_mgs_get('stats',keys(i))
    endfor
  endif
  
end

pro _ger_reflect_wid

  if (not fn_wid_exists(fn_wra_get_index('applied',"Gate Form",0))) then return
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

  _pr_dsp_reflect

end



; return an array of keys for all active gates
function _ger_gates_on, sorted

  sorted = ['x']
  if (fn_mgs_get_sorted(keys)) then begin
    for i=0,n_elements(keys)-1 do begin
      if (fn_mgs_get_enable(keys(i))) then begin
        sorted = [sorted,keys(i)]
      endif
    endfor
  endif

  if (n_elements(sorted) eq 1) then return, 0
  sorted = sorted(1:*)
  return, 1

end

; dgp rev 9/13/05 move to a new location in the list
; then read that file
pro _pr_fcr_move, test

_pr_gen_ident ; dgp rev 1/28/10 debugging

  _pr_gen_report1, "BeginMove"
  info, !quiet
  !quiet = 0

    if (test ge 0 and test lt fn_pfl_get_count()) then begin
      print, "Change to ",test
  ; get key from file index    
      key = fn_pfl_get_name(test)
      if (size(key,/type) eq 7) then begin
  ; if key doesn't exist in ifs, then load data
        if (not fn_ifs_exists(key)) then begin
          if (not fn_ifs_read(test)) then begin
            print, "Error loading data file"
          endif
        endif
        ; set "ifs key" and "file index" after successful read
        if (fn_ifs_exists(key)) then begin
          _pr_dmf_set, 'ifs key', key
          _pr_dmf_set, 'file index' ,test
          _pr_dmf_set, 'mgs flag' ,1
          ; recalculate the indices 
          ; for the newly loaded file
          _ssl_recalc
          _fcr_reflect_menu
          ; trim extraneous IFS memory
          _pr_ifs_cull
        endif
      endif else begin
        print, "Invalid move"
      endelse
    endif

  _pr_gen_report1, "EndMove"

end

; move data file for current display
; for "locked display" call routine while looping thru displays
; dgp rev 10/25/05 relative move based upon current file index
function fn_fcr_move_relative, value

; get all displays to update
  dlist = fn_dmf_get_dlist()
  cur = fn_dmf_current()
; loop thru each display
  for i=0,n_elements(dlist)-1 do begin
    _pr_dmf_switch, dlist(i)
    test = fn_dmf_get('file index') + value
    if (test lt 0) then test = fn_pfl_get_count() - 1
    if (test ge fn_pfl_get_count()) then test = 0
    _pr_fcr_move, test
  endfor
  _pr_dmf_switch, cur
; no reflect, as DMF manipulation shouldn't change
;  _pr_dmf_reflect

  return, 1
 
end

; dgp rev 11/29/2010 loop thru all the files
pro _pr_tst_read

  _pr_gen_report1, "BeginReadTest"
  cnt = fn_pfl_get_count() - 1

  ; get all displays to update
    dlist = fn_dmf_get_dlist()
    cur = fn_dmf_current()
  
  print, "Looping thru files"
  for fidx=0, cnt do begin
    print, "  File "+string(fidx)
  ; loop thru each display
    for didx=0,n_elements(dlist)-1 do begin
      if dlist(didx) ne 'default' then begin
        _pr_dmf_switch, dlist(didx)
        _pr_fcr_move, fidx
      endif
    endfor
  endfor
  _pr_fmf_reflect
  _pr_reflect_cdr
  _pr_gen_report1, "EndReadTest"
 
end


; dgp rev 11/29/2010 loop thru all the files
pro _pr_tst_display

  _pr_gen_report1, "BeginDisplayTest"
  cnt = fn_pfl_get_count() - 1
  _pr_gen_set, 'movement', 2

  ; get all displays to update
    dlist = fn_dmf_get_dlist()
    cur = fn_dmf_current()
  _pr_gen_set, 'display', 'cycle'
  
  print, "Looping thru files"
  for fidx=0, cnt do begin

    print, "  File "+string(fidx)
  ; loop thru each display
    for didx=0,n_elements(dlist)-1 do begin
      if dlist(didx) ne 'default' then begin
        _pr_dmf_switch, dlist(didx)
        _pr_fcr_move, fidx
        draw_cells
      endif
    endfor

  endfor

  _pr_gen_set, 'display', 'single'
  _pr_fmf_reflect
  _pr_reflect_cdr
  _pr_gen_report1, "EndDisplayTest"
 
end



; same as above routine only moving to an absolute position
function fn_fcr_move_absolute, value

; get all displays to update
  dlist = fn_dmf_get_dlist()
  cur = fn_dmf_current()

; loop thru each display
  for i=0,n_elements(dlist)-1 do begin
    _pr_dmf_switch, dlist(i)
    test = value
    _pr_fcr_move, test
  endfor
  _pr_dmf_switch, cur
;  _pr_dmf_reflect

  return, 1
 
end

function _ssl_exists, key

  common cm_ifs, cv_ifs_struct

  file_key = fn_dmf_get('ifs key')

  if (isaskey(cv_ifs_struct(file_key),key)) then begin

    return, isaskey(cv_ifs_struct(file_key,'subsets'),key)

  endif
  return, 0
  
end

function _mgs_enabled_keys, ekeys

  if (fn_mgs_get_sorted(skeys)) then begin
      loops = n_elements(skeys)-1
      ekeys = make_array(1,/string)
      count = 0
      for i=0, loops do begin
        if (fn_mgs_get_enable(skeys(i))) then begin
          ekeys = [ekeys,skeys(i)]
          count = count + 1
        endif
      endfor
      if (count eq 0) then return, 0
      ekeys = ekeys(1:*)
  endif else begin
    return, 0
  endelse
  return, 1

end

function fn_mgs_process, ss_hash

  ; loop thru the entire hash
  main_keys = askeys(ss_hash)
  for major=0, n_elements(ss_hash)-1 do begin
    ; OR loop thru each sub hash 
    sub_keys = askeys(ss_hash(main_keys(major)))
    combo = ss_hash(main_keys(major),sub_keys(0))
    for minor=1,n_elements(sub_keys)-1 do begin
      next_sub = ss_hash(main_keys(major),sub_keys(minor))
      combo = INDEX_OR(combo, next_sub)  
    endfor
    ; AND each combination
    if (major eq 0) then begin
      tally = combo
    endif else begin
      tally = INDEX_AND(tally,combo)
    endelse
  endfor
  return, tally
  
end

pro _mgs_combo

_pr_gen_ident ; dgp rev 1/28/10 debugging

  if (fn_dmf_get('mgs flag')) then begin
    _pr_dmf_set, 'mgs flag', 0
    if (_mgs_enabled_keys(sorted)) then begin

      subset  = fn_mgs_get_subset(sorted(0))
      name    = _mgs_get_name(sorted(0))
      prefix  = strmid(name,0,4)
      ss_hash = asarr(prefix,asarr(name,subset))
      for i=1,n_elements(sorted)-1 do begin
        subset  = fn_mgs_get_subset(sorted(i))
        name    = _mgs_get_name(sorted(i))
        prefix  = strmid(name,0,4)
        if (isaskey(ss_hash,prefix)) then begin
          ss_hash(prefix) = [ss_hash(prefix),asarr(name,subset)]
        endif else begin
          ss_hash = [ss_hash,asarr(prefix,asarr(name,subset))]
        endelse
      endfor 
      arr = fn_mgs_process(ss_hash) 
      _pr_dmf_set, 'mgs count', n_elements(arr)
      _pr_dmf_set, 'mgs indices', arr

    endif else begin
      arr = fn_full_indices()
      _pr_dmf_set, 'mgs count', n_elements(arr)
      _pr_dmf_set, 'mgs indices', arr
      print, "No gates applied"
    endelse
  endif else begin
    print, "No change to indices in ",fn_dmf_current()
  endelse
  
end
; dgp rev 11/28/05 load the multiple gate structure
; 
pro _mgs_load_struct

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_mgs_info, cv_mgs_struct
  common cm_icp_mgs, cv_icp_mgs_on
    
  file_name = '_mgs_struct.xxx'

  restname = fn_gen_build_path(fn_gen_getstr('work path'),file_name)

  found = findfile(restname,count=cnt)
    
  if (cnt eq 0) then begin
      cv_mgs_struct = complex(1)
  endif else begin
    restore, restname
    if (size(_mgs_struct,/type) ne 0) then cv_mgs_struct = _mgs_struct
  endelse
  _mgs_resort_keys
  
end

; dgp rev 1/4/06 reflect the current filename
pro _fcr_reflect_menu

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common fcr_info, menu_wid, fcr_range_slider

;  menu_wid = fn_wra_get_wid(  
  if (size(menu_wid,/type) eq 0) then return
  ; dgp rev 3/14/08 make sure widget is scalar, not array
  if (size(menu_wid,/ndim) ne 0) then menu_wid = menu_wid(0)
  if (fn_wid_exists(menu_wid)) then begin
     index = fn_dmf_get('file index') + 1
     status = WtSet(menu_wid, {, whichItem: index})
  endif
  _pr_wra_set_wid, 'cur file', "Control Panel", fn_dmf_get('ifs key')
  
end

; dgp rev 1/4/06 reflect the current filename
; dgp rev 1/4/06 associate it with the proper control
pro _pr_fmf_reflect

_pr_gen_ident ; dgp rev 1/28/10 debugging

  frm_name = 'File Form'
  menu_name = 'Pull File '+strtrim(fn_gen_get('movement'),2)
  menu_wid = fn_wra_get(menu_name,frm_name)
  menu_wid = menu_wid(0)
  if (fn_wid_exists(menu_wid)) then begin
     f_idx = fn_dmf_get('file index') + 1
     status = WtSet(menu_wid, {, whichItem: f_idx})
  endif
  
end

pro _cfg_report

_pr_gen_ident ; dgp rev 1/28/10 debugging

  var_list = STRARR(1)
  
  work_path = fn_gen_get('work path')
  cfg_list = fn_gen_get('cfg list')
  info, work_path
  pm, work_path
  print, "Working path is ",work_path
  print, "Files list ",cfg_list
  status = findfile(cfg_list,count=cnt)
  if (cnt > 0) then begin
    if (DC_READ_FREE(cfg_list, var_list,delim=[' '], resize=[1]) eq 0) then begin
      print, "Data files: "
      for i=0,n_elements(var_list)-1 do begin
        print, var_list(i)
      endfor
    endif
  endif

end
; dgp rev 12/26/05 spawn the configuration routine
; The configuration routine creates a new configuration
; file which is then processed during the initialization
; phase.
pro _pr_cfg_setup

_pr_gen_ident ; dgp rev 1/28/10 debugging

     flag = getenv('pvwave_setup_flag')
     
     print, "Grab the pvw_setup_flag ",flag

     if (STRMATCH(flag, 'skip')) then return

     dwnld  = "setup.exe"
     
     file = findfile(dwnld,count=cnt)
  
     if (cnt ne 0) then begin
        SPAWN,file(0),results
     endif else begin
        print, "Configuration error - no ",dwnld
        exit
     endelse   

end

; dgp rev 8/22/05 scale the font based up display size
pro fnt_scale

_pr_gen_ident ; dgp rev 1/28/10 debugging

    basis = fn_gen_get('pixel count')
    
    !P.charsize = float(basis)/360.0

end

function _wid_get_id, wid

  common wid_db, wid_name_hash
  
  keys = askeys(wid_name_hash)
  for i=0, n_elements(keys)-1 do begin
    print, keys(i), " = " , wid_name_hash(keys(i))
  endfor
  
  if (size(wid_name_hash,/type) ne 0) then begin
    if (isaskey(wid_name_hash,wid)) then begin
      name = wid_name_hash(wid)
      return, name
    endif
  endif
  
  return, ""
  
end

function _wid_record, wid

  common wid_db, wid_name_hash
  
  if (fn_wid_exists(wid)) then begin
    name = dgpGetValue(wid)
    if (size(wid_name_hash,/type) eq 0) then begin
      print, "New ",name
      wid_name_hash = asarr(name,wid)
    endif else begin
      print, "Add ",name
      tmp = asarr(name,wid)
      wid_name_hash = [wid_name_hash,tmp]
    endelse
    return, 1
  endif else begin
    print, "No such widget"
    return, 0
  endelse
  

end

pro _debug_proc

_pr_gen_ident ; dgp rev 1/28/10 debugging

  info, level=-1,calls=current
  ;print, "Current routine is ",current
  info, trace=tree

end
; dgp rev 12/23/05 initialize the holding structure
; for the current gate being created
pro _pr_mgc_clear

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_mgc_info, cv_mgc_cur

  cv_mgc_cur = asarr('flag',0)

end
; dgp rev 12/23/05 debug routine
pro _pr_mgc_debug

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_mgc_info, cv_mgc_cur

  stop ;debug

end

pro _wid_state_data

_pr_gen_ident ; dgp rev 1/28/10 debugging

    _sense_mod,'listbut','/sensitive' 
    _sense_mod,'clusterbut','/sensitive' 
    _sense_mod,'derivebut','/sensitive' 
    _sense_mod,'dcbut','/sensitive' 

end

pro _wid_state_nodata

_pr_gen_ident ; dgp rev 1/28/10 debugging

    _sense_mod,'listbut','/nonsensitive' 
    _sense_mod,'clusterbut','/nonsensitive' 
    _sense_mod,'derivebut','/nonsensitive' 
    _sense_mod,'dcbut','/nonsensitive' 

end

pro _set_nofile

_pr_gen_ident ; dgp rev 1/28/10 debugging

    _sense_mod,'savebut','/nonsensitive' 
    _sense_mod,'restbut','/nonsensitive' 
    _sense_mod,'listbut','/nonsensitive' 
    _sense_mod,'clusterbut','/nonsensitive' 
    _sense_mod,'gatebut','/nonsensitive' 
    _sense_mod,'derivebut','/nonsensitive' 
    _sense_mod,'dclubut','/nonsensitive' 
    _sense_mod,'dcbut','/nonsensitive' 

end

pro _set_file

_pr_gen_ident ; dgp rev 1/28/10 debugging

    _sense_mod,'savebut','/sensitive' 
    _sense_mod,'restbut','/sensitive' 
    _sense_mod,'listbut','/sensitive' 
    _sense_mod,'clusterbut','/sensitive' 
    _sense_mod,'gatebut','/sensitive' 
    _sense_mod,'derivebut','/sensitive' 
    _sense_mod,'dclubut','/sensitive' 
    _sense_mod,'dcbut','/sensitive' 

end
; dgp rev 12/30/05 gather environment information
pro _pr_env_gather, flowroot

_pr_gen_ident ; dgp rev 1/28/10 debugging

  ; determine home
  cd, current=home
  _pr_gen_set, 'home', home

  test_string = "Test"

; parse subfolders the home (or version) subdirectories
; should be in the format
; {device}:\flowroot\distribution\{version}
  subfolders = strsplit(strlowcase(home),fn_gen_getstr('backslash'))
  info, subfolders
  pm, subfolders
  
  ; retrieve the username
  username = strtrim(strlowcase(getenv('username')),2)
  if (username eq "") then username = 'default'

  idx = where(subfolders eq 'flowroot',cnt)

  if (cnt eq 0) then begin
    print, "Invalid installation - exiting program"
  ; create a default config location
    _pr_gen_exit
  endif

  ; define the flowroot 
  flowroot = strjoin(subfolders(0:idx(0)),fn_gen_getstr('backslash')) + fn_gen_getstr('backslash')
  _pr_gen_set, 'flowroot', flowroot
  _pr_gen_set, 'username', username

end
; dgp rev 8/07/05 analyze the general environment
; root folder
; user specific information
function fn_env_scan

  _pr_env_gather, flowroot

  ; scan flowroot subdirectories 
  com = 'dir '+flowroot+ ' /a:d /l /b'
  spawn, com, tree_list
  tree_list = strtrim(STRLOWCASE(tree_list),2)

  ; create any missing required subdirs - settings, work, data, users
  required = ['users','data']
  regexp = strjoin(required,"|")

  results = strmatch(tree_list,regexp)
  idx = where(results,cnt)
  
  if (cnt eq 0) then return, 0

  req_cnt = 0

  for i=0,n_elements(required)-1 do begin
    regexp = '^' + required(i) + '.?$'
    results = strmatch(tree_list,regexp,/exact)  
    if (sum(results ne 0)) then begin
      ; required path exists
      req_cnt = req_cnt + 1
    endif else begin
      ; must create required path 
      name = required(i)
      strput, name, strupcase(strmid(name,0,1)), 0
      new_path = fn_gen_build_path(flowroot,name)

      com = "mkdir " + new_path
      SPAWN,com,results
      status = CHECKFILE(new_path, /Read, Is_Dir = isdir)
      if (status+isdir eq 2) then req_cnt = req_cnt + 1
    endelse
  endfor

  if (req_cnt eq n_elements(required)) then return, 1
  
  return, 0

end

; dgp rev 9/07/05 search for the config file
; possible locations:
; ...FlowRoot...username...Distribution
; f:\FlowRoot\users\plugged\Distribution\Beta
; i:\FlowRoot\Distribution\Beta
; search for subfolders - FlowRoot, Distribution, username, and/or settings
; 
; f:\FlowRoot\users\plugged\Distribution\Beta

; dgp rev 1/3/06 read the assign configuration file
pro _pr_cfg_read

_pr_gen_ident ; dgp rev 1/28/10 debugging

  cfg_file = fn_gen_get('cfg file')   
     
  rawarr = make_array(1,/string)
  print, "Load configuration file  ",cfg_file
  ; use resize so 
  on_ioerror, read_err
  status = DC_READ_FREE(cfg_file, rawarr,resize=[1],delim= '\011')
  
  information = strjoin(rawarr,"")   
  sep = string(STRMID (information, 0, 1))
  information = strmid(information,1,strlen(information)-1)
  information = strsubst(information,sep+sep,sep,/global)

  tokens = STRSPLIT(information,sep)
  
  for i=0,n_elements(tokens)-1, 2 do begin
    if ((size(tokens(i),/type) ne 0) and (size(tokens(i+1),/type) ne 0)) then begin
      _pr_gen_set, tokens(i), tokens(i+1)
    endif
  endfor

  read_err:

end

; dgp rev 1/2/06 obsolete -- now _pr_cfg_read
pro _cfg_read

_pr_gen_ident ; dgp rev 1/28/10 debugging

    if (tokens(i) eq 'work_path') then begin
      work_path = tokens(i+1)+fn_gen_getstr('backslash')
      _pr_gen_set, 'work path', work_path
      cfg_list  = tokens(i+1)+fn_gen_getstr('backslash')+'fcs_files.lis'
      _pr_gen_set, 'cfg list', cfg_list
      print, "Work Path: ",work_path
      print, "List file: ",cfg_list
      command = 'workspace=' + work_path
      print, "Environment command is ",command
      setenv, 'workspace=' + work_path
      results = getenv('workspace')
      print, "Variable is ",results
  
      arr  = strsplit(work_path,fn_gen_getstr('backslash'))
      cnt = n_elements(arr)
      if (cnt gt 1) then begin
        _pr_gen_set, 'session', arr(cnt-1)
        _pr_gen_set, 'project', arr(cnt-2)
      endif
    endif
    if (tokens(i) eq 'setup_path') then begin
      setup_path = tokens(i+1)+fn_gen_getstr('backslash')
      _pr_gen_set, 'setup path', setup_path
      print, "Settings Path: ",setup_path
    endif else begin
      cfg_file = fn_gen_get('cfg file')   
      parsefilename, cfg_file, path=setup_path
      _pr_gen_set, 'setup path', setup_path
      print, "Settings Path: ",setup_path
    endelse


end

function _gen_unique_name

  DT_TO_STR, today(), d, t, Date_Fmt =5, time_fmt=-1
  file = strsubst(d+t,'/','',/glob)
  file = strsubst(file,':','',/glob)
  file = STRMID(file, 0, 14)
  return, file

end

pro _pfl_init

_pr_gen_ident ; dgp rev 1/28/10 debugging

; Read an FCS file list and place into structure
  common cm_pfl, cv_pfl_list, cv_file_count, cv_pfl_name_list
      
  cv_file_count = 0    
  val_cnt = 0
  cfg_list = fn_gen_get('cfg list')

  if (size(cfg_list,/type) ne 0) then begin
  
    print, "Reading ",cfg_list

    results = FINDFILE(cfg_list,count=found)
    cv_pfl_list = make_array(1,/string)
    exist_list = make_array(1,/string)
    name_list = make_array(1,/string)

    if (found > 0) then begin  
      status = DC_READ_FREE(cfg_list, cv_pfl_list, Resize=[1], Delim='\012') 
      cv_file_count = n_elements(cv_pfl_list)
      print, cv_file_count," in list"
      for i=0,cv_file_count-1 do begin
        x = findfile('"'+cv_pfl_list(i)+'"',count=found)
        if (found > 0) then begin
          exist_list = [exist_list,cv_pfl_list(i)]
          parsefilename, cv_pfl_list(i), fileroot=name
          name_list = [name_list,name]
          val_cnt = val_cnt + 1
        endif
      endfor
      ;print, val_cnt," files exist"
    endif else begin
      print, "No fcs list file ",cfg_list
    endelse
  endif else begin
    print, "No list file name defined"
  endelse

  if (val_cnt gt 0) then begin
    cv_pfl_list = exist_list(1:*)
    cv_pfl_name_list = name_list(1:*)
    subfolders = strsplit(cv_pfl_list(0),fn_gen_getstr('backslash'))
    run_hash = asarr(subfolders(n_elements(subfolders)-1),subfolders(n_elements(subfolders)-2))
    for i=1, n_elements(cv_pfl_list)-1 do begin
      subfolders = strsplit(cv_pfl_list(i),fn_gen_getstr('backslash'))
      run_hash = [run_hash,asarr(subfolders(n_elements(subfolders)-1),subfolders(n_elements(subfolders)-2))]
    endfor
    _pr_gen_set, 'setnames', run_hash
    
    cv_file_index = 0
    _wid_state_data
  endif else begin
    _wid_state_nodata
  endelse

end
; dgp rev 12/30/05 define the working and settings path
pro _pr_cfg_validate

_pr_gen_ident ; dgp rev 1/28/10 debugging

    if (fn_gen_getstr('work path') ne "" or fn_gen_getstr('work_path') ne "") then begin
      work_path = fn_gen_getstr('work path')
      if (work_path eq "") then work_path = fn_gen_getstr('work_path')
      work_path = work_path+fn_gen_getstr('backslash')
      _pr_gen_set, 'work path', work_path
      cfg_list  = fn_gen_build_path(work_path,'fcs_files.lis')
      _pr_gen_set, 'cfg list', cfg_list
      print, "Work Path: ",work_path
      print, "List file: ",cfg_list
  
      arr  = strsplit(work_path,fn_gen_getstr('backslash'))
      cnt = n_elements(arr)
      if (cnt gt 1) then begin
        _pr_gen_set, 'session', arr(cnt-1)
        _pr_gen_set, 'project', arr(cnt-2)
      endif
    endif

    if (fn_gen_getstr('setup path') ne "" or fn_gen_getstr('setup_path') ne "") then begin
      setup_path = fn_gen_getstr('setup path')
      if (setup_path eq "") then setup_path = fn_gen_getstr('setup_path')
      setup_path = setup_path+fn_gen_getstr('backslash')
      _pr_gen_set, 'setup path', setup_path
    endif else begin
      parsefilename, cfg_file, path=setup_path
      _pr_gen_set, 'setup path', setup_path
    endelse
    print, "Setup Path: ",setup_path
    
end
; dgp rev 9/8/05 configure the environment
; - assign a location for the configuration file
; - spawn the external configuration routine
; - read and parse the specified configuration file
pro _pr_cfg_init

_pr_gen_ident ; dgp rev 1/28/10 debugging

  _pr_cfg_prep
  ; run the external program used to modify the 
  ; select configuration file
  _pr_cfg_setup
  ; read the prefined configuration file
  _pr_cfg_read
  
  _pr_cfg_validate
  
  _pfl_init
  
  _cfg_report

  _pr_cfg_load

end

pro cb_fcs_data, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

    _pr_cfg_init
    
;    _pr_dcr_init

    _ssl_recalc
  
end

pro _new_data

_pr_gen_ident ; dgp rev 1/28/10 debugging


end

pro _olc_init

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common overlay, olc_subsets, olc_hash, olc_count, olc_sort
  common overlay4, olc_norm_val, olc_norm_on, olc_levels, olc_smooth
  common ol_par, ol_p1, ol_p2, ol_pp

  ; olverlay parameters
  ol_pp = [3,4]
  ; overlay init
  olc_count = 0

end

pro _com_init

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common win_info, win_cells, win_cluster1, win_cluster2
  common active, use_clusters, use_gates

  ; active subsets
  use_clusters = 1
  use_gates  = 1
  
  ; window status
  win_cells = 0
  win_cluster1 = 0
  win_cluster2 = 0

  ; data normalization off  
  active = 0
  
end

; dgp rev 10/27/05 preload the log/lin table for x and y axis labeling
; create the log and linear arrays and initialize the current settings to log
pro _pr_lgln_init

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_log, cv_mnlog, cv_mxlog, cv_lblog
  common cm_lin, cv_mnlin, cv_mxlin, cv_lblin
  common cm_lgln_info, cv_lgln_hash

     decades = 4
     intv    = 9
     mintks = make_array(1,/float,value=1)
     for d=1,decades do begin
         major = 10^d
         last  = 10^(d-1)
         tally = last
       for i=1,intv do begin
         portion = (float(major - last)/float(intv))
         tally = tally + portion
         mintks = [mintks,tally]
       endfor
     endfor
 
     reso = 1024

     cv_lblog = strtrim(string(indgen(decades+1)),2)
     cv_lblog = '!P1310!U' + strtrim(cv_lblog,2) +'!N'

     tkmx_hi = 1000
     tkmx_cnt = 5

     cv_lblin = strtrim(string(indgen((tkmx_cnt+1))*(tkmx_hi/tkmx_cnt)),2)
     cv_lblin = '!P13' + cv_lblin + '!N'
     
     cv_mnlog = alog10(mintks)*(reso/decades) ; (1 10 100 1000 10000) natural log times 16
     cv_mxlog = (indgen(decades+1))*(reso/decades)

     tkmin_incr = 50
     
     cv_mnlin = indgen(tkmx_hi/tkmin_incr)*tkmin_incr
     cv_mxlin = indgen(tkmx_cnt+1)*(tkmx_hi/tkmx_cnt)
     
     lgln_mx   = [asarr('x',cv_mxlog),asarr('y',cv_mxlog)]     
     lgln_min  = [asarr('x',cv_mnlog),asarr('y',cv_mnlog)]     
     lgln_lbls = [asarr('x',cv_lblog),asarr('y',cv_lblog)]
     tmp = REPLICATE(' ', n_elements(cv_mnlog)+1)
     lgln_blanks = [asarr('x',tmp),asarr('y',tmp)]

  tmp_log = asarr('x min tick',[cv_mnlog])
  tmp_log = [tmp_log,asarr('x max ticks',[cv_mxlog])]
  tmp_log = [tmp_log,asarr('x labels',[cv_lblog])]
  blk_log = REPLICATE(' ', n_elements(cv_mnlog)+1)
  tmp_log = [tmp_log,asarr('x blanks',[blk_log])]
  tmp_log = [tmp_log,asarr('y min ticks',[cv_mnlog])]
  tmp_log = [tmp_log,asarr('y max ticks',[cv_mxlog])]
  tmp_log = [tmp_log,asarr('y labels',[cv_lblog])]
  tmp_log = [tmp_log,asarr('y blanks',[blk_log])]

  tmp_lin = asarr('x min tick',[cv_mnlin])
  tmp_lin = [tmp_lin,asarr('x max ticks',[cv_mxlin])]
  tmp_lin = [tmp_lin,asarr('x labels',[cv_lblin])]
  blk_lin = REPLICATE(' ', n_elements(cv_mnlin)+1)
  tmp_lin = [tmp_lin,asarr('x blanks',[blk_lin])]
  tmp_lin = [tmp_lin,asarr('y min ticks',[cv_mnlin])]
  tmp_lin = [tmp_lin,asarr('y max ticks',[cv_mxlin])]
  tmp_lin = [tmp_lin,asarr('y labels',[cv_lblin])]
  tmp_lin = [tmp_lin,asarr('y blanks',[blk_lin])]

  cv_lgln_hash = [asarr('1',tmp_log),asarr('0',tmp_lin)]

end

pro _dmf_lvl_init

_pr_gen_ident ; dgp rev 1/28/10 debugging

  _pr_dmf_set, 'lvl min type', 0
  _pr_dmf_set, 'lvl min val', [.0004,1.0,1.0]
  _pr_dmf_set, 'lvl mult type', 1
  _pr_dmf_set, 'lvl mult val', [.01,2.0,4.0]

end
; dgp rev 9/07/05 retrieve the global home directory
function _env_get_home

  if (fn_gen_getstr('home') eq "") then begin

    ; determine home
    cd, current=c
    _pr_gen_set, 'home', c

  endif
  
  return, c
  
end
; dgp rev 9/07/05 return to the global home directory
pro _env_go_home

_pr_gen_ident ; dgp rev 1/28/10 debugging

  if (fn_gen_getstr('home') eq "") then begin

    ; determine home
    cd, current=c
    _pr_gen_set, 'home', c

  endif
  cd, c

end

; dgp rev 8/22/05 a one time calculation based upon display size
function get_pixel_perc

  pixel_count = fn_gen_get('pixel count')

  perc = long(100*float(pixel_count)/float(!display_size.y))

  return, perc
  
end
; dgp rev 10/19/05 filter out only the control items in DMF
; reset other necessary keys
; dgp rev 10/20/05 position must be restored in addition to control settings
; dgp rev 8/29/07 change the default dot size

function fn_dmf_restore_prep, hash_item

  control_keys = ['dual res','hist res','format','ymax perc','quad on','quad rep','quad width','quad degree']
  control_keys = [control_keys,'smo on','smo rep','smo width','pars','pix perc','color','position']
  control_keys = [control_keys,'lvl min type','lvl min val','lvl mult type','lvl mult val','locked']
  control_keys = [control_keys,'dot max','applied','par select','1only','description']
  control_keys = [control_keys,'line thick','char thick','char size','dot size']
  control_keys = [control_keys,'outliers','psym']
  control_keys = [control_keys,'label font', 'mgs font','tube on']
  reset_keys   = ['file index','ifs key','state']

  ; fill-in any missing vlaues
  if (not isaskey(hash_item,'line thick')) then hash_item = [hash_item,asarr('line thick',!p.thick)]
  if (not isaskey(hash_item,'char thick')) then hash_item = [hash_item,asarr('char thick',!p.charthick)]
  if (not isaskey(hash_item,'char size')) then hash_item = [hash_item,asarr('char size',!p.charsize)]
  if (not isaskey(hash_item,'dot size')) then hash_item = [hash_item,asarr('dot size',.25)]

  dmf_font = 'Times New Roman, 12, Bold'
  if (not isaskey(hash_item,'label font')) then hash_item = [hash_item,asarr('label font',dmf_font)]
  if (not isaskey(hash_item,'mgs font')) then hash_item = [hash_item,asarr('mgs font',dmf_font)]

  ; keep only the required fields
  keys = askeys(hash_item)
  keys = keys(sort(keys))
  key_count = n_elements(keys)
  valid_idx = wherein(keys,control_keys,cnt)
  tmp = hash_item(keys(valid_idx))  

  mx = fn_ifs_get_header('$PAR',fn_pfl_get_name(0))
  pars = hash_item('pars')  

  idx = where(pars gt mx,cnt)
  if (cnt eq 1) then pars(idx) = mx
  if (cnt eq 2) then begin
    pars(0) = mx
    pars(1) = mx - 1    
  endif
  if (pars(0) eq pars(1)) then pars(1) = pars(0) - 1
  
  info, pars
  pm, pars

  ; assign dynamic values
  tmp = [tmp,asarr('pars',pars)]
  tmp = [tmp,asarr('file index',-1)]
  tmp = [tmp,asarr('ifs key',fn_pfl_get_name(0))]
  tmp = [tmp,asarr('state','init')]
  tmp = [tmp,asarr('mgs flag',1)]
    
  return, tmp

end

; dgp rev 12/8/05 
function fn_dmf_find_file, restname

  file_name = 'dmf_struct.sav'

  restname = fn_gen_build_path(fn_gen_getstr('work path'),file_name)

  found = findfile(restname,count=cnt)
    
  if (cnt eq 0) then begin

    restname = fn_gen_build_path(fn_gen_getstr('setup path'),file_name)

    found = findfile(restname,count=cnt)

  endif

  return, cnt

end

; dgp rev 12/5/05 check for valid DMF id info
; this includes the unique and description keywords
pro _pr_dmf_id

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_dmf_info, cv_dmf_key, cv_dmf_hash

  if (size(cv_dmf_hash,/type) eq 11) then begin
    if (isaskey(cv_dmf_hash,'default')) then begin
      if(not isaskey(cv_dmf_hash('default'),'description')) then begin
        cv_dmf_hash('default') = [cv_dmf_hash('default'),asarr('description','Default Description')]
      endif
    endif
  endif

end
; dgp rev 9/06/05 restore procedure for DMF settings
; dgp rev 12/29/05 revised to avoid duplicate file search
pro _pr_dmf_restore

  common cm_dmf_info, cv_dmf_key, cv_dmf_hash

  cv_dmf_hash = 0
  ; dmf file is known to exists, so simply restore it
  if (fn_dmf_find_file(dmf_restname)) then begin
  
    print, "DMF file ",dmf_restname
    restore, dmf_restname
  ; prep each key in the DMF by pruning excess info and 
  ; adding several default settings
    keys = askeys(cv_dmf_hash)
    keys = keys(sort(keys))
    key_count = n_elements(keys)
    tmp = asarr(keys(0),fn_dmf_restore_prep(cv_dmf_hash(keys(0))))
  
    if (key_count gt 1) then begin
      for i=1, key_count-1 do begin
        tmp = [tmp,asarr(keys(i),fn_dmf_restore_prep(cv_dmf_hash(keys(i))))]    
      endfor
    endif

    cv_dmf_hash = tmp
    cv_dmf_key = 'default'
  endif

end

; dgp rev 9/06/05 create default DMF settings
pro _pr_dmf_default

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_dmf_info, cv_dmf_key, cv_dmf_hash

  cv_dmf_key = "default"
  
  dmf_default = asarr('smo algor',2)                    ; 1 - boxcar, 2 - median, 3 - lee
  dmf_default = [dmf_default,asarr('smo on',0)]         ; linear smoothing off
  dmf_default = [dmf_default,asarr('smo rep',1)]        ; linear reps
  dmf_default = [dmf_default,asarr('smo width',1)]      ; linear width
  dmf_default = [dmf_default,asarr('quad on',0)]        ; quadratic smoothing off
  dmf_default = [dmf_default,asarr('quad degree',2)]    ; quadratic degrees
  dmf_default = [dmf_default,asarr('quad width',2)]     ; quadratic width
  dmf_default = [dmf_default,asarr('quad rep',1)]       ; quadratic rep
  dmf_default = [dmf_default,asarr('pars',[1,0])]       ; parameter pair
  ; dgp rev 8/23/07 enhancement - itemize xpar, ypar and par count
  dmf_default = [dmf_default,asarr('xpar',1)]       	; x axis
  dmf_default = [dmf_default,asarr('ypar',0)]       	; y axis
  dmf_default = [dmf_default,asarr('parcnt',1)]       	; parameter count

  dmf_default = [dmf_default,asarr('dual res',7)]       ; resolution for duals
  dmf_default = [dmf_default,asarr('hist res',10)]      ; resolution for singles
  dmf_default = [dmf_default,asarr('format','dot')]     ; display format
  dmf_default = [dmf_default,asarr('file index',-1)]    ; current file as -1, none loaded
  dmf_default = [dmf_default,asarr('ifs key',fn_pfl_get_name(0))]     
  dmf_default = [dmf_default,asarr('mgs flag',1)]     
  dmf_default = [dmf_default,asarr('1only',"")]     
                                                        ; current ifs key for this display
  dmf_default = [dmf_default,asarr('color',0)]          ; default to black
  dmf_default = [dmf_default,asarr('gate type','Rect')]     
  dmf_default = [dmf_default,asarr('par select',2)]     
  dmf_default = [dmf_default,asarr('tube on',1)]     
  dmf_default = [dmf_default,asarr('ymax perc',1.0)]     
  dmf_default = [dmf_default,asarr('state','init')]     
  dmf_default = [dmf_default,asarr('line thick',!p.thick)]     
  dmf_default = [dmf_default,asarr('char thick',1.5)]     
  dmf_default = [dmf_default,asarr('pix perc', get_pixel_perc())]     
  dmf_default = [dmf_default,asarr('position', [0,!display_size.y/3])]     
  dmf_default = [dmf_default,asarr('description','Default Layout')]     
  
  dmf_default = [dmf_default,asarr('lvl min type', 0)]
  dmf_default = [dmf_default,asarr('lvl min val', [.0004,1.0,1.0])]
  dmf_default = [dmf_default,asarr('lvl mult type', 1)]
  dmf_default = [dmf_default,asarr('lvl mult val', [.01,2.0,4.0])]

  cv_dmf_hash = asarr(cv_dmf_key,dmf_default)

end

; create a default dmf structure for when no displays are open
; pfl file list must already be initialized in order to define
; the first 'ifs key'
pro _pr_dmf_init

  ; restore a previous DMF structure or create new
  if (fn_dmf_find_file(restname)) then begin
    print, "Restore DMF settings"
    _pr_dmf_restore
  endif else begin
    print, "Use default DMF"
    _pr_dmf_default
  endelse

  _pr_dmf_force
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

pro _pr_cfg_get_wms

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_cfg_perm, cv_cfg_hash
  common cm_wms_info, cv_main_hash
  
  if (size(cv_cfg_hash,/type) ne 11) then return

  if (isaskey(cv_cfg_hash,'wms')) then begin
    cv_main_hash = cv_cfg_hash('wms')
  endif

end

pro _pr_cfg_load

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_cfg_perm, cv_cfg_hash

  path = fn_gen_get('setup path')
  restname = strcompress(string(path,'setting.cfg'))
  
  found = findfile(restname,count=cnt)
  
  if (cnt eq 0) then return

  restore, restname
  
  _pr_cfg_get_wms  

end

; dgp rev 11/9/05 create a color select grid (csg)
pro _pr_csg_init

_pr_gen_ident ; dgp rev 1/28/10 debugging

  dim  = 5
  squ  = 15
  full = dim*squ

  strip = rebin(bindgen(dim),full,squ)
  for i=1,dim-1 do begin
    strip = [[strip],[rebin(bindgen(dim)+(i*dim),full,squ)]]
  endfor

  _pr_gen_set, 'csg', strip

end

; dgp rev 10/03/05 calculate table once at beginning
pro _pr_calc_tables

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_tables, cv_base2, cv_base10

  pwr2  = 19
  pwr10 = 7
  cv_base2  = 2^findgen(pwr2)
  cv_base10 = 10^findgen(pwr10)
  
  _pr_csg_init
  
end

; dgp rev 10/19/05 general setup
pro _pr_gen_init

_pr_gen_ident ; dgp rev 1/28/10 debugging

  _pr_calc_tables

  _pr_gen_set, 'olay redraws', 10000

  _pr_lgln_init  

  ; initialize the common blocks
  _com_init
  
end
;dgp rev 1/3/06 test the location of the config file
pro _pr_cfg_prep

_pr_gen_ident ; dgp rev 1/28/10 debugging

  ; determine initial config file and set environment
  ; variable, so the spawned VB program can find it.
  flowroot = fn_gen_get('flowroot')
  username = fn_gen_get('username')
  test_string = "Just a test string"
  
  test_file = fn_gen_build_path(fn_gen_build_path(flowroot,"Users"),username+".tst")
  cfg_file  = fn_gen_build_path(fn_gen_build_path(flowroot,"Users"),username+".cfg")
  print, "Test config ",test_file
  status = DC_WRITE_FREE(test_file, test_string)
  if (status eq 0) then begin
    del_file, test_file
    _pr_gen_set, 'cfg file', cfg_file
    setenv, 'config=' + cfg_file
    print, "Config file is ",cfg_file

  endif else begin
    ; can not configure, just quit
    _pr_gen_exit
  endelse
    
end

; dgp rev 10/19/05 gather environment
pro _pr_env_init

_pr_gen_ident ; dgp rev 1/28/10 debugging

  ; scan the directory structure
  if (fn_env_scan()) then begin

  endif else begin
  ; environment doesn't check out
    print, "Environment scan failed"
    _pr_gen_exit
  
  endelse
  
end

; dgp rev 10/19/05 gate initialization
pro _pr_mgs_init

_pr_gen_ident ; dgp rev 1/28/10 debugging

  _mgs_load_struct

end

; dgp rev 10/19/05 pv-wave initialization
pro _pr_pvw_init

_pr_gen_ident ; dgp rev 1/28/10 debugging
  
  math_init
  stat_init

  ; Load external files
  Flow_Drawing
  
  Flow_DispCells

  Flow_DispClust

  Flow_colors      

  Flow_Cluster 

  Flow_Enhance
  
  Flow_Overlay

  Flow_ListCells

  Flow_gate

  _pr_prt_init
 
  _pr_gen_set, 'backslash' , string('5c'xb)

; The display has one format ratio for duals and another
; ratio for singles.  Additionally, the display size is 
; a factor of the actual screen size.
  !p.position = [.2,.33,.95,.9]
    
  !p.font = 0

  !p.ticklen = -.02
  _pr_fnt_init

  pixel_count = long(float(!display_size.y)*(10.0/36.0))

  _pr_gen_set, 'pixel count', pixel_count
  
  fnt_scale

end

; dgp rev 3/16/2010
pro _pr_sym_init

  cnt = fn_gen_get('user sym')
  if (cnt eq 0) then cnt = 16

  print, "PSym ",cnt

  A = FINDGEN(cnt) * ( !Pi * 2 / float(cnt) ) 
  
  ; Make a vector of 16 points, ai = 2pi / 16. 
  
  USERSYM, COS(A), SIN(A), /Fill 

end

; dgp rev 10/03/05 setup the environment before
; dgp rev 10/19/05 proper order for initialization
; - PV-Wave initialization
; - calculate tables, setup non-prerequisite defaults
; - gater environment variables, directories and files
; - read configuration file and FCS file list
; - load IFS structure with first file in list
; - setup DMF structure
; - define any widget info
pro _init_setup

_pr_gen_ident ; dgp rev 1/28/10 debugging

  print, "Init setup"
  ; PV-Wave initialization
  _pr_pvw_init

; - calculate tables, setup non-prerequisite defaults
  _pr_gen_init
  
  _pr_sym_init
  ; gather environment information
  _pr_env_init

  ; user configuration initialization
  _pr_cfg_init       
  
  ; load the first file into ifs structure
  _pr_ifs_init                   ; initialize the ifs

  ; load the gates
  _pr_mgs_init

  ; display manipulation 
  _pr_dmf_init      ; configure the first dmf entry as first pfl file

  ; custom display settings
  _pr_cds_scan

  ; overlay setup
  _pr_olay_init
  
end

pro _pr_wms_debug

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_wms_info, cv_main_hash

  stop ;debug
  
end

pro _pr_wms_set_cfg

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_cfg_perm, cv_cfg_hash
  common cm_wms_info, cv_main_hash
  
  if (size(cv_main_hash,/type) ne 11) then return

  keys = askeys(cv_main_hash)
  for i=0,n_elements(keys)-1 do begin
    keys(i) = 0
  endfor

  if (size(cv_cfg_hash,/type) ne 11) then begin
    cv_cfg_hash = asarr('wms',cv_main_hash)
  endif else begin
    cv_cfg_hash = [cv_cfg_hash,asarr('wms',cv_main_hash)]
  endelse

end

; dgp rev 8/30/05 given a form name, return the main widget
; if the widget has been closed or doesn't exist,then return 0
function _wms_get_main, ident

  common cm_wms_info, cv_main_hash

  if (ISASKEY(cv_main_hash, ident)) then begin
    wid = cv_main_hash(ident,'wid')
    if (fn_wid_exists(wid)) then return, wid
  endif
  
  return, 0

end

function _get_file_cnt

  common per_file, file_cnt
  return, file_cnt

end


PRO CB_Default, wid, index

_pr_gen_ident ; dgp rev 1/28/10 debugging

  ;print, "Double-click file name"

END

function fn_calc_pos, x, y

  x_max = !display_size.x
  y_max = !display_size.y

  x = x mod 10
  y = y mod 10

  return, [(float(x)/10)*x_max,(float(y)/10)*y_max]

end
; dgp rev 12/8/05 set a variable in the windows management system (WMS) namespace
pro _pr_wms_set, label, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_frm_info, cv_frm_hash
  
  if (size(cv_frm_hash,/type) ne 11) then begin
    cv_frm_hash = asarr('reform',0)
    cv_frm_hash = [cv_frm_hash,asarr('create',1)]
  endif

  cv_frm_hash = [cv_frm_hash,asarr(label,value)]

end


; initialize the top window
pro _wms_init_main, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_wms_info, cv_main_hash
  
  cv_main_hash = asarr('top',asarr('wid',value))


  pos = fn_calc_pos(0,0)  
  cv_main_hash = [cv_main_hash,asarr("Description",asarr('pos',pos))]
  pos = fn_calc_pos(2,1)  
  cv_main_hash = [cv_main_hash,asarr("Command Interface",asarr('pos',pos))]
  pos = fn_calc_pos(6,6)  
  cv_main_hash = [cv_main_hash,asarr("File Form",asarr('pos',pos))]
  pos = fn_calc_pos(6,6)  
  cv_main_hash = [cv_main_hash,asarr("Setup Form",asarr('pos',pos))]
  pos = fn_calc_pos(6,1)  
  cv_main_hash = [cv_main_hash,asarr("Display Form",asarr('pos',pos))]
  pos = fn_calc_pos(0,3)  
  cv_main_hash = [cv_main_hash,asarr("PPT Setup",asarr('pos',pos))]
  pos = fn_calc_pos(4,6)  
  cv_main_hash = [cv_main_hash,asarr("Style Form",asarr('pos',pos))]
  pos = fn_calc_pos(6,3)  
  cv_main_hash = [cv_main_hash,asarr("Gate Type",asarr('pos',pos))]
  pos = fn_calc_pos(8,3)  
  cv_main_hash = [cv_main_hash,asarr("Set Levels Form",asarr('pos',pos))]
  pos = fn_calc_pos(0,5)  
  cv_main_hash = [cv_main_hash,asarr("Levels Form",asarr('pos',pos))]
  pos = fn_calc_pos(2,5)  
  cv_main_hash = [cv_main_hash,asarr("Smooth Menu",asarr('pos',pos))]
  pos = fn_calc_pos(1,1)  
  cv_main_hash = [cv_main_hash,asarr("Control Panel",asarr('pos',pos))]
  pos = fn_calc_pos(6,5)  
  cv_main_hash = [cv_main_hash,asarr("Stat Graphics",asarr('pos',pos))]
  pos = fn_calc_pos(8,5)  
  cv_main_hash = [cv_main_hash,asarr("Report Preview",asarr('pos',pos))]
  pos = fn_calc_pos(0,7)  
  cv_main_hash = [cv_main_hash,asarr("Statistics Report",asarr('pos',pos))]
  pos = fn_calc_pos(2,5)  
  cv_main_hash = [cv_main_hash,asarr("Gate Form",asarr('pos',pos))]
  pos = fn_calc_pos(4,7)  
  cv_main_hash = [cv_main_hash,asarr("Gate Statistics",asarr('pos',pos))]
  pos = fn_calc_pos(6,7)  
  cv_main_hash = [cv_main_hash,asarr("Gate Modify",asarr('pos',pos))]
  pos = fn_calc_pos(8,7)  
  cv_main_hash = [cv_main_hash,asarr("Raw Data",asarr('pos',pos))]

  _pr_wms_set, 'state', 'create'
   
end


; dgp rev 8/30/05 set the widget value for the main window, ident
pro _wms_set_main, ident, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_wms_info, cv_main_hash
  
  print, "Record ",ident," as ",value
  ident = Strtrim(ident,2)
  if (isaskey(cv_main_hash,ident)) then begin
    cv_main_hash(ident) = [cv_main_hash(ident),asarr('wid',value)]
  endif else begin
    cv_main_hash = [cv_main_hash,asarr(ident, asarr('wid',value))]
  endelse
  
end

pro dbg_wms_show

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_wms_info, cv_main_hash

  keys = ASKEYS(cv_main_hash)
  
  FOR I = 0, N_ELEMENTS(cv_main_hash) - 1 DO begin
       print, keys(i)," is ",cv_main_hash(keys(i))
      
  ENDFOR

end

; dgp rev 8/30/05 save the position of the widget
pro _pr_wms_set_pos, name, pos

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_wms_info, cv_main_hash

  if (isaskey(cv_main_hash,name)) then begin
    print, "position of ",name," is ",pos
    cv_main_hash(name) = [cv_main_hash(name),asarr('pos',pos)]
  endif  
end

; dgp rev 9/1/05 given a wid value, determine the hash name and set it to zero
pro ccb_wms_destory, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  if (fn_wid_exists(wid)) then begin
    wid = _wid_get_parent(wid)
    _pr_wms_set_pos, WtGet(wid,/name), dgpGetValue(wid,/position)
  endif
  
end
; dgp rev 9/1/05 given a wid value, determine the hash name and set it to zero
pro cccb_wms, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  print, "Confirm close"

  if (fn_wid_exists(wid)) then begin
    wid = _wid_get_parent(wid)
    _pr_wms_set_pos, WtGet(wid,/name), dgpGetValue(wid,/position)
    s = WwSetValue(wid,/close)
  endif
  
end

; dgp rev 1/28/10 simply tracking the flow
pro _pr_gen_ident

  return

  info, trace=tree
  cnt = n_elements(tree)
  line = make_array(cnt,/string,value=' ')
  
  for i=1, n_elements(tree)-1 do begin

    line = line + string(" ",tree(i), Format='(A,X)')
    
  endfor

  print, line

end

; dgp rev 8/30/05 make the form visible
pro _pr_wms_display, name

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_wms_info, cv_main_hash

    if (isaskey(cv_main_hash,name)) then begin
      wid = cv_main_hash(name,'wid')
      if (fn_wid_exists(wid)) then begin
        shell_name = WtGet(wid, /name)
        if (shell_name eq name) then begin
          status = WwHandler(wid, 'kphd_keystroke', 'KeyPressMask')
          status = WwSetValue(wid, /display)
        endif else begin
          cv_main_hash(name,'wid') = 0
        endelse
      endif
    endif

end

; dgp rev 9/1/05
; Check for widget by name.  If a valid
; widget exists, then raise it.
function fn_wms_raise, name

  common cm_wms_info, cv_main_hash

  if (isaskey(cv_main_hash,name)) then begin
    if (isaskey(cv_main_hash(name),'wid')) then begin      
      wid = cv_main_hash(name,'wid')
      if (fn_wid_exists(wid)) then begin
        shell_name = WtGet(wid, /name)
        if (shell_name eq name) then begin
          print, "Raise ",name
          status = WtSet(wid, /unrealize)
          status = WtSet(wid, /realize)
          return, 1
        endif else begin
          cv_main_hash(name,'wid') = 0
        endelse
      endif
    endif
  endif

  return, 0
  
end
; check hash for main widget by name.  If it exists, then 
; check to be sure it matches the current widget.  Finally,
; grab the position of the widget and close
; return the position of the destroyed widget
function _wms_rebuild, name

  common cm_wms_info, cv_main_hash

  pos = [0,0]
    if (isaskey(cv_main_hash,name)) then begin
      wid = cv_main_hash(name,'wid')
      if (fn_wid_exists(wid)) then begin
        shell_name = WtGet(wid, /name)
        if (shell_name eq name) then begin
          pos = dgpGetValue(wid, /position)
          cv_main_hash = [cv_main_hash,asarr(name, asarr('pos',pos))]
          status = WwSetValue(wid, /close)
          cv_main_hash(name,'wid') = 0
        endif
      endif
    endif

  return, pos
  
end

; dgp rev 8/25/05
; the trick to closing a form is to not only validate 
; that the widget is open, but also that it is the 
; correct widget.  Widget identifiers get reused.
; Also save the position of the form.
pro _pr_wms_close, name

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_wms_info, cv_main_hash

  print, "_pr_wms_close"
  
    if (isaskey(cv_main_hash,name)) then begin
      if (isaskey(cv_main_hash(name),'wid')) then begin
        wid = cv_main_hash(name,'wid')
        if (fn_wid_exists(wid)) then begin
          shell_name = WtGet(wid, /name)
          if (shell_name eq name) then begin
            pos = dgpGetValue(wid, /position)
            cv_main_hash = [cv_main_hash,asarr(name, asarr('pos',pos))]
            status = WwSetValue(wid, /close)
            cv_main_hash(name,'wid') = 0
          endif
        endif
      endif
    endif
  
end

pro _wms_close_branch, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  if (WtGet(wid ,/destroy) ne 1) then begin
    while (WtGet(wid ,/shell) ne 1) do wid = WtGet(wid ,/parent)
  
    parent  = WtGet(wid,/name)
  
    if (WtGet(wid ,/destroy) ne 1) then begin
      status = WtClose(wid)
    endif
  endif

end

; dgp rev 8/30/05 when a window is closed, clear the wms widget and save
; the position
pro ccb_wms_close, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  if (fn_wid_exists(wid)) then begin
    wid = _wid_get_parent(wid)
    _pr_wms_set_pos, WtGet(wid,/name), dgpGetValue(wid,/position)
  endif
  
end

pro _wms_end_app, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  ;print, "Ending the application"
  ;print, "widget ",wid," value ",value

end

pro _sense_add, str, num, state

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common sense, com_sense, com_widhash

  if (STRPOS(state, "nons") eq -1) then begin
    state = "/sensitive"
  endif else begin
    state = "/nonsensitive"
  endelse
  
  if (dgpGetValue(num,/exists)) then begin
    if (not dgpGetValue(num,/destroyed)) then begin
      failed = "false"
      widstr = string(num)
      com = "status = WwSetValue("+widstr+","+state+")"
      results = execute(com)

    ;  status = WwSetValue(num,state)
      if (size(com_sense,/type) eq 0) then begin
    ; create the structure
        com_sense   = asarr(str,state)
        com_widhash = asarr(str,num)
      endif else begin
        if (ISASKEY(com_sense, str)) then begin
          com_sense(str) = state
          com_widhash(str) = num
        endif else begin
          tmp1 = asarr(str, state)
          com_sense = [com_sense,tmp1]
          tmp2 = asarr(str, num)
          com_widhash = [com_widhash,tmp2]
        endelse
      endelse
    endif
  endif
end


; dgp rev 1/28/10 revise
pro _sense_mod, str, state

  common sense, com_sense, com_widhash

;  print, "Mod ",str," to ",state
  if (size(com_widhash,/type) ne 0) then begin
    if (ISASKEY(com_widhash, str)) then begin
      num = com_widhash(str)
      if (dgpGetValue(num,/exists)) then begin
        if (not dgpGetValue(num,/destroyed)) then begin
          if (STRPOS(state, "nons") eq -1) then begin
            state = "/sensitive"
          endif else begin
            state = "/nonsensitive"
          endelse
          widstr = string(num)
          com = "status = WwSetValue("+widstr+","+state+")"
          results = execute(com)
          com_sense(str) = state
          com_widhash(str) = num
        endif
      endif
    endif
  endif

end

pro _wid_register, uname, pass, new_value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common wid_reg, value_hash, id_hash
  
  if (size(pass,/ndim) eq 0) then begin
    wig_id = pass
  endif else begin
    wig_id = pass(new_value)
    new_value = 1
  endelse
  
  if (dgpGetValue(wig_id,/exists)) then begin
    if (not dgpGetValue(wig_id,/destroyed)) then begin
      widstr = string(wig_id)
      wid_class = WtGet(wig_id, /class)
      case wid_class of 
      
        7 :   com = "status = WwSetValue("+widstr+","+strtrim(new_value,2)+")"
        
        else: com = "status = WwSetValue("+widstr+",'"+strtrim(new_value,2)+"')"
      endcase
;      print, com
      results = execute(com)

    ;  status = WwSetValue(wig_id,new_value)
      if (size(value_hash,/type) eq 0) then begin
    ; create the structure
        value_hash   = asarr(uname,new_value)
        id_hash = asarr(uname,pass)
      endif else begin
        if (ISASKEY(value_hash, uname)) then begin
          value_hash(uname) = new_value
          id_hash(uname) = pass
        endif else begin
          value_hash = [value_hash,asarr(uname, new_value)]
          id_hash = [id_hash,asarr(uname, pass)]
        endelse
      endelse
    endif
  endif
end


pro _wid_modify, uname, new_value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common wid_reg, value_hash, id_hash

  if (size(id_hash,/type) ne 0) then begin
    if (ISASKEY(id_hash, uname)) then begin
      pass = id_hash(uname)

      if (size(pass,/ndim) eq 0) then begin
        wig_id = pass
      endif else begin
        wig_id = pass(new_value)
        new_value = 1
      endelse

      if (dgpGetValue(wig_id,/exists)) then begin
        if (not dgpGetValue(wig_id,/destroyed)) then begin
          widstr = strtrim(wig_id,2)
          wid_class = WtGet(wig_id, /class)
          case wid_class of 
      
            7 : begin
                  com = "status = WwSetValue("+widstr+",new_value)"
                end
                
            else: com = "status = WwSetValue("+widstr+",'"+strtrim(new_value,2)+"')"

          endcase
          
;          print, com
          results = execute(com)
          value_hash(uname) = new_value
          id_hash(uname) = wig_id
        endif
      endif
    endif
  endif

end

pro _show_sense

_pr_gen_ident ; dgp rev 1/28/10 debugging

    common sense,   com_sense, com_widhash
     
    keys = ASKEYS(com_sense)
  
    FOR I = 0, N_ELEMENTS(com_sense) - 1 DO begin
       butt = keys(i)
       stat = string(com_sense(keys(i)))   
       print, butt," state ",stat
    ENDFOR

end
; dgp rev 12/27/05 save the current settings
pro _pr_cfg_save

  _pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_cfg_perm, cv_cfg_hash

  if (size(cv_cfg_hash,/type) eq 0) then return

  ON_IOERROR, problem

  path = fn_gen_get('setup path')
  savename = strcompress(string(path,'setting.cfg')) 

  save, cv_cfg_hash, filename=savename

   problem:
   print, !err
   print, !err_string

end

; dgp rev 1/2/06 exit the entire program gracefully.
pro _pr_gen_exit

_pr_gen_ident ; dgp rev 1/28/10 debugging

  _pr_gen_set, 'state', 'exit'
  
  _pr_wms_set_cfg
  
  _pr_cfg_save

  exit

end

; dgp rev 1/2/06 callback to gracefully exit the entire program.
pro CB_gen_exit, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging
    
  _pr_gen_exit

end

PRO CB_Exit, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  parent_wid = _wid_get_parent(wid)
  if (parent_wid ne 0) then begin 
    name   = WtGet(wid,/name)
    parent = WtGet(parent_wid,/name)
    status = WtClose(parent_wid)
  endif

END

PRO CB_Grab, wid, index

_pr_gen_ident ; dgp rev 1/28/10 debugging
   
   ;print, "Grabbed display into clipboard"
   status = wcopy()

END

PRO CB_NoOp, wid, index

_pr_gen_ident ; dgp rev 1/28/10 debugging

END

PRO CB_Derive, wid, index

_pr_gen_ident ; dgp rev 1/28/10 debugging
   
END
; dgp rev 10/25/05 relative move back in file list, with wrap
pro CB_Prev_all, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging
  
  _pr_gen_set, 'movement', 2
  if (fn_fcr_move_relative(-1)) then begin
    _pr_dsp_draw_locked
  endif 
  _pr_fmf_reflect
  _pr_reflect_cdr
  
end
; dgp rev 10/25/05 relative move back in file list, with wrap
pro CB_Prev_left, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging
  
  _pr_gen_set, 'movement', 0
  if (fn_fcr_move_relative(-1)) then begin
    _pr_dsp_draw_locked
  endif 
  _pr_fmf_reflect
  _pr_reflect_cdr
  
end
; dgp rev 10/25/05 relative move back in file list, with wrap
pro CB_Prev_right, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging
  
  _pr_gen_set, 'movement', 1
  if (fn_fcr_move_relative(-1)) then begin
    _pr_dsp_draw_locked
  endif 
  _pr_fmf_reflect
  _pr_reflect_cdr
  
end
; dgp rev 10/25/05 relative move forward in file list, with wrap
pro CB_Next_all, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging
  
  _pr_gen_set, 'movement', 2
  if (fn_fcr_move_relative(+1)) then begin
    _pr_dsp_draw_locked
  endif 
  _pr_fmf_reflect
  _pr_reflect_cdr

end

; dgp rev 10/25/05 relative move forward in file list, with wrap
pro CB_Next_left, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  _pr_gen_set, 'movement', 0
  if (fn_fcr_move_relative(+1)) then begin
    _pr_dsp_draw_locked
  endif 
  _pr_fmf_reflect
  _pr_reflect_cdr

end

; dgp rev 10/25/05 relative move forward in file list, with wrap
pro CB_Next_right, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging
  
  _pr_gen_set, 'movement', 1
  if (fn_fcr_move_relative(+1)) then begin
    _pr_dsp_draw_locked
  endif 
  _pr_fmf_reflect
  _pr_reflect_cdr

end


pro show_range, layout

_pr_gen_ident ; dgp rev 1/28/10 debugging

   range_lo = WwLayout(layout,border=4, /Horizontal)
   
   t3   = WwText(range_lo, 'NoOpCB', /Label, $
                 Text='No ranges set', /Right)
end

function _mgs_get_sort_names, names

  common cm_mgs_sort, cv_sorted_keys, cv_sorted_count, cv_sorted_names, cv_sorted_prefix

  if (cv_sorted_count gt 0) then begin
    names = cv_sorted_names
    return, 1
  endif
  names = ['']
  return, 0    

end

; sort gates by unique name, so that the parameter pairs are
; all grouped together.  Then save 
pro _mgs_resort_keys

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_mgs_sort, cv_sorted_keys, cv_sorted_count, cv_sorted_names, cv_sorted_prefix
  common cm_mgs_info, cv_mgs_struct

  if (size(cv_mgs_struct,/type) ne 11) then begin
    cv_sorted_count = 0
    cv_sorted_keys  = ['']
  endif else begin
    keys = ASKEYS(cv_mgs_struct)
    loops = n_elements(keys)
    unames = make_array(1,/string)
    names = make_array(1,/string)
    for i=0, loops-1 do begin
      unames = [unames,cv_mgs_struct(keys(i)).uname]
      names = [names,cv_mgs_struct(keys(i)).name]
    endfor
    names = names(1:*)
    unames = unames(1:*)
    sindex = sort(unames)
    cv_sorted_names = names(sindex)
    cv_sorted_prefix = strmid(cv_sorted_names,0,4)
    cv_sorted_keys = keys(sindex)
    cv_sorted_count = loops
  endelse

end

function fn_mgs_get_key, index

  common cm_mgs_sort, cv_sorted_keys, cv_sorted_count, cv_sorted_names, cv_sorted_prefix

  if (size(cv_sorted_keys,/type) eq 0) then return, 0
  cnt = n_elements(cv_sorted_keys)
  if (index ge cnt) then return, 0
  if (index lt 0) then return, 0
  return, cv_sorted_keys(index)

end

pro RadioCB, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common wgi_info, prev_state, saved_state

  internal = dgpGetValue(wid)

  if (size(prev_state,/type) eq 0) then prev_state = [-1,-1]
  print, "Passed   ",value," Internal ",internal
  test_state = [value,internal]

  if (same(test_state,prev_state)) then begin
    s = WwSetValue(wid,0)
    print, "Return to ",saved_state
  endif

  if (internal eq 0) then saved_state = test_state
  prev_state = test_state

End


pro cb_wgi_goto, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common wgi_info, prev_state, saved_state

  key = fn_mgs_get_key(value-1)
  internal = dgpGetValue(wid)
  if (size(prev_state,/type) eq 0) then begin
    prev_state  = [-1,-1]
    saved_state = key
  endif
  test_state = [value,internal]

  if (same(test_state,prev_state)) then begin
    s = WwSetValue(wid,0)
    print, "Return to ",saved_state
    _gen_set_pars, saved_state
    draw_cells
  endif else begin
    if (internal eq 1) then begin
      key = fn_mgs_get_key(value-1)
      print, "Keys is ",key
      if (key ne 0) then begin
        _gen_set_pars, key
        draw_cells
      endif
    endif
  endelse
  
  if (internal eq 0) then saved_state = key
  prev_state = test_state

end

function _mgs_remove_key, key

  common cm_mgs_info, cv_mgs_struct

  if (size(cv_mgs_struct,/type) eq 11) then begin
    if (ISASKEY(cv_mgs_struct, key)) then begin
      keys = ASKEYS(cv_mgs_struct)
      loops = n_elements(keys)
      if (loops eq 1) then begin
        cv_mgs_struct = complex(1)
      endif else begin
        count = 0
        for i=0, loops-1 do begin
          if (keys(i) ne key) then begin
            count = count + 1
            if (count eq 1) then begin
              temp1 = asarr(keys(i),cv_mgs_struct(keys(i)))
            endif else begin
              one = asarr(keys(i),cv_mgs_struct(keys(i)))
              temp1 = [temp1,one]
            endelse
          endif
        endfor
        cv_mgs_struct = temp1
        return, 1
      endelse
    endif
  endif
  return, 0

end

pro cb_wgi_delete, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  if (dgpGetValue(wid) eq 1) then begin
    key = fn_mgs_get_key(value-1)

    main = _wid_get_parent(wid)
    message = ['Permanently removed key']
    button=WwAlert(main, message, ["OK","Cancel"])

    if (key ne 0 and button eq 1) then begin
      ; remove the key from the struct
      status = _mgs_remove_key(key)
      ; resort the remaining keys
      _mgs_resort_keys
      ; save the key struct to file
      _mgs_save_struct
      ; reopen the gate window
      _pr_mgs_rebuild_frm, wid
      ; recombine subsets
      _pr_dmf_set, 'mgs flag', 1
      ; merge subsets
      _ssl_recalc
      ; redraw the display
      draw_cells
    endif else begin
      s = WwSetValue(wid,0)
    endelse
  endif

end

pro cb_wgi_out_all, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging
  
  val = dgpGetValue(wid)
  if (fn_mgs_get_sorted(keys)) then begin
    for index=0,n_elements(keys) -1 do begin
      _pr_wra_set_index, 'mgs outline',"Gate Form", index, val
      _pr_mgs_set,'outline',fn_mgs_get_key(index),val
    endfor
    draw_cells
  endif

  
end

pro cb_wgi_lbl_all, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging
  
  val = dgpGetValue(wid)
  if (fn_mgs_get_sorted(keys)) then begin
    for index=0,n_elements(keys) -1 do begin
      _pr_wra_set_index, 'mgs label',"Gate Form", index, val
      _pr_mgs_set,'label',fn_mgs_get_key(index),val
    endfor
    draw_cells
  endif

  
end

pro cb_wgi_label, wid, index

_pr_gen_ident ; dgp rev 1/28/10 debugging

  state = dgpGetValue(wid)
  _pr_mgs_set,'label',fn_mgs_get_key(index-1),state
  draw_cells
  
end

pro cb_wgi_perc, wid, index

_pr_gen_ident ; dgp rev 1/28/10 debugging

  state = dgpGetValue(wid)
  _pr_mgs_set,'perc',fn_mgs_get_key(index-1),state
  draw_cells
  
end

pro cb_wgi_perc_all, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging
  
  val = dgpGetValue(wid)
  if (fn_mgs_get_sorted(keys)) then begin
    for index=0,n_elements(keys) -1 do begin
      _pr_wra_set_index, 'mgs perc',"Gate Form", index, val
      _pr_mgs_set,'perc',fn_mgs_get_key(index),val
    endfor
    draw_cells
  endif
  
end
; dgp rev 12/9/05 set all stat toggles
pro cb_delete_tog_all, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  
end
; dgp rev 12/9/05 set all stat toggles
pro cb_stats_tog_all, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  val = dgpGetValue(wid)
  if (fn_mgs_get_sorted(keys)) then begin
    for index=0,n_elements(keys) -1 do begin
      _pr_wra_set_index, 'mgs stats',"Gate Form", index, val
      _pr_mgs_set,'stats',fn_mgs_get_key(index),val
    endfor
  endif
  
end

pro CB_wgi_outline, wid, index

_pr_gen_ident ; dgp rev 1/28/10 debugging

  state = dgpGetValue(wid)
  _pr_mgs_set,'outline',fn_mgs_get_key(index-1),state
  draw_cells

end

pro _pr_show_gates, layout

_pr_gen_ident ; dgp rev 1/28/10 debugging
    
  bb = 0
  ss = 0
  gate_lo  = WwLayout(layout, border=bb, /hor)
  ;_sense_add,'gate_lo', gate_lo, '/sensitive' 
  ;_mgs_reflect_exists

  gate_lo1 = WwLayout(gate_lo, border=bb, /ver)
  gate_lo15 = WwLayout(gate_lo, border=bb, /ver)
  gate_lo2 = WwLayout(gate_lo, border=bb, /ver)
  gate_lo3 = WwLayout(gate_lo, border=bb, /ver)
  gate_lo4 = WwLayout(gate_lo, border=bb, /ver)
  gate_lo5 = WwLayout(gate_lo, border=bb, /ver)
  gate_lo6 = WwLayout(gate_lo, border=bb, /ver)
  gate_lo7 = WwLayout(gate_lo, border=bb, /ver)
  gate_lo8 = WwLayout(gate_lo, border=bb, /ver)

  text = make_array(1,/string,value="Gates: ")

  if (_mgs_get_sort_names(snames)) then begin
  
      count = N_ELEMENTS(snames)
      blanks = make_array(count,/string,value=' ')  

      t1    = WwText(gate_lo1, 'NoOpCB', /Label, Text='Always On')

      rad1  = WwRadioBox(gate_lo1, snames, 'CB_gate_Tog', $
                      /Vert, /Nofmany, spacing=ss, $
                      toggle=togarr, border=0, /alignleft)
      _pr_wra_reg, 'applied', togarr
      
      t1    = WwText(gate_lo15, 'NoOpCB', /Label, Text='Only One')

      rad1  = WwRadioBox(gate_lo15, blanks, 'CB_gate1_Tog', $
                      /Vert, /Oneofmany, spacing=ss, $
                      toggle=togarr, border=0, /alignleft)
      _pr_wra_reg, '1only', togarr
      
      t2    = WwText(gate_lo2, 'NoOpCB', /Label, Text='Outline')

      rad2  = WwRadioBox(gate_lo2, blanks, 'CB_wgi_outline', $
                      /Vert, /Nofmany, spacing=ss, $
                      toggle=togarr, border=0)
      _pr_wra_reg, 'mgs outline', togarr
                      
      all2  = WwRadioBox(gate_lo2, 'All', 'CB_wgi_out_all', $
                      /Vert, /nofmany, /AlignLeft, border=0)

      t3    = WwText(gate_lo3, 'NoOpCB', /Label, Text='Label')

      rad3  = WwRadioBox(gate_lo3, blanks, 'CB_wgi_label', $
                      /Vert, /Nofmany, spacing=ss, $
                      toggle=togarr, border=0)
      _pr_wra_reg, 'mgs label', togarr

      all3  = WwRadioBox(gate_lo3, 'All', 'CB_wgi_lbl_all', $
                      /Vert, /nofmany, /AlignLeft, border=0)

      t4    = WwText(gate_lo4, 'NoOpCB', /Label, Text='Percent')

      rad4  = WwRadioBox(gate_lo4, blanks, 'CB_wgi_perc', $
                      /Vert, /Nofmany, spacing=ss, $
                      toggle=togarr, border=0)
      _pr_wra_reg, 'mgs perc', togarr

      all4  = WwRadioBox(gate_lo4, 'All', 'CB_wgi_perc_all', $
                      /Vert, /nofmany, /AlignLeft, border=0)

      t5    = WwText(gate_lo5, 'NoOpCB', /Label, Text='Goto')

      rad5  = WwRadioBox(gate_lo5, blanks, 'cb_wgi_goto', $
                      /Vert, /oneofmany, spacing=ss, $
                      toggle=togarr, border=0)

      t6    = WwText(gate_lo6, 'NoOpCB', /Label, Text='Delete')

      rad6  = WwRadioBox(gate_lo6, blanks, 'cb_wgi_delete', $
                      /Vert, /oneofmany, spacing=ss, $
                      toggle=togarr, border=0)
      all6  = WwRadioBox(gate_lo6, 'All', 'CB_delete_Tog_all', $
                      /Vert, /nofmany, /AlignLeft, border=0)

      t7    = WwText(gate_lo7, 'NoOpCB', /Label, Text='Stats')

      rad7  = WwRadioBox(gate_lo7, blanks, 'cb_wgi_stats', $
                      /Vert, /Nofmany, spacing=0, $
                      toggle=togarr, border=0)
      _pr_wra_reg, 'mgs stats', togarr
      
      all7  = WwRadioBox(gate_lo7, 'All', 'CB_stats_Tog_all', $ 
                      /Vert, /nofmany, /AlignLeft, border=0)

      t8    = WwText(gate_lo8, 'NoOpCB', /Label, Text='View')

      rad8  = WwRadioBox(gate_lo8, blanks, 'cb_wgi_view', $
                      /Vert, /Oneofmany, spacing=0, $
                      toggle=togarr, border=0)
      _pr_wra_reg, 'mgs view', togarr
                                       
  endif else begin
    text = "No gates"
    t1   = WwText(gate_lo, 'NoOpCB', /Label, /Top, $
                    Text='No Gates')
  endelse
  
end

pro show_clusters, layout

_pr_gen_ident ; dgp rev 1/28/10 debugging

   if (_car_active()) then begin
     _car_show, layout
   endif else begin
     cluster_lo = WwLayout(layout,border=4, /Horizontal, /Form)   
     t1   = WwText(cluster_lo, 'NoOpCB', /Label, Text='No clusters set')
   endelse
               
end

function fn_stat_get_pptfile

  filename = fn_gen_getstr('pptfile')
  if (filename eq "") then begin
    name = "ppt.dat"
    path = _cfg_get_work()
    filename = strcompress(string(path,name))
    found = findfile(filename,count=cnt)
    _pr_gen_set, 'pptfile', filename
  endif

  return, filename

end

pro _pr_dsp_reg, disp_name, win

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_disp_info, cv_name_hash
  
  if (size(cv_name_hash,/type) ne 11) then begin
    cv_name_hash = asarr(disp_name,win)
  endif else begin
    cv_name_hash = [cv_name_hash,asarr(disp_name,win)]
  endelse

end
;  dgp rev 9/1/05 callback for display locking
pro cb_dsp_lock, wid, index

_pr_gen_ident ; dgp rev 1/28/10 debugging

  value = dgpGetValue(wid)
  
  disp_list = fn_gen_get('dsp lst')

  print, "Index    ", index
  print, "Internal ",value

  name = disp_list(index)

  _pr_dsp_lock, name, value

end

; dgp rev 8/26/05 return a sorted list of active displays, including "default"
pro _pr_dsp_scan

_pr_gen_ident ; dgp rev 1/28/10 debugging

  arr = ["Select Display"]
  
  keys = fn_dmf_get_keys()

  nkeys = n_elements(keys)
  
  for i=0,nkeys-1 do begin
    ; skip default
    if (keys(i) ne 'default') then begin
      ; check for valid key
      if (fn_dmf_key_exists(keys(i))) then begin
        arr = [arr,keys(i)]
      endif
    endif
  endfor

  _pr_gen_set, 'dsp lst', arr
  
end

; dgp rev 9/2/05 lock all the displays together
pro cb_dsp_lock_all, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  internal = dgpGetValue(wid)

  disp_list = fn_gen_get('dsp lst')

  for i=1,n_elements(disp_list)-1 do begin

    _pr_dsp_lock, disp_list(i), internal

  endfor

  _pr_reflect_dsp
  _pr_reflect_locks

end

function fn_wms_get, label

  common cm_frm_info, cv_frm_hash
    
  if (size(cv_frm_hash,/type) ne 11) then begin
    cv_frm_hash = asarr('reform',0)
    cv_frm_hash = [cv_frm_hash,asarr('create',1)]
  endif

  if (isaskey(cv_frm_hash,label)) then return, cv_frm_hash(label)

  return, 0

end

function fn_wms_get_str, label

  common cm_frm_info, cv_frm_hash
    
  if (size(cv_frm_hash,/type) ne 11) then begin
    cv_frm_hash = asarr('reform',0)
    cv_frm_hash = [cv_frm_hash,asarr('create',1)]
  endif

  if (isaskey(cv_frm_hash,label)) then return, string(cv_frm_hash(label))

  return, ""

end
; dgp rev 1/4/06 determine the positioning of the form
function fn_wms_pos, frm_name

  common cm_wms_info, cv_main_hash

  pos = [(!display_size.x / 3),(!display_size.y / 3)]
  if (isaskey(cv_main_hash,frm_name)) then begin
    if (isaskey(cv_main_hash(frm_name),'pos')) then pos = cv_main_hash(frm_name,'pos') 
    print, frm_name," position is ",pos
  endif

  return, pos

end

; dgp rev 12/20/05 Window Mangement System (WMS) routine
; opens a new main window and registers the window id
; routine to open, reopen or raise a form
function fn_wms_form, frm_name, layout

   state = fn_wms_get('state')

   ; if create then look to raise existing form first
   if (state eq 'create') then begin
     if (fn_wms_raise(frm_name)) then return, 1
   endif

   ; if reform then close, after saving position
   if (state eq 'reform') then _pr_wms_close, frm_name

   pos = fn_wms_pos(frm_name)

   cb_routine = fn_wms_get_str('cb')
   if (cb_routine eq "") then cb_routine = 'ccb_wms_destory'
   qual = fn_wms_get_str('qual')

   ; get top widget
   top  = _wms_get_main('top')
   ; create shell form
   if (frm_name eq "Control Panel") then begin                               
     main = WwMainWindow(top, layout,cb_routine, shell_name=frm_name, pos=pos)
   endif else begin
     main  = WwMainWindow(top, layout, cb_routine, confirmclose='cccb_wms', $
                                shell_name=frm_name,position=pos,/ver) 
   endelse
                                
   ; record the form name
   _wms_set_main, frm_name, main
   ; set flag back to 'form'
   _pr_wms_set, 'state', 'create'
   
   return, 0
     
end


pro _pr_frm_template

_pr_gen_ident ; dgp rev 1/28/10 debugging

    frm_name = "Template"
  
     if (fn_wms_form(frm_name,layout)) then return
    
     widget_layout = WwLayout(layout, /Vertical)
    
; form widgets
    
     _pr_wms_display, frm_name
     
end

; is the form flag to be rebuilt
function fn_wms_reform

  if (size(cv_frm_reform,/type) eq 0) then cv_frm_reform = 0

  return, cv_frm_reform

end
; dgp rev 12/9/05 enable/disable export locking on a display
pro cb_export_tog, wid, index

_pr_gen_ident ; dgp rev 1/28/10 debugging

  value = dgpGetValue(wid)
  
  disp_list = fn_gen_get('dsp lst')

  name = disp_list(index)

  _pr_dsp_export, name, value

end
; dgp rev 12/9/05 enable/disable export locking on all displays
; 
pro cb_export_all, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  internal = dgpGetValue(wid)

  disp_list = fn_gen_get('dsp lst')

  for i=1,n_elements(disp_list)-1 do begin

    _pr_dsp_export, disp_list(i), internal

  endfor
  
  _pr_reflect_export

end
; dgp rev 8/26/05 show active displays on display form
pro _pr_dsp_show, layout

_pr_gen_ident ; dgp rev 1/28/10 debugging
    
  bb = 0
  ss = 0
  dsp_lo  = WwLayout(layout, border=bb, /hor)
  dsp_lo1 = WwLayout(dsp_lo, border=bb, /ver)
  dsp_lo2 = WwLayout(dsp_lo, border=bb, /ver)

  text = make_array(1,/string,value="Displays: ")
; holds the display list after a display scan, to 
; avoid excess scanning of displays
  disp_list = fn_gen_get('dsp lst')

  disp_cnt = n_elements(disp_list)
  wid_arr = make_array(disp_cnt,/long)

  if (disp_cnt gt 1) then begin
    
      disp_list = disp_list(1:*)
      count = N_ELEMENTS(disp_list)
      blanks = make_array(count,/string,value=' ')  
      blank = replicate(' ',count)
      
      t1    = WwText(dsp_lo1, 'NoOpCB', /Label, Text='Export')
      
      rad1  = WwRadioBox(dsp_lo1, blank, 'CB_export_tog', $
                            /Vert, /nofmany, spacing=ss, $
                            toggle=togarr, border=0, /alignleft)

      _pr_wra_reg, 'export', togarr

      all1  = WwRadioBox(dsp_lo1, 'All', 'CB_export_all', $
                      /Vert, /nofmany, /AlignLeft, border=0)

      t2    = WwText(dsp_lo2, 'NoOpCB', /Label, Text='Lock')

      rad2  = WwRadioBox(dsp_lo2, disp_list, 'CB_dsp_lock', $
                      /Vert, /Nofmany, spacing=ss, $
                      toggle=togarr, border=0)
      _pr_wra_reg, 'lock', togarr

      all2  = WwRadioBox(dsp_lo2, 'All', 'CB_dsp_lock_all', $
                      /Vert, /nofmany, /AlignLeft, border=0)
                     
  endif else begin
    text = "No Displays"
    t1   = WwText(dsp_lo, 'NoOpCB', /Label, /Top, Text='No Displays Active')
  endelse
  
end


pro dbg_cluster

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_car_nums, counts_cluster, means_cluster, cluster_group

  stop ;debug

end

pro dbg_gates

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_mgs_info, cv_mgs_struct

  stop ;debug

end

pro dbg_data

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common cm_dataset, cv_cells

  stop ;debug

end

PRO CB_menu, wid, index

_pr_gen_ident ; dgp rev 1/28/10 debugging

  value = dgpGetValue(wid)

  case value of
  'Cluster': dbg_cluster
  'Gates': dbg_gates
  'Data': dbg_data
  endcase

END


pro CB_Debug, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  frm_name = "Debug Form"

     if (fn_wms_form(frm_name,layout)) then return

     widget_layout = WwLayout(layout, /Vertical, /Top, /Left)

     fonts = {,callback:'CB_menu', $
       button:'Cluster',$
       button:'Gates',$
       button:'Data'}
     opmenu = WwOptionMenu(layout, 'Blocks:', $
       fonts, Position=[0,150])

     status        = WwSetvalue(main, /Display)
          
end

pro _cb_text, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  sav_wid = dgpGetValue(wid,/userdata)
  status    = WwSetValue(sav_wid,/sensitive)
  
end

pro cb_save_desc_ses, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common _descript, _desc_ses_flag, _ses_text, _desc_proj_flag, _proj_text  

  data_arr = dgpGetValue(wid,/userdata)
  wid = long(data_arr(0))
  text =    dgpGetValue(wid)
  filename = data_arr(1)
  status = DC_WRITE_FREE(filename, text)

end

; dgp rev 5/25/2010 Description File
pro _cb_descr, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  print, "Look for description file"
  path = _cfg_get_work()

  restname = strcompress(string(path,'description.txt'))
  
  found = findfile(restname,count=cnt) 
  if (cnt ne 0) then begin
    print, "Edit file ",restname
    frm_name = "Description"

     if (fn_wms_form(frm_name,layout)) then return
    
      text_lo = WwLayout(layout, /Vertical, /Top, /Left)
      txt_wid = WwText(text_lo, '_cb_Text', $
      	File = restname, Cols=40, Rows=20)
      sav = WwButtonBox(text_lo,'Save','cb_save_desc_ses')
      status = WwSetValue(txt_wid,userdata=sav)
      data_arr = [string(txt_wid),restname]
      status = WwSetValue(sav,userdata=data_arr,/nonsensitive)

      status = WwSetValue(main, /Display)

  endif

end

function _pfl_get_names, items

  common cm_pfl, cv_pfl_list, cv_file_count, cv_pfl_name_list

  if (size(cv_pfl_name_list,/type) ne 0) then begin
    items = cv_pfl_name_list
    return, 1
  endif
  items = ""
  return, 0

end

pro _fcr_init_menu, wid

_pr_gen_ident ; dgp rev 1/28/10 debugging

  common fcr_info, menu_wid, fcr_range_slider
  
  menu_wid = wid
  _fcr_reflect_menu

end
; dgp rev 12/20/05 file movement menu and controls
; dgp rev 12/20/05 for changing the current data file
; dgp rev 1/4/06 separate the reflection of the 3 menus
pro file_create_menu, widget_layout, index

_pr_gen_ident ; dgp rev 1/28/10 debugging

   common fil_menu, fil_menu_wid, fil_menu_callback
   
   ; determine which control to create
   index = index mod 3

   opt_arr = ['left','right','all']
   prev_cmd = 'cb_prev_' + opt_arr(index)
   next_cmd = 'cb_next_' + opt_arr(index)
   menu_name = 'Pull File ' + strtrim(index,2)
   
   fil_menu_callback = 'cb_pullfile_' + opt_arr(index)

   _pr_gen_set, 'movement', index

   lo_move       = WwLayout(widget_layout, /Hor, /Bot)
   lo_pull       = WwLayout(widget_layout, /Hor, /Bot)
   prebut   = WwButtonBox(lo_move, '<<Prev ',  prev_cmd, /hor)
   nxtbut   = WwButtonBox(lo_move, ' Next>>',  next_cmd, /hor)

   fil_wids = ['pullfile','fileopt']
     
   fil_sel = {,callback:fil_menu_callback, $
               button:'Select File'}
     
   fil_menu_wid = WwOptionMenu(lo_pull, '', fil_sel, name=fil_wids )
          
   fil_name = WtGet(fil_menu_wid,/name)
   fil_kids = dgpGetValue(fil_menu_wid,/children)
     
   ; set pulldown to the current file
   if (_pfl_get_names(fil_list)) then begin
     
     fil_opt = strtrim(fil_list,2)
     idx = 1
     for i=0,n_elements(fil_opt)-1  do begin
       status = WwMenuItem(fil_menu_wid, idx, fil_opt(i), fil_menu_callback, /Add)
     endfor

     _pr_wra_reg, menu_name, fil_menu_wid
     _fcr_init_menu, fil_menu_wid
   endif
   
end

pro cb_setup_sel, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  print, wid
  print, dgpGetValue(wid)
  print, value
  
end

pro CB_dmf_desc, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  print, wid
  print, dgpGetValue(wid)
  print, value

end
; dgp rev 12/5/05 DMF settings form
pro _pr_setup_menu, widget_layout

_pr_gen_ident ; dgp rev 1/28/10 debugging

   savbut = WwButtonBox(widget_layout,'Save Current','cb_dmf_save')
   desc   = WwText(widget_layout, 'NoOpCB', /Label, Text='Description')
   destxt = WwText(widget_layout, 'CB_dmf_desc', Text='', col=15)
   setup_list = ['this is a valid setup']
   setup_list = [setup_list,'here is another one']
   setup_list = [setup_list,'one more for the road']

   setup_menu_callback = 'cb_setup_sel'

   lo_move       = WwLayout(widget_layout, /Hor, /Bot)
   lo_pull       = WwLayout(widget_layout, /Hor, /Bot)

   setup_wids = ['pullsetup','setupopt']
     
   setup_sel = {,callback:setup_menu_callback, $
               button:'Select File'}
     
   setup_menu_wid = WwOptionMenu(lo_pull, '', setup_sel, name=setup_wids )
          
   setup_name = WtGet(setup_menu_wid,/name)
   setup_kids = dgpGetValue(setup_menu_wid,/children)
     
   idx = 1
   for i=0,n_elements(setup_list)-1  do begin
     status = WwMenuItem(setup_menu_wid, idx, setup_list(i), 'cb_setup_sel', /Add)
   endfor
   
end

pro cb_cominput, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  internal = dgpGetValue(wid)
  print, "Process ",internal
  results = execute(internal)
  
  res = dgpGetValue(wid,/userdata)
  s = WwSetValue(res,string(results))

end

pro cb_command, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  frm_name = "Command Interface"

  if (fn_wms_form(frm_name,layout)) then return
 
  sp = 2
  bd = 1

  lo_form1 = WwLayout(layout,      /ver,spac=sp,bord=bd)
  lo_form2 = WwLayout(layout,      /ver,spac=sp,bord=bd)
  lo_batch = WwLayout(layout,      /ver,spac=sp,bord=bd)
  
  lbl      = WwText(lo_form1,'CB_ComInput',col=15,text='')
  res      = WwText(lo_form2,'CBNOOP',col=15,row=3,text='')
  
  s = WwSetValue(lbl,userdata=res)
  
       fil_wids = ['pullbatch','batchopt']
       
       fil_sel = {,callback:'cb_batch', $
         button:'Select File'}
       
       fil_menu = WwOptionMenu(lo_batch, '', fil_sel, name=fil_wids )
            
       fil_name = WtGet(fil_menu,/name)
       fil_kids = dgpGetValue(fil_menu,/children)
       
       fcb_list = findfile('*.fcb',count=cnt)
       
       if (cnt > 0) then begin
         print, "Found ",cnt," batch files"
       
         fil_opt = strtrim(fcb_list,2)
         idx = 1
         for i=0,n_elements(fil_opt)-1  do begin
           status = WwMenuItem(fil_menu, idx, fil_opt(i), 'cb_batch', /Add)
         endfor
       
       endif else begin
         print, "No batch files"
       endelse


  exitbut  = WwButtonBox(layout, 'Exit', 'CB_exit')
      
  status   = WwSetvalue(main, /Display)
  
end

; dgp rev 11/3/05 should users be able to edit the header??
pro CB_hdr_edit, wid, cell, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  name = WtGet(wid,/name)
  
  print, Name
  info, cell
  pm, cell
  print, "Value ",value

end
; dgp rev 11/3/05 create a header table
pro _pr_hdr_table, layout

_pr_gen_ident ; dgp rev 1/28/10 debugging

  header = fn_ifs_get('header')
  info, header
  
  keys = askeys(header)
  keys = keys(sort(keys))
  
  arr = [[keys(0)],[header(keys(0))]]
  for i=1, n_elements(keys)-1 do begin
    tmp = [[keys(i)],[header(keys(i))]]
    arr = [arr,tmp]
  endfor
  
  table    = WwTable(layout,'CB_hdr_edit',arr, $
                      visible=[10,10], cwidth=10, /ver)

end

; dgp rev 11/3/05 form to report on header info
pro cb_hdr_form, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

   frm_name = "Header Report"
  
   _pr_wms_set, 'state', 'reform'
  
   if (fn_wms_form(frm_name,layout)) then return
    
   widget_layout = WwLayout(layout, /Vertical)
    
; form widgets
   _pr_hdr_table, widget_layout

   _pr_wms_display, frm_name  

end
; dgp rev 12/20/05 select file movement controls
pro CB_fmf_map, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  internal = dgpGetValue(wid)
  udata    = dgpGetValue(wid,/userdata)

  if (internal eq 0) then return
  
  _pr_dmf_set_other, udata(0), 'movement', udata(1)

end
; dgp rev 8/26/05 create a two column list of check boxes
; one for each display
pro _pr_fmf_displays, layout

_pr_gen_ident ; dgp rev 1/28/10 debugging
    
  bb = 0
  ss = 0
  lo_dsp  = WwLayout(layout, border=bb, /ver)

  _pr_dsp_scan
  disp_list = fn_gen_get('dsp lst')

  if (n_elements(disp_list) gt 1) then begin
    
    disp_list = disp_list(1:*)
    disp_cnt = n_elements(disp_list)

    wid_arr = [0L,0L]

    blank = replicate(' ',disp_cnt)

    radio_pair = [' ',' ']
      
    t1    = WwText(lo_dsp, 'NoOpCB', /Label, Text='Displays')
    for i=0, disp_cnt-1 do begin

      lo_row = WwLayout(lo_dsp,/hor)
      
      radio_pair(0) = disp_list(i)

      t1    = WwText(lo_row, 'NoOpCB', /Label, Text='<<',cols=2)
      rad1  = WwRadioBox(lo_row, [disp_list(i),'>>'], 'CB_fmf_map', $
                            /hor, /oneofmany, spacing=ss, $
                            toggle=togarr, border=0, /alignleft)

      _pr_dmf_set_other, disp_list(i), 'fmf wids' , togarr

      udata0 = [disp_list(i),'0']
      udata1 = [disp_list(i),'1']
      if (fn_dmf_get_other(disp_list(i),'locked')) then begin
        s= WwSetValue(togarr(0),userdata=udata0,/sensitive)
        s= WwSetValue(togarr(1),userdata=udata1,/sensitive)
      endif else begin
        s= WwSetValue(togarr(0),userdata=udata0,/nonsensitive)
        s= WwSetValue(togarr(1),userdata=udata1,/nonsensitive)
      endelse

    endfor

  endif else begin
    text = "No Displays"
    t1   = WwText(lo_dsp, 'NoOpCB', /Label, /Top, Text='No Displays Active')
  endelse
  
end

; dgp rev 11/3/05 form to control file selection
; form also contains header reporting option
pro _pr_frm_files

_pr_gen_ident ; dgp rev 1/28/10 debugging

  frm_name = "File Form"
  
  if (fn_wms_form(frm_name,lo_full)) then return

  lo_main = WwLayout(lo_full, /hor, bord=1 )
  lo_cols = WwLayout(lo_full, /hor)
  
  lo_left   = WwLayout(lo_cols, /ver)
  lo_center = WwLayout(lo_cols, /ver)
  lo_right  = WwLayout(lo_cols, /ver)
  
  ; header report button    
  s = WwButtonBox(lo_main, 'Header Report', 'cb_hdr_form')
  file_create_menu, lo_main, 2
       
; identical menus on each side
  file_create_menu, lo_left, 0
  
  _pr_fmf_displays, lo_center
  
  file_create_menu, lo_right, 1

  _pr_wms_display, frm_name
     
end

pro CB_File, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  _pr_frm_files

end

; dgp rev 3/24/2010 Change general font information
pro cb_gen_font, wid, value

  def_font = fn_dmf_get('label font')
  print, "Default dsp font ",def_font
  font_string = win32_pick_font(/scal,default=def_font)
  _pr_dmf_set_all, 'label font', font_string
  _pr_dsp_draw_all

end

; dgp rev 3/24/2010 Change general font information
pro cb_mgs_font, wid, value

  def_font = fn_dmf_get('mgs font')
  print, "Default mgs font ",def_font
  font_string = win32_pick_font(/scal,default=def_font)
  _pr_dmf_set_all, 'mgs font', font_string
  _pr_dsp_draw_all

end

; dgp rev 12/5/05 DMF settings form
pro _pr_frm_setup

_pr_gen_ident ; dgp rev 1/28/10 debugging

    frm_name = "Setup Form"
  
     if (fn_wms_form(frm_name,layout)) then return
    
     widget_layout = WwLayout(layout, /Vertical)
    
; form widgets
     _pr_setup_menu, widget_layout
       
       
     ; dgp rev 3/24/2010 Display font
     fntbut   = WwButtonBox(widget_layout, 'Display Font', 'cb_gen_font')
     ; dgp rev 3/24/2010 Display font
     fntbut   = WwButtonBox(widget_layout, 'Gate Font', 'cb_mgs_font')

     _pr_wms_display, frm_name
     
end
; dgp rev 12/5/05 settings callback
; for now, just DMF
pro CB_settings, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  _pr_frm_setup

end

; dgp rev 9/9/05 display version information
pro _pr_frm_version

_pr_gen_ident ; dgp rev 1/28/10 debugging

    frm_name = "Version"
  
     if (fn_wms_form(frm_name,layout)) then return
    
     widget_layout = WwLayout(layout, /Vertical)
    
     version = 'Version: '+fn_gen_get('version')

     txt   = WwText(widget_layout,'NoOpCB',/label,text=version)
    
     _pr_wms_display, frm_name
     
end
; dpg rev 11/3/05 check for a valid file
function fn_beta_check, filename

  s = findfile(filename,count=cnt)
  
  if (cnt eq 0) then begin
    
    t2 = SYSTIME(0)
    OPENW, unit, filename, /Get_Lun
    printf, unit, "Starting Discussion on "+t2
    close, unit
    
  endif

  s = findfile(filename,count=cnt)

  return, (cnt gt 0)

end


; dgp rev 11/3/05 save beta discussion 
pro cb_beta_save, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  frm_name = "Benchmarking"

  if (fn_gen_get('beta mod')) then begin 
    filename = fn_gen_get('beta discussion')
    txtwid = fn_wra_get('beta notes',frm_name)
    info, txtwid
    pm, txtwid
    if (fn_wid_exists(txtwid(0))) then begin
      info, txtwid
      text = dgpGetValue(txtwid(0))
      s = dc_write_free(filename, text)
    endif
    _pr_gen_set, 'beta mod', 0
    _pr_wra_disable, 'beta save'
  endif

end
; dgp rev 11/3/05 beta discussion has been modified
pro cb_beta_mod, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  _pr_gen_set, 'beta mod', 1  
  _pr_wra_enable, 'beta save'

end
; dgp rev 9/9/05 display version information
pro _pr_frm_beta

_pr_gen_ident ; dgp rev 1/28/10 debugging

  frm_name = "Benchmark Testing"
  
  if (fn_wms_form(frm_name,layout)) then return
    
  lo_text = WwLayout(layout, /Vertical)
  lo_controls = WwLayout(layout, /Hor)
    
  filename = fn_gen_get('beta discussion')
     
  if (fn_beta_check(filename)) then begin

    txtwid  = WwText(lo_text, 'cb_beta_mod', file=filename, cols=40, row=15, /vscroll) 
    _pr_wra_reg, 'beta notes', txtwid
    
  endif

  sav = WwButtonBox(lo_controls,'Save','cb_beta_save')
  tst = WwButtonBox(lo_controls,'Test','cb_beta_test')
  _pr_wra_reg, 'beta save', sav
  ext = WwButtonBox(lo_controls,'Exit','cb_exit')
  _pr_wra_disable, 'beta save'
  _pr_wms_display, frm_name
     
end
; dgp rev 9/9/05 callback to display version information
pro cb_version, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  _pr_frm_version  

end
; dgp rev 11/3/05 beta testing discussion interface
pro cb_beta, wid, value

_pr_gen_ident ; dgp rev 1/28/10 debugging

  _pr_frm_beta

end

; dgp rev 12/15/2010
pro _pr_tst_all

   _pr_tst_read
   
   _pr_tst_display
   
   _pr_gen_exit

end

; dgp rev 12/15/2010
pro cb_beta_test, wid, value

  _pr_tst_all

end

; dgp rev 12/15/2010
pro _pr_frm_benchmark, wid, value

  _pr_frm_beta

end

; dgp rev 9/9/05 Main Control panel
PRO Flow_Control

   Common colour, Wht, Blk
   COMMON values, thickness, file, seed_count
   COMMON radcall, value

   ; run the initialization routines
   _init_setup
  
   version = fn_gen_get('version')
   _pr_gen_set, 'beta discussion', "I:\FlowRoot\Distribution\Notes.txt"
  
   top           = WwInit('FCS Controls', 'FCS Controls', layout, /Form)        

   _wms_init_main, top
  
;   _wms_show_main
   
   ; setup layout
   widget_layout = WwLayout(layout, /Vertical, /Top, /Left)

   ; create the array of buttons
   debut         = WwButtonBox(widget_layout, 'Version', 'CB_version', /Left, /Bottom)

   debut         = WwButtonBox(widget_layout, 'Debug', 'CB_Debug', /Left, /Bottom)

   setbut        = WwButtonBox(widget_layout, 'Settings', 'CB_Settings', /Left, /Bottom)

   descbut       = WwButtonBox(widget_layout, 'Description', '_cb_descr', name='Description', /Left, /Bottom)

   loadbut       = WwButtonBox(widget_layout, 'FCS Data', 'cb_fcs_data', /Left, /Bottom)
   status        = WwSetValue(loadbut, Userdata=top)
   
   clustbut      = WwButtonBox(widget_layout, 'Cluster', 'CB_car_form', $
                               name='Cluster Menu', /Left, /Bottom)

   listbut       = WwButtonBox(widget_layout, 'Raw Data', 'cb_raw_data', /Left, /Bottom)
   status        = WwSetValue(listbut, Userdata=top)

   derivebut     = WwButtonBox(widget_layout, 'Derive', 'CB_Derive', /Left, /Bottom)

   dclubut       = WwButtonBox(widget_layout, 'N Plots', 'cb_dcl_form', $
                               name='Display Cluster', /Left, /Bottom)
   status        = WwSetValue(dclubut, Userdata=top)
      
   dcbut         = WwButtonBox(widget_layout, 'Histograms', 'cb_cdr_form', $
                               name='Display Cells', /Left, /Bottom)
   status        = WwSetValue(dcbut, Userdata=top)
   
   prtrbut       = WwButtonBox(widget_layout, 'Printer',  'CB_Printer')

   combut        = WwButtonBox(widget_layout, 'Command',  'CB_Command')

   filbut        = WwButtonBox(widget_layout, 'Files',  'CB_File')

   filbut        = WwButtonBox(widget_layout, 'Benchmarking',  'CB_Beta')

   exitbut       = WwButtonBox(widget_layout, 'Exit', 'CB_gen_exit', /Left, /Bottom)
   
   status        = WwSetValue(exitbut, Userdata=top ,/sensitive)
   
   ; set the initial sense of the buttons   
   _sense_add, 'listbut',    listbut, "/sensitive"
   _sense_add, 'loadbut',    loadbut, "/sensitive"
   _sense_add, 'derivebut',  derivebut, "/sensitive"
   _sense_add, 'dclubut',    dclubut, "/nonsensitive"
   _sense_add, 'dcbut',      dcbut, "/sensitive"

   status = WwHandler(top, 'kphd_keystroke', 'KeyPressMask')
   status = WwSetvalue(top, /Display)
   
   on_error, 1, /continue

   _pr_cdr_form
   
   _pr_frm_files
            
   _pr_dmf_open_active

;   status = WtProcessEvent(/Drain)
   
;   _pr_frm_benchmark

   WwLoop, /Noblock
  
END
