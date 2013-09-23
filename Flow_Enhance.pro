;
; $Id: wwmenuitem.pro,v 1.7 1999/04/28 21:25:12 timc Exp $
;             (c) Copyright Visual Numerics, Inc., 1992
;                     Boulder, Colorado, U.S.A.
;
;                        All Rights Reserved.
;
declare func, dgpGetValue

FUNCTION mymenuitem, parent, item, value, callback, $
             Update=Update, Add=Add, Delete=Delete, $
             Button=Button, Toggle=Toggle, Icon=Icon, $
             Menu=Menu, Name=Name, Wid=Wid
;+
; NAME: mymenuitem
; PURPOSE:
; Adds/Deletes/Modifies the specified menu item.
; CALLING SEQUENCE:
; status = mymenuitem(parent, item, value, callback,
;                    [,/UPDATE, /ADD, /DELETE
;                     ,/BUTTON, /TOGGLE, /ICON])
; INPUT:
;   - parent, menu pane id equired using MENUS= keyword.
;   - item:index of the menu item (1..); for /UPDATE, /DELETE
;       - value of the menu item; for /UPDATE, /ADD
;       - callback for the menu; for /ADD
; OUTPUT:
;   - 1-success, 0-failure
; KEYWORDS:
;   UPDATE  - modifies the specified item
;   ADD - appends the specified item
;          BUTTON
;          TOGGLE
;          ICON
;          MENU (cascade)
;   DELETE  - deletes the specified item
;   NAME    - names the widget created
; RESTRICTIONS:
;-
   ON_ERROR_GOTO, mymenuitem_Fail
   
@wtwacclasses
@wtwacconsts
   
   IF N_ELEMENTS(parent) EQ 0 THEN BEGIN
      WwError, 'Null Parent', /Fatal
   ENDIF ELSE IF parent EQ 0 THEN BEGIN
      WwError, 'Null Parent', /Fatal
   ENDIF

   ditem = item
   IF item GT 0 THEN BEGIN
      children = WtGet(parent, /CHILD) ; Menu Items
      IF N_ELEMENTS(children) LT item THEN BEGIN
         IF NOT KEYWORD_SET(Add) THEN BEGIN
            IF KEYWORD_SET(Delete) THEN BEGIN
               ditem = N_ELEMENTS(children)
            ENDIF ELSE BEGIN
               WwError, 'Menu item out of range', /Continue
               RETURN, 0        ; ? In range
            ENDELSE
         ENDIF
      ENDIF
      index = N_ELEMENTS(children)
   ENDIF ELSE index = 0
   
   IF KEYWORD_SET(Add) THEN BEGIN
      IF KEYWORD_SET(Button) THEN BEGIN
         IF KEYWORD_SET(Name) THEN b_name = Name ELSE $
         b_name = 'button__'+strtrim(string(index), 2)
         args = {, text:value, menuitemType:WacMenuString}
         w = WtCreate(b_name, menuitemWidgetClass, parent, args)
         IF w EQ 0 THEN WwError, "Unable to add menu item", /Fatal
         
         IF N_ELEMENTS(callback) NE 0 THEN BEGIN
            status = WtAddCallback(w, 'activateCallback', callback, index+1, /NOPARAMS)
         ENDIF
      ENDIF ELSE IF KEYWORD_SET(Menu) THEN BEGIN
         IF KEYWORD_SET(Name) THEN p_name = Name ELSE $
         p_name = 'pane__'+strtrim(string(index), 2)
         args = {, text:value}
         pane = WtCreate(p_name, menuWidgetClass, parent, args)
         IF pane EQ 0 THEN WwError, "Unable to create menu pane", /Fatal       
      ENDIF ELSE IF KEYWORD_SET(Icon) THEN BEGIN
         IF KEYWORD_SET(Name) THEN i_name = Name ELSE $
         i_name = 'icon__'+strtrim(string(index), 2)
         args = {, menuitemType: WacMenuBitmap, text:value}
         w = WtCreate(i_name, menuitemWidgetClass, parent, args)
         IF w EQ 0 THEN WwError, "Unable to add menu item", /Fatal

         IF N_ELEMENTS(callback) NE 0 THEN BEGIN
            status = WtAddCallback(w, 'activateCallback', $
                                   callback, index+1, /NOPARAMS)
         ENDIF
      ENDIF ELSE IF KEYWORD_SET(Toggle) THEN BEGIN
         IF KEYWORD_SET(Name) THEN t_name = Name ELSE $
         t_name = 'toggle__'+strtrim(string(index), 2)
         args = {, menuitemType: WacMenuToggle, text:value}
         w = WtCreate(t_name, menuitemWidgetClass, parent, args)
         IF w EQ 0 THEN WwError, "Unable to add menu item", /Fatal

         IF N_ELEMENTS(callback) NE 0 THEN BEGIN
            status = WtAddCallback(w, 'activateCallback', $
                                   callback, index+1, /NOPARAMS)
         ENDIF
      ENDIF ELSE BEGIN
         IF KEYWORD_SET(Name) THEN b_name = Name ELSE $
         b_name = 'button__'+strtrim(string(index), 2)
         args = {, text:value, foreground:'pink', menuitemType:WacMenuString}
         w = WtCreate(b_name, menuitemWidgetClass, parent, args)
         IF w EQ 0 THEN begin
           WwError, "Unable to add menu item", /Fatal
         endif else begin
           Wid = w
         endelse
         IF N_ELEMENTS(callback) NE 0 THEN BEGIN
            status = WtAddCallback(w, 'activateCallback', $
                                   callback, index+1, /NOPARAMS)
         ENDIF
      ENDELSE
   ENDIF ELSE IF KEYWORD_SET(Delete) THEN BEGIN
      IF item GT 0 THEN status = WtSet(children(ditem-1), /DESTROY)
   ENDIF ELSE IF KEYWORD_SET(Update) THEN BEGIN
      IF item GT 0 THEN status = WwSetValue(children(item-1), value, /Menuitem)
   ENDIF
   
   RETURN, 1

mymenuitem_Fail:
   RETURN, 0
END

; $Id: tensor_prod.pro,v 1.2 1996/07/29 21:51:21 jeffry Exp $
;+
; NAME:  TENSOR_PROD.PRO
;
; PURPOSE:  COMPUTE THE TENSOR PRODUCT OF TWO ARRAYS ALONG WITH AN OPTIONAL   
;           CONTRACTION OF INDICIES
;           
; USAGE:  C = TENSOR_PROD(A,B,N)
;
; INPUT:  A AND B ARE ARRAYS OF UP TO FOUR DIMENSIONS EACH.  N IS THE NUMBER OF 
;         INDICIES OVER WHICH TO SUM.  THE LAST N DIMENSIONS OF A ARE 
;         CONTRACTED WITH THE FIRST N DIMENSIONS OF B.  NOTE THAT DIMENSION 
;         LENGTHS MUST BE COMPATIBLE AND THAT SETTING N = 0 GIVES THE TENSOR 
;         PRODUCT (WITH NO CONTRACTION). 
;
; OUTPUT:  C IS THE TENSOR PRODUCT AB WHERE THE LAST N INDICIES OF A ARE 
;          CONTRACTED WITH THE FIRST N INDICIES OF B
;
; HISTORY:  1996  VNI  WIENECKE
;-

FUNCTION TENSOR_PROD, A, B, N

SA = SIZE(A)
SB = SIZE(B)
M = 1    &    FOR I = 1,SA(0)-N DO M = M*SA(I)
P = N_ELEMENTS(A)/M
Q = N_ELEMENTS(B)/P

IA = ''    &    FOR I = 1,SA(0)-N DO IA = IA + ',SA('+STRING(I)+')'
IB = ''    &    FOR I = 1,SB(0)-N DO IB = ',SB('+STRING(SB(0)+1-I)+')' + IB

..locals 1 0
S = EXECUTE('C = REFORM(REFORM(REFORM(A,M,P)#REFORM(B,P,Q)'+IA+IB+'))')

RETURN, C

END

; $Id: polyeval.pro,v 1.2 1998/02/10 20:42:48 allanw Exp $
;+
; name:     polyeval
;
; purpose:  evaluate a polynomial in n variables
;
; usage:    a = polyeval( c, x )
;
; input:    c is an n dimensional array of polynomial coefficients.  x is a
;           m x n matrix representing m points in n space.
;
; output:   a is a m element vector containing the value of the polynomial at
;           each of the m input points:
;
;           a(k) = c(j1, ... ,jn) * x(k,0)^j1 * ... * x(k,n-1)^jn
;
;           where summation over the indicies j1, ... jn is implied
; 
; history:  1998  vni  wienecke
;-

function polyeval, c, x

  sc = size( c )    &    n = sc(0)    &    tc = sc(n+1)    &    nc = sc(n+2)

  sx = size( x )    &    p = sx(0)    &    tx = sx(p+1)    &    y = 1d * x

  b = make_array( size = [ n+1, sx(1), sc(1:n), max([tc,tx,5]), sx(1)*nc ] )

  k = index_conv( c, lindgen(nc) )    &    if nc eq 1 then return, b + c(0)

  d = ' b(*,k(j,0) '    &    e = ' y(*,0) ^ k(j,0) '

  for i = 1, n-1 do d = d + ',k(j,' + string(i) + ')'

  for i = 1, n-1 do e = e + '*y(*,' + string(i) + ')^k(j,' + string(i) + ')'

  ..locals 1 0
  for j = 0, nc-1 do t = execute( d + ')=' + e )

  return, tensor_prod( b, c, n )

end
; $Id: polyfitn.pro,v 1.2 1998/04/22 01:55:20 allanw Exp $
;+
; name:     polyfitn
;
; purpose:  fit a polynomial to some n dimensional data
;
; usage:    p = polyfitn( d, a )
;
; input:    d is a real m x n+1 matrix of m data points in n independent
;           variables and 1 dependent variable; the last column contains the
;           dependent variable.  a is an n dimensional array containing only
;           0's and 1's; the nonzero entries designate the nonzero terms
;           of the polynomial: for example if n = 5 (with independent
;           variables x0,x1,x2,x3,x4) and a(i0,i1,i2,i3,i4) = 0 then polyfitn
;           sets the coefficient of x0^i0 * x1^i1 * x2^i2 * x3^i3 * x4^i4
;           equal to zero; otherwise a coefficient for that term is computed.
;
; output:   p is the n dimensional array of polynomial coefficients for the
;           least squares approximation:
;
;           dependent_variable = p(j1, ... ,jn) * x1^j1 * ... * xn^jn
;
;           where summation over the indicies j1, ... jn is implied
;
; options:  this routine requires the math option
;
; history:  1998  vni  wienecke
;-

function polyfitn, d, a

  if not option_is_loaded( 'math' ) then math_init

  s = size( a )    &    n = s(0)    &    p = s(n+2)    &    x = double( d )

  k = index_conv( a, lindgen(p) )    &    c = dblarr( n_elements(d(*,0)), p )

  b = ' c(*,j) = x(*,0) ^ k(j,0) '

  for i = 1, n-1 do b = b + '*x(*,' + string(i) + ')^k(j,' + string(i) + ')'

  ..locals 1 0

  for j = 0, p-1 do if a(j) ne 0 then t = execute( b )

  k = transpose( c )    &    c = svdcomp( k#c, invers=i, /double )

  return, a * (i#k#x(*,n))

end

; date:     12/05/05
; name:     poly_smooth
;
; purpose:  Implimentation of the Savitzky-Golay smoothing algorithm in PV-Wave
;
; usage:    kernel = poly_smooth( neighbor, degree )
;
; input:    'neighbor' is the distance out from the center that the 
;           kernel extends.  Neighbor is used as a normal in case this
;           routine is extended to more dimensions.  The width is 
;           calculated from this value as "neighbor*2+1".  The 1 is for 
;           the center and the neighbor extends out on each side of
;           the center.
;           
;           'degree' is the degree of the polynomial.  It must be
;           less than the width of the kernel.
;
; output:   'kernel' is a matrix of coefficients (width, width).
;           The kernel is can then be used with CONVOL on the dataset
;
; options:  this routine requires the math option
;

function poly_smooth, neighbor, degrees, dim

  ; the neighbor count refers to the distance
  ; out from center.  Therefore, the width in 
  ; a 2 dimensional system is out on each side
  ; plus the center
  ;dim = 2
  
  width = (2*neighbor+1)

  ; degrees must be less than the full width
  if (degrees ge width) then return, 0

  ; the size of the actual patch is the width
  ; squared -- the square can be replaced by 
  ; the number of dimensions.
  patch = width^dim

  ; Fill the values for all but the last column 
  ; of the "d" variable
  values = indgen(width)-(width/2)

  ; create the d using the above values and a column
  ; of all zeros, except for the center value of 1
  if (dim eq 1) then begin
    d = [ [cprod(list(values))], [findgen(patch) eq (patch/2)] ]
  endif else begin
    d = [ [cprod(list(values,values))], [findgen(patch) eq (patch/2)] ]
  endelse
  ; create the upper triangular matrix 
  ; humm...probably a better way to do it
  row = make_array(degrees+1,value=1)
  upper = row
  if (dim eq 2) then begin
    for i=1,degrees do begin
      row(degrees+1-i) = 0  
      upper = [[upper],[row]]
    endfor
  endif
  ; create the function
  ;save, d, upper, filename='poly.dat'
  f = polyfitn( d, upper)
  ; solve the function
  if (dim eq 2) then begin
    kernel = float(reform(polyeval(f,d(*,0:1)),width,width))
  endif else begin
    kernel = float(reform(polyeval(f,d(*,0)),width))
  endelse  
  ; return the new kernel
  return, kernel

end


; CALLING SEQUENCE:
;	Kernel = SAVGOL(Nleft, Nright, Order, Degree)
;
; INPUTS:
;	Nleft = An integer specifying the number of data points
;           used to the left of each point.
;
;	Nright = An integer specifying the number of data points
;            used to the right of each point.
;            The total width of the filter is Nleft + Nright + 1.
;
;	Order = An integer specifying the order of the derivative desired.
;           For example, set Order to 0 for the smoothed function,
;           Order to 1 for the smoothed first derivative, etc.
;           Order must be less than or equal to degree.
;
;	Degree = An integer specifying the degree of smoothing polynomial.
;            Typical values are 2, 4, or 6.
;            Degree must be less than the filter width.
;
; KEYWORD PARAMETERS:
;	DOUBLE = Set this keyword to do the calculation in double precision.
;            This is suggested for Degree greater than 9.
;
; OUTPUTS:
;	Result = the desired smoothing kernel.
;            Use CONVOL to apply this kernel to each dataset.
;
; PROCEDURE:
;	SAVGOL returns a kernel array that when used
;	with CONVOL, fits a polynomial of the desired degree to each
;	local neighborhood of a dataset with equally spaced points.
;
;	The coefficients returned by SAVGOL() do not depend on the data
;	to be fitted, but depend only on the extent of the window, the
;	degree of the polynomial used for the fit, and the order of the
;	desired derivative.
;
;   SAVGOL is based on the Numerical Recipes routine described in
;   section 14.8 of Numerical Recipes in C: The Art of Scientific Computing
;   (Second Edition), published by Cambridge University Press, and is used
;   by permission. This routine is written in the IDL language.
;   Its source code can be found in the file savgol.pro in the lib
;   subdirectory of the IDL distribution.
;
; EXAMPLE:
; The code below creates a noisy 800-point vector, with 8 gaussian peaks of
; decreasing width.  It then plots the vector, the vector smoothed
; with a 33-point Boxcar smoother (SMOOTH), and the vector smoothed
; with 33-point wide Savitsky-Golay filters of degree 4 and 6.
;
; It may be seen that the Savitsky-Golay filters do a much better job
; of preserving the narrower peaks, when compared to SMOOTH.
;
; n = 800                         ;# of points
; np = 8                          ;# of peaks
; y = replicate(0.5, n)           ;Form baseline
; x = findgen(n)                  ;Index array
; for i=0, np-1 do begin          ;Add each Gaussian peak
;     c = (i + 0.5) * float(n)/np ;Center of peak
;     y = y + exp(-(2 * (x-c) / (75. / 1.5 ^ i))^2)  ;Add Gaussian
; endfor
;
; !p.multi=[0,1,4]                ;Display 4 plots
; y1 = y + 0.10 * randomn(-121147, n) ;Add noise, StDev = 0.10
;
; plot, y1, TITLE='Noisy Data'    ;Show effects of different filters
; plot, y, LINESTYLE=1, TITLE='Smooth of 33'
; oplot, smooth(y1, 33, /EDGE_TRUNCATE)
; plot, y, LINESTYLE=1, TITLE='S-G of (16,16,4)'
; oplot, convol(y1, savgol(16, 16, 0, 4), /EDGE_TRUNCATE)
; plot, y, LINESTYLE=1, TITLE='S-G of (16,16,6)'
; oplot, convol(y1, savgol(16, 16, 0, 6), /EDGE_TRUNCATE)
;
;
; MODIFICATION HISTORY:
;	DMS	RSI	January, 2000
;   CT RSI February 2000: added error checking. Changed argument names.
;          Added /DOUBLE keyword.
;-

function savgol, nleft_in, nright_in, order_in, degree_in, $
	DOUBLE=double
; nLeft, nRight = number of points to left and right to use.
; order = order of derivative, 0 for fcn, 1 for 1st deriv, etc.
; degree = order of smoothing polynomial, usually 2 or 4.

ON_ERROR, 2
IF (N_PARAMS() NE 4) THEN MESSAGE, $
	'Incorrect number of arguments. r = SAVGOL(Nleft,Nright,Order,Degree)'
nLeft = nleft_in(0)   ; change 1-element vectors into scalars
nRight = nright_in(0)
order = order_in(0)
degree  = degree_in(0)
IF ((nLeft LT 0) OR (nRight LT 0)) THEN MESSAGE, $
	'Nleft and Nright must both be positive.'
IF (order GT degree) THEN MESSAGE, $
	'Order must be less than or equal to Degree.'

np = nLeft + nRight + 1                ;# of parameters to return
IF (degree GE np) THEN MESSAGE, $
	'Degree must be less than filter width.'

double = KEYWORD_SET(double)


IF (double) THEN BEGIN
	a = DBLARR(degree+1, degree+1)
	b = DBLARR(degree+1)                 ;Right hand side of equations
	cr = DINDGEN(nRight > 1) + 1d
	cl = -(DINDGEN(nLeft > 1) + 1d)
	power = DINDGEN(degree+1)
	c = DBLARR(np)                  ;Result array
ENDIF ELSE BEGIN
	a = fltarr(degree+1, degree+1)
	b = fltarr(degree+1)                 ;Right hand side of equations
	cr = findgen(nRight > 1) + 1.
	cl = -(findgen(nLeft > 1) + 1.)
	power = FINDGEN(degree+1)
	c = fltarr(np)                  ;Result array
ENDELSE

for ipj = 0, degree*2 do begin       ;Make the coefficient matrix
        sum = 0.0
	if (ipj EQ 0) then sum = 1.0
	IF (nRight GT 0) THEN sum = sum + total(cr ^ ipj)
	IF (nLeft GT 0) THEN sum = sum + total(cl ^ ipj)
    mm = ipj < (2 * degree - ipj)
    for imj = -mm, mm, 2 do a((ipj+imj)/2, (ipj-imj)/2) = sum
endfor

;ludc, a, Indx, DOUBLE=double
ludcmp, a, Indx, d
b(order) = 1.0                     ;= 1 for the derivative we want.
;b = lusol(a, Indx, b, DOUBLE=double)           ;Solve the system of equations
;b = make_array(degree+1,/float,value=1.0)
lubksb, a, Indx, b           ;Solve the system of equations
;for k=-nLeft, nRight do c(k+nLeft) = total(b * k ^ power, DOUBLE=double)
for k=-nLeft, nRight do c(k+nLeft) = total(b * k ^ power)

return, c
end


Function T_Histn, D, Axes, Binnum=Binnum, Binsize=Binsize, $

   Scale=Scale, Quantile=Quantile, Compatible=Compatible, Range=Range

On_Error, 2    ; Return To User On Error
If Keyword_Set(Compatibile) Then $
   Message, 'Compatible Keyword Is No Longer Required.'
If Keyword_Set(Binnum) And Keyword_Set(Binsize) Then $
   Message, 'Set Only Binnum Or Binsize.'
Sz = Size(D)
M = Sz(1)
If Sz(0) Eq 1 Then N = 1 Else N = Sz(2)

Dd = Double(D)
Mn = Min(Dd, Max=Mx, Dimension=0)
Mn = [0,0]

;if n_elements(range) eq 0 then range = Mx - Mn else range=range
Range = Mx - Mn
;print, "Range is ",range
;print, "Min ",Mn," Max ",Mx
If N_Elements(Binnum) Gt 0 Then Begin
   If N_Elements(Binnum) Lt N Then Binnum = Replv(Binnum,[1,N],1)
   Binsize = Range/Long(Binnum)
Endif Else If Keyword_Set(Binsize) Then Begin
   If N_Elements(Binsize) Lt N Then Binsize = Replv(Binsize,[1,N],1)
   Binsize = Double(Binsize)
   Binnum = Long(Range/Binsize) + 1L
Endif Else Begin
   Binsize = Replicate(1D,1,N)
   Binnum = Long(Range/Binsize) + 1L
Endelse

; T Contains The Bin Number Each Value Falls Into
If Keyword_Set(Quantile) Then $     ; Binnum-Central
   T = Nint((Dd-Rebin(Mn,M,N))*(Rebin(Binnum,M,N)-1)/Rebin(Range,M,N), /Long) $
Else $                              ; Binsize-Central
   T = Long((Dd-Rebin(Mn,M,N))/Rebin(Binsize,M,N)) < (Rebin(Binnum,M,N)-1)

C = Make_Array(Dimen=Binnum, /Long)
Ix = Index_Conv(C, T)
Ix = Histogram(Ix, Min=0, Max=N_Elements(C)-1)
C(0) = Ix

; Sd Computation Is Slow, So Only Do It If Needed
If N_Params() Gt 1 Then Begin
   Sd = Stdev(Dd, Dimension=0)
   Axes = Listarr(N)          ; Contains The Properly Scaled Coordinate Dims
   For I = 0L,N-1 Do $
      Axes(I) = (Lindgen(Binnum(I))*Binsize(I) + Mn(I) - 0.5*Binsize(I))/Sd(I)
Endif

If Keyword_Set(Scale) Then Begin  ; Scale The Surface Height So It Has Unit Volume
   If Size(Sd, /Type) Eq 0 Then Sd = Stdev(Dd, Dimension=0)
   C = C * ((Double(Binnum)*Binnum/(M*Product(Range)) * Product(Sd)))(0)
Endif

Return, C
End



function _hist, d, b=b, r=r, a=a

; d is (m,n) array of m points in n-space
; b is an n-element vector of binsizes
; r is (2,n) array where r(*,i) is the desired range of dimension i
; a is output: an n-element list of coordinate vectors for the histogram 


  s = size(d)
  m = s(1)
  if s(0) eq 1 then n = 1 else n = s(2)
  e = double(d)
  if n_elements(b) eq 0 then b = replicate(1d,1,n) else b = transpose(b)
  if n_elements(r) eq 0 then l = min(e,max=u,dimension=0) else begin
    l = r(0,*)
    u = r(1,*)
  endelse
  p = small_int((u-l)/b) + 1
  a = listarr(n)
  for i = 0, n-1 do a(i) = interpol( [l(i),u(i)], p(i) )
  h = make_array( dimension=p, /long )
  j = index_conv( h, long((e-rebin(l,m,n))/rebin(b,m,n)) )
  j = histogram( j, min=0, max=n_elements(h)-1 )
  h(0) = j

  return, h

end 

; $Id: k_means.pro,v 1.1.1.1 1995/06/19 19:44:40 rita Exp $
FUNCTION kmeans, x, $                             ;INPUT 1-D or 2-D array: floating point
                  seeds, $                         ;INPUT 1-D or 2-D array: floating point
                  Var_Columns=var_columns, $       ;INPUT 1-D array: LONG
                  Weights=weights, $               ;INPUT 1-D array: floating point
                  Frequencies=frequencies, $       ;INPUT 1-D array: floating point
                  Itmax=itmax, $                   ;INPUT Scalar LONG
                  Double=double, $                 ;INPUT Scalar ON/OFF flag
                  Means_Cluster=means_cluster, $   ;OUTPUT 2-D array: floating point
                  Ssq_Cluster=ssq_cluster, $       ;OUTPUT 1-D array: floating point
                  Counts_Cluster=counts_cluster    ;OUTPUT 1-D array: floating point
   COMMON CMAST$gen_com, on_err_action, FALSE, TRUE, $
     TYP_FLOAT, TYP_DOUBLE, TYP_COMPLEX, TYP_DCMPLX, $
     TYP_LONG
 
   ON_ERROR, on_err_action
   ;
   ; Error checking.
   ; Make sure the input arguments are 1-D or 2-D arrays.
   ;
   nargs = n_params()
   IF (nargs NE 2) THEN $
         message, "Incorrect number of arguments."
   size_x = size(x)
   IF ((size_x(0) LT 1) OR (size_x(0) GT 2)) THEN BEGIN 
      message, "X, the array containing the observations to be clustered, must be either 1-D or 2-D"
   END
   nobs = size_x(1)
   IF (size_x(0) EQ 1) THEN BEGIN
      nvar = 1L
   END ELSE BEGIN
      nvar = size_x(2)
   END
   IF (KEYWORD_SET(weights) EQ TRUE) THEN BEGIN
     size_weights = size(weights)
     IF (size_weights(0) NE 1) THEN BEGIN
        message, "The WEIGHTS array must be 1-D"
     END
     IF (size_weights(N_ELEMENTS(size_weights)-1) NE nobs) THEN BEGIN
        message, "The length of the WEIGHTS array be equal to the first dimension of X"
     END
   END
   IF (KEYWORD_SET(frequencies) EQ TRUE) THEN BEGIN
     size_frequencies = size(frequencies)
     IF (size_frequencies(0) NE 1) THEN BEGIN
        message, "The FREQUENCIES array must be 1-D"
     END
     IF (size_frequencies(N_ELEMENTS(size_frequencies)-1) NE nobs) THEN BEGIN
        message, "The length of the FREQUENCIES array must be equal to the first dimension of X"
     END
   END
   x_col_dim = LONG(nvar)
   IF (KEYWORD_SET(var_columns) EQ TRUE) THEN BEGIN
     size_tmp = size(var_columns)
     IF (size_tmp(0) NE 1) THEN BEGIN
        message, "The VAR_COLUMNS array must be 1-D"
     END
     nvar = size_tmp(N_ELEMENTS(size_tmp)-1)
     IF (nvar GT x_col_dim) THEN BEGIN
        message, "The length of the VAR_COLUMNS array must be less than " + $
                 "or equal to the second dimension of X"
     END
   END
   size_seeds = size(seeds)
   IF ((size_seeds(0) LT 1) OR (size_seeds(0) GT 2)) THEN BEGIN
      message, "SEEDS, the array containing the cluster seeds, must be either 1-D or 2-D"
   END
   seeds_dim0 = size_seeds(1)
   n_clusters = seeds_dim0
   IF (size_seeds(0) EQ 1) THEN BEGIN
      seeds_dim1 = 1L
   END ELSE BEGIN
      seeds_dim1 = size_seeds(2)
   END
   IF (seeds_dim1 NE nvar) THEN BEGIN
      IF (KEYWORD_SET(var_columns) EQ TRUE) THEN  $
        message, "The second dimension of SEEDS must be equal to the length of VAR_COLUMNS." $
        ELSE message, "The second dimension of SEEDS must be equal to the second dimension of X."
   END
   ;
   ; Decide on what precision to use.
   ; Use the highest precision of the input argument(s).
   ;
   type = TYP_FLOAT
   IF (size_x(N_ELEMENTS(size_x)-2) GT TYP_FLOAT) THEN type = TYP_DOUBLE
   IF (size_seeds(N_ELEMENTS(size_seeds)-2) GT TYP_FLOAT) THEN type = TYP_DOUBLE
   IF (KEYWORD_SET(double) EQ true) THEN type = TYP_DOUBLE
   ;
   ; Setup the parameters for the call to the system function.
   ;
   ; Result array
   l_result = lonarr(nobs)
   ;
   ; Input LONG Keywords
   IF (KEYWORD_SET(var_columns) EQ TRUE) THEN $
        var_columns_cvt = long(var_columns)
   IF (KEYWORD_SET(itmax) EQ TRUE) THEN $
     itmax_cvt = (long(itmax))(0)
   ;
   ; Output LONG Keywords
   IF (PARAM_PRESENT(counts_cluster) EQ TRUE) THEN $
     counts_cl_spc = lonarr(n_clusters)
   ;
   ; Floating point arguments and keywords
   IF (type EQ TYP_DOUBLE) THEN BEGIN
      ; 
      ; Input 
      x_cvt = double(transpose(x))
      seeds_cvt = double(transpose(seeds))
      IF (KEYWORD_SET(weights) EQ TRUE) THEN $
        weights_cvt = double(weights)
      IF (KEYWORD_SET(frequencies) EQ TRUE) THEN $
        frequencies_cvt = double(frequencies)
      ; 
      ; Output 
      IF (PARAM_PRESENT(means_cluster) EQ TRUE) THEN $
        means_clstr_spc = dblarr(nvar, n_clusters)
      IF (PARAM_PRESENT(ssq_cluster) EQ TRUE) THEN $
        ssq_cluster_spc = dblarr(n_clusters)
   END ELSE BEGIN
      ; 
      ; Input 
      x_cvt = float(transpose(x))
      seeds_cvt = float(transpose(seeds))
      IF (KEYWORD_SET(weights) EQ TRUE) THEN $
        weights_cvt = float(weights)
      IF (KEYWORD_SET(frequencies) EQ TRUE) THEN $
        frequencies_cvt = float(frequencies)
      ; 
      ; Output 
      IF (PARAM_PRESENT(means_cluster) EQ TRUE) THEN $
        means_clstr_spc = fltarr(nvar, n_clusters)
      IF (PARAM_PRESENT(ssq_cluster) EQ TRUE) THEN $
        ssq_cluster_spc = fltarr(n_clusters)
   END
   ;
   ; Call the system function.
   ;
    err_status = 0L
  MATHSTAT_153, type,  err_status, x_cvt, x_col_dim, nobs, nvar, seeds_cvt, n_clusters, $
                weights_cvt, $
                frequencies_cvt, $
                var_columns_cvt, $
                itmax_cvt, $
                means_clstr_spc, $
                ssq_cluster_spc, $
                counts_cl_spc, $
                l_result
   ;
   ; Now copy over all output keywords results.
   ;
   IF (PARAM_PRESENT(counts_cluster) EQ TRUE) THEN $
     counts_cluster=counts_cl_spc
      IF (PARAM_PRESENT(means_cluster) EQ TRUE) THEN $
        means_cluster=transpose(means_clstr_spc) ; NOTE: transpose()
      IF (PARAM_PRESENT(ssq_cluster) EQ TRUE) THEN $
        ssq_cluster=ssq_cluster_spc
   ;
   ; Return.
   ;
   RETURN, l_result
END



PRO WGFS_GETDIR, wid, junk

  COMMON wgfileselectionStuff, topShell, buttons, kwds
  COMMON wgfileselection_output, theDir
   theDir = dgpGetValue(wid)
   status = WwSetValue(topShell, /Close)

END


PRO WGFS_CANCEL, wid, which

  COMMON wgfileselectionStuff, topShell, buttons, kwds
   status = WwSetValue(topShell, /Close)
END


PRO WGFS_HELP, wid, which

   HELP, /Contents

END


PRO WGFS_SHOWIT, wid, which


COMMON wgfileselectionStuff, topShell, buttons, kwds
   fsel = WwFileSelection(buttons(0), 'WGFS_GETDIR', 'WGFS_CANCEL', $
        'WGFS_HELP', Dir=kwds.dir, File=kwds.file, Pattern=kwds.pattern, $
        Position=[100,300], Title=kwds.title, Background=kwds.back, $
        Font=kwds.font, MSfont=kwds.font, Foreground=kwds.fore)
   ; some OSes dont move the topShell off the screen. On Linux we can just 
   ; unrealize the topShell and are left with WgFilesSelection. On Solaris,
   ; the best we can do is make the topShell nonsensitive and small
   IF !Version.OS EQ "Linux" THEN status = WtSet(topShell, /Unrealize) $
        ELSE status = WtSet(topShell, /Nonsensitive) 

END


PRO WgFileSelection, path, Dir=dir, File=file, Parent=parent, $
     Pattern=pattern, Position=position, Shell=shell,$
     Title=title, Background=background, Font=font, Foreground=foreground
COMMON wgfileselectionStuff, topShell, buttons, kwds
COMMON wgfileselection_output, theDir
   ; get all keywords to pass to WwFileSelection called from WGFS_SHOWIT
   IF KEYWORD_SET(Dir) THEN dir2 = dir $
      ELSE BEGIN
         CD, Current=pwd
         dir2 = pwd
      ENDELSE
   IF KEYWORD_SET(File) THEN file2 = file ELSE file2 = 0
   IF KEYWORD_SET(Pattern) THEN pattern2 = pattern ELSE pattern2 = 0 
   IF KEYWORD_SET(Position) THEN position2 = position ELSE position2 = 0
   IF KEYWORD_SET(Title) THEN  title2 = title ELSE title2 = 'WgFileSelection'
   IF KEYWORD_SET(Background) THEN back2 = background ELSE back2 = 0
   IF KEYWORD_SET(Font) THEN font2 = font ELSE font2 = 0
   IF KEYWORD_SET(Foreground) THEN fore2 = foreground ELSE fore2 = 0
   kwds = {, dir:dir2, file:file2, pattern:pattern2, position:position2, $
             Title:title2, back:back2, font:font2, fore:fore2}

   ; The underlying routine WwFileSelection is a pop up widget and therefore,
   ; as documented, it cannot have topShell or mainWorkarea as its parent.
   ; So, create the mainWorkarea, but move it off of the screen so we just
   ; see the pop up WwFileSelection widget
   IF N_ELEMENTS(parent) EQ 0 THEN BEGIN
      topShell = WwInit('WgFileSelection','WgFileSelection', mainWorkarea, $
           Position=[-100,-100], Width=1, Height=1, Font=font2, MSfont=font2)
      loop = 1
   ENDIF ELSE BEGIN
      topShell = WwMainWindow(parent, mainWorkarea, Position=[-100,-100], $
           Font=font2, MSfont=font2)
      loop = 0
   ENDELSE
   ; The button ID associated with junkBox is what will serve as the pop up
   ; widget WwFileSelection's parent
   junkBox = WwButtonBox(mainWorkarea, 'junk', 'WGFS_SHOWIT', $
        Buttons=buttons, Font=font2, MSfont=font2)

   status = WwSetValue(topShell, /Display)
   ; call WGFS_SHOWIT to show the WwFileSelection widget
   WGFS_SHOWIT, buttons(0), -9
   theDir = ''

   IF loop THEN WwLoop
   path = theDir
   shell = topShell
END

;
; $Id: defroi.pro,v 1.6 1997/12/19 18:56:58 landers Exp $
;
FUNCTION MyRoi, Sx, Sy, Xverts, Yverts, X0=x0, Y0=y0, Zoom=ZOOM, $
                 Noregion=Noregion

   COMMON Colors, orig_red, orig_green, orig_blue, red, green, blue

   ON_ERROR, 2 ;Return to caller if error

   nc = !D.table_size           ;# of colors available
   IF nc EQ 0 THEN message, 'defroi.static_device', /Lookup
   IF N_ELEMENTS(red) LE 0 THEN BEGIN ;Define bw table?
   red = indgen(nc) & green = red & blue = red & ENDIF

   IF N_ELEMENTS(x0)   LE 0 THEN x0 = 0
   IF N_ELEMENTS(y0)   LE 0 THEN y0 = 0
   IF N_ELEMENTS(zoom) LE 0 THEN zoom = 1
      
      ;;
      ;; For X device do graphics_function, not write_mask.
      ;;
      
   device, get_graphics_function = old_mask
   
   device, set_graphics_function = 6 ; GXxor function
   

Again:
   n = 0
   PRINT, STRLOOKUP('defroi.instructions1') ; 'Left button to mark point'
   PRINT, STRLOOKUP('defroi.instructions2') ; 'Middle button to erase previous point'
   PRINT, STRLOOKUP('defroi.instructions3') ; 'Right button to close region'
   maxpnts = 1000               ;max # of points.
   xverts = intarr(maxpnts)     ;arrays
   yverts = intarr(maxpnts)
   xprev = -1
   yprev = -1
   done  = 0
   
   REPEAT BEGIN
      
      !Mouse.button = 0
      cursor, xx, yy, 0, /Device ;get x,y, no wait, device coords.
      
      CASE !Mouse.button OF

         ;;
         ;; MB1 is used to add a vertex to the polygon.
         ;;
         
         1 : BEGIN
            
            xx = (xx - x0) / zoom ;To image coords
            yy = (yy - y0) / zoom
            
            IF (xx GE 0) AND (xx LT sx) AND (yy GE 0) AND (yy LT sy) AND $
              ((xx NE xprev) OR (yy NE yprev)) THEN BEGIN
               
               xprev = xx
               yprev = yy
               
               IF n GE (maxpnts-1) THEN BEGIN
                  print, '' + STRLOOKUP('defroi.too_many')
                  n = n-1
               ENDIF
               
               n = n + 1
               
               xverts(n-1) = xx ;In pixel coords
               yverts(n-1) = yy

               IF n GT 1 THEN BEGIN
                  
                   px = xverts(n-2:n-1) * zoom + x0
                   py = yverts(n-2:n-1) * zoom + y0
                   
                   c = !P.Color
                   
                   plots, px, py, /Device, /NoClip, Color = c
               ENDIF
               
            ENDIF
            
         END                    ;; !Mouse.Button EQ 1

         ;;
         ;; MB2 is used to remove the last vertex from the list.
         ;;
         
         2 : BEGIN

            ;; Wait til user releases the button so that this only happens
            ;; on mouse button up events.  Otherwise, two vertices get
            ;; removed for each click of MB2.

            WHILE !Err EQ 2 DO Cursor, foo, bar, 0, /Device
            
            IF n GT 0 THEN n = n - 1
            IF n GT 0 THEN BEGIN
               
               px = xverts(n-1:n) * zoom + x0
               py = yverts(n-1:n) * zoom + y0

               c = !P.color
               plots, px, py, /Device, /NoClip, Color = c
               
               WAIT, 0.1         ;dont erase too quickly
               
            ENDIF
            
         END                    ;; !Mouse.Button EQ 2

         ;;
         ;; MB3 is used to complete the region definition.
         ;;
         
         4 : done = 1

         ;;
         ;; Ignore chorded mouse button events.
         ;;
         
         ELSE : ; do nothing
         
      ENDCASE

   ENDREP UNTIL (done NE 0)

   IF n LT 3 THEN BEGIN
      PRINT, STRLOOKUP('defroi.too_few')
      GOTO, again
   ENDIF
   
   xverts = xverts(0:n-1)       ; truncate
   yverts = yverts(0:n-1)
   
   px = [xverts(0), xverts(n-1)] * zoom + x0
   py = [yverts(0), yverts(n-1)] * zoom + y0
   
   c = !P.Color

   plots, px, py, /Device, Color = c  ; Complete the polygon
   
   IF !Order NE 0 THEN yverts = sy-1-yverts ;Invert Y?

   device, set_graphics_function = old_mask
   
 
   IF KEYWORD_SET(noregion) THEN a = 0 $
     ELSE a = polyfillv(xverts, yverts, sx, sy) ; get subscripts inside area

   RETURN, a

END ;; DEFROI

Function Pad, Si$, Nd$


On_Error, 2
If N_Params() Ne 2 Then $
    Message, '2 Parameters Are Required: Value And # Of Characters.'

Type = Size(Si$, /Type)
If Type Gt 5 And Type Ne 7 Then $
    Message, 'Only Simple Data Types Cad Be Padded'
If Type Ne 7 Then $
        S = Strtrim(Long(Si$), 2) $
Else	S = Si$

Num = N_Elements(S)
Z = Strarr(Num)

If N_Elements(Nd$) Eq 1 Then $
        Nd = Replicate(Nd$(0), Num) $
Else    Nd = Nd$

If N_Elements(Nd) Ne Num Then $
    Message, 'Number Of Digits Array Must Be Same Size As Value Array'

For I = 0,Num-1 Do $
    If Nd(I) Gt Strlen(S(I)) Then $
            Z(I) = Strjoin(Replicate('0', Nd(I)-Strlen(S(I)))) $
    Else    Z(I) = ''

Return, Z + S
End

;
; $Id: myradiobox.pro,v 1.15 2000/05/04 19:07:46 thaux Exp $
;             (c) Copyright Visual Numerics, Inc., 1992
;                     Boulder, Colorado, U.S.A.
;
;                        All Rights Reserved.
;
;+
; NAME:	myradioBox
; PURPOSE:
; Creates an array of toggle/check box buttons with labels with specified 
; number of columns or rows.
; Implementation:
;	- container widget is horizontal/vertical RowColumn/RubberTile
;		- name: radio
;	- buttons are toggleButton/checkBox
;		- name: toggle_n
; CALLING SEQUENCE:
;   container=myradioBox(parent, labels, callback
;				[,{/HORIZONTAL | /VERTICAL},MEASURE=num
;			         ,SPACING=num,BORDER=num
;                                ,FOREGROUND=color,BACKGROUND=color
;                                ,BASECOLOR=color,MSFONT=font])
;                                ,RIGHT[=wid],BOTTOM[=wid],LEFT[=wid],TOP[=wid]
;                                ,/Form,CENTER=[<positions>]
;                                ,Layout_name=<string>, Name=<string>
;                                ,POSITION=<x,y>,TOGGLES=<wid array>]
;                                ,{/ALIGNLEFT | /ALIGNRIGHT})
; INPUT:
;	- parent
;	- array of labels
;	- callback
; OUTPUT:
;	- container widget
; KEYWORDS:
;	- orientation: /HORIZONTAL, /VERTICAL
;       - container: /FORM - use form as container (default is RowColumn) 
;	- measure (#cols or rows): MEASURE=<number>
;	- children spacing: SPACING=<number>
;	- container and children border width: BORDER=<number>
;	- indicator type: /ONEOFMANY, /NOFMANY
;	- colors: FOREGROUND,BACKGROUND,BASECOLOR=<colorname>
;	- font: MSFONT=<fontname>
;	- attachments: RIGHT[=wid],BOTTOM[=wid],LEFT[=wid],TOP[=wid]
;	- centering: CENTER=[<positions>]
;       - position: POSITION=<x,y>
;       - layout widget name: LAYOUT_NAME=<string>
;       - radio box widget name: NAME=<string>
;	- return array of toggle button widget ids - TOGGLE
;       - alignment: /ALIGNLEFT, /ALIGNRIGHT (default is align center)
;
; CALLBACK PARAMETERS:
;	- toggle button widget id
;	- index of toggle buttons changed (1-n)
;
; RESTRICTIONS:
;-
PRO myradioBoxResetCB, wid, toggles
   IF WtGet(wid, /Value) THEN BEGIN
      i = 0
      WHILE i LT N_ELEMENTS(toggles) DO BEGIN
         IF wid NE toggles(i) THEN BEGIN
            IF WtGet(toggles(i), /Value) THEN $
              status = WtSet(toggles(i), Value = [0, 1])
         ENDIF
         i = i + 1
      ENDWHILE
   ENDIF
END

FUNCTION myradioBox, parent, labels, callback, $
                     Horizontal=Horizontal, Vertical=Vertical, $
                     Measure=Measure, Spacing=Spacing, Border=Border, $
                     Oneofmany=Oneofmany, Nofmany=Nofmany, $
                     Foreground=Foreground, Background=Background, $
                     Basecolor=Basecolor, Font=Font, MSFont=MSFont, $
                     Form=Form, Left=Left, Top=Top, $
                     Right=Right, Bottom=Bottom, $
                     Position=Position, Center=Center, $
                     Layout_name=Layout_name, Name=Name, $
                     AlignLeft=AlignLeft, AlignRight=AlignRight, $
                     Toggles=Toggles

   ON_ERROR_GOTO, myradioBox_Fail
   
@wtwacclasses
@wtwacconsts
   
   IF N_ELEMENTS(parent) EQ 0 THEN BEGIN
      WwError, 'Null Parent', /Fatal
   ENDIF ELSE IF parent EQ 0 THEN BEGIN
      WwError, 'Null Parent', /Fatal
   ENDIF

   IF KEYWORD_SET(Layout_name) THEN l_name = Layout_name ELSE l_name = 'radio'
   
   num_items = N_ELEMENTS(labels)
   w = parent

   IF num_items EQ 0 THEN BEGIN
      IF NOT KEYWORD_SET(Name) THEN $
         WwError, 'Either LABELS parameter or NAME keyword must be used', $
                  /Fatal $
      ELSE num_items = N_ELEMENTS(Name)
   ENDIF

   IF (num_items EQ 0) THEN BEGIN
      WwError, 'Some parameters are missing.', /Cont
      RETURN, 0
   ENDIF
   
   label_text = STRARR(num_items)
   FOR n = 0, num_items-1 DO BEGIN
      use_resource=0
      IF n GE N_ELEMENTS(labels) THEN use_resource=1 $
      ELSE IF STRLEN(labels(n)) EQ 0 THEN use_resource=1
      IF use_resource EQ 1 THEN BEGIN
         IF KEYWORD_SET(Name) AND n LT N_ELEMENTS(Name) THEN $
            b_name = name(n) $
         ELSE b_name = 'button_' + strtrim(string(n), 2)
         b_name_rspec = WtResource(Spec=parent) + '.' + l_name + '.' $
                                   + b_name + '.labelString'
         label_text(n) = WtResource(b_name_rspec, default='')
         IF strlen(label_text(n)) EQ 0 THEN BEGIN
            b_name_rspec = WtResource(spec = parent) + '.' + l_name + '.' $
              + b_name + '.text'
            label_text(n) = WtResource(b_name_rspec, default = '')
         ENDIF
      ENDIF ELSE label_text(n) = labels(n)
   ENDFOR

   Toggles = LONARR(num_items)

   IF KEYWORD_SET(Nofmany) THEN itype = WacAUTOCHECKBOX $
     ELSE itype = WacAUTORADIOBUTTON
      
   IF PARAM_PRESENT(Border) THEN $
     bwidth = Border $
     ELSE IF itype EQ WacAUTOCHECKBOX THEN $
	   bwidth = 0 $
	 ELSE $
	   bwidth = 2

   IF KEYWORD_SET(Measure) THEN ncols = Measure ELSE ncols = 1
   IF PARAM_PRESENT(Spacing) THEN bspace = Spacing ELSE bspace = 2
      
   IF KEYWORD_SET(Vertical) THEN $
     oriented = WacVERTICAL $
     ELSE IF KEYWORD_SET(Horizontal) THEN $
     oriented = WacHORIZONTAL $
     ELSE $
     oriented = WacHORIZONTAL

   IF KEYWORD_SET(Center) THEN BEGIN
      base = 100
      args = {, fractionBase:base}
      fract = FLOAT(base) / FLOAT(num_items*2+1)
      w = WtCreate(l_name, formWidgetClass, parent, args)
   ENDIF ELSE IF KEYWORD_SET(Form) THEN BEGIN
      spaceval = 0
      IF PARAM_PRESENT(Spacing) THEN spaceval = Spacing
      IF oriented EQ WacHORIZONTAL THEN $
        args = {, horizontalSpacing:spaceval} $
        ELSE $
        args = {, verticalSpacing:spaceval}
      w = WtCreate(l_name, formWidgetClass, parent, args)
   ENDIF ELSE BEGIN
      ARGexe = 'args = {,sameWidth:TRUE,sameHeight:TRUE'
	  ARGexe = ARGexe + ',horizontalJustify:XtJustifyFill,verticalJustify:XtVJustifyFill'
	  ARGexe = ARGexe + ',borderWidth:bwidth'
	  ARGexe = ARGexe + ',verticalSpacing:bspace,horizontalSpacing:bspace'
      IF (itype EQ WacAUTORADIOBUTTON) OR $
        (itype EQ WacRADIOBUTTON) THEN $
        ARGexe = ARGexe + ',radioBehavior:TRUE'
      ARGexe = ARGexe + '}'
      status = execute(ARGexe)
   
      w = WtCreate(l_name, arrayWidgetClass, parent, args)
   ENDELSE
   IF w EQ 0 THEN WwError, "Unable to create radio box", /Fatal
      
   IF KEYWORD_SET(Basecolor) THEN BEGIN
      args = {, background:Basecolor}
      status = WtSet(w, args)
   ENDIF ELSE IF KEYWORD_SET(Background) THEN BEGIN
      args = {, background:Background}
      status = WtSet(w, args)
   ENDIF
   IF KEYWORD_SET(Foreground) THEN BEGIN
      args = {, foreground:Foreground}
      status = WtSet(w, args)
   ENDIF
      
   layout = w

   wrap = (num_items + ncols - 1) / ncols
   ncol = -1 & nrow = -1
   
   FOR i = 0, num_items-1 DO BEGIN
      name = 'button_' + strtrim(string(i), 2)
      label = label_text(i)
      
      ARGexe = 'args = {,text:label,borderWidth:0'
      ARGexe = ARGexe + ',buttonType: itype'
      IF KEYWORD_SET(Background) THEN $
        ARGexe = ARGexe + ',background:Background'
      IF KEYWORD_SET(Foreground) THEN $
        ARGexe = ARGexe + ',foreground:Foreground'
      IF KEYWORD_SET(MSFont) THEN $
        ARGexe = ARGexe + ',font:MSFont'
      IF KEYWORD_SET(AlignLeft) THEN $
        ARGexe = ARGexe + ',justify:XtJustifyLeft'
      IF KEYWORD_SET(AlignRight) THEN $
        ARGexe = ARGexe + ',justify:XtJustifyRight'
      
      IF KEYWORD_SET(Center) THEN BEGIN
         ltpos = FIX((i*2+1)*fract)
         rbpos = FIX((i*2+2)*fract)
         IF N_ELEMENTS(Center) GE (i+1)*2 THEN BEGIN
            ltpos = FIX(FLOAT(Center(i*2))*FLOAT(base)/100.)
            rbpos = FIX(FLOAT(Center(i*2+1))*FLOAT(base)/100.)
         ENDIF
         IF oriented EQ WacVERTICAL THEN BEGIN
            ARGexe = ARGexe+',topAttachment:WacATTACH_POSITION,'
            ARGexe = ARGexe+'bottomAttachment:WacATTACH_POSITION,'
            ARGexe = ARGexe+'topPosition:ltpos, bottomPosition:rbpos'
         ENDIF ELSE BEGIN
            ARGexe = ARGexe+',leftAttachment:WacATTACH_POSITION,'
            ARGexe = ARGexe+'rightAttachment:WacATTACH_POSITION,'
            ARGexe = ARGexe+'leftPosition:ltpos, rightPosition:rbpos'
         ENDELSE
      ENDIF ELSE IF KEYWORD_SET(Form) THEN BEGIN
         attach = 'WacATTACH_FORM'
         attachwid = '0'
         IF oriented EQ WacVERTICAL THEN BEGIN
            IF KEYWORD_SET(Bottom) THEN attachdir = 'bottom' $
              ELSE attachdir = 'top'
         ENDIF ELSE BEGIN
            IF KEYWORD_SET(Right) THEN attachdir = 'right' $
              ELSE attachdir = 'left'
         ENDELSE
         spaceval = '0'
         IF i GT 0 THEN BEGIN
            attach = 'WacATTACH_WIDGET'
            attachwid = 'Toggles(i-1)'
         ENDIF
         IF PARAM_PRESENT(Spacing) THEN spaceval = 'Spacing'
         ARGexe = ARGexe+','+attachdir+'Attachment:'+ $
           attach+','+attachdir+'Widget:'+attachwid+ $
           ','+attachdir+'Offset:'+spaceval
         IF oriented EQ WacVERTICAL THEN BEGIN
            IF KEYWORD_SET(Right) THEN $
              ARGexe = ARGexe + ',rightAttachment:WacATTACH_FORM'
            IF KEYWORD_SET(Left) THEN $
              ARGexe = ARGexe + ',leftAttachment:WacATTACH_FORM'
         ENDIF ELSE BEGIN
            IF KEYWORD_SET(Top) THEN $
              ARGexe = ARGexe + ',topAttachment:WacATTACH_FORM'
            IF KEYWORD_SET(Bottom) THEN $
              ARGexe = ARGexe + ',bottomAttachment:WacATTACH_FORM'
         ENDELSE
         IF i EQ num_items -1 THEN BEGIN
            attachopp = ' '
            CASE attachdir OF
               'bottom': IF KEYWORD_SET(Top) THEN attachopp = 'top'
               'top':    IF KEYWORD_SET(Bottom) THEN attachopp = 'bottom'
               'left':   IF KEYWORD_SET(Right) THEN attachopp = 'right'
               'right':  IF KEYWORD_SET(Left) THEN attachopp = 'left'
            ENDCASE
            IF attachopp NE ' ' THEN $
              ARGexe = ARGexe + ','+attachopp+'Attachment:WacATTACH_FORM'
        ENDIF
      ENDIF ELSE BEGIN
         IF (i MOD wrap) EQ 0 THEN BEGIN
            IF oriented EQ WacHORIZONTAL THEN BEGIN
               nrow = nrow + 1
               ncol = -1
            ENDIF ELSE BEGIN
               ncol = ncol + 1
               nrow = -1
            ENDELSE
         ENDIF
         ARGexe = ARGexe + ',sameWidth:TRUE,sameHeight:TRUE,horizontalJustify:XtJustifyFill,verticalJustify:XtVJustifyFill'
         IF oriented EQ WacHORIZONTAL THEN $
           ncol = ncol + 1 $
           ELSE $
           nrow = nrow + 1
         ARGexe = ARGexe + ',row:nrow,column:ncol'

      ENDELSE

      ARGexe = ARGexe + '}'
      status = execute(ARGexe)
      
      button = WtCreate(name, buttonWidgetClass, layout, args)
      IF button EQ 0 THEN WwError, "Unable to create radio button", /Fatal
      
      Toggles(i) = button
      
      IF N_ELEMENTS(callback) GT 0 THEN $
        status = WtAddCallback(button, 'valueChangedCallback', $
                               callback, i+1, /NOPARAMS)
   ENDFOR
   
   IF (KEYWORD_SET(Center) OR KEYWORD_SET(Form)) AND $
     KEYWORD_SET(Oneofmany) THEN BEGIN
      FOR i = 0, num_items-1 DO $
        status = WtAddCallback(Toggles(i), 'valueChangedCallback', $
                               'myradioBoxResetCB', Toggles, /NOPARAMS)
   ENDIF

   IF KEYWORD_SET(Center) THEN BEGIN
      IF oriented EQ WacVERTICAL THEN BEGIN
         IF NOT KEYWORD_SET(Top) THEN Top = 1
         IF NOT KEYWORD_SET(Bottom) THEN Bottom = 1
      ENDIF ELSE BEGIN
         IF NOT KEYWORD_SET(Left) THEN Left = 1
         IF NOT KEYWORD_SET(Right) THEN Right = 1
      ENDELSE
   ENDIF
   WwAttachWidget, parent, w, Left = Left, Top = Top, $
     Right = Right, Bottom = Bottom, Position = Position
   
   RETURN, w

myradioBox_Fail:
   RETURN, 0L
END

;
; $Id: dgpgetvalue.pro,v 1.18 2001/06/07 22:50:03 estewart Exp $
;             (c) Copyright Visual Numerics, Inc., 1995
;                     Boulder, Colorado, U.S.A.
;
;                        All Rights Reserved.
;
FUNCTION dgpGetValue, wid, $
                     Destroyed=Destroyed, $
                     Shown=Shown, $
                     Sensitive=Sensitive, $
                     Children=Children, $
                     Parent=Parent, $
                     Class=Class, $
                     Userdata=Userdata, $
                     Size=WHSize, $
                     Position=Position, $
                     Exists=Exists
; NAME:	dgpGetValue
; PURPOSE:
; INPUT:
;	- widget
; OUTPUT:
;	- widget value
; KEYWORDS:
;	- is widget being destroyed?: /DESTROYED
;       - get children of the widget: /CHILDREN
;       - get parent of the widget: /PARENT
;       - get class of the widget: /CLASS
;	- is widget shown?: /SHOW
;	- is widget sensitive?: /SENSITIVE
;	- get user data: /USERDATA
;       - get width and height: /SIZE
;       - get x and y location of the upper-left corner: /POSITION
;	- does the widget exist?: /EXISTS
; RESTRICTIONS:
; REVISION HISTORY:
;-
   ON_ERROR_GOTO, dgpGetValue_Fail
   q = !Quiet
      
@wtwacclasses
@wtwacconsts

   widget = wid
   IF KEYWORD_SET(Exists) THEN BEGIN
      q = !Quiet
      !Quiet = 3
      status = WtGet(widget, /CLASS)
      !Quiet = q
      IF status EQ 0 THEN RETURN, 0 $
      ELSE RETURN, 1
   ENDIF
   IF N_ELEMENTS(widget) EQ 0 THEN BEGIN
      WwError, 'Null Widget', /Cont
      RETURN, 0
   ENDIF ELSE IF widget EQ 0 THEN BEGIN
      WwError, 'Null Widget', /Cont
      RETURN, 0
   ENDIF

   IF KEYWORD_SET(Destroyed) THEN BEGIN
      value = WtGet(widget, /DESTROYED)
      RETURN, value
   ENDIF ELSE IF KEYWORD_SET(Shown) THEN BEGIN
      IF WtGet(widget, /SHELL) THEN $
        value = WtGet(widget, /REALIZED) $
        ELSE $
        value = WtGet(widget, /MANAGED)
      RETURN, value
   ENDIF ELSE IF KEYWORD_SET(Sensitive) THEN BEGIN
      value = WtGet(widget, /SENSITIVE)
      RETURN, value
   ENDIF ELSE IF KEYWORD_SET(Children) THEN BEGIN
      value = WtGet(widget, /Child)
      RETURN, value
   ENDIF ELSE IF KEYWORD_SET(Parent) THEN BEGIN
      value = WtGet(widget, /Parent)
      RETURN, value
   ENDIF ELSE IF KEYWORD_SET(Class) THEN BEGIN
      value = WtGet(widget, /Class)
      RETURN, value
   ENDIF ELSE IF KEYWORD_SET(Userdata) THEN BEGIN
      class = WtGet(widget, /CLASS)
      IF class EQ topLevelShellWidgetClass OR $
        class EQ dialogShellWidgetClass $
        THEN BEGIN
         children = WtGet(widget, /CHILD)
         IF (N_ELEMENTS(children) GT 0) AND (children(0) NE 0) THEN $
           value = WtGet(children(0), /USERDATA) $
           ELSE $
           value = WtGet(widget, /USERDATA)
      ENDIF ELSE value = WtGet(widget, /USERDATA)
      RETURN, value
   ENDIF ELSE IF KEYWORD_SET(WHSIZE) THEN BEGIN
      value = INTARR(2)
      value(0) = WtGet(widget, 'width')
      value(1) = WtGet(widget, 'height')
      RETURN, value
   ENDIF ELSE IF KEYWORD_SET(POSITION) THEN BEGIN
      value = INTARR(2)
      value(0) = WtGet(widget, 'x')
      value(1) = WtGet(widget, 'y')
      RETURN, value
   ENDIF

   cl = WtGet(widget, /CLASS)

   CASE 1 OF

      cl EQ buttonWidgetClass : BEGIN
         itype = WtGet(widget, 'buttonType')
         CASE 1 OF
            (itype AND WacAUTOCHECKBOX) NE 0 OR $
              (itype AND WacAUTORADIOBUTTON) NE 0 OR $
              (itype AND WacCHECKBOX) NE 0 OR $
              (itype AND WacRADIOBUTTON) NE 0 : $
              value = WtGet(widget, 'set')
            (itype AND WacPUSHBUTTON) NE 0 : value = WtGet(widget, 'text')
            (itype EQ WacBITMAPBUTTON) NE 0 : value = WtGet(widget, 'bitmap')
            ELSE: BEGIN
               WwError, 'Unknown button type', /Cont
               RETURN, 0
            END
         ENDCASE
      END

      cl EQ menuitemWidgetClass : BEGIN
         itype = WtGet(widget, 'menuitemType')
         CASE 1 OF
            itype EQ WacMENUTOGGLE: value = WtGet(widget, 'checked')
            itype EQ WacMenuString: value = WtGet(widget, 'text')
            itype EQ WacMenuBitmap: value = WtGet(widget, 'bitmap')
            itype EQ WacMenuSeparator OR itype EQ WacMenuTitle: value = 0
            ELSE: BEGIN
               WwError, 'Unknown menu item type', /Cont
               RETURN, 0
            END
         ENDCASE
      END

      cl EQ staticWidgetClass : BEGIN
         IF WtGet(widget, 'staticType') EQ WacSTATIC_ICON THEN $
           value = WtGet(widget, 'icon') $
           ELSE $
           value = WtGet(widget, 'text')
      END

      cl EQ topLevelShellWidgetClass OR cl EQ dialogShellWidgetClass: $
        BEGIN
         children = WtGet(widget, /CHILD)
         FOR i = 0, N_ELEMENTS(children)-1 DO BEGIN
            IF WtGet(children(i), /CLASS) EQ commandWidgetClass OR $
              WtGet(children(i), /CLASS) EQ selectionBoxWidgetClass THEN BEGIN
               widget = children(i)
               GOTO, gotchild
            ENDIF
         ENDFOR

 Gotchild:
         CASE WtGet(widget, /CLASS) OF
            commandWidgetClass: 		GOTO, CommandWidget
            selectionBoxWidgetClass:		GOTO, SelectionWidget
            ELSE:   WwError, 'Unknown widget class for shell widget', /Fatal
         ENDCASE
      END

      cl EQ commandWidgetClass: $
        BEGIN
CommandWidget:
         listBox = WtGet (widget, Child = WacDIALOG_HISTORY_LIST)
         itemCount = WtGet (listBox, 'itemCount')
         IF itemCount EQ 0 THEN value = '' ELSE $
           value = WtGet (listBox, 'items', Count = itemCount)
      END

      cl EQ scrolledWindowWidgetClass:$
        BEGIN
         widget = WtGet(widget, 'workWindow')
         CASE WtGet(widget, /CLASS) OF
            drawingAreaWidgetClass: 	GOTO, DrawingAreaWidget
            editWidgetClass:		GOTO, EditWidget
            ELSE: BEGIN
               hscb = WtGet(widget, 'horizontalScrollBar')
               vscb = WtGet(widget, 'verticalScrollBar')
               value = lonarr(2) & value(0) = 0 & value(1) = 0
               IF hscb NE 0 THEN value(0) = WtSet(hscb, 'value')
               IF vscb NE 0 THEN value(1) = WtSet(vscb, 'value')
            END
         ENDCASE
      END

      cl EQ drawingAreaWidgetClass: $
        BEGIN
DrawingAreaWidget:
         value = WtGet(widget, /WINDOW)
      END

      cl EQ fileSelectWidgetClass: $
        BEGIN
         value = WtGet(widget, 'file')
      END

      cl EQ listboxWidgetClass:$
        BEGIN
         count = WtGet(widget, 'selectedItemCount')
         IF count EQ 0 THEN value = '' ELSE $
           value = WtGet(widget, 'selectedItems', COUNT = count)
      END

      cl EQ scaleWidgetClass: $
        BEGIN
         value = WtGet(widget, /VALUE)
      END

      cl EQ scrollbarWidgetClass:  $
        BEGIN
         value = WtGet(widget, /VALUE)
      END

      cl EQ selectionBoxWidgetClass: $
        BEGIN
SelectionWidget:
         value = WtGet(widget, 'textString')
      END

      cl EQ promptBoxWidgetClass: $
        BEGIN
         value = WtGet(widget, 'textString')
      END

      cl EQ editWidgetClass : $
        BEGIN
EditWidget:
         value = WtGet(widget, 'text')
      END


      cl EQ xbaeMatrixWidgetClass: $
        BEGIN
	nrows=WtGet(widget,'rows')
	ncols=WtGet(widget,'columns')
        value = WtGet(widget, 'selectedCells', Nrows = nrows, Ncols = ncols)
      END

      cl EQ xvnPreviewWidgetClass: $
        BEGIN
        value = WtGet(widget, 'value')
      END

      cl EQ optionMenuWidgetClass: $
        BEGIN
         children = WtGet(widget, /Child)
         value = WtGet(widget, 'whichItem') + 1
      END

      ELSE: BEGIN
         WwError, 'Value NOT supported for this widget class',/Cont
         RETURN, 0
      END
   ENDCASE

   RETURN, value

dgpGetValue_Fail:
      !Quiet = q
   print, !err_string
   Print, "% error dgpGetValue"
   info, trace=tree
   pm, tree

   RETURN, 0L
END


pro Flow_Enhance

end
