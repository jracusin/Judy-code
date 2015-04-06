;;==========================================================================
;;
;; Copyright 2008 Indiana University Research and Technology
;; Corporation Licensed under the Apache License, Version 2.0 (the
;; "License"); you may not use this file except in compliance with the
;; License.  You may obtain a copy of the License at
;; http://www.apache.org/licenses/LICENSE-2.0 Unless required by
;; applicable law or agreed to in writing, software distributed under
;; the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES
;; OR CONDITIONS OF ANY KIND, either express or implied.  See the
;; License for the specific language governing permissions and
;; limitations under the License
;;
;; Polywash is a procedure that draws a polygonal outline and washes
;; the interior with a color.  It relies on the coyote libraries from
;; dfanning.com.
;;
;; Author: M. Miller, mmiller3@iupui.edu
;; Original Revision: Nov 2005
;;
;; Inputs:
;;    x, y:   boundary points of polygon
;;
;; Optional parameters:
;;    alpha=alpha:    alpha blending for color wash (default = 0.8)
;;    color=color:    color name (handled by fsc_color)
;;    edges:          If set, draw edges
;;    background:     Background color index (wash all points except
;;                      those that match this)
;;    thick=thick:    Edge thickness
;;
;;==========================================================================
pro polywash, x, y, alpha=alpha, color=color, edges=edges, thick=thick
wID = !d.window

if n_elements(alpha) eq 0 then alpha=0.8

xmin = max([0,long(min(x))])
xmax = min([!d.x_size,long(max(x))])
ymin = max([0,long(min(y))])
ymax = min([!d.y_size,long(max(y))])
xsize = xmax-xmin
ysize = ymax-ymin
;print, xmin, xmax, xsize
;print, ymin, ymax, ysize

;; get a copy of what is in the window now:
w = float(tvrd(xmin, ymin, xsize, ysize, true=3))

;; Create a pixmap:
window, /free, /pixmap, xsize=xsize, ysize=ysize
pID = !d.window

;; erase with background color:
if n_elements(background) ne 1 then background = 0
erase, color=background

;; polyfill with foreground color:
if n_elements(color) ne 1 then color = 255
polyfill, x-xmin, y-ymin, color=color, /device

;; Get a copy of the pixmap
p3 = float(tvrd(true=3))

;; Clean up
wdelete, pID

;; Extract RGB components:
pR = p3[*,*,0]
pG = p3[*,*,1]
pB = p3[*,*,2]

wR = w[*,*,0]
wG = w[*,*,1]
wB = w[*,*,2]

;; Blend the polyfill image with the original image:
wpR = wR
wpG = wG
wpB = wB

;; Find the points in the polyfill mask that are not the same as the
;; background color:
tvlct, R, G, B, /get
points = where((pR ne R[background]) or (pG ne G[background]) or (pB ne B[background]))

;; If there are no points that don't match the background color, return:
if n_elements(points) eq 1 then if points eq -1 then return

;; Blend the polyfill mask with the original image:
wpR[points] = alpha*wR[points] + (1-alpha)*pR[points]
wpG[points] = alpha*wG[points] + (1-alpha)*pG[points]
wpB[points] = alpha*wB[points] + (1-alpha)*pB[points]

data = reform([[wpR],[wpG],[wpB]], xsize, ysize, 3)

;; Display the result
wset, wID
tv, data, xmin, ymin, true=3

if keyword_set(edges) then begin
    ;; Mark the edges:
    if n_elements(thick) eq 0 then thick=1
    plots, [x, x[0]], [y, y[0]], /device, color=color, thick=thick
endif

end
