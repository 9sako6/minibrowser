val render :
  html:string ->
  css:string ->
  ?window_width:int ->
  ?window_height:int ->
  unit ->
  unit

val render_on_png :
  html:string ->
  css:string ->
  ?window_width:int ->
  ?window_height:int ->
  png_file_name:string ->
  unit ->
  unit
