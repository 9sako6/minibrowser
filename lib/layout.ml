type rect = {
  x : float;
  y : float;
  width : float;
  height : float;
}

type edge = {
  top : float;
  right : float;
  bottom : float;
  left : float;
}

type box = {
  content : rect;
  padding : edge;
  border : edge;
  margin : edge;
}

type t = Layout_box of Style.t

let create style = Layout_box style
