type point  = { x : float; y : float; z : float }
type dpoint = { dx : float; dy : float; dz : float }
type physical_object = { position : point; velocity : dpoint }

let move p dp =
  {x= p.x +. dp.dx; y= p.y +. dp.dy; z = p.z +. dp.dz} ;;

let next obj = {
  position = {
    x = obj.position.x +. obj.velocity.dx; 
    y = obj.position.y +. obj.velocity.dy;
    z = obj.position.z +. obj.velocity.dz;
  };
  velocity= obj.velocity
};;

let will_collide_soon p1 p2 =
  let p1next = next p1 and p2next = next p2 in 
  let   dx = (p1next.position.x -. p2next.position.x) ** 2.0
    and dy = (p1next.position.y -. p2next.position.y) ** 2.0
    and dz = (p1next.position.z -. p2next.position.z) ** 2.0 in 
  let dist = (dx +. dy +. dz)**0.5 in
    Printf.printf "p1next: %f, %f, %f\n" p1next.position.x p1next.position.y p1next.position.z;
    Printf.printf "p2next: %f, %f, %f\n" p2next.position.x p2next.position.y p2next.position.z;
    Printf.printf "dxyz: %f, %f, %f\n" dx dy dz;
    Printf.printf "distance: %f\n" dist;
    dist < 2.0
  ;;