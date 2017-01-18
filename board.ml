open Color
open Player

type city = {name : string; connections : string list}
type route = {c0 : city; c1 : city; color : color; owner : player; length : int}


(* the type of a Ticket tr Ride board *)
type board = {cities : city list; routes : route list}


let vc = {name="Vancouver";connections=["Seattle";"Calgary"]}
let se = {name="Seattle";connections=["Vancouver";"Calgary";"Portland";"Helena"]}
let ca = {name="Calgary";connections=["Vancouver";"Seattle";"Helena";"Winnipeg"]}
let po = {name="Portland";connections=["Seattle";"Salt Lake City";
  "San Francisco"]}
let sf = {name="San Francisco";connections=["Portland";"Salt Lake City";
  "Los Angeles"]}
let la = {name="Los Angeles";connections=["San Francisco";"Las Vegas";"Phoenix";
  "El Paso"]}
let sl = {name="Salt Lake City";connections=["San Francisco";"Portland";
  "Las Vegas";"Helena";"Denver"]}
let hl = {name="Helena";connections=["Seattle";"Calgary";"Winnipeg";"Duluth";
  "Omaha";"Denver";"Salt Lake City"]}
let lv = {name="Las Vegas";connections=["Los Angeles";"Salt Lake City"]}
let ph = {name="Phoenix";connections=["Los Angeles";"Denver";"Santa fe";
  "El Paso"]}
let fe = {name="Santa fe";connections=["Phoenix";"Denver";"El Paso";
  "Oklahoma City"]}
let ep = {name="El Paso";connections=["Los Angeles";"Phoenix";"Santa fe";
  "Oklahoma City";"Dallas";"Houston"]}
let de = {name="Denver";connections=["Salt Lake City";"Helena";"Omaha";
  "Kansas City"; "Oklahoma City";"Santa fe";"Phoenix"]}
let wn = {name="Winnipeg";connections=["Calgary";"Helena";"Duluth";
  "Sault st. Marie"]}
let du = {name="Duluth";connections=["Helena";"Winnipeg";"Sault st. Marie";
  "Toronto";"Chicago";"Omaha"]}
let oc = {name="Oklahoma City";connections=["Denver";"Kansas City";"Little Rock";
  "Dallas";"El Paso";"Santa fe"]}
let om = {name="Omaha";connections=["Duluth";"Chicago";"Kansas City";"Denver";
  "Helena"]}
let kc = {name="Kansas City";connections=["Omaha";"Saint Louis";"Oklahoma City";
  "Denver"]}
let da = {name="Dallas";connections=["El Paso";"Oklahoma City";"Little Rock";
  "Houston"]}
let hu = {name="Houston";connections=["Dallas";"El Paso";"New Orleans"]}
let lr = {name="Little Rock";connections=["Oklahoma City";"Dallas";"New Orleans";
  "Nashville";"Saint Louis"]}
let ol = {name="New Orleans";connections=["Houston";"Little Rock";"Atlanta";
  "Miami"]}
let sm = {name="Sault st. Marie";connections=["Winnipeg";"Duluth";"Toronto";
  "Montreal"]}
let ci = {name="Chicago";connections=["Duluth";"Omaha";"Saint Louis";
  "Pittsburgh";"Toronto"]}
let st = {name="Saint Louis";connections=["Kansas City";"Chicago";"Pittsburgh";
  "Nashville";"Little Rock"]}
let tr = {name="Toronto";connections=["Sault st. Marie";"Duluth";"Chicago";
  "Pittsburgh";"Montreal"]} (*tr*)
let na = {name="Nashville";connections=["Saint Louis";"Little Rock";
  "Pittsburgh";"Atlanta";"Raleigh"]}(*na*)
let al = {name="Atlanta";connections=["Nashville";"Raleigh";"Charleston";
  "Miami";"New Orleans"]}
let pi = {name="Pittsburgh";connections=["Chicago";"Saint Louis";"Nashville";
  "Raleigh";"Washington";"New York";"Toronto"]}
let rl = {name="Raleigh";connections=["Nashville";"Atlanta";"Charleston";
  "Washington";"Pittsburgh"]}
let cr = {name="Charleston";connections=["Raleigh";"Atlanta";"Miami"]}
let mi = {name="Miami";connections=["New Orleans";"Atlanta";"Charleston"]}
let wa = {name="Washington";connections=["Raleigh";"Pittsburgh";"New York"]}
let ny = {name="New York";connections=["Washington";"Pittsburgh";"Montreal";
  "Boston"]}
let bo = {name="Boston";connections=["New York";"Montreal"]}
let mo = {name="Montreal";connections=["Sault st. Marie";"Toronto";"Boston";
  "New York"]}

let ab1 = {c0=vc;c1=se;color=Colorless;owner=None;length=1}
let ab2 = {c0=vc;c1=se;color=Colorless;owner=None;length=1}
let ac = {c0=vc;c1=ca;color=Colorless;owner=None;length=3}
let bc = {c0=se;c1=ca;color=Colorless;owner=None;length=4}
let bh = {c0=se;c1=hl;color=Yellow;owner=None;length=6}
let bd1 = {c0=se;c1=po;color=Colorless;owner=None;length=1}
let bd2 = {c0=se;c1=po;color=Colorless;owner=None;length=1}
let de1 = {c0=po;c1=sf;color=Green;owner=None;length=5}
let de2 = {c0=po;c1=sf;color=Pink;owner=None;length=5}
let dg = {c0=po;c1=sl;color=Blue;owner=None;length=6}
let ef1 = {c0=sf;c1=la;color=Yellow;owner=None;length=3}
let ef2 = {c0=sf;c1=la;color=Pink;owner=None;length=3}
let eg1 = {c0=sf;c1=sl;color=White;owner=None;length=5}
let eg2 = {c0=sf;c1=sl;color=Orange;owner=None;length=5}
let fi = {c0=la;c1=lv;color=Colorless;owner=None;length=2}
let fj = {c0=la;c1=ph;color=Colorless;owner=None;length=3}
let fl = {c0=la;c1=ep;color=Black;owner=None;length=6}
let ig = {c0=lv;c1=sl;color=Orange;owner=None;length=3}
let gm1 = {c0=sl;c1=de;color=Red;owner=None;length=3}
let gm2 = {c0=sl;c1=de;color=Yellow;owner=None;length=3}
let gh = {c0=sl;c1=hl;color=Pink;owner=None;length=3}
let jm = {c0=ph;c1=de;color=White;owner=None;length=5}
let jk = {c0=ph;c1=fe;color=Colorless;owner=None;length=3}
let jl = {c0=ph;c1=ep;color=Colorless;owner=None;length=3}
let lk = {c0=ep;c1=fe;color=Colorless;owner=None;length=2}
let km = {c0=fe;c1=de;color=Colorless;owner=None;length=2}
let ch = {c0=ca;c1=hl;color=Colorless;owner=None;length=4}
let cn = {c0=ca;c1=wn;color=White;owner=None;length=6}
let hn = {c0=hl;c1=wn;color=Blue;owner=None;length=4}
let ho = {c0=hl;c1=du;color=Orange;owner=None;length=6}
let hq = {c0=hl;c1=om;color=Red;owner=None;length=5}
let hm = {c0=hl;c1=de;color=Green;owner=None;length=4}
let mq = {c0=de;c1=om;color=Pink;owner=None;length=4}
let mr1 = {c0=de;c1=kc;color=Black;owner=None;length=4}
let mr2 = {c0=de;c1=kc;color=Orange;owner=None;length=4}
let mp = {c0=de;c1=oc;color=Red;owner=None;length=4}
let kp = {c0=fe;c1=oc;color=Blue;owner=None;length=3}
let lp = {c0=ep;c1=oc;color=Yellow;owner=None;length=5}
let ls = {c0=ep;c1=da;color=Red;owner=None;length=4}
let lt = {c0=ep;c1=hu;color=Green;owner=None;length=6}
let ts1 = {c0=hu;c1=da;color=Colorless;owner=None;length=1}
let ts2 = {c0=hu;c1=da;color=Colorless;owner=None;length=1}
let tv = {c0=hu;c1=ol;color=Colorless;owner=None;length=2}
let sp1 = {c0=da;c1=oc;color=Colorless;owner=None;length=2}
let sp2 = {c0=da;c1=oc;color=Colorless;owner=None;length=2}
let su = {c0=da;c1=lr;color=Colorless;owner=None;length=2}
let pr1 = {c0=oc;c1=kc;color=Colorless;owner=None;length=2}
let pr2 = {c0=oc;c1=kc;color=Colorless;owner=None;length=2}
let pu = {c0=oc;c1=lr;color=Colorless;owner=None;length=2}
let ry1 = {c0=kc;c1=st;color=Blue;owner=None;length=2}
let ry2 = {c0=kc;c1=st;color=Pink;owner=None;length=2}
let rq1 = {c0=kc;c1=om;color=Colorless;owner=None;length=1}
let rq2 = {c0=kc;c1=om;color=Colorless;owner=None;length=1}
let qo1 = {c0=om;c1=du;color=Colorless;owner=None;length=2}
let qo2 = {c0=om;c1=du;color=Colorless;owner=None;length=2}
let qx = {c0=om;c1=ci;color=Blue;owner=None;length=4}
let nw = {c0=wn;c1=sm;color=Colorless;owner=None;length=6}
let no = {c0=wn;c1=du;color=Black;owner=None;length=4}
let ow = {c0=du;c1=sm;color=Colorless;owner=None;length=3}
let oz = {c0=du;c1=tr;color=Pink;owner=None;length=6}
let ox = {c0=du;c1=ci;color=Red;owner=None;length=3}
let xz = {c0=ci;c1=tr;color=White;owner=None;length=4}
let xc_11 = {c0=ci;c1=pi;color=Orange;owner=None;length=3}
let xc_12 = {c0=ci;c1=pi;color=Black;owner=None;length=3}
let xy1 = {c0=ci;c1=st;color=White;owner=None;length=2}
let xy2 = {c0=ci;c1=st;color=Green;owner=None;length=2}
let yu = {c0=st;c1=lr;color=Colorless;owner=None;length=2}
let ya_1 = {c0=st;c1=na;color=Colorless;owner=None;length=2}
let yc_1 = {c0=st;c1=pi;color=Green;owner=None;length=5}
let ua_1 = {c0=lr;c1=na;color=White;owner=None;length=3}
let vu = {c0=ol;c1=lr;color=Green;owner=None;length=3}
let vb_11 = {c0=ol;c1=al;color=Yellow;owner=None;length=4}
let vb_12 = {c0=ol;c1=al;color=Orange;owner=None;length=4}
let vf_1 = {c0=ol;c1=mi;color=Red;owner=None;length=6}
let f_1b_1 = {c0=mi;c1=al;color=Blue;owner=None;length=5}
let f_1e_1 = {c0=mi;c1=cr;color=Pink;owner=None;length=4}
let e_1b_1 = {c0=cr;c1=al;color=Colorless;owner=None;length=2}
let e_1d_1 = {c0=cr;c1=rl;color=Colorless;owner=None;length=2}
let d_1b_11 = {c0=rl;c1=al;color=Colorless;owner=None;length=2}
let d_1b_12 = {c0=rl;c1=al;color=Colorless;owner=None;length=2}
let d_1a_1 = {c0=rl;c1=na;color=Black;owner=None;length=3}
let d_1c_1 = {c0=rl;c1=pi;color=Colorless;owner=None;length=2}
let d_1g_11 = {c0=rl;c1=wa;color=Colorless;owner=None;length=2}
let d_1g_12 = {c0=rl;c1=wa;color=Colorless;owner=None;length=2}
let b_1a_1 = {c0=al;c1=na;color=Colorless;owner=None;length=1}
let a_1c_1 = {c0=na;c1=pi;color=Yellow;owner=None;length=4}
let c_1z = {c0=pi;c1=tr;color=Colorless;owner=None;length=2}
let c_1g_1 = {c0=pi;c1=wa;color=Colorless;owner=None;length=2}
let c_1h_11 = {c0=pi;c1=ny;color=White;owner=None;length=2}
let c_1h_12 = {c0=pi;c1=ny;color=Green;owner=None;length=2}
let wz = {c0=sm;c1=tr;color=Colorless;owner=None;length=2}
let wj_1 = {c0=sm;c1=mo;color=Black;owner=None;length=5}
let g_1h_11 = {c0=wa;c1=ny;color=Orange;owner=None;length=2}
let g_1h_12 = {c0=wa;c1=ny;color=Black;owner=None;length=2}
let h_1i_11 = {c0=ny;c1=bo;color=Yellow;owner=None;length=2}
let h_1i_12 = {c0=ny;c1=bo;color=Red;owner=None;length=2}
let j_1i_11 = {c0=mo;c1=bo;color=Colorless;owner=None;length=2}
let j_1i_12 = {c0=mo;c1=bo;color=Colorless;owner=None;length=2}
let j_1h_1 = {c0=mo;c1=ny;color=Blue;owner=None;length=3}
let j_1z = {c0=mo;c1=tr;color=Colorless;owner=None;length=3}


(* [empty] is the empty board where for all routes have no owner *)
let new_board = {cities=[vc;se;ca;po;sf;la;sl;hl;lv;ph;fe;ep;de;wn;du;oc;om;kc;da;hu;lr;ol;sm;ci;st;tr;
  na;al;pi;rl;cr;mi;wa;ny;bo;mo]; routes = [ab1;ab2;ac;bc;bh;bd1;bd2;
  de1;de2;dg;ef1;ef2;eg1;eg2;fi;fj;fl;ig;gm1;gm2;gh;jm;jk;jl;lk;km;ch;cn;hn;ho;
  hq;hm;mq;mr1;mr2;mp;kp;lp;ls;lt;ts1;ts2;tv;sp1;sp2;su;pr1;pr2;pu;ry1;ry2;rq1;
  rq2;qo1;qo2;qx;nw;no;ow;oz;ox;xz;xc_11;xc_12;xy1;xy2;yu;ya_1;yc_1;ua_1;vu;
  vb_11;vb_12;vf_1;f_1b_1;f_1e_1;e_1b_1;e_1d_1;d_1b_11;d_1b_12;d_1a_1;d_1c_1;
  d_1g_11;d_1g_12;b_1a_1; a_1c_1;c_1z;c_1g_1;c_1h_11;c_1h_12;wz;wj_1;g_1h_11;
  g_1h_12;h_1i_11;h_1i_12;j_1i_11;j_1i_12;j_1h_1;j_1z]}

(* Helper function of [cnames_list ()]:
 * Retrieves string names of all cities and returns as a list *)
let rec get_names clist =
   match clist with
   | [] -> []
   | h::t -> h.name::(get_names t)

(* Returns list of string names of all cities on board *)
let cnames_list () =
  get_names (new_board.cities)

(* a list of routes between two cities on a board *)
let routes_between c0 c1 b =
	let rec rl c0 c1 i l =
		match l with
		|[] -> i
		|h::t when h.c0=c0 && h.c1=c1 || h.c0=c1 && h.c1=c0 ->
			rl c0 c1 (h::i) t
		|h::t -> rl c0 c1 i t
	in
	rl c0 c1 [] b.routes

let routes_between_string s0 s1 b =
	let rec rl c0 c1 i l =
		match l with
		|[] -> i
		|h::t when h.c0.name=c0 && h.c1.name=c1 || h.c0.name=c1 && h.c1.name=c0 ->
			rl c0 c1 (h::i) t
		|h::t -> rl c0 c1 i t
	in
	rl s0 s1 [] b.routes


(* determines the shortest path needed tr connect two cities on a board *)
let shortest_path p c0 c1 b =
	let rec update_connections p (c,d,r) b l cl =
		let rec update_connection w rn l ct =
			match l with
			|[] -> ()
			|(cl,dl,rl)::t when cl.name = ct -> if w >= !dl then ()
				else (dl := w; rl := rn); ()			(*update distance/route, return list*)
			|(cl,dl,rl)::t ->  update_connection w rn t ct
		in
		match cl with
		|[] -> ()
		|h::t -> let rb = routes_between_string c.name h b |> List.hd in
			if rb.owner = None then (update_connection ((!d)+(rb.length)) (rb::(!r)) l h;
				update_connections p (c,d,r) b l t)
			else if rb.owner = p then (update_connection !d (rb::!r) l h;
				update_connections p (c,d,r) b l t)
			else if (List.length (routes_between_string c.name h b)) = 2 then
				let rb2 = List.nth (routes_between_string c.name h b) 1 in
				if rb2.owner = None then (update_connection (!d+rb2.length) (rb2::!r) l h;
				update_connections p (c,d,r) b l t)
				else if rb2.owner = p then (update_connection !d (rb2::!r) l h;
				update_connections p (c,d,r) b l t)
				else ()
			else ()
	in
	let rec step p c0 c1 b col dol rol l=
		let (c,d,r) = List.hd l
		in
		if (c,!d,!r) = (col,dol,rol) then (0,[])
		else if c=c1 then (!d,!r)
		else (update_connections p (c,d,r) b l c.connections;
			List.fast_sort (fun (c0,d0,r0) (c1,d1,r1) -> compare !d0 !d1) (List.tl l)
			|>step p c0 c1 b c !d !r)

	in
	step p c0 c1 b vc 100000000 [] ((List.map (fun x -> if x=c0 then (x, ref 0, ref [])
		else (x, ref 50000000, ref [])) b.cities) |>
			List.fast_sort (fun (c0,d0,r0) (c1,d1,r1) ->
				let c = compare !d0 !d1 in if c <> 0 then c else
					compare (List.length !r0) (List.length !r1)))

(* [dfs p c0 c1 b visited connections] traverses the board [b] and returns true
 * if the player [p] owns a path on the board [b] from the city [c0] tr the city
 * [c1], otherwise, [dfs p c0 c1 b visited connections] returns false.
 * Preconditions: [visited] is the list of names of cities that have already
 * been visited in the traversal, [connections] is the list of names of
 * unvisited cities that are one route away from the city [c0] *)
let rec dfs p c0 c1 b (visited: string list ref) connections =
  match connections with
  | [] -> false
  | h::t when h = c1.name -> (let route = List.find (fun r -> r.c0 = c0 &&
                                         r.c1 = c1 || r.c0 = c1 && r.c1 = c0)
                                         b.routes in
                             if route.owner = p then true
                             else false)
  | h::t -> (let city = List.find (fun c -> c.name = h) b.cities in
            let route = List.find (fun r -> c0 = c0 &&  c1 = c1 ||
                        c0 = c1 && c1 = c0) b.routes in
            let _ = if not (List.exists (fun x -> x = h) !visited) then
                      visited := h::!visited
                    else
                      () in
            let cities = List.filter (fun c -> not (List.exists (fun x -> x = c)
                                               !visited)) city.connections in
            if route.owner = p then
              dfs p city c1 b visited cities ||  dfs p c0 c1 b visited t
            else
              dfs p c0 c1 b visited t)

(* determines if a player controls a route between two cities on a
 * board *)
let has_connected p c0 c1 b =
  dfs p c0 c1 b (ref [c0.name]) c0.connections

(* attempts tr claim a route on a board *)
let claim_route p r b =
  if List.mem r b.routes then
    if r.owner = None then
      let r1 = {r with owner = p} in
      let routes1 = List.filter (fun x -> x.c0<>r.c0 || x.c1<>r.c1) b.routes in
      (true, {b with routes = r1::routes1})
    else
      (false, b)
  else
    (false, b)


