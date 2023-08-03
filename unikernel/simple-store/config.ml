open Mirage

(* uTCP *)

let tcpv4v6_direct_conf () =
  let packages_v = Key.pure [ package "utcp" ~sublibs:[ "mirage" ] ] in
  let connect _ modname = function
    | [_random; _mclock; _time; ip] ->
      Fmt.str "Lwt.return (%s.connect %s)" modname ip
    | _ -> failwith "direct tcpv4v6"
  in
  impl ~packages_v ~connect "Utcp_mirage.Make"
    (random @-> mclock @-> time @-> ipv4v6 @-> (tcp: 'a tcp typ))

let direct_tcpv4v6
    ?(clock=default_monotonic_clock)
    ?(random=default_random)
    ?(time=default_time) ip =
  tcpv4v6_direct_conf () $ random $ clock $ time $ ip

let remote =
  let doc = Key.Arg.info ~doc:"Remote Git repository." [ "r"; "remote" ] in
  Key.(create "remote" Arg.(required string doc))

let port =
  let doc = Key.Arg.info ~doc:"The port where to listen." [ "p"; "port" ] in
  Key.(create "port" Arg.(opt int 8080 doc))

type hash = Hash
type git = Git

let hash = typ Hash
let sha1 = impl ~packages:[ package "digestif" ] "Digestif.SHA1" hash
let git = typ Git

let git_impl path =
  let packages = [ package "git" ~min:"3.10.0" ~max:"3.14.0" ] in
  let keys = match path with
    | None -> []
    | Some path -> [ Key.v path ] in
  let connect _ modname _ = match path with
    | None ->
        Fmt.str
          {ocaml|%s.v (Fpath.v ".") >>= function
                 | Ok v -> Lwt.return v
                 | Error err -> Fmt.failwith "%%a" %s.pp_error err|ocaml}
          modname modname
    | Some key ->
        Fmt.str
          {ocaml|( match Option.map Fpath.of_string %a with
                 | Some (Ok path) -> %s.v path
                 | Some (Error (`Msg err)) -> failwith err
                 | None -> %s.v (Fpath.v ".") ) >>= function
                 | Ok v -> Lwt.return v
                 | Error err -> Fmt.failwith "%%a" %s.pp_error err|ocaml}
          Key.serialize_call (Key.v key) modname modname modname in
  impl ~packages ~keys ~connect "Git.Mem.Make" (hash @-> git)

let minigit =
  foreign "Unikernel.Make"
    ~packages:[
      package "ptime" ;
      package "hxd" ~sublibs:[ "core"; "string" ] ;
    ]
    ~keys:[ Key.v remote ; Key.v port ]
    (stackv4v6 @-> git @-> git_client @-> job)

let git path hash = git_impl path $ hash

(* User space *)

let ssh_key =
  let doc = Key.Arg.info ~doc:"The private SSH key." [ "ssh-key" ] in
  Key.(create "ssh_seed" Arg.(opt (some string) None doc))

let ssh_password =
  let doc = Key.Arg.info ~doc:"The private SSH password." [ "ssh-password" ] in
  Key.(create "ssh-password" Arg.(opt (some string) None doc))

let ssh_authenticator =
  let doc = Key.Arg.info ~doc:"SSH public key of the remote Git repository." [ "ssh-authenticator" ] in
  Key.(create "ssh_authenticator" Arg.(opt (some string) None doc))

let https_authenticator =
  let doc = Key.Arg.info ~doc:"TLS authenticator of the remote Git repository." [ "https-authenticator" ] in
  Key.(create "https_authenticator" Arg.(opt (some string) None doc))

let stack =
  let ethernet = etif default_network in
  let arp = arp ethernet in
  let i4 = create_ipv4 ethernet arp in
  let i6 = create_ipv6 default_network ethernet in
  let i4i6 = create_ipv4v6 i4 i6 in
  let tcpv4v6 = direct_tcpv4v6 i4i6 in
  let ipv4_only = Key.ipv4_only () in
  let ipv6_only = Key.ipv6_only () in
  direct_stackv4v6 ~tcp:tcpv4v6 ~ipv4_only ~ipv6_only default_network ethernet arp i4 i6

let git_client =
  let dns = generic_dns_client stack in
  let git = mimic_happy_eyeballs stack dns (generic_happy_eyeballs stack dns) in
  let tcp = tcpv4v6_of_stackv4v6 stack in
  merge_git_clients (git_tcp tcp git)
    (merge_git_clients (git_ssh ~key:ssh_key ~password:ssh_password ~authenticator:ssh_authenticator tcp git)
      (git_http ~authenticator:https_authenticator tcp git))

let git     = git None sha1

let () =
  register "minigit"
    [ minigit $ stack $ git $ git_client ]
