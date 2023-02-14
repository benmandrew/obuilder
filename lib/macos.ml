(* Extensions to the Os module for macOS *)
open Lwt.Syntax
open Os

let ( / ) = Filename.concat

let user_exists ~user =
  let+ s = pread ["sudo"; "dscl"; "."; "list"; "/Users"] in
  List.exists (Astring.String.equal user) (Astring.String.cuts ~sep:"\n" s)

(* Generates a new MacOS user called `<prefix><uid>' *)
let create_new_user ~username ~home_dir ~uid ~gid =
  let* exists = user_exists ~user:username in
  if exists then Lwt.return_ok ()
  else
    let user = "/Users" / username in
    let pp s ppf = Fmt.pf ppf "[ Mac ] %s\n" s in
    let dscl = [ "dscl"; "."; "-create"; user ] in
    sudo_result ~pp:(pp "UniqueID") (dscl @ [ "UniqueID"; uid ]) >>!= fun _ ->
    sudo_result ~pp:(pp "PrimaryGroupID") (dscl @ [ "PrimaryGroupID"; gid ]) >>!= fun _ ->
    sudo_result ~pp:(pp "UserShell") (dscl @ [ "UserShell"; "/bin/bash" ]) >>!= fun _ ->
    sudo_result ~pp:(pp "NFSHomeDirectory") (dscl @ [ "NFSHomeDirectory"; home_dir ])

let delete_user ~user =
  let* exists = user_exists ~user in
  match exists with
    | false ->
      Log.info (fun f -> f "Not deleting %s as they don't exist" user);
      Lwt_result.return ()
    | true ->
      let user = "/Users" / user in
      let pp s ppf = Fmt.pf ppf "[ Mac ] %s\n" s in
      let delete = ["dscl"; "."; "-delete"; user ] in
        sudo_result ~pp:(pp "Deleting") delete

let rec kill_users_processes ~uid =
  let pp _ ppf = Fmt.pf ppf "[ PKILL ]" in
  let delete = ["pkill"; "-9"; "-U"; string_of_int uid ] in
  let* t = sudo_result ~pp:(pp "PKILL") delete in
    match t with
    | Ok () -> kill_users_processes ~uid
    | Error (`Msg _) ->
      Log.info (fun f -> f "pkill all killed");
      Lwt.return ()

let rm ~directory =
  let pp _ ppf = Fmt.pf ppf "[ RM ]" in
  let delete = ["rm"; "-r"; directory ] in
  let* t = sudo_result ~pp:(pp "RM") delete in
    match t with
    | Ok () -> Lwt.return ()
    | Error (`Msg m) ->
      Log.warn (fun f -> f "Failed to remove %s because %s" directory m);
      Lwt.return ()

let get_tmpdir ~user =
  ["sudo"; "-u"; user; "-i"; "getconf"; "DARWIN_USER_TEMP_DIR"]
