open Js_of_ocaml
open Webrtc
open Plugin_types
open Utils
open Adapter
open Lwt_result.Infix

let send_sdp (t : t) :
      (_RTCSessionDescriptionInit Js.t, string) Lwt_result.t =
  Log.ign_info "Sending offer/answer SDP...";
  match t.webrtc.local_sdp, t.webrtc.pc with
  | None, _ ->
     let s = "Local SDP instance is invalid, not sending anything..." in
     Log.ign_warning s;
     Lwt_result.fail s
  | _, None ->
     let s = "Peer Connection is invalid, not sending anything..." in
     Log.ign_warning s;
     Lwt_result.fail s
  | Some _, Some pc ->
     match Js.Opt.to_option pc##.localDescription with
     | None ->
        let s = "localDescription is empty, not sending anything..." in
        Log.ign_warning s;
        Lwt_result.fail s
     | Some (ld : _RTCSessionDescription Js.t)->
        let (my_sdp : _RTCSessionDescriptionInit Js.t) = Js.Unsafe.obj [||] in
        my_sdp##._type := ld##._type;
        my_sdp##.sdp := ld##.sdp;
        if not t.webrtc.trickle
        then (Js.Unsafe.coerce my_sdp)##.trickle := Js._false;
        t.webrtc <- { t.webrtc with local_sdp = Some my_sdp };
        Lwt_result.return my_sdp

let send_trickle_candidate (t : t)
      (candidate : Api.Msg.candidate Js.t)
    : (unit, string) Lwt_result.t =
  is_connected_lwt t.is_connected
  >>= fun () ->
  let (request : Api.Msg.req Js.t) =
    Api.Msg.make_req
      ?token:t.token
      ?apisecret:t.apisecret
      ~candidate:candidate
      ~janus:"trickle"
      ~transaction:(String.random 12)
      () in
  (* TODO add websockets *)
  Lwt.Infix.(
    Api.http_call ~meth:`POST
      ~with_credentials:t.with_credentials
      ~body:request
      (Printf.sprintf "%s/%d/%d" t.server t.session_id t.id)
    >|= function
    | Error e -> Error (Api.error_to_string e)
    | Ok rsp ->
       match Api.Msg.check_msg ~key:"ack" rsp with
       | Error e -> Error e
       | Ok rsp ->
          Log.ign_debug "Candidate sent!";
          Log.ign_debug ~inspect:rsp "";
          Ok ())

let on_ice_conn_state_change (t : t) (_ : #Dom_html.event Js.t) : bool Js.t =
  begin match t.webrtc.pc with
  | None -> ()
  | Some (pc : _RTCPeerConnection Js.t) ->
     let (state : ice_connection_state) =
       ice_connection_state_of_string
       @@ Js.to_string pc##.iceConnectionState in
     Option.iter (fun f -> f state) t.on_ice_state
  end;
  Js._true

let handle_end_of_candidates (t : t) : (unit, string) Lwt_result.t =
  Log.ign_info "End of candidates";
  t.webrtc <- { t.webrtc with ice_done = true };
  if t.webrtc.trickle
  then
    (* Notify end of candidates *)
    let (candidate : Api.Msg.candidate Js.t) = Js.Unsafe.obj [||] in
    candidate##.completed := Js._true;
    send_trickle_candidate t candidate
  else
    (* No trickle, time to send the complete SDP (including all candidates) *)
    (* FIXME no coercion *)
    Lwt_result.(send_sdp t >|= fun _ -> ())

let on_ice_candidate (t : t)
      (e : _RTCPeerConnectionIceEvent Js.t) : bool Js.t =
  let eoc = Js.string "endOfCandidates" in
  Log.ign_debug ~inspect:e "";
  let (t : (unit, string) Lwt_result.t) =
    match get_browser (), Js.Opt.to_option e##.candidate with
    | _, None -> handle_end_of_candidates t
    | "edge", Some c when c##.candidate##indexOf eoc > 0 ->
       handle_end_of_candidates t
    | _, Some (c : _RTCIceCandidate Js.t) ->
       (* JSON.stringify doesn't work on some WebRTC objects anymore
				  See https://code.google.com/p/chromium/issues/detail?id=467366 *)
       if not t.webrtc.trickle then Lwt.return_ok () else (
         let (candidate : Api.Msg.candidate Js.t) = Js.Unsafe.obj [||] in
         candidate##.candidate := c##.candidate;
         candidate##.sdpMid := c##.sdpMid;
         candidate##.sdpMLineIndex := c##.sdpMLineIndex;
         send_trickle_candidate t candidate) in
  (* XXX what to do with the thread? *)
  Lwt.ignore_result t;
  Js._true

let on_track (t : t) (e : _RTCTrackEvent Js.t) : bool Js.t =
  Log.ign_info "Handling Remote Track";
  Log.ign_debug ~inspect:e "";
  match Js.to_array e##.streams with
  | [||] -> Js._true
  | arr ->
     let (stream : mediaStream Js.t) = arr.(0) in
     Option.iter (fun f -> f stream) t.on_remote_stream;
     if Js.Optdef.test (Js.Unsafe.coerce e)##.track
        && Js.Optdef.test (Js.Unsafe.coerce e)##.track##.onended
     then (
       Log.ign_info ~inspect:e##.track "Adding onended callback to track:";
       let onended = fun (e : mediaStreamTrack Dom.event Js.t) ->
         Log.ign_info ~inspect:e "Remote track removed:";
         match t.webrtc.remote_stream, Js.Opt.to_option e##.target with
         | None, _ | _, None -> Js._true
         | Some s, Some target ->
            s##removeTrack target;
            Option.iter (fun f -> f s) t.on_remote_stream;
            Js._true in
       e##.track##.onended := Dom.handler onended);
     Js._true

let init (t : t) (pc : _RTCPeerConnection Js.t) : unit =
  Log.ign_info_f
    "Preparing local SDP and gathering candidates (trickle=%b)"
    t.webrtc.trickle;
  pc##.oniceconnectionstatechange := Dom.handler (on_ice_conn_state_change t);
  pc##.onicecandidate := Dom.handler (on_ice_candidate t);
  pc##.ontrack := Dom.handler (on_track t)

let create (t : t) : _RTCPeerConnection Js.t =
  let (pc_config : _RTCConfiguration Js.t) = Js.Unsafe.obj [||] in
  pc_config##.iceServers := Js.array @@ Array.of_list t.ice_servers;
  Option.iter (fun (x : ice_transport_policy) ->
      let v = Js.string @@ ice_transport_policy_to_string x in
      pc_config##.iceTransportPolicy := v) t.ice_transport_policy;
  Option.iter (fun (x : bundle_policy) ->
      let v = Js.string @@ bundle_policy_to_string x in
      pc_config##.bundlePolicy := v) t.bundle_policy;
  (* For Chrome versions before 72, we force a plan-b semantic,
     and unified-plan otherwise *)
  if check_browser ~browser:"chrome" ()
  then (
    let sdp_semantics =
      if check_browser ~ver:72 ~ver_cmp:(<) ()
      then "plan-b" else "unified-plan" in
    (Js.Unsafe.coerce pc_config)##.sdpSemantics := Js.string sdp_semantics);
  (* If this is Edge, enable BUNDLE explicitly *)
  if check_browser ~browser:"edge" ()
  then (
    let v = Js.string @@ bundle_policy_to_string Max_bundle in
    pc_config##.bundlePolicy := v);
  let pc_constraints = Js.Unsafe.(
      let dtls = obj [|"DtlsSrtpKeyAgreement", inject Js._true|] in
      let ipv6 = if not t.ipv6 then None
                 else obj [|"googIPv6", inject Js._true|] in
      let optional =
        List.map obj t.rtc_constraints
        |> List.cons dtls
        |> List.cons_maybe ipv6
        |> Array.of_list
        |> Js.array in
      obj [|"optional", inject optional|]) in
  Log.ign_info "Creating PeerConnection";
  Log.ign_debug ~inspect:pc_constraints "";
  let (pc_constr : (_RTCConfiguration Js.t ->
                    'a Js.t -> (* XXX constraints (legacy) *)
                    _RTCPeerConnection Js.t) Js.constr) =
    Js.Unsafe.global##.RTCPeerConnection in
  let pc = new%js pc_constr pc_config pc_constraints in
  Log.ign_debug ~inspect:pc "";
  pc
