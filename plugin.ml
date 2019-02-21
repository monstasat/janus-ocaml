type properties =
  { opaque_id : string
  ; token : string option
  }

type volume =
  { value : unit option
  ; timer : unit option
  }

type bitrate =
  { value : unit option
  ; bsnow : unit option
  ; bsbefore : unit option
  ; tsnow : unit option
  ; tsbefore : unit option
  ; timer : unit option
  }

type webrtc_stuff =
  { started : bool
  ; local_stream : unit option
  ; stream_external : bool
  ; remote_stream : unit option
  ; local_sdp : unit option
  ; media_constraints : unit option
  ; pc : unit option
  ; data_channel : unit option
  ; dtmf_sender : unit option
  ; trickle : bool
  ; ice_done : bool
  ; volume : volume
  }

type t =
  { id : int64
  }

let id (t : t) : int64 =
  t.id
