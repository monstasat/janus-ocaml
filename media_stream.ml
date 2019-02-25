open Js_of_ocaml

class type mediaTrackSettings =
  object

    (** A DOMString indicating the current value of the deviceId property.
          The device ID is a origin-unique string identifying the source of
          the track; this is usually a GUID. This value is specific to the
          source of the track's data and is not usable for setting constraints;
          it can, however, be used for initially selecting media when calling
          MediaDevices.getUserMedia(). *)
    method deviceId : Js.js_string Js.t Js.prop

    (** A DOMString indicating the current value of the groupId property.
          The group ID is a browsing session-unique string identifying the
          source group of the track. Two devices (as identified by the deviceId)
          are considered part of the same group if they are from the same
          physical device. For instance, the audio input and output devices for
          the speaker and microphone built into a phone would share the same
          group ID, since they're part of the same physical device.
          The microphone on a headset would have a different ID, though.
          This value is specific to the source of the track's data and is not
          usable for setting constraints; it can, however, be used for initially
          selecting media when calling MediaDevices.getUserMedia(). *)
    method groupId : Js.js_string Js.t Js.prop

    (* AUDIO *)

    (** A Boolean which indicates the current value of the autoGainControl
          property, which is true if automatic gain control is enabled and is
          false otherwise. *)
    method audioGainControl : bool Js.t Js.prop

    (** A long integer value indicating the current value of the channelCount
          property, specifying the number of audio channels present on the track
          (therefore indicating how many audio samples exist in each audio
          frame). This is 1 for mono, 2 for stereo, and so forth. *)
    method channelCount : int Js.t Js.prop

    (** A Boolean indicating the current value of the echoCancellation
          property, specifying true if echo cancellation is enabled,
          otherwise false. *)
    method echoCancellation : bool Js.t Js.prop

    (** A double-precision floating point value indicating the current
          value of the latency property, specifying the audio latency,
          in seconds. Latency is the amount of time which elapses between
          the start of processing the audio and the data being available to
          the next stop in the audio utilization process.
          This value is a target value; actual latency may vary to some extent
          for various reasons. *)
    method latency : float Js.t Js.prop

    (** A Boolean which indicates the current value of the noiseSupression
          property, which is true if noise suppression is enabled and is false
          otherwise. *)
    method noiseSuppression : bool Js.t Js.prop

    (** A long integer value indicating the current value of the sampleRate
          property, specifying the sample rate in samples per second of the
          audio data. Standard CD-quality audio, for example, has a sample rate
          of 41,000 samples per second. *)
    method sampleRate : int Js.t Js.prop

    (** A long integer value indicating the current value of the sampleSize
          property, specifying the linear size, in bits, of each audio sample.
          CD-quality audio, for example, is 16-bit, so this value would be 16
          in that case. *)
    method sampleSize : int Js.t Js.prop

    (** A double-precision floating point value indicating the current value
          of the volume property, specifying the volume level of the track.
          This value will be between 0.0 (silent) to 1.0
          (maximum supported volume). *)
    method volume : float Js.t Js.prop

    (* VIDEO *)

    (** A double-precision floating point value indicating the current value
          of the aspectRatio property, specified precisely to 10 decimal places.
          This is the width of the image in pixels divided by its height in
          pixels. Common values include 1.3333333333 (for the classic televison
          4:3 "standard" aspect ratio, also used on tablets such as Apple's
          iPad), 1.7777777778 (for the 16:9 high-definition widescreen aspect
          ratio), and 1.6 (for the 16:10 aspect ratio common among widescreen
          computers and tablets). *)
    method aspectRatio : float Js.t Js.prop

    (** A DOMString indicating the current value of the facingMode property,
          specifying the direction the camera is facing. The value will be one
          of:
          - "user"
          A camera facing the user (commonly known as a "selfie cam"), used for
          self-portraiture and video calling.
          - "environment"
          A camera facing away from the user (when the user is looking at the
          screen). This is typically the highest quality camera on the device,
          used for general photography.
          - "left"
          A camera facing toward the environment to the user's left.
          - "right"
          A camera facing toward the environment to the user's right. *)
    method facingMode : Js.js_string Js.t Js.prop

    (** A double-precision floating point value indicating the current
          value of the frameRate property, specifying how many frames of
          video per second the track includes. If the value can't be determined
          for any reason, the value will match the vertical sync rate of the
          device the user agent is running on. *)
    method frameRate : float Js.t Js.prop

    (** A long integer value indicating the current value of the height
          property, specifying the height of the track's video data in
          pixels. *)
    method height : int Js.t Js.prop

    (** A long integer value indicating the current value of the width
          property, specifying the width of the track's video data in pixels. *)
    method width : int Js.t Js.prop

    (** A DOMString indicating the current value of the resizeMode property,
          specifying the mode used by the user agent to derive the resolution
          of the track. The value will be one of:
          - "none"
          The track has the resolution offered by the camera, its driver or
          the OS.
          - "crop-and-scale"
          The track's resolution might be the result of the user agent using
          cropping or downscaling from a higher camera resolution. *)
    method resizeMode : Js.js_string Js.t Js.prop

    (* SHARED SCREEN TRACKS *)

    (** A DOMString which indicates whether or not the mouse cursor is being
          included in the generated stream and under what conditions.
          Possible values are:
          - always
          The mouse is always visible in the video content of the
          {domxref("MediaStream"), unless the mouse has moved outside the area
          of the content.
          - motion
          The mouse cursor is always included in the video if it's moving, and
          for a short time after it stops moving.
          - never
          The mouse cursor is never included in the shared video. *)
    method cursor : Js.js_string Js.t Js.prop

    (** A DOMString which specifies the type of source the track contains;
          one of:
          - application
          The stream contains all of the windows of the application chosen
          by the user rendered into the one video track.
          - browser
          The stream contains the contents of a single browser tab selected
          by the user.
          - monitor
          The stream's video track contains the entire contents of one or
          more of the user's screens.
          - window
          The stream contains a single window selected by the user for
          sharing. *)
    method displaySurface : Js.js_string Js.t Js.prop

    (** A Boolean value which, if true, indicates that the video contained in
          the stream's video track contains a background rendering context,
          rather than a user-visible one. This is false if the video being
          captured is coming from a foreground (user-visible) source. *)
    method logicalSurface : bool Js.t Js.prop

  end


class type mediaTrackConstraints =
  object
    
  end

class type mediaStreamTrack =
  object

    (** Returns a DOMString containing a unique identifier (GUID)
          for the track; it is generated by the browser. *)
    method id : Js.js_string Js.t Js.readonly_prop

    (** A Boolean whose value of true if the track is enabled,
          that is allowed to render the media source stream;
          or false if it is disabled, that is not rendering the media source
          stream but silence and blackness. If the track has been disconnected,
          this value can be changed but has no more effect. *)
    method enabled : bool Js.t Js.prop

    (** A string that may be used by the web application to provide
          a hint as to what type of content the track contains to guide
          how it should be treated by API consumers. *)
    method contentHint : Js.js_string Js.t Js.prop

    (** Returns a Boolean value which is true if the track is isolated;
          that is, the track cannot be accessed by the document that owns
          the MediaStreamTrack. This happens when the peerIdentity property
          is set, or if the track comes from a cross-origin source. *)
    method isolated : bool Js.t Js.readonly_prop

    (** Returns a DOMString set to "audio" if the track is an audio track
          and to "video", if it is a video track. It doesn't change if the
          track is deassociated from its source. *)
    method kind : Js.js_string Js.t Js.readonly_prop

    (** Returns a DOMString containing a user agent-assigned label that
          identifies the track source, as in "internal microphone". The string
          may be left empty and is empty as long as no source has been
          connected. When the track is deassociated from its source, the label
          is not changed. *)
    method label : Js.js_string Js.t Js.readonly_prop

    (** Returns a Boolean value indicating whether the track is unable to
          provide media data due to a technical issue. *)
    method muted : bool Js.t Js.readonly_prop

    (** Returns a Boolean value with a value of true if the track is
          readonly (such a video file source or a camera that settings can't
          be modified), false otherwise. *)
    method readonly : bool Js.t Js.readonly_prop

    (** Returns an enumerated value giving the status of the track.
          This will be one of the following values:

          "live" which indicates that an input is connected and does
          its best-effort in providing real-time data. In that case, the
          output of data can be switched on or off using the enabled attribute.

          "ended" which indicates that the input is not giving any more data and
          will never provide new data.*)
    method readyState : Js.js_string Js.t Js.readonly_prop

    method applyConstraints : mediaTrackConstraints Js.t -> unit Js.meth

    method clone : mediaStreamTrack Js.t Js.meth

    method getCapabilities : unit Js.meth

    method getConstraints : mediaTrackConstraints Js.t Js.meth

    method getSettings : mediaTrackSettings Js.t Js.meth

    method stop : unit Js.meth

  end

class type mediaStream =
  object
    inherit Dom_html.eventTarget

    (** A DOMString containing 36 characters denoting a universally
        unique identifier (UUID) for the object.*)
    method id : Js.js_string Js.t Js.prop

    (** A Boolean value that returns true if the MediaStream is active,
        or false otherwise. *)
    method active : bool Js.t Js.prop

    (** An EventHandler containing the action to perform when an addtrack
        event is fired when a new MediaStreamTrack object is added. *)
    method onaddtrack
           : ('b Js.t, Dom_html.event Js.t) Dom_html.event_listener
               Js.writeonly_prop

    (** An EventHandler containing the action to perform when a removetrack
        event is fired when a  MediaStreamTrack object is removed from it. *)
    method onremovetrack
           : ('b Js.t, Dom_html.event Js.t) Dom_html.event_listener
               Js.writeonly_prop

    method addTrack : mediaStreamTrack Js.t -> unit Js.meth

    method clone : mediaStream Js.t Js.meth

    method getAudioTracks : mediaStreamTrack Js.t Js.js_array Js.t Js.meth

    method getTrackById : Js.js_string Js.t ->
                          mediaStreamTrack Js.t Js.opt Js.meth

    method getTracks : mediaStreamTrack Js.t Js.js_array Js.t Js.meth

    method getVideoTracks : mediaStreamTrack Js.t Js.js_array Js.t Js.meth

    method removeTrack : mediaStreamTrack Js.t -> unit Js.meth

  end
